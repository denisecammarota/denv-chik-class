# Base run of all classifiers and for all states (new)
# Classifiers: RF, XGBoost, AdaBoost, CatBoost
# Main-text run: all features (incl. month, lat, long) ###

import os ###
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import joblib
import optuna
import random
from sklearn.metrics import precision_recall_curve, auc, f1_score, confusion_matrix
from sklearn.metrics import precision_score, recall_score, average_precision_score
from sklearn.metrics import roc_auc_score
from sklearn.metrics import brier_score_loss, log_loss ###
from sklearn.calibration import calibration_curve ###
from sklearn.linear_model import LogisticRegression ###
from sklearn.isotonic import IsotonicRegression ###
from sklearn.model_selection import StratifiedKFold, train_test_split
from sklearn.ensemble import RandomForestClassifier
from catboost import CatBoostClassifier
from sklearn.ensemble import AdaBoostClassifier
from sklearn.metrics import confusion_matrix
import seaborn as sns
import xgboost as xgb

np.random.seed(123)

RUN = 'full' ### output folder: 'full' (main text) or 'partial' (Appendix B)
DROP_COLS = ['CHIK', 'SG_UF', 'CRITERIO'] ### columns removed from features

#states = [31, 33, 35]
#states = [33]
#states = [31]
states = [35]
classifiers = ['rf', 'xg', 'cat', 'ada']

EPS = 1e-6 ###


# ---------------- helper functions ---------------- ###

def logit(p): ###
    p = np.clip(p, EPS, 1 - EPS) ###
    return np.log(p / (1 - p)).reshape(-1, 1) ###


def fit_calibrator(method, s, y): ###
    # calibrator saved as a plain dict of sklearn objects, so it loads in any script ###
    if method == 'platt': ###
        fitted = LogisticRegression(C=1e10, max_iter=1000).fit(logit(s), y) ###
    else: ###
        fitted = IsotonicRegression(out_of_bounds='clip', y_min=EPS, y_max=1 - EPS).fit(s, y) ###
    return {'method': method, 'fitted': fitted} ###


def apply_calibrator(cal, s): ###
    # copy this function to any script that loads a saved calibrator ###
    if cal['method'] == 'platt': ###
        return cal['fitted'].predict_proba(logit(s))[:, 1] ###
    return cal['fitted'].predict(s) ###


def choose_calibrator(s, y, n_splits=5): ###
    # picks ONE calibrator (Platt or isotonic) by out-of-fold Brier score on validation, ###
    # so isotonic is not favoured just because it overfits; then refits it on all validation ###
    skf = StratifiedKFold(n_splits=n_splits, shuffle=True, random_state=0) ###
    scores = {} ###
    for method in ['platt', 'isotonic']: ###
        oof = np.zeros(len(y)) ###
        for tr, te in skf.split(s.reshape(-1, 1), y): ###
            cal = fit_calibrator(method, s[tr], y[tr]) ###
            oof[te] = apply_calibrator(cal, s[te]) ###
        scores[method] = brier_score_loss(y, oof) ###
    best = min(scores, key=scores.get) ###
    return fit_calibrator(best, s, y), scores ###


def calib_line(y, p): ###
    # calibration intercept and slope: logistic regression of y on logit(p); ideal = 0 and 1 ###
    lr = LogisticRegression(C=1e10, max_iter=1000).fit(logit(p), y) ###
    return lr.intercept_[0], lr.coef_[0, 0] ###


def split_metrics(prefix, y, s_raw, p_cal, thr): ###
    # threshold metrics use labels; AUC/APR use raw scores (ranking, no isotonic ties) ###
    yhat = (p_cal >= thr).astype(int) ###
    cal_int, cal_slope = calib_line(y, p_cal) ###
    m = { ###
        f'{prefix} Precision': precision_score(y, yhat), ###
        f'{prefix} Recall': recall_score(y, yhat), ###
        f'{prefix} F1-Score': f1_score(y, yhat), ###
        f'{prefix} APR': average_precision_score(y, s_raw), ###
        f'{prefix} AUC-ROC': roc_auc_score(y, s_raw), ###
        f'{prefix} Brier Raw': brier_score_loss(y, s_raw), ###
        f'{prefix} Brier Cal': brier_score_loss(y, p_cal), ###
        f'{prefix} LogLoss Cal': log_loss(y, np.clip(p_cal, EPS, 1 - EPS)), ###
        f'{prefix} Cal Intercept': cal_int, ###
        f'{prefix} Cal Slope': cal_slope, ###
        f'{prefix} Observed Chik': int(y.sum()), ###
        f'{prefix} Expected Chik Cal': p_cal.sum(), ###
        f'{prefix} Thresholded Chik': int(yhat.sum()), ###
    } ###
    return m, yhat ###


def plot_cm(y, yhat, path): ###
    cm = confusion_matrix(y, yhat, labels=[0, 1]) ###
    plt.figure(figsize=(6, 5))
    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', xticklabels=['DENV', 'CHIK'], yticklabels=['DENV', 'CHIK']) ###
    plt.xlabel('Predicted Label')
    plt.ylabel('True Label')
    plt.title('Confusion Matrix')
    plt.tight_layout()
    plt.savefig(path) ###
    plt.show()


def calib_curve_df(y, s_raw, p_cal, split, n_bins=10): ###
    # quantile bins: with low prevalence, uniform bins leave the upper range empty ###
    fr, mr = calibration_curve(y, s_raw, n_bins=n_bins, strategy='quantile') ###
    fc, mc = calibration_curve(y, p_cal, n_bins=n_bins, strategy='quantile') ###
    return pd.concat([ ###
        pd.DataFrame({'split': split, 'type': 'raw', 'mean_pred': mr, 'frac_pos': fr}), ###
        pd.DataFrame({'split': split, 'type': 'cal', 'mean_pred': mc, 'frac_pos': fc}), ###
    ]) ###


def plot_calibration(df_cc, split, method, path): ###
    d = df_cc[df_cc['split'] == split] ###
    lim = max(d['mean_pred'].max(), d['frac_pos'].max()) * 1.05 ###
    plt.figure(figsize=(5, 5)) ###
    plt.plot([0, lim], [0, lim], 'k--', lw=1, label='Perfect calibration') ###
    raw = d[d['type'] == 'raw'] ###
    cal = d[d['type'] == 'cal'] ###
    plt.plot(raw['mean_pred'], raw['frac_pos'], 'o-', label='Raw') ###
    plt.plot(cal['mean_pred'], cal['frac_pos'], 's-', label=f'Calibrated ({method})') ###
    plt.xlabel('Mean predicted probability') ###
    plt.ylabel('Observed fraction of chikungunya') ###
    plt.title(f'Calibration ({split})') ###
    plt.legend() ###
    plt.tight_layout() ###
    plt.savefig(path) ###
    plt.show() ###


# ---------------- data ---------------- ###

df = pd.read_csv('dados_ML.csv')
df = df.drop(columns = ['Unnamed: 0'])

df_lab = df.loc[df['CRITERIO'] == 1,] # laboratoriais
df_epi = df.loc[df['CRITERIO'] == 2,] # epidemiologicos


for n_sg in states:
    out_dir = os.path.join(f'state_{n_sg}', RUN) ### e.g. state_35/full or state_35/partial
    os.makedirs(out_dir, exist_ok=True) ###
    df_res = pd.DataFrame()
    for classifier in classifiers:
        random.seed(0)
        np.random.seed(0)
        
        print(n_sg, classifier)
        
        df_aux = pd.DataFrame()
        
        df_lab_sg = df_lab.loc[df_lab['SG_UF'] == n_sg,]
        df_epi_sg = df_epi.loc[df_epi['SG_UF'] == n_sg,]
        
        X_lab_sg = df_lab_sg.drop(columns = DROP_COLS) ###
        X_epi_sg = df_epi_sg.drop(columns = DROP_COLS) ###
        y_lab_sg = df_lab_sg['CHIK']
        y_epi_sg = df_epi_sg['CHIK']
        print(len(X_lab_sg.columns), 'features:', X_lab_sg.columns.tolist()) ### for the predictor table
        
        X_train, X_test, y_train, y_test = train_test_split(X_lab_sg, y_lab_sg, stratify = y_lab_sg, test_size = 0.3, random_state = 0)
        X_test, X_val, y_test, y_val = train_test_split(X_test, y_test, stratify = y_test, test_size = 0.5, random_state = 0)
        
        # saving split membership (same for all classifiers, so reproducible later) ###
        pd.concat([pd.DataFrame({'idx': X_train.index, 'split': 'train'}), ###
                   pd.DataFrame({'idx': X_val.index, 'split': 'val'}), ###
                   pd.DataFrame({'idx': X_test.index, 'split': 'test'})]).to_csv( ###
            os.path.join(out_dir, 'split_indices.csv'), index=False) ###
        
        if(classifier == 'rf'):
            model = RandomForestClassifier(random_state = 0, n_jobs = -1)
        elif(classifier == 'xg'):
            model = xgb.XGBClassifier(random_state = 0, n_jobs=-1)
        elif(classifier == 'cat'):
            model = CatBoostClassifier(random_state = 0, thread_count=-1, verbose=0) ###
        else:
            model = AdaBoostClassifier(random_state = 0)
            
        # fitting the model
        model.fit(X_train, y_train)
        
        # saving the model
        joblib.dump(model, os.path.join(out_dir, f'model_{classifier}.pkl')) ###
        
        # raw scores on all datasets ###
        s_train = model.predict_proba(X_train)[:, 1] ###
        s_val = model.predict_proba(X_val)[:, 1] ###
        s_test = model.predict_proba(X_test)[:, 1] ###
        s_epi = model.predict_proba(X_epi_sg)[:, 1] ###
        
        # calibration: fitted on validation, one method chosen by out-of-fold Brier ###
        calibrator, cal_cv = choose_calibrator(s_val, y_val.values) ###
        joblib.dump(calibrator, os.path.join(out_dir, f'calibrator_{classifier}.pkl')) ###
        p_train = apply_calibrator(calibrator, s_train) ###
        p_val = apply_calibrator(calibrator, s_val) ###
        p_test = apply_calibrator(calibrator, s_test) ###
        p_epi = apply_calibrator(calibrator, s_epi) ###
        
        # fitting the threshold on calibrated validation probabilities (vectorised F1) ###
        precision, recall, thresholds = precision_recall_curve(y_val, p_val) ###
        f1_scores = 2 * precision[:-1] * recall[:-1] / (precision[:-1] + recall[:-1] + 1e-12) ###
        best_thr = thresholds[np.argmax(f1_scores)]
        
        # metrics on train, validation and test ###
        # note: validation calibration metrics are optimistic (calibrator fitted there) ###
        m_train, yhat_train = split_metrics('Train', y_train.values, s_train, p_train, best_thr) ###
        m_val, yhat_val = split_metrics('Val', y_val.values, s_val, p_val, best_thr) ###
        m_test, yhat_test = split_metrics('Test', y_test.values, s_test, p_test, best_thr) ###
        
        plot_cm(y_train, yhat_train, os.path.join(out_dir, f'cm_train_{classifier}.pdf')) ###
        plot_cm(y_val, yhat_val, os.path.join(out_dir, f'cm_val_{classifier}.pdf')) ###
        plot_cm(y_test, yhat_test, os.path.join(out_dir, f'cm_test_{classifier}.pdf')) ###
        
        # calibration curves: data saved for all splits, plotted on test (honest one) ###
        df_cc = pd.concat([ ###
            calib_curve_df(y_train.values, s_train, p_train, 'train'), ###
            calib_curve_df(y_val.values, s_val, p_val, 'val'), ###
            calib_curve_df(y_test.values, s_test, p_test, 'test'), ###
        ]) ###
        df_cc.to_csv(os.path.join(out_dir, f'calib_curve_{classifier}.csv'), index=False) ###
        plot_calibration(df_cc, 'test', calibrator['method'], ###
                         os.path.join(out_dir, f'calib_test_{classifier}.pdf')) ###
        
        # calculating correction for epidemiological data
        n_lab_denv = sum(df_lab_sg['CHIK'] == 0)
        n_lab_chik = sum(df_lab_sg['CHIK'] == 1)
        n_epi_denv = sum(df_epi_sg['CHIK'] == 0)
        n_epi_chik = sum(df_epi_sg['CHIK'] == 1) 
        n_epi_chik_corr = int((p_epi >= best_thr).sum()) ### thresholded (sensitivity)
        n_epi_denv_corr = len(p_epi) - n_epi_chik_corr ###
        n_epi_chik_cal = p_epi.sum() ### sum of calibrated probabilities (main estimate)
        n_epi_denv_cal = len(p_epi) - n_epi_chik_cal ###
        
        # per-case probabilities for clinical/epi cases, for maps and tables in another script ###
        # lat/long/mes taken from df_epi_sg, so they exist also in the partial run ###
        pd.DataFrame({ ###
            'idx': df_epi_sg.index, ### row of dados_ML.csv
            'lat': df_epi_sg['lat'].values, ###
            'long': df_epi_sg['long'].values, ###
            'mes': df_epi_sg['mes'].values, ###
            'CHIK_notified': y_epi_sg.values, ###
            'p_raw': s_epi, ###
            'p_cal': p_epi, ###
            'pred_thr': (p_epi >= best_thr).astype(int), ###
        }).to_csv(os.path.join(out_dir, f'probs_epi_{classifier}.csv'), index=False) ###
        
        
        # saving all metrics
        df_aux = pd.DataFrame({
            # basic information
            'State': [n_sg],
            'Model': [classifier],
            'Validation Threshold': [best_thr], ### now on calibrated scale
            'Calibrator': [calibrator['method']], ###
            'CV Brier Platt': [cal_cv['platt']], ###
            'CV Brier Isotonic': [cal_cv['isotonic']], ###
            # train, validation and test statistics ###
            **{k: [v] for k, v in {**m_train, **m_val, **m_test}.items()}, ###
            # corrections to epidemiological data 
            'Lab Denv': [n_lab_denv],
            'Lab Chik': [n_lab_chik],
            'Epi Denv': [n_epi_denv],
            'Epi Chik': [n_epi_chik],
            'Epi Denv Corr': [n_epi_denv_corr],
            'Epi Chik Corr': [n_epi_chik_corr],
            'Epi Denv Cal': [n_epi_denv_cal], ###
            'Epi Chik Cal': [n_epi_chik_cal], ###
            })
        
        df_res = pd.concat([df_aux, df_res])
        df_curve = pd.DataFrame({'pr_valid': precision[:-1], 
                                 'rc_valid': recall[:-1], 
                                 'thr_valid':thresholds})
        df_curve.to_csv(os.path.join(out_dir, f'pr_curve_val_{classifier}.csv')) ###
        
    
    # model selection on validation APR (independent of calibration and threshold) ###
    df_res['Selected'] = df_res['Val APR'] == df_res['Val APR'].max() ###
    
    # saving results for the state under study
    df_res.to_csv(os.path.join(out_dir, 'results.csv'), index=False) ###