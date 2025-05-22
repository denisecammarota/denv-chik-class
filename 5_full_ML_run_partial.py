# Base run of all classifiers and for all states (new)
# Classifiers: RF, XGBoost, AdaBoost, CatBoost

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import joblib
import optuna
import random
from sklearn.metrics import precision_recall_curve, auc, f1_score, confusion_matrix
from sklearn.metrics import precision_score, recall_score, average_precision_score
from sklearn.metrics import roc_auc_score
from sklearn.model_selection import StratifiedKFold, train_test_split
from sklearn.ensemble import RandomForestClassifier
from catboost import CatBoostClassifier
from sklearn.ensemble import AdaBoostClassifier
from sklearn.metrics import confusion_matrix
import seaborn as sns
import xgboost as xgb

np.random.seed(123)

#states = [31, 33, 35]
states = [35]
classifiers = ['rf', 'xg', 'cat', 'ada']

df = pd.read_csv('dados_ML.csv')
df = df.drop(columns = ['Unnamed: 0'])

df_lab = df.loc[df['CRITERIO'] == 1,] # laboratoriais
df_epi = df.loc[df['CRITERIO'] == 2,] # epidemiologicos


for n_sg in states:
    df_res = pd.DataFrame()
    for classifier in classifiers:
        random.seed(0)
        np.random.seed(0)
        
        print(n_sg, classifier)
        
        df_aux = pd.DataFrame()
        
        df_lab_sg = df_lab.loc[df_lab['SG_UF'] == n_sg,]
        df_epi_sg = df_epi.loc[df_epi['SG_UF'] == n_sg,]
        
        X_lab_sg = df_lab_sg.drop(columns = ['CHIK', 'SG_UF', 'CRITERIO', 'mes', 'lat', 'long']) 
        X_epi_sg = df_epi_sg.drop(columns = ['CHIK', 'SG_UF', 'CRITERIO', 'mes', 'lat', 'long']) 
        y_lab_sg = df_lab_sg['CHIK']
        y_epi_sg = df_epi_sg['CHIK']
        
        X_train, X_test, y_train, y_test = train_test_split(X_lab_sg, y_lab_sg, stratify = y_lab_sg, test_size = 0.3, random_state = 0)
        X_test, X_val, y_test, y_val = train_test_split(X_test, y_test, stratify = y_test, test_size = 0.5, random_state = 0)
        
        
        if(classifier == 'rf'):
            model = RandomForestClassifier(random_state = 0, n_jobs = -1)
        elif(classifier == 'xg'):
            model = xgb.XGBClassifier(random_state = 0, n_jobs=-1)
        elif(classifier == 'cat'):
            model = CatBoostClassifier(random_state = 0, thread_count=-1)
        else:
            model = AdaBoostClassifier(random_state = 0)
            
        # fitting the model
        model.fit(X_train, y_train)
        
        # saving the model
        joblib.dump(model, classifier + '_' + str(n_sg) + '.pkl')
        
        # fitting the threshold
        y_val_proba = model.predict_proba(X_val)[:, 1]
        precision, recall, thresholds = precision_recall_curve(y_val, y_val_proba)
        f1_scores = [f1_score(y_val, y_val_proba >= t) for t in thresholds]
        best_thr = thresholds[np.argmax(f1_scores)]
        
        # calculating the metrics we will report on training data
        y_train_corr = model.predict_proba(X_train)[:, 1]
        filt_1 = (y_train_corr >= best_thr)
        filt_2 = (y_train_corr < best_thr)
        y_train_corr[filt_1] = 1
        y_train_corr[filt_2] = 0
        pr_train = precision_score(y_train, y_train_corr)
        rc_train = recall_score(y_train, y_train_corr)
        f1_train = f1_score(y_train, y_train_corr)
        apr_train = average_precision_score(y_train, y_train_corr)
        auc_train = roc_auc_score(y_train, y_train_corr)
        label_names = {0: "DENV", 1: "CHIK"}
        labels = list(label_names.keys())
        cm = confusion_matrix(y_train, y_train_corr, labels = labels)
        plt.figure(figsize=(6, 5))
        sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', xticklabels=label_names, yticklabels=label_names)
        plt.xlabel('Predicted Label')
        plt.ylabel('True Label')
        plt.title('Confusion Matrix')
        plt.tight_layout()
        plt.savefig('cm_train_'+str(n_sg)+'_'+classifier+'.pdf')
        plt.show()
        
        
        # calculating the metrics we will report on test data
        y_test_corr = model.predict_proba(X_test)[:, 1]
        filt_1 = (y_test_corr >= best_thr)
        filt_2 = (y_test_corr < best_thr)
        y_test_corr[filt_1] = 1
        y_test_corr[filt_2] = 0
        pr_test = precision_score(y_test, y_test_corr)
        rc_test = recall_score(y_test, y_test_corr)
        f1_test = f1_score(y_test, y_test_corr)
        apr_test = average_precision_score(y_test, y_test_corr)
        auc_test = roc_auc_score(y_test, y_test_corr)
        label_names = {0: "DENV", 1: "CHIK"}
        labels = list(label_names.keys())
        cm = confusion_matrix(y_test, y_test_corr, labels = labels)
        plt.figure(figsize=(6, 5))
        sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', xticklabels=labels, yticklabels=labels)
        plt.xlabel('Predicted Label')
        plt.ylabel('True Label')
        plt.title('Confusion Matrix')
        plt.tight_layout()
        plt.savefig('cm_test_'+str(n_sg)+'_'+classifier+'.pdf')
        plt.show()
        
        # calculating correction for epidemiological data
        corr_epi = model.predict_proba(X_epi_sg)[:, 1]
        n_lab_denv = sum(df_lab_sg['CHIK'] == 0)
        n_lab_chik = sum(df_lab_sg['CHIK'] == 1)
        n_epi_denv = sum(df_epi_sg['CHIK'] == 0)
        n_epi_chik = sum(df_epi_sg['CHIK'] == 1) 
        n_epi_chik_corr = sum(corr_epi >= best_thr)
        n_epi_denv_corr = sum(corr_epi < best_thr)
        best_threshold = best_thr
        best_precision = pr_test
        best_recall = rc_test
        best_f1_score = f1_test
        best_apr_score = apr_test
        
        
        # saving all metrics
        df_aux = pd.DataFrame({
            # basic information
            'State': [n_sg],
            'Model': [classifier],
            'Validation Threshold': [best_thr],
            # train statistics
            'Train Precision': [pr_train],
            'Train Recall': [rc_train],
            'Train F1-Score': [f1_train],
            'Train APR': [apr_train],
            'Train AUC-ROC': [auc_train],
            # test statistics
            'Test Precision': [pr_test],
            'Test Recall': [rc_test],
            'Test F1-Score': [f1_test],
            'Test APR': [apr_test],
            'Test AUC-ROC': [auc_test],
            # corrections to epidemiological data 
            'Lab Denv': [n_lab_denv],
            'Lab Chik': [n_lab_chik],
            'Epi Denv': [n_epi_denv],
            'Epi Chik': [n_epi_chik],
            'Epi Denv Corr': [n_epi_denv_corr],
            'Epi Chik Corr': [n_epi_chik_corr],
            })
        
        df_res = pd.concat([df_aux, df_res])
        df_curve = pd.DataFrame({'pr_valid': precision[:-1], 
                                 'rc_valid': recall[:-1], 
                                 'thr_valid':thresholds})
        name_curve = 'res_val_' + str(classifier) + '_' + str(n_sg) + '.csv'
        df_curve.to_csv(name_curve)
        
    
    # saving results for the state under study
    name_res = 'res_'+ str(n_sg) +'.csv'
    df_res.to_csv(name_res)
        
        
        
        
        
        
        



