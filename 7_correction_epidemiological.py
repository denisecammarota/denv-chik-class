# Correction of epidemiological data for calculating total ####################

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import joblib
import optuna
import random
from sklearn.metrics import precision_recall_curve, auc, f1_score, confusion_matrix
from sklearn.metrics import precision_score, recall_score, average_precision_score
from sklearn.model_selection import StratifiedKFold, train_test_split
from sklearn.ensemble import RandomForestClassifier
from catboost import CatBoostClassifier
from sklearn.ensemble import AdaBoostClassifier
from sklearn.metrics import confusion_matrix
import seaborn as sns
import xgboost as xgb
from xgboost import XGBClassifier

np.random.seed(123)

# Loading data, filtering municipalities of interest
df = pd.read_csv('dados_ML.csv')
df = df.drop(columns = ['Unnamed: 0'])
df = df[df['SG_UF'].isin([31, 33, 35])]

# Separating laboratorial and epidemiological
df_lab = df.loc[df['CRITERIO'] == 1,] # laboratoriais
df_epi = df.loc[df['CRITERIO'] == 2,] # epidemiologicos

# Loading data on municipalities
df_muns = pd.read_csv('data/latlong_muns.csv')
df_muns = df_muns.drop(columns = 'Unnamed: 0')
df_muns

# Obtaining epidemiological and laboratorial in each municipality in each stat + probability
states = [31, 33, 35]
state_thr = [0.31667185327852504, 0.21217881904351316, 0.27537186920222456]

i = 0

for n_sg in states: 
    print(n_sg)
    thr = state_thr[i]
    # Laboratorial data - adding municipality in question
    df_lab_sg = df_lab.loc[df_lab['SG_UF'] == n_sg,]
    df_lab_sg = df_lab_sg.merge(df_muns, how = 'left', on = ['lat', 'long'])
    df_lab_sg['CHIK_CORR'] = df_lab_sg['CHIK']
    print(sum(df_lab_sg['ID_MN_RESI'].isnull()))
    
    # Epidemiological - adding municipality in question
    df_epi_sg = df_epi.loc[df_epi['SG_UF'] == n_sg,]
    df_epi_sg = df_epi_sg.merge(df_muns, how = 'left', on = ['lat', 'long'])
    print(sum(df_epi_sg['ID_MN_RESI'].isnull()))
    
    # Loading the model for this sg
    name_model = 'state_'+str(n_sg)+'/'+'cat_'+str(n_sg)+'.pkl'
    model = joblib.load(name_model)
    
    # Adding model score (chikungunya probability)
    X_epi_sg = df_epi_sg.drop(columns = ['CHIK', 'SG_UF', 'CRITERIO']) 
    y_epi_sg = df_epi_sg['CHIK']
    y_epi_score = model.predict_proba(X_epi_sg)[:, 1]
    filt_1 = (y_epi_score >= thr)
    filt_2 = (y_epi_score < thr)
    y_epi_pred = y_epi_score.copy()
    y_epi_pred[filt_1] = 1
    y_epi_pred[filt_2] = 0
    df_epi_sg['CHIK_CORR'] = y_epi_pred
    
    # Saving the total thing
    df = pd.concat([df_lab_sg, df_epi_sg])    
    df.to_csv('state_'+str(n_sg)+'/'+str(n_sg)+'_'+'total_state.csv')
    i = i + 1
