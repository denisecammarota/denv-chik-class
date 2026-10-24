import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import joblib
import optuna
import shap
from sklearn.metrics import precision_recall_curve, auc, f1_score, confusion_matrix
from sklearn.metrics import precision_score, recall_score, average_precision_score
from sklearn.model_selection import StratifiedKFold, train_test_split
from sklearn.ensemble import RandomForestClassifier
np.random.seed(123)


states = [31, 33, 35]
models = ['cat', 'cat', 'xg']
state_name = ['Minas Gerais', 'Rio de Janeiro', 'São Paulo']

df = pd.read_csv('dados_ML.csv')
df = df.drop(columns = ['Unnamed: 0'])

df_lab = df.loc[df['CRITERIO'] == 1,] # laboratoriais
df_epi = df.loc[df['CRITERIO'] == 2,] # epidemiologicos

i = 0

for n_sg in states:
    print(n_sg)
    model_name = models[i]

    df_lab_sg = df_lab.loc[df_lab['SG_UF'] == n_sg,]
    df_epi_sg = df_epi.loc[df_epi['SG_UF'] == n_sg,]

    X_lab_sg = df_lab_sg.drop(columns = ['CHIK', 'SG_UF', 'CRITERIO'])
    X_epi_sg = df_epi_sg.drop(columns = ['CHIK', 'SG_UF', 'CRITERIO'])
    y_lab_sg = df_lab_sg['CHIK']
    y_epi_sg = df_epi_sg['CHIK']

    X_train, X_test, y_train, y_test = train_test_split(X_lab_sg, y_lab_sg, stratify = y_lab_sg, test_size = 0.3, random_state = 0)
    X_test, X_val, y_test, y_val = train_test_split(X_test, y_test, stratify = y_test, test_size = 0.5, random_state = 0)


    name_model = 'state_'+str(n_sg)+'/full/model_'+ model_name +'.pkl' 
    model = joblib.load(name_model)

    feature_names = ['Fever', 'Myalgia', 'Headache', 'Rash', 'Vomiting', 'Nausea', 'Back pain', 'Conjunctivitis', 'Arthritis', 'Arthralgia', 'Petechiae', 'Leukopenia', 'Tourniquet test', 'Retro orbital pain', 'Patient age', 'Hospitalization status', 'Latitude', 'Longitude', 'Month of symptom onset', 'Days since symptom onset', 'Patient sex']

    explainer = shap.TreeExplainer(model)
    shap_values = explainer.shap_values(X_test)
    shap.summary_plot(shap_values, X_test, feature_names=feature_names, max_display = len(feature_names), show = False) 
    plt.title(state_name[i], fontsize = 18) 
    plt.tight_layout()
    plt.savefig('figures/shap_test_full_'+str(n_sg)+'.pdf')
    plt.show()
    i = i + 1