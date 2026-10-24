# Correction of epidemiological data for calculating total ####################

import numpy as np
import pandas as pd

np.random.seed(123)

# Loading data, filtering municipalities of interest
df = pd.read_csv('dados_ML.csv')
df = df.drop(columns = ['Unnamed: 0'])
df = df[df['SG_UF'].isin([31, 33, 35])]
df['idx'] = df.index ### row of dados_ML.csv, same index saved in probs_epi_*.csv

# Separating laboratorial and epidemiological
df_lab = df.loc[df['CRITERIO'] == 1,] # laboratoriais
df_epi = df.loc[df['CRITERIO'] == 2,] # epidemiologicos

# Loading data on municipalities
df_muns = pd.read_csv('data/latlong_muns.csv')
df_muns = df_muns.drop(columns = 'Unnamed: 0')
df_muns

# Obtaining epidemiological and laboratorial in each municipality in each stat + probability
states = [31, 33, 35]
models = ['cat', 'cat', 'xg']

i = 0

for n_sg in states:
    print(n_sg)
    model_name = models[i]


    # Laboratorial data - adding municipality in question
    df_lab_sg = df_lab.loc[df_lab['SG_UF'] == n_sg,]
    df_lab_sg = df_lab_sg.merge(df_muns, how = 'left', on = ['lat', 'long'])
    df_lab_sg['CHIK_CAL'] = df_lab_sg['CHIK'] ### lab cases keep their confirmed label
    print(sum(df_lab_sg['ID_MN_RESI'].isnull()))
    print(df_lab_sg.columns)

    # Epidemiological - adding municipality in question
    df_epi_sg = df_epi.loc[df_epi['SG_UF'] == n_sg,]
    df_epi_sg = df_epi_sg.merge(df_muns, how = 'left', on = ['lat', 'long'])
    print(sum(df_epi_sg['ID_MN_RESI'].isnull()))
    print(df_epi_sg.columns)

    # Loading saved probabilities of the selected model (no need to load the model) ###
    probs = pd.read_csv('state_'+str(n_sg)+'/full/probs_epi_'+ model_name +'.csv') ###
    df_epi_sg = df_epi_sg.merge(probs[['idx', 'p_cal']], how = 'left', on = 'idx') ### match by row
    print(sum(df_epi_sg['p_cal'].isnull())) ### should be 0
    df_epi_sg['CHIK_CAL'] = df_epi_sg['p_cal'] ### calibrated probability (main estimate)
    df_epi_sg = df_epi_sg.drop(columns = ['p_cal']) ###

    # Saving the total thing
    df_total = pd.concat([df_lab_sg, df_epi_sg]) ### renamed so the loaded df is not overwritten
    df_total.to_csv('state_'+str(n_sg)+'/full/'+str(n_sg)+'_'+'total_state.csv')
    i = i + 1
    
# Final checks of total cases/total chik corr

d = pd.read_csv('state_31/full/31_total_state.csv')
print(len(d))                                        # = Lab Denv + Lab Chik + Epi Denv + Epi Chik
print(d.loc[d['CRITERIO'] == 2, 'CHIK_CAL'].sum())   # = Epi Chik Cal in results.csv


d = pd.read_csv('state_33/full/33_total_state.csv')
print(len(d))                                        # = Lab Denv + Lab Chik + Epi Denv + Epi Chik
print(d.loc[d['CRITERIO'] == 2, 'CHIK_CAL'].sum())   # = Epi Chik Cal in results.csv


d = pd.read_csv('state_35/full/35_total_state.csv')
print(len(d))                                        # = Lab Denv + Lab Chik + Epi Denv + Epi Chik
print(d.loc[d['CRITERIO'] == 2, 'CHIK_CAL'].sum())   # = Epi Chik Cal in results.csv




