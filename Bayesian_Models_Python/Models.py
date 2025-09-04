import pytensor

pytensor.config.linker = "vm"

import pandas as pd
import bambi as bmb
import arviz as az 

import matplotlib.pyplot as plt
plt.show()

from tabulate import tabulate

filename = 'latinos_cmps_2016.csv'
cmps2016 = pd.read_csv(filename)

dvs = ["Discrimination_Scale", "Latino_Disc"]
ivs = ["conc_lat_index_16", "latino_conc_16"]
controls = ["Age", "Gender", "Party", "More_Than_SecondGen"]
cluster_var = "State" 

cmps2016_clean = cmps2016.copy()

## making sure na's are dropped
all_vars = dvs + ivs + controls + [cluster_var]
cmps2016_clean = cmps2016_clean.dropna(subset=all_vars)
# to make sure the cols are correct
cmps2016[all_vars].head()

categorical_vars = ["State"]  # you can add "Gender", "Party", etc. if needed
# Convert each to 'category' dtype
for col in categorical_vars:
    cmps2016_clean[col] = cmps2016_clean[col].astype("category")

## Testing out bayesian 
model = bmb.Model("Discrimination_Scale ~ conc_lat_index_16 + (1|State)", data=cmps2016_clean)
trace = model.fit(draws=100, chains=1)  # small run to test compilation

def run_bayesian_hier(dvs, ivs, controls, cluster_var, data, iter=2000, chains=4, target_accept=0.99):
    results = {}
    
    for Y in dvs:
        for X in ivs:
            formula = f"{Y} ~ {X} + {' + '.join(controls)} + (1|{cluster_var})"
            print(f"Running model: {formula}")
            
            model = bmb.Model(formula, data)
            fitted = model.fit(draws=iter, chains=chains, target_accept=target_accept)
            
            results[f"{Y}_{X}"] = fitted
    return results


bayes_results = run_bayesian_hier(dvs, ivs, controls, cluster_var, cmps2016_clean)

# Summary of a single model
az.summary(bayes_results["Discrimination_Scale_conc_lat_index_16"])

# Posterior plots
az.plot_trace(bayes_results["Discrimination_Scale_conc_lat_index_16"])
plt.show() 

# Option 1: loop through keys only
for key in bayes_results:
    print(f"\nModel: {key}")
    print(az.summary(bayes_results[key]))

## rerunning emotions 
dvs_emos = ["Fear_Election", "Angry_Election", "Sad_Election", "Pride_Election", "Hope_Election"]
bayes_results_emos = run_bayesian_hier(dvs_emos, ivs, controls, cluster_var, cmps2016_clean)


## Storing all of them in one single dataframe
summary_dfs = {}

for key, fitted_model in bayes_results.items():
    summary_dfs[key] = az.summary(fitted_model, round_to=3)  # round for neatness

summary_dfs_emos = {}

for key, fitted_model in bayes_results_emos.items():
    summary_dfs_emos[key] = az.summary(fitted_model, round_to=3)  # round for neatness
### Separating into two separate table - disc & emos 
combined_df_disc = pd.concat(
    [df.assign(Model=key) for key, df in summary_dfs.items()],
    axis=0
)

# Move 'Model' column to front
cols = ['Model'] + [c for c in combined_df_disc.columns if c != 'Model']
combined_df_disc = combined_df_disc[cols]

combined_df_emos = pd.concat(
    [df.assign(Model=key) for key, df in summary_dfs_emos.items()],
    axis=0
)

# Move 'Model' column to front
cols = ['Model'] + [c for c in combined_df_emos.columns if c != 'Model']
combined_df_emos = combined_df_emos[cols]
combined_df_emos['Model'].unique() # to get model names

## Rearranging to make it cleaner/easier to read
model_name_map = {
    'Discrimination_Scale_conc_lat_index_16': 'Discrimination Experience + Stigma Index',
    'Discrimination_Scale_latino_conc_16': 'Discrimination Experience + Stigma',
    'Latino_Disc_conc_lat_index_16': 'Perception of Disc. + Stigma Index',
    'Latino_Disc_latino_conc_16': 'Perception of Disc. + Stigma',
    'Fear_Election_conc_lat_index_16' : 'Fear + Stigma Index',
    'Fear_Election_latino_conc_16' : 'Fear + Stigma',
    'Angry_Election_conc_lat_index_16' : 'Anger + Stigma Index',
    'Angry_Election_latino_conc_16' : 'Anger + Stigma',
    'Sad_Election_conc_lat_index_16' : 'Sad + Stigma Index',
    'Sad_Election_latino_conc_16' : 'Sad + Stigma',
    'Pride_Election_conc_lat_index_16' : 'Pride + Stigma Index', 
    'Pride_Election_latino_conc_16' : 'Pride + Stigma',
    'Hope_Election_conc_lat_index_16' : 'Hope + Stigma Index',
    'Hope_Election_latino_conc_16' : 'Hope + Stigma'
}

combined_df_disc_reset = combined_df_disc.reset_index().rename(columns={'index': 'Parameter'})
combined_df_emos_reset = combined_df_emos.reset_index().rename(columns = {'index': 'Parameter'})

# Replace model names using the mapping
combined_df_disc_reset['Model'] = combined_df_disc_reset['Model'].map(model_name_map)
combined_df_emos_reset['Model'] = combined_df_emos_reset['Model'].map(model_name_map)

# Optional: select only the columns you want for LaTeX
cols_for_latex = ['Parameter', 'Model', 'mean', 'sd', 'hdi_3%', 'hdi_97%']
latex_df_disc = combined_df_disc_reset[cols_for_latex]
latex_df_emos = combined_df_emos_reset[cols_for_latex]
## Cleaning up parameter names 
param_map = {
    'sigma': 'Residual SD',
    'Intercept': 'Intercept',
    'conc_lat_index_16': 'Stigma Index',
    'Age': 'Age',
    'Gender': 'Gender',
    'Party': 'Party',
    'More_Than_SecondGen': 'Second Generation +',
    'latino_conc_16' : 'Stigma'
}

latex_df_disc_copy = latex_df_disc.copy()
# Extract the state codes from the parameter names
latex_df_disc_copy['Parameter'] = latex_df_disc_copy['Parameter'].apply(
    lambda x: param_map.get(x, x)
)

latex_df_emos_copy = latex_df_emos.copy()
latex_df_emos_copy['Parameter'] = latex_df_emos_copy['Parameter'].apply(
    lambda x: param_map.get(x,x)
)

#### Making the LATEX tables 
latex_disc = latex_df_disc_copy.to_latex(
    index=False,              # don’t print row numbers
    escape=False,             # don’t escape LaTeX symbols
    caption="Discrimination Model Results",
    label="tab:disc_results"
)

# For emotion results
latex_emos = latex_df_emos_copy.to_latex(
    index=False,
    escape=False,
    caption="Emotion Model Results",
    label="tab:emo_results"
)


# Print to screen
print(latex_disc)
print(latex_emos)

## Saving as tex files
with open("disc_results.tex", "w") as f:
    f.write(latex_disc)

with open("emo_results.tex", "w") as f:
    f.write(latex_emos)