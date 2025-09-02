# run_model.py

import pytensor 
pytensor.config.cxx = '/usr/bin/clang++'

import pandas as pd
import bambi as bmb

# load data
filename = "latinos_cmps_2016.csv"
cmps2016 = pd.read_csv(filename)

# define variables
dvs = ["Discrimination_Scale", "Latino_Disc"]
ivs = ["conc_lat_index_16", "latino_conc_16"]
controls = ["Age", "Gender", "Party", "More_Than_SecondGen"]
cluster_var = "State"

# convert cluster var to categorical
cmps2016[cluster_var] = cmps2016[cluster_var].astype("category")

# run your Bayesian model function (make sure it's defined/imported)
bayes_results = run_bayesian_hier(dvs, ivs, controls, cluster_var, cmps2016)