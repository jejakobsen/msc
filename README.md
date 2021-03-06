# Master thesis

This repository contains data sets and scripts to produce my master's thesis. 

The majority of the scripts are written in R with version 4.0.3. For version and packages in Python, please see `data_creation_process.md` located at *scripts/data*.

The *scripts* folder contains scripts sorted into subfolders representing each chapter in the thesis. In each of these subfolders there exist a *figs* folder - which is empty. If the user runs a script which generates a figure, then the figure will be stored in the *figs* folder. For example; if running `sim_pois.R` in *scripts/theory*, then a figure will be stored in *scripts/theory/figs*.  

The *data* folder contains the data sets used in the thesis, along with a subfolder for raw data files. 

If the user want to create data sets with `create_datasets.py`, please unzip the files in the *data/raw_data* folder and locate the resulting *.csv* files in *data/raw_data*.  
