# Mind the meter

This is the accompanying repository to "Mind the Meter: Systematic Language Variation in Poetry Corpora Confounds Computational Analysis". 

NB! To reproduce the analysis, `data/` folder is required in the root of the repository, [available here](https://data.ucl.cas.cz/s/6sPPq7scKt6CwEp).

- `00_scripts.R`. Collects useful functions that we keep using through the work;
- `01_main_analysis.R` Prepares data and produces all main figures of the paper;
- `02_RF_classification.R` Classification pipeline;
- `03_data_for_distinctive.R` Prepares datasets for SHAP analysis


## 📁 /plots

- `/CM_plots/`: has all confusion matrices heatmaps  
- `/paper`: main body and appendix figures  
- `/shap`: has a variety of SHAP plots + `shap.ipynb` that produces them


## 📁 /results

- `/conf_matricies`: individual confusion matrices from RF classification;  
- `formulas_examples.csv`: Table 2 of the paper;  
- `res_foot_kfold.csv` Classification results by foot type;  
- `res_form_kfold.csv` Classification results by metrical form;  


##  📁 [/data](https://data.ucl.cas.cz/s/6sPPq7scKt6CwEp) 

- `/dump`: the original data dump from PoeTree;  
- `/dump_features`: vectorized data for SHAP analysis, produced by `03_data_for_distinctive.R`  
- `/prepared` prepd dataset for metrical form based analysis, produced in`01_main_analysis.R`  
- `/prepared_foot` prepd dataset for foot based analysis, produced in `01_main_analysis.R`  

