COVID predictive model doc

R version >= 3.4

Requirements: ggplot2,swat,caret,pROC,rmarkdown,classifierplots

read_exam_data.R: connects to COVID Caslib and reads laboratory exam

covid_analisi_modello_soloLAB_NEW.R: loads the outcome, selects the features, trains the model. It also creates the model with glm function and the shap explainer to be used in the dashboard

test_modello_baseline: reads test data and tests the model
