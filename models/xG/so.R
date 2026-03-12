source(file.path("models", "xG", "train1.R"))
source(file.path("models", "xG", "train2.R"))
run_xgboost_training("so")
run_lightgbm_training("so")
