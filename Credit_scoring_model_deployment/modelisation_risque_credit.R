# Importation des données
df_raw <- read.csv('https://raw.githubusercontent.com/JosueAfouda/Credit-Risk-Modeling/master/credit_risk_dataset.csv')
head(df_raw)
summary(df_raw)

# Suppression des données manquantes
df <- df_raw[complete.cases(df_raw), ]
summary(df)
df$loan_status <- as.factor(df$loan_status)

# Modèle de forêt aléatoire
library(caret)
set.seed(10000)
fitControl = trainControl(method="cv", number=3)

rf_model = train(loan_status~., 
                   data=df, 
                   method="rf", 
                   trControl=fitControl, 
                   verbose=F)
rf_model

saveRDS(rf_model, "credit_scoring_rf_model.rds")

# Exemple d'utilisation du modèle
new_data = data.frame(
  "person_age" = 20,
  "person_income" = 500000000,
  "person_home_ownership" = "RENT",
  "person_emp_length" = 5,
  "loan_intent" = "PERSONAL",
  "loan_grade" = "D",
  "loan_amnt" = 40000,
  "loan_int_rate" = 17,
  "loan_percent_income" = 0.6,
  "cb_person_default_on_file" = "Y",
  "cb_person_cred_hist_length" = 5
)

new_data
  
pred = predict(rf_model, newdata = new_data, type = "raw")
pred
  
  