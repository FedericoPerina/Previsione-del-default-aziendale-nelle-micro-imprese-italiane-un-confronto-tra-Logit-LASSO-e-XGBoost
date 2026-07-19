set.seed(123)
outer_folds = createFolds(
  y = y,
  k = 3,
  returnTrain = FALSE
)

conteggio = table(y)
peso_default = as.numeric(conteggio["False"] / conteggio["True"])

ctrl_cost_sensitive = caret::trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  search = "random"
)

pesi = c(8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 27.44)

risultati_pesi = data.frame()

for (peso in pesi) {
  
  set.seed(123)
  
  modello = nestcv.train(
    y = y,
    x = x,
    method = "xgbTree",
    trControl = ctrl_cost_sensitive,
    metric = "ROC",
    tuneLength = 30,
    na.option = "pass",
    cv.cores = cores,
    scale_pos_weight = peso,
    n_outer_folds = 3,
    n_inner_folds = 3,
    verbose = TRUE,
    finalCV = FALSE
  )
  
  y_oof = factor(modello$output$testy, levels = c("False", "True"))
  prob_oof = modello$output$predyp
  
  roc_obj = roc(
    response = y_oof,
    predictor = prob_oof,
    levels = c("False", "True"),
    direction = "<",
    quiet = TRUE
  )
  
  soglia = as.numeric(coords(
    roc_obj,
    x = "best",
    best.method = "youden",
    ret = "threshold"
  ))
  
  pred = factor(
    ifelse(prob_oof >= soglia, "True", "False"),
    levels = c("False", "True")
  )
  
  cm = confusionMatrix(pred, y_oof, positive = "True")
  
  precision = cm$byClass["Pos Pred Value"]
  recall = cm$byClass["Sensitivity"]
  specificity = cm$byClass["Specificity"]
  balanced_accuracy = cm$byClass["Balanced Accuracy"]
  
  f1 = 2 * precision * recall / (precision + recall)
  
  risultati_pesi = rbind(
    risultati_pesi,
    data.frame(
      peso = peso,
      soglia = soglia,
      AUC = as.numeric(auc(roc_obj)),
      Precision = precision,
      Recall = recall,
      Specificity = specificity,
      Balanced_Accuracy = balanced_accuracy,
      F1 = f1,
      Predicted_Default_Rate = mean(pred == "True")
    )
  )
}

risultati_pesi
risultati_pesi[order(-risultati_pesi$Balanced_Accuracy), ]
risultati_pesi[order(-risultati_pesi$Recall), ]

# Setup comune
set.seed(123)

y_xgb = factor(y, levels = c("False", "True"))

outer_folds_xgb = createFolds(
  y = y_xgb,
  k = 3,
  returnTrain = FALSE
)

###-----------------------------------------------------------------------------
# XGBoost senza bilanciamento

ctrl_xgb_no_balance = caret::trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  search = "random"
)

set.seed(123)

res_xgb_no_balance2 = nestcv.train(
  y = y_xgb,
  x = x,
  method = "xgbTree",
  trControl = ctrl_xgb_no_balance,
  metric = "ROC",
  tuneLength = 30,
  na.option = "pass",
  outer_folds = outer_folds_xgb,
  n_inner_folds = 3,
  cv.cores = cores,
  verbose = TRUE,
  finalCV = FALSE
)

summary(res_xgb_no_balance2)
calibra_soglia_youden(res_xgb_no_balance2)

###-----------------------------------------------------------------------------
# XGBoost con undersampling

ctrl_xgb_under = caret::trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  search = "random",
  sampling = "down"
)

set.seed(123)

res_xgb_under2 = nestcv.train(
  y = y_xgb,
  x = x,
  method = "xgbTree",
  trControl = ctrl_xgb_under,
  metric = "ROC",
  tuneLength = 30,
  na.option = "pass",
  outer_folds = outer_folds_xgb,
  n_inner_folds = 3,
  cv.cores = cores,
  verbose = TRUE,
  finalCV = FALSE
)

summary(res_xgb_under2)
calibra_soglia_youden(res_xgb_under2)

###-----------------------------------------------------------------------------
# XGBoost con SMOTE

ctrl_xgb_smote = caret::trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  search = "random",
  sampling = "smote"
)

set.seed(123)

res_xgb_smote2 = nestcv.train(
  y = y_xgb,
  x = x,
  method = "xgbTree",
  trControl = ctrl_xgb_smote,
  metric = "ROC",
  tuneLength = 30,
  modifyX = imputa_mediana,
  modifyX_useY = FALSE,
  na.option = "pass",
  outer_folds = outer_folds_xgb,
  n_inner_folds = 3,
  cv.cores = cores,
  verbose = TRUE,
  finalCV = FALSE
)

summary(res_xgb_smote2)
calibra_soglia_youden(res_xgb_smote2)

###-----------------------------------------------------------------------------
# XGBoost Cost-Sensitive=

conteggio = table(y_xgb)

peso_default = as.numeric(conteggio["False"] / conteggio["True"])

ctrl_xgb_cost_sensitive = caret::trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  search = "random"
)

set.seed(123)

res_xgb_cost_sensitive2 = nestcv.train(
  y = y_xgb,
  x = x,
  method = "xgbTree",
  trControl = ctrl_xgb_cost_sensitive,
  metric = "ROC",
  tuneLength = 30,
  na.option = "pass",
  cv.cores = cores,
  scale_pos_weight = peso_default,
  outer_folds = outer_folds_xgb,
  n_inner_folds = 3,
  verbose = TRUE,
  finalCV = FALSE
)

summary(res_xgb_cost_sensitive2)
calibra_soglia_youden(res_xgb_cost_sensitive2)

xgb.importance(
  model = res_xgb_cost_sensitive2$final_fit$finalModel
)

risultati_xgb2 = data.frame(
  Modello = c(
    "No bilanciamento",
    "Undersampling",
    "SMOTE",
    "Cost-sensitive"
  ),
  AUC_ROC = c(
    summary(res_xgb_no_balance2)$roc,
    summary(res_xgb_under2)$roc,
    summary(res_xgb_smote2)$roc,
    summary(res_xgb_cost_sensitive2)$roc
  ),
  PR_AUC = c(
    calcola_pr_auc(res_xgb_no_balance2)$auc.integral,
    calcola_pr_auc(res_xgb_under2)$auc.integral,
    calcola_pr_auc(res_xgb_smote2)$auc.integral,
    calcola_pr_auc(res_xgb_cost_sensitive2)$auc.integral
  )
)

risultati_xgb2
