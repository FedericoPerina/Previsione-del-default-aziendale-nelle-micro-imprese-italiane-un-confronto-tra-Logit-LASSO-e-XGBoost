# ALE plots per modello logit LASSO con SMOTE
library(dplyr)
library(ggplot2)
library(iml)
library(patchwork)

###-----------------------------------------------------------------------------
res_lasso_interpretazione = res_lasso_smote

modello_lasso = res_lasso_interpretazione$final_fit

lambda_lasso = 0.0001807

n_variabili_ale = 6

grid_size_ale = 20

usa_campione_ale = TRUE
n_campione_ale = 10000
set.seed(123)

# Preparazione dati X e y
x_ale_full = imputa_mediana(x)

x_ale_full = as.data.frame(x_ale_full)

y_char = as.character(y)

if (all(y_char %in% c("False", "True"))) {
  y_factor = factor(y_char, levels = c("False", "True"))
} else if (all(y_char %in% c("0", "1"))) {
  y_factor = factor(ifelse(y_char == "1", "True", "False"), levels = c("False", "True"))
} else {
  stop("La variabile y non è nel formato atteso: deve essere False/True oppure 0/1.")
}

y_numeric = ifelse(y_factor == "True", 1, 0)

# Funzione di predizione grezza del LASSO
pred_lasso_raw = function(model, newdata) {
  
  newdata = as.data.frame(newdata)
  
  newx = as.matrix(newdata[, colnames(x_ale_full), drop = FALSE])
  
  pred = as.numeric(
    predict(
      model,
      newx = newx,
      s = lambda_lasso,
      type = "response"
    )
  )
  return(pred)
}

# Controllo direzione della probabilità
pred_raw_full = pred_lasso_raw(
  model = modello_lasso,
  newdata = x_ale_full
)

# Controllo AUC della predizione grezza e della predizione invertita
if (requireNamespace("pROC", quietly = TRUE)) {
  
  roc_raw = pROC::roc(
    response = y_factor,
    predictor = pred_raw_full,
    levels = c("False", "True"),
    direction = "<",
    quiet = TRUE
  )
  
  roc_invertita = pROC::roc(
    response = y_factor,
    predictor = 1 - pred_raw_full,
    levels = c("False", "True"),
    direction = "<",
    quiet = TRUE
  )
  
  auc_raw = as.numeric(pROC::auc(roc_raw))
  auc_invertita = as.numeric(pROC::auc(roc_invertita))
  
  cat("\nAUC predizione grezza:\n")
  print(auc_raw)
  
  cat("\nAUC predizione invertita:\n")
  print(auc_invertita)
  
  inverti_probabilita = auc_invertita > auc_raw
  
} else {
  
  warning("Pacchetto pROC non installato. Uso il confronto delle medie per decidere se invertire la probabilità.")
  
  media_false = mean(pred_raw_full[y_factor == "False"], na.rm = TRUE)
  media_true = mean(pred_raw_full[y_factor == "True"], na.rm = TRUE)
  
  inverti_probabilita = media_true < media_false
}

# Funzione definitiva di predizione della classe Default
pred_lasso_default = function(model, newdata) {
  
  pred = pred_lasso_raw(
    model = model,
    newdata = newdata
  )
  
  if (inverti_probabilita == TRUE) {
    pred = 1 - pred
  }
  
  return(pred)
}

# Controllo finale delle predizioni orientate verso il default
pred_default_full = pred_lasso_default(
  model = modello_lasso,
  newdata = x_ale_full
)

if (requireNamespace("pROC", quietly = TRUE)) {
  
  roc_finale = pROC::roc(
    response = y_factor,
    predictor = pred_default_full,
    levels = c("False", "True"),
    direction = "<",
    quiet = TRUE
  )
  
  cat("\nAUC finale predizioni orientate al default:\n")
  print(as.numeric(pROC::auc(roc_finale)))
}

# Creazione campione per ALE
if (usa_campione_ale == TRUE) {
  
  n_campione_ale = min(n_campione_ale, nrow(x_ale_full))
  
  idx_true = which(y_factor == "True")
  idx_false = which(y_factor == "False")
  
  quota_true = length(idx_true) / nrow(x_ale_full)
  n_true_sample = round(n_campione_ale * quota_true)
  n_false_sample = n_campione_ale - n_true_sample
  
  n_true_sample = min(n_true_sample, length(idx_true))
  n_false_sample = min(n_false_sample, length(idx_false))
  
  idx_ale = c(
    sample(idx_true, n_true_sample),
    sample(idx_false, n_false_sample)
  )
  
  idx_ale = sample(idx_ale)
  
  x_ale = x_ale_full[idx_ale, , drop = FALSE]
  y_ale = y_numeric[idx_ale]
  
} else {
  
  x_ale = x_ale_full
  y_ale = y_numeric
}

cat("\nDimensione dati usati per ALE:\n")
print(dim(x_ale))

# Oggetto Predictor per iml
predictor_lasso = Predictor$new(
  model = modello_lasso,
  data = x_ale,
  y = y_ale,
  predict.function = pred_lasso_default
)

# Controllo rapido sul Predictor
pred_check = predictor_lasso$predict(x_ale[1:20, , drop = FALSE])

cat("\nControllo predictor iml:\n")
print(head(pred_check))
print(summary(pred_check[[1]]))
print(range(pred_check[[1]], na.rm = TRUE))

# Estrazione coefficienti LASSO
coef_lasso = as.matrix(
  coef(
    modello_lasso,
    s = lambda_lasso
  )
)

coef_lasso_df = data.frame(
  variabile = rownames(coef_lasso),
  coefficiente = as.numeric(coef_lasso[, 1])
)

coef_lasso_df = coef_lasso_df %>%
  filter(
    variabile != "(Intercept)",
    coefficiente != 0
  ) %>%
  mutate(
    abs_coefficiente = abs(coefficiente)
  ) %>%
  arrange(desc(abs_coefficiente))

cat("\nPrime variabili selezionate dal LASSO:\n")
print(head(coef_lasso_df, 20))

# Esclusione dummy, variabili categoriche e missing indicators
pattern_esclusione = paste(
  c(
    "^zero_",
    "^one_",
    "^is_na_",
    "n_na",
    "NA_count",
    "na_count",
    "missing",
    "bvd_indep",
    "corp_group",
    "consolidation",
    "macro_area",
    "legal_form",
    "region",
    "area",
    "sector",
    "ateco",
    "factor",
    "dummy"
  ),
  collapse = "|"
)

coef_lasso_continue = coef_lasso_df %>%
  filter(
    !grepl(pattern_esclusione, variabile, ignore.case = TRUE),
    variabile %in% colnames(x_ale)
  )

cat("\nPrime variabili continue candidate per ALE:\n")
print(head(coef_lasso_continue, 20))

# Selezione variabili per ALE
variabili_ale_lasso = coef_lasso_continue %>%
  slice_head(n = n_variabili_ale) %>%
  pull(variabile)

cat("\nVariabili selezionate per ALE:\n")
print(variabili_ale_lasso)

tabella_variabili_ale_lasso = coef_lasso_continue %>%
  slice_head(n = 10) %>%
  select(
    variabile,
    coefficiente,
    abs_coefficiente
  )

cat("\nTabella variabili principali LASSO:\n")
print(tabella_variabili_ale_lasso)

# Funzione per creare ALE plot
crea_ale_plot = function(var_name, predictor_object, grid_size = 20) {
  
  ale_obj = FeatureEffect$new(
    predictor = predictor_object,
    feature = var_name,
    method = "ale",
    grid.size = grid_size
  )
  
  p = plot(ale_obj) +
    labs(
      title = var_name,
      x = var_name,
      y = "Effetto ALE sull'output del modello"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  return(
    list(
      plot = p,
      object = ale_obj
    )
  )
}

# Calcolo ALE plots
risultati_ale_lasso = lapply(
  variabili_ale_lasso,
  crea_ale_plot,
  predictor_object = predictor_lasso,
  grid_size = grid_size_ale
)

names(risultati_ale_lasso) = variabili_ale_lasso

grafici_ale_lasso = lapply(
  risultati_ale_lasso,
  function(x) x$plot
)

oggetti_ale_lasso = lapply(
  risultati_ale_lasso,
  function(x) x$object
)

# Grafico finale
grafico_ale_lasso = wrap_plots(
  grafici_ale_lasso,
  ncol = 2
)

print(grafico_ale_lasso)

ggsave(
  filename = "grafico_ale_lasso_smote.png",
  plot = grafico_ale_lasso,
  width = 10,
  height = 8,
  dpi = 300
)

# Etichette leggibili

etichette_variabili = c(
  curr_liab_to_assets_2017 = "Current liabilities / Total assets",
  bt_profit_log = "Before-tax profit (log)",
  rcr_log = "RCR (log)",
  assets_log = "Total assets (log)",
  daw_log = "DAW (log)",
  curr_assets_log = "Current assets (log)"
)

# Tema
tema_ale = function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14,
        hjust = 0.5,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = 10.5,
        hjust = 0.5,
        margin = margin(b = 8)
      ),
      axis.title.x = element_text(
        size = 11.5,
        face = "bold",
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        size = 11.5,
        face = "bold",
        margin = margin(r = 8)
      ),
      axis.text = element_text(size = 10.5, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.35, colour = "grey80"),
      axis.line = element_line(colour = "black", linewidth = 0.3),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10)
    )
}

# Funzione ALE plot
crea_ale_plot = function(var_name, predictor_object, grid_size = 20) {
  
  ale_obj = FeatureEffect$new(
    predictor = predictor_object,
    feature = var_name,
    method = "ale",
    grid.size = grid_size
  )
  
  titolo = ifelse(
    var_name %in% names(etichette_variabili),
    etichette_variabili[var_name],
    var_name
  )
  
  p = plot(ale_obj) +
    labs(
      title = titolo,
      x = titolo,
      y = "ALE effect on model output"
    ) +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    tema_ale()
  
  return(list(plot = p, object = ale_obj))
}

# Costruzione grafici
risultati_ale_lasso = lapply(
  variabili_ale_lasso,
  crea_ale_plot,
  predictor_object = predictor_lasso,
  grid_size = 20
)

names(risultati_ale_lasso) = variabili_ale_lasso

grafici_ale_lasso = lapply(
  risultati_ale_lasso,
  function(x) x$plot
)

oggetti_ale_lasso = lapply(
  risultati_ale_lasso,
  function(x) x$object
)

# grafico finale
grafico_ale_lasso_finale = wrap_plots(
  grafici_ale_lasso,
  ncol = 2
) +
  plot_annotation(
    title = "Accumulated Local Effects (ALE) plots - LASSO with SMOTE",
    subtitle = "Effects are interpreted on the probabilistic output of the rebalanced model",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    )
  )

grafico_ale_lasso_finale

# Salvataggio
ggsave(
  filename = "grafico_ale_lasso.png",
  plot = grafico_ale_lasso_finale,
  width = 11,
  height = 8.5,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = "grafico_ale_lasso.pdf",
  plot = grafico_ale_lasso_finale,
  width = 11,
  height = 8.5,
  bg = "white"
)

# Titoli personalizzati dei 6 grafici
titoli_variabili = c(
  curr_liab_to_assets_2017 = "Incidenza delle passività correnti sull’attivo",
  bt_profit_log = "Utile ante imposte",
  rcr_log = "Consumi di materie prime",
  assets_log = "Totale attivo",
  daw_log = "Ammortamenti e svalutazioni",
  curr_assets_log = "Attivo corrente"
)

# Tema grafico
tema_ale = function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 13.5,
        hjust = 0.5,
        margin = margin(b = 8)
      ),
      axis.title.x = element_text(
        size = 11,
        face = "bold",
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        size = 11,
        face = "bold",
        margin = margin(r = 8)
      ),
      axis.text = element_text(size = 10.5, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.35, colour = "grey80"),
      axis.line = element_line(colour = "black", linewidth = 0.3),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10)
    )
}

# Funzione per creare i singoli ALE plot
crea_ale_plot = function(var_name, predictor_object, grid_size = 20) {
  
  ale_obj = FeatureEffect$new(
    predictor = predictor_object,
    feature = var_name,
    method = "ale",
    grid.size = grid_size
  )
  
  titolo_grafico = ifelse(
    var_name %in% names(titoli_variabili),
    titoli_variabili[var_name],
    var_name
  )
  
  p = plot(ale_obj) +
    labs(
      title = titolo_grafico,
      x = var_name,
      y = "Effetto ALE"
    ) +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    tema_tesi_ale()
  
  return(list(plot = p, object = ale_obj))
}

# Costruzione grafici ALE
risultati_ale_lasso = lapply(
  variabili_ale_lasso,
  crea_ale_plot,
  predictor_object = predictor_lasso,
  grid_size = 20
)

names(risultati_ale_lasso) = variabili_ale_lasso

grafici_ale_lasso = lapply(
  risultati_ale_lasso,
  function(x) x$plot
)

oggetti_ale_lasso = lapply(
  risultati_ale_lasso,
  function(x) x$object
)

# Figura finale SENZA titolo generale
grafico_ale_lasso_finale = wrap_plots(
  grafici_ale_lasso,
  ncol = 2
)

grafico_ale_lasso_finale

# Salvataggio
ggsave(
  filename = "grafico_ale_lasso_tesi.png",
  plot = grafico_ale_lasso_finale,
  width = 11,
  height = 8.5,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = "grafico_ale_lasso_tesi.pdf",
  plot = grafico_ale_lasso_finale,
  width = 11,
  height = 8.5,
  bg = "white"
)

# ALE plots per modello XGBoost cost-sensitive
# Prime 6 variabili continue per importanza XGBoost

titoli_variabili = c(
  shareholders_funds_log = "Patrimonio netto",
  solv_ratio_log = "Indice di solvibilità",
  leverage_log = "Leverage",
  cash_flow_log = "Flusso di cassa",
  ebitda_log = "Margine operativo lordo",
  roe_2017 = "ROE"
)

# Impostazioni generali
res_xgb_interpretazione = res_xgb_cost_sensitive2

modello_xgb = res_xgb_interpretazione$final_fit

n_variabili_ale = 6
grid_size_ale = 20

usa_campione_ale = TRUE
n_campione_ale = 10000
set.seed(123)

# Preparazione dati X e y
x_ale_full = imputa_mediana(x)
x_ale_full = as.data.frame(x_ale_full)

if (!is.null(res_xgb_interpretazione$output$testy) &&
    length(res_xgb_interpretazione$output$testy) == nrow(x_ale_full)) {
  
  y_char = as.character(res_xgb_interpretazione$output$testy)
  
} else {
  
  y_char = as.character(y)
}

if (all(y_char %in% c("False", "True"))) {
  
  y_factor = factor(y_char, levels = c("False", "True"))
  
} else if (all(y_char %in% c("0", "1"))) {
  
  y_factor = factor(
    ifelse(y_char == "1", "True", "False"),
    levels = c("False", "True")
  )
  
} else {
  
  stop("La variabile y non è nel formato atteso: deve essere False/True oppure 0/1.")
}

y_numeric = ifelse(y_factor == "True", 1, 0)

# Estrazione booster XGBoost
estrai_booster_xgb = function(model) {
  
  if (inherits(model, "xgb.Booster")) {
    return(model)
  }
  
  if (!is.null(model$finalModel) && inherits(model$finalModel, "xgb.Booster")) {
    return(model$finalModel)
  }
  
  stop("Non riesco a trovare un oggetto xgb.Booster dentro il modello.")
}

booster_xgb = estrai_booster_xgb(modello_xgb)

cat("\nClasse oggetto booster:\n")
print(class(booster_xgb))

# Funzione di predizione grezza XGBoost
pred_xgb_raw = function(model, newdata) {
  
  newdata = as.data.frame(newdata)
  
  newx = as.matrix(newdata[, colnames(x_ale_full), drop = FALSE])
  colnames(newx) = colnames(x_ale_full)
  
  booster = estrai_booster_xgb(model)
  
  pred = as.numeric(
    predict(
      booster,
      newdata = xgboost::xgb.DMatrix(newx)
    )
  )
  
  return(pred)
}

# Controllo direzione della probabilità
pred_raw_full = pred_xgb_raw(
  model = modello_xgb,
  newdata = x_ale_full
)

if (requireNamespace("pROC", quietly = TRUE)) {
  
  roc_raw = pROC::roc(
    response = y_factor,
    predictor = pred_raw_full,
    levels = c("False", "True"),
    direction = "<",
    quiet = TRUE
  )
  
  roc_invertita = pROC::roc(
    response = y_factor,
    predictor = 1 - pred_raw_full,
    levels = c("False", "True"),
    direction = "<",
    quiet = TRUE
  )
  
  auc_raw = as.numeric(pROC::auc(roc_raw))
  auc_invertita = as.numeric(pROC::auc(roc_invertita))
  
  cat("\nAUC predizione grezza:\n")
  print(auc_raw)
  
  cat("\nAUC predizione invertita:\n")
  print(auc_invertita)
  
  inverti_probabilita = auc_invertita > auc_raw
  
} else {
  
  warning("Pacchetto pROC non installato. Uso il confronto delle medie per decidere se invertire la probabilità.")
  
  media_false = mean(pred_raw_full[y_factor == "False"], na.rm = TRUE)
  media_true = mean(pred_raw_full[y_factor == "True"], na.rm = TRUE)
  
  inverti_probabilita = media_true < media_false
}

cat("\nDevo invertire la probabilità?\n")
print(inverti_probabilita)

# Funzione definitiva di predizione orientata al default
pred_xgb_default = function(model, newdata) {
  
  pred = pred_xgb_raw(
    model = model,
    newdata = newdata
  )
  
  if (inverti_probabilita == TRUE) {
    pred = 1 - pred
  }
  
  return(pred)
}

pred_default_full = pred_xgb_default(
  model = modello_xgb,
  newdata = x_ale_full
)

cat("\nSummary predizioni finali orientate al default:\n")
print(summary(pred_default_full))

cat("\nPredizioni finali per classe osservata:\n")
print(tapply(pred_default_full, y_factor, summary))

if (requireNamespace("pROC", quietly = TRUE)) {
  
  roc_finale = pROC::roc(
    response = y_factor,
    predictor = pred_default_full,
    levels = c("False", "True"),
    direction = "<",
    quiet = TRUE
  )
  
  cat("\nAUC finale predizioni orientate al default:\n")
  print(as.numeric(pROC::auc(roc_finale)))
}

# Creazione campione per ALE
if (usa_campione_ale == TRUE) {
  
  n_campione_ale = min(n_campione_ale, nrow(x_ale_full))
  
  idx_true = which(y_factor == "True")
  idx_false = which(y_factor == "False")
  
  quota_true = length(idx_true) / nrow(x_ale_full)
  
  n_true_sample = round(n_campione_ale * quota_true)
  n_false_sample = n_campione_ale - n_true_sample
  
  n_true_sample = min(n_true_sample, length(idx_true))
  n_false_sample = min(n_false_sample, length(idx_false))
  
  idx_ale = c(
    sample(idx_true, n_true_sample),
    sample(idx_false, n_false_sample)
  )
  
  idx_ale = sample(idx_ale)
  
  x_ale = x_ale_full[idx_ale, , drop = FALSE]
  y_ale = y_numeric[idx_ale]
  
} else {
  
  x_ale = x_ale_full
  y_ale = y_numeric
}

# Oggetto Predictor per iml
predictor_xgb = Predictor$new(
  model = modello_xgb,
  data = x_ale,
  y = y_ale,
  predict.function = pred_xgb_default
)

pred_check = predictor_xgb$predict(x_ale[1:20, , drop = FALSE])

# Importanza variabili XGBoost
importanza_xgb = xgboost::xgb.importance(
  feature_names = colnames(x_ale_full),
  model = booster_xgb
)

# Esclusione dummy, variabili categoriche e missing indicators
pattern_esclusione = paste(
  c(
    "^zero_",
    "^one_",
    "^is_na_",
    "n_na",
    "NA_count",
    "na_count",
    "missing",
    "bvd_indep",
    "corp_group",
    "consolidation",
    "macro_area",
    "legal_form",
    "region",
    "area",
    "sector",
    "ateco",
    "factor",
    "dummy"
  ),
  collapse = "|"
)

variabili_numeriche_continue = names(x_ale_full)[
  sapply(x_ale_full, is.numeric) &
    sapply(x_ale_full, function(z) length(unique(z[!is.na(z)])) > 10)
]

importanza_xgb_continue = importanza_xgb %>%
  filter(
    Feature %in% colnames(x_ale_full),
    Feature %in% variabili_numeriche_continue,
    !grepl(pattern_esclusione, Feature, ignore.case = TRUE)
  ) %>%
  arrange(desc(Gain))

print(head(importanza_xgb_continue, 20))

# Selezione prime 6 variabili continue per ALE
variabili_ale_xgb = importanza_xgb_continue %>%
  slice_head(n = n_variabili_ale) %>%
  pull(Feature)

cat("\nPrime 6 variabili continue selezionate per ALE XGBoost:\n")
print(variabili_ale_xgb)

if (length(variabili_ale_xgb) < n_variabili_ale) {
  warning("Sono state trovate meno di 6 variabili continue valide per ALE.")
}

tabella_variabili_ale_xgb = importanza_xgb_continue %>%
  slice_head(n = 10) %>%
  select(
    Feature,
    Gain,
    Cover,
    Frequency
  )
print(tabella_variabili_ale_xgb)

# Titoli personalizzati, se alcune variabili sono note
titoli_variabili = c(
  curr_liab_to_assets_2017 = "Incidenza delle passività correnti sull’attivo",
  bt_profit_log = "Utile ante imposte",
  rcr_log = "Consumi di materie prime",
  assets_log = "Totale attivo",
  daw_log = "Ammortamenti e svalutazioni",
  curr_assets_log = "Attivo corrente",
  roa_log = "Redditività dell’attivo",
  roe_log = "Redditività del capitale proprio",
  ros_log = "Redditività delle vendite",
  ebitda_to_sales_log = "EBITDA su vendite",
  solv_ratio_log = "Indice di solvibilità",
  liq_ratio_log = "Indice di liquidità",
  st_payables_log = "Debiti a breve termine",
  cash_flow_log = "Cash flow",
  shareholders_funds_log = "Patrimonio netto",
  tang_fix_assets_log = "Immobilizzazioni materiali"
)

# 13. Tema grafico
tema_ale = function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 13.5,
        hjust = 0.5,
        margin = margin(b = 8)
      ),
      axis.title.x = element_text(
        size = 11,
        face = "bold",
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        size = 11,
        face = "bold",
        margin = margin(r = 8)
      ),
      axis.text = element_text(size = 10.5, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.35, colour = "grey80"),
      axis.line = element_line(colour = "black", linewidth = 0.3),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10)
    )
}

# Funzione per creare i singoli ALE plot
crea_ale_plot = function(var_name, predictor_object, grid_size = 20) {
  
  ale_obj = FeatureEffect$new(
    predictor = predictor_object,
    feature = var_name,
    method = "ale",
    grid.size = grid_size
  )
  
  risultati = ale_obj$results
  
  titolo_grafico = ifelse(
    var_name %in% names(titoli_variabili),
    titoli_variabili[var_name],
    gsub("_", " ", var_name)
  )
  
  dati_plot = risultati %>%
    mutate(
      effetto_ale_pp = .value * 100
    )
  
  x_rug = predictor_object$data$get.x()[[var_name]]
  
  p = ggplot(
    dati_plot,
    aes(
      x = .data[[var_name]],
      y = effetto_ale_pp
    )
  ) +
    geom_hline(
      yintercept = 0,
      linewidth = 0.35,
      colour = "grey65"
    ) +
    geom_line(
      linewidth = 0.9,
      colour = "black"
    ) +
    geom_rug(
      data = data.frame(x = x_rug),
      aes(x = x, y = NULL),
      inherit.aes = FALSE,
      sides = "b",
      alpha = 0.20,
      linewidth = 0.25
    ) +
    labs(
      title = titolo_grafico,
      x = var_name,
      y = "Effetto ALE"
    ) +
    scale_y_continuous(
      labels = label_number(accuracy = 0.01)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 13.5,
        hjust = 0.5,
        margin = margin(b = 8)
      ),
      axis.title.x = element_text(
        size = 11,
        face = "bold",
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        size = 11,
        face = "bold",
        margin = margin(r = 8)
      ),
      axis.text = element_text(
        size = 10.5,
        colour = "black"
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(
        linewidth = 0.35,
        colour = "grey80"
      ),
      axis.line = element_line(
        colour = "black",
        linewidth = 0.3
      ),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10)
    )
  
  return(
    list(
      plot = p,
      object = ale_obj
    )
  )
}

# Calcolo ALE plots
risultati_ale_xgb = lapply(
  variabili_ale_xgb,
  crea_ale_plot,
  predictor_object = predictor_xgb,
  grid_size = grid_size_ale
)

names(risultati_ale_xgb) = variabili_ale_xgb

grafici_ale_xgb = lapply(
  risultati_ale_xgb,
  function(x) x$plot
)

oggetti_ale_xgb = lapply(
  risultati_ale_xgb,
  function(x) x$object
)

# Figura finale
grafico_ale_xgb_finale = wrap_plots(
  grafici_ale_xgb,
  ncol = 2
)

grafico_ale_xgb_finale

# Salvataggio
ggsave(
  filename = "grafico_ale_xgb_cost_sensitive_tesi.png",
  plot = grafico_ale_xgb_finale,
  width = 11,
  height = 8.5,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = "grafico_ale_xgb_cost_sensitive_tesi.pdf",
  plot = grafico_ale_xgb_finale,
  width = 11,
  height = 8.5,
  bg = "white"
)