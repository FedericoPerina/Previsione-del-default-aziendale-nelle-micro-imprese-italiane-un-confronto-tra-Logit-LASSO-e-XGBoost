# Conditional SHAP values per modello XGBoost cost-sensitive
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(shapr)
library(pROC)
library(xgboost)

# Impostazioni generali
res_xgb_interpretazione = res_xgb_cost_sensitive2

modello_xgb = res_xgb_interpretazione$final_fit

set.seed(123)

n_background_shap_xgb = 500
max_n_coalitions_shap_xgb = 50
n_MC_samples_shap_xgb = 50
approach_shap_xgb = "gaussian"

n_variabili_principali_shap_xgb = 12

# Preparazione dati X e y
x_shap_full_xgb = imputa_mediana(x)
x_shap_full_xgb = as.data.frame(x_shap_full_xgb)

x_shap_full_xgb[] = lapply(x_shap_full_xgb, as.numeric)

y_char = as.character(y)

if (all(y_char %in% c("False", "True"))) {
  
  y_factor_xgb = factor(y_char, levels = c("False", "True"))
  
} else if (all(y_char %in% c("0", "1"))) {
  
  y_factor_xgb = factor(
    ifelse(y_char == "1", "True", "False"),
    levels = c("False", "True")
  )
  
} else {
  
  stop("La variabile y non è nel formato atteso: deve essere False/True oppure 0/1.")
}

y_numeric_xgb = ifelse(y_factor_xgb == "True", 1, 0)

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

# Funzione di predizione XGBoost
pred_xgb_raw = function(model, newdata) {
  
  newdata = as.data.frame(newdata)
  newdata[] = lapply(newdata, as.numeric)
  
  newx = as.matrix(newdata[, colnames(x_shap_full_xgb), drop = FALSE])
  colnames(newx) = colnames(x_shap_full_xgb)
  
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
pred_raw_full_xgb = pred_xgb_raw(
  model = modello_xgb,
  newdata = x_shap_full_xgb
)

roc_raw_xgb = pROC::roc(
  response = y_factor_xgb,
  predictor = pred_raw_full_xgb,
  levels = c("False", "True"),
  direction = "<",
  quiet = TRUE
)

roc_invertita_xgb = pROC::roc(
  response = y_factor_xgb,
  predictor = 1 - pred_raw_full_xgb,
  levels = c("False", "True"),
  direction = "<",
  quiet = TRUE
)

auc_raw_xgb = as.numeric(pROC::auc(roc_raw_xgb))
auc_invertita_xgb = as.numeric(pROC::auc(roc_invertita_xgb))

inverti_probabilita_xgb = auc_invertita_xgb > auc_raw_xgb

pred_xgb_default = function(model, newdata) {
  
  pred = pred_xgb_raw(
    model = model,
    newdata = newdata
  )
  
  if (inverti_probabilita_xgb == TRUE) {
    pred = 1 - pred
  }
  
  return(pred)
}

pred_default_full_xgb = pred_xgb_default(
  model = modello_xgb,
  newdata = x_shap_full_xgb
)

roc_finale_xgb = pROC::roc(
  response = y_factor_xgb,
  predictor = pred_default_full_xgb,
  levels = c("False", "True"),
  direction = "<",
  quiet = TRUE
)

# Background distribution per conditional SHAP
if (exists("x_background_shap") &&
    all(colnames(x_background_shap) == colnames(x_shap_full_xgb))) {
  
  cat("\nUso lo stesso background SHAP del LASSO.\n")
  
  x_background_shap_xgb = x_background_shap
  x_background_shap_xgb[] = lapply(x_background_shap_xgb, as.numeric)
  
} else {
  
  cat("\nCreo un nuovo background SHAP per XGBoost.\n")
  
  idx_true = which(y_factor_xgb == "True")
  idx_false = which(y_factor_xgb == "False")
  
  quota_true = length(idx_true) / nrow(x_shap_full_xgb)
  
  n_true_background = round(n_background_shap_xgb * quota_true)
  n_false_background = n_background_shap_xgb - n_true_background
  
  n_true_background = min(n_true_background, length(idx_true))
  n_false_background = min(n_false_background, length(idx_false))
  
  idx_background_xgb = c(
    sample(idx_true, n_true_background),
    sample(idx_false, n_false_background)
  )
  
  idx_background_xgb = sample(idx_background_xgb)
  
  x_background_shap_xgb = x_shap_full_xgb[idx_background_xgb, , drop = FALSE]
}

phi0_xgb = mean(
  pred_xgb_default(
    model = modello_xgb,
    newdata = x_background_shap_xgb
  ),
  na.rm = TRUE
)

# Osservazioni da spiegare
if (exists("osservazioni_shap_info") &&
    "id_originale" %in% names(osservazioni_shap_info) &&
    max(osservazioni_shap_info$id_originale, na.rm = TRUE) <= nrow(x_shap_full_xgb)) {
  
  cat("\nUso le stesse osservazioni spiegate nel LASSO-SMOTE.\n")
  
  idx_explain_xgb = osservazioni_shap_info$id_originale
  
} else {
  
  cat("\nOsservazioni LASSO non trovate. Creo un nuovo campione stratificato.\n")
  
  set.seed(123)
  
  n_explain_shap_xgb = 1000
  
  idx_true = which(y_factor_xgb == "True")
  idx_false = which(y_factor_xgb == "False")
  
  n_true_explain = min(35, length(idx_true))
  n_false_explain = min(n_explain_shap_xgb - n_true_explain, length(idx_false))
  
  idx_explain_xgb = c(
    sample(idx_true, n_true_explain),
    sample(idx_false, n_false_explain)
  )
  
  idx_explain_xgb = sample(idx_explain_xgb)
}

osservazioni_shap_info_xgb = data.frame(
  id_originale = idx_explain_xgb,
  y = y_factor_xgb[idx_explain_xgb],
  pred_default = pred_default_full_xgb[idx_explain_xgb]
) %>%
  mutate(
    gruppo = ifelse(y == "True", "Default", "Non default"),
    explain_id = row_number()
  )

x_explain_shap_xgb = x_shap_full_xgb[
  osservazioni_shap_info_xgb$id_originale,
  ,
  drop = FALSE
]

# Selezione variabili principali XGBoost
importanza_xgb = xgboost::xgb.importance(
  feature_names = colnames(x_shap_full_xgb),
  model = booster_xgb
)

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

variabili_numeriche_continue = names(x_shap_full_xgb)[
  sapply(x_shap_full_xgb, is.numeric) &
    sapply(x_shap_full_xgb, function(z) length(unique(z[!is.na(z)])) > 10)
]

importanza_xgb_continue = importanza_xgb %>%
  filter(
    Feature %in% colnames(x_shap_full_xgb),
    Feature %in% variabili_numeriche_continue,
    !grepl(pattern_esclusione, Feature, ignore.case = TRUE)
  ) %>%
  arrange(desc(Gain))

variabili_principali_shap_xgb = importanza_xgb_continue %>%
  slice_head(n = n_variabili_principali_shap_xgb) %>%
  pull(Feature)

# Gruppi SHAP: variabili principali + altre variabili
altre_variabili_xgb = setdiff(
  colnames(x_shap_full_xgb),
  variabili_principali_shap_xgb
)

gruppi_shap_xgb = c(
  as.list(variabili_principali_shap_xgb),
  list(altre_variabili = altre_variabili_xgb)
)

names(gruppi_shap_xgb) = c(
  variabili_principali_shap_xgb,
  "altre_variabili"
)

# Funzioni richieste da shapr per modello custom XGBoost
predict_model_xgb_shapr = function(model, newdata) {
  
  pred_xgb_default(
    model = model,
    newdata = newdata
  )
}

get_model_specs_xgb = function(model) {
  
  labels = colnames(x_shap_full_xgb)
  
  classes = rep("numeric", length(labels))
  names(classes) = labels
  
  factor_levels = vector("list", length(labels))
  names(factor_levels) = labels
  
  return(
    list(
      labels = labels,
      classes = classes,
      factor_levels = factor_levels
    )
  )
}

# Calcolo conditional SHAP values XGBoost
set.seed(123)

spiegazione_shap_xgb = shapr::explain(
  model = modello_xgb,
  x_explain = x_explain_shap_xgb,
  x_train = x_background_shap_xgb,
  approach = approach_shap_xgb,
  phi0 = phi0_xgb,
  group = gruppi_shap_xgb,
  n_MC_samples = n_MC_samples_shap_xgb,
  max_n_coalitions = max_n_coalitions_shap_xgb,
  seed = 123,
  predict_model = predict_model_xgb_shapr,
  get_model_specs = get_model_specs_xgb,
  verbose = "basic"
)

# Estrazione risultati SHAP XGBoost
shap_values_xgb = as.data.frame(
  shapr::get_results(
    spiegazione_shap_xgb,
    what = "shapley_est"
  )
)

colonne_phi_xgb = setdiff(
  names(shap_values_xgb),
  c("explain_id", "none")
)

shap_values_xgb$pred_ricostruita = shap_values_xgb$none +
  rowSums(shap_values_xgb[, colonne_phi_xgb, drop = FALSE])

shap_values_xgb = shap_values_xgb %>%
  left_join(
    osservazioni_shap_info_xgb %>%
      select(
        explain_id,
        id_originale,
        y,
        pred_default,
        gruppo
      ),
    by = "explain_id"
  )

# Dataset lungo per grafici
shap_long_xgb = shap_values_xgb %>%
  select(
    explain_id,
    y,
    gruppo,
    pred_default,
    all_of(colonne_phi_xgb)
  ) %>%
  pivot_longer(
    cols = all_of(colonne_phi_xgb),
    names_to = "variabile",
    values_to = "shap_value"
  ) %>%
  mutate(
    shap_value_pp = shap_value * 100
  )

# Importanza conditional SHAP XGBoost
importanza_conditional_shap_xgb = shap_long_xgb %>%
  group_by(variabile) %>%
  summarise(
    mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
    mean_abs_shap_pp = mean(abs(shap_value_pp), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_abs_shap))

# Grafico importanza globale XGBoost
dati_importanza_shap_xgb_plot = importanza_conditional_shap_xgb %>%
  dplyr::filter(variabile != "altre_variabili") %>%
  dplyr::slice_head(n = 15) %>%
  dplyr::mutate(
    variabile_label = dplyr::recode(
      variabile,
      "shareholders_funds_log" = "Patrimonio netto",
      "solv_ratio_log" = "Indice di solvibilità",
      "leverage_log" = "Leverage",
      "cash_flow_log" = "Flusso di cassa",
      "ebitda_log" = "Margine operativo lordo",
      "roe_2017" = "ROE",
      "rcr_log" = "Consumi di materie prime",
      "tang_fix_assets_log" = "Immobilizzazioni materiali",
      "roa_log" = "ROA",
      "services_log" = "Costi per servizi",
      "liq_ratio_2017" = "Liquidity ratio",
      "st_payables_log" = "Debiti a breve termine",
      "g_profit_log" = "Gross profit",
      "quality_of_earnings" = "Qualità degli utili",
      "bt_profit_log" = "Utile ante imposte",
      "daw_log" = "Ammortamenti e svalutazioni",
      "ebitda_to_sales_log" = "EBITDA margin",
      "curr_assets_log" = "Attivo corrente",
      "labor_efficiency" = "Efficienza del lavoro",
      "curr_ratio_2017" = "Current ratio",
      "intang_fix_assets_log" = "Immobilizzazioni immateriali",
      "inventories_log" = "Rimanenze",
      "assets_log" = "Totale attivo",
      "ros_2017" = "ROS",
      "net_work_cap_log" = "Capitale circolante netto",
      "turnover_per_emp_log" = "Ricavi per dipendente",
      "added_value_per_emp_log" = "Valore aggiunto per dipendente",
      "added_value_log" = "Valore aggiunto",
      "rev_from_sales_log" = "Ricavi di vendita",
      "fin_fix_assets_log" = "Immobilizzazioni finanziarie",
      "lt_due_to_suppliers_log" = "Debiti verso fornitori a lungo termine",
      "lt_due_to_banks_log" = "Debiti verso banche a lungo termine",
      "curr_liab_to_assets_2017" = "Incidenza delle passività correnti sull’attivo",
      .default = variabile
    ),
    variabile_label = stringr::str_wrap(variabile_label, width = 55),
    variabile_label = factor(
      variabile_label,
      levels = rev(variabile_label)
    ),
    label_valore = scales::number(
      mean_abs_shap_pp,
      accuracy = 0.001,
      decimal.mark = ","
    )
  )

grafico_importanza_shap_xgb = ggplot(
  dati_importanza_shap_xgb_plot,
  aes(
    x = variabile_label,
    y = mean_abs_shap_pp
  )
) +
  geom_col(
    aes(fill = mean_abs_shap_pp),
    width = 0.68,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = label_valore,
      y = mean_abs_shap_pp
    ),
    hjust = -0.15,
    size = 3.4,
    colour = "black"
  ) +
  coord_flip(clip = "off") +
  scale_fill_gradient(
    low = "#c7e9c0",
    high = "#006d2c"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    x = NULL,
    y = "Conditional SHAP medio (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(
      size = 10.5,
      colour = "black"
    ),
    axis.text.x = element_text(
      size = 10.5,
      colour = "black"
    ),
    axis.title.x = element_text(
      size = 11.5,
      face = "bold",
      margin = margin(t = 8)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.35,
      colour = "grey82"
    ),
    plot.margin = margin(10, 35, 10, 10)
  )

grafico_importanza_shap_xgb

ggsave(
  filename = "grafico_importanza_conditional_shap_xgb_cost_sensitive.png",
  plot = grafico_importanza_shap_xgb,
  width = 8.5,
  height = 6.2,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = "grafico_importanza_conditional_shap_xgb_cost_sensitive.pdf",
  plot = grafico_importanza_shap_xgb,
  width = 8.5,
  height = 6.2,
  bg = "white"
)

# Grafico distribuzione conditional SHAP XGBoost
ordine_variabili_xgb = importanza_conditional_shap_xgb %>%
  filter(variabile != "altre_variabili") %>%
  slice_head(n = 15) %>%
  pull(variabile)

grafico_distribuzione_shap_xgb = shap_long_xgb %>%
  filter(
    variabile %in% ordine_variabili_xgb,
    variabile != "altre_variabili"
  ) %>%
  mutate(
    variabile = factor(variabile, levels = rev(ordine_variabili_xgb)),
    classe = factor(
      y,
      levels = c("False", "True"),
      labels = c("Non default", "Default")
    )
  ) %>%
  ggplot(
    aes(
      x = shap_value_pp,
      y = variabile,
      color = classe
    )
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.35,
    colour = "grey65"
  ) +
  geom_point(
    alpha = 0.70,
    size = 2,
    position = position_jitter(height = 0.12, width = 0)
  ) +
  scale_color_manual(
    values = c("Non default" = "#4E79A7", "Default" = "#E15759")
  ) +
  labs(
    x = "Conditional SHAP value (%)",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text = element_text(size = 10.5, colour = "black"),
    axis.title = element_text(size = 11.5, face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 10.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.35, colour = "grey80"),
    plot.margin = margin(10, 10, 10, 10)
  )

grafico_distribuzione_shap_xgb

ggsave(
  filename = "grafico_distribuzione_conditional_shap_xgb_cost_sensitive.png",
  plot = grafico_distribuzione_shap_xgb,
  width = 8,
  height = 6,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = "grafico_distribuzione_conditional_shap_xgb_cost_sensitive.pdf",
  plot = grafico_distribuzione_shap_xgb,
  width = 8,
  height = 6,
  bg = "white"
)

# Funzione per creare waterfall XGBoost
crea_waterfall_xgb = function(
    id_locale_xgb,
    nome_file,
    colore_aumenta,
    colore_riduce,
    colore_valori
) {
  
  info_locale_xgb = osservazioni_shap_info_xgb %>%
    dplyr::filter(explain_id == id_locale_xgb)
  
  if (nrow(info_locale_xgb) != 1) {
    stop("id_locale_xgb non valido: non trovo una sola osservazione in osservazioni_shap_info_xgb.")
  }
  
  waterfall_df_xgb = shap_long_xgb %>%
    dplyr::filter(explain_id == id_locale_xgb) %>%
    dplyr::mutate(
      shap_value_pp = shap_value * 100,
      variabile = dplyr::recode(
        variabile,
        "tang_fix_assets_log" = "Immobilizzazioni materiali",
        "lt_due_to_suppliers_log" = "Debiti verso fornitori a lungo termine",
        "services_log" = "Costi per servizi",
        "bt_profit_log" = "Utile ante imposte",
        "daw_log" = "Ammortamenti e svalutazioni",
        "rcr_log" = "Consumi di materie prime",
        "curr_liab_to_assets_2017" = "Incidenza delle passività correnti sull’attivo",
        "turnover_per_emp_log" = "Ricavi per dipendente",
        "curr_assets_log" = "Attivo corrente",
        "added_value_per_emp_log" = "Valore aggiunto per dipendente",
        "inventories_log" = "Rimanenze",
        "assets_log" = "Totale attivo",
        "shareholders_funds_log" = "Patrimonio netto",
        "solv_ratio_log" = "Solvency ratio",
        "leverage_log" = "Leverage",
        "cash_flow_log" = "Flusso di cassa",
        "ebitda_log" = "Margine operativo lordo",
        "roe_2017" = "ROE",
        "roa_log" = "ROA",
        "liq_ratio_2017" = "Liquidity ratio",
        "curr_ratio_2017" = "Current ratio",
        "st_payables_log" = "Debiti a breve termine",
        "rev_from_sales_log" = "Ricavi di vendita",
        "g_profit_log" = "Gross profit",
        "intang_fix_assets_log" = "Immobilizzazioni immateriali",
        "added_value_log" = "Valore aggiunto",
        "wages_log" = "Salari e stipendi",
        "op_margin_log" = "Margine operativo",
        "ebitda_to_sales_log" = "EBITDA margin",
        "fin_fix_assets_log" = "Immobilizzazioni finanziarie",
        "lt_due_to_banks_log" = "Debiti verso banche a lungo termine",
        "net_work_cap_log" = "Capitale circolante netto",
        "assets_turnover_2017" = "Asset turnover",
        "quality_of_earnings" = "Quality of earnings",
        "labor_efficiency" = "Efficienza del lavoro",
        "altre_variabili" = "altre variabili",
        .default = variabile
      )
    ) %>%
    dplyr::arrange(desc(abs(shap_value_pp)))
  
  waterfall_top_xgb = waterfall_df_xgb
  
  baseline_pp_xgb = phi0_xgb * 100
  pred_finale_pp_xgb = info_locale_xgb$pred_default * 100
  
  waterfall_top_xgb = waterfall_top_xgb %>%
    dplyr::mutate(
      start = baseline_pp_xgb + dplyr::lag(cumsum(shap_value_pp), default = 0),
      end = baseline_pp_xgb + cumsum(shap_value_pp),
      xmin = pmin(start, end),
      xmax = pmax(start, end),
      direzione = ifelse(shap_value_pp >= 0, "Aumenta", "Riduce"),
      label_valore = paste0(
        ifelse(shap_value_pp >= 0, "+", ""),
        round(shap_value_pp, 2)
      )
    )
  
  ordine_steps_xgb = c(
    "Baseline",
    waterfall_top_xgb$variabile,
    "Predizione finale"
  )
  
  df_posizioni_xgb = data.frame(
    step = ordine_steps_xgb,
    y_pos = rev(seq_along(ordine_steps_xgb))
  )
  
  waterfall_top_xgb = waterfall_top_xgb %>%
    dplyr::mutate(step = variabile) %>%
    dplyr::left_join(df_posizioni_xgb, by = "step")
  
  df_start_end_xgb = data.frame(
    step = c("Baseline", "Predizione finale"),
    value = c(baseline_pp_xgb, pred_finale_pp_xgb)
  ) %>%
    dplyr::left_join(df_posizioni_xgb, by = "step") %>%
    dplyr::mutate(
      xmin = pmin(0, value),
      xmax = pmax(0, value)
    )
  
  limiti_x_xgb = range(
    c(
      0,
      baseline_pp_xgb,
      pred_finale_pp_xgb,
      waterfall_top_xgb$xmin,
      waterfall_top_xgb$xmax
    ),
    na.rm = TRUE
  )
  
  offset_x_xgb = diff(limiti_x_xgb) * 0.015
  
  if (!is.finite(offset_x_xgb) || offset_x_xgb == 0) {
    offset_x_xgb = 0.01
  }
  
  grafico_waterfall_shap_xgb = ggplot() +
    
    geom_vline(
      xintercept = 0,
      linewidth = 0.35,
      colour = "grey65"
    ) +
    
    geom_rect(
      data = df_start_end_xgb,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = y_pos - 0.35,
        ymax = y_pos + 0.35
      ),
      fill = colore_valori,
      colour = "black",
      linewidth = 0.25
    ) +
    
    geom_rect(
      data = waterfall_top_xgb,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = y_pos - 0.35,
        ymax = y_pos + 0.35,
        fill = direzione
      ),
      colour = "black",
      linewidth = 0.25
    ) +
    
    geom_text(
      data = waterfall_top_xgb,
      aes(
        x = ifelse(shap_value_pp >= 0, xmax + offset_x_xgb, xmin - offset_x_xgb),
        y = y_pos,
        label = label_valore,
        hjust = ifelse(shap_value_pp >= 0, 0, 1)
      ),
      size = 3.2
    ) +
    
    geom_text(
      data = df_start_end_xgb,
      aes(
        x = xmax + offset_x_xgb,
        y = y_pos,
        label = round(value, 2)
      ),
      hjust = 0,
      size = 3.3,
      fontface = "bold"
    ) +
    
    scale_y_continuous(
      breaks = df_posizioni_xgb$y_pos,
      labels = df_posizioni_xgb$step
    ) +
    
    scale_fill_manual(
      values = c(
        "Aumenta" = colore_aumenta,
        "Riduce" = colore_riduce
      )
    ) +
    
    labs(
      x = "Output del modello (%)",
      y = NULL
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      axis.text.y = element_text(size = 10.5, colour = "black"),
      axis.text.x = element_text(size = 10.5, colour = "black"),
      axis.title.x = element_text(size = 11.5, face = "bold", margin = margin(t = 8)),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.35, colour = "grey80"),
      plot.margin = margin(10, 20, 10, 10)
    )
  
  print(grafico_waterfall_shap_xgb)
  
  ggsave(
    filename = paste0(nome_file, ".png"),
    plot = grafico_waterfall_shap_xgb,
    width = 9,
    height = 7,
    dpi = 400,
    bg = "white"
  )
  
  ggsave(
    filename = paste0(nome_file, ".pdf"),
    plot = grafico_waterfall_shap_xgb,
    width = 9,
    height = 7,
    bg = "white"
  )
  
  return(grafico_waterfall_shap_xgb)
}

# Creo i due waterfall XGBoost sulle stesse imprese del LASSO
grafico_xgb_default = crea_waterfall_xgb(
  id_locale_xgb = id_default_xgb,
  nome_file = "grafico_waterfall_conditional_shap_xgb_cost_sensitive_default",
  colore_aumenta = "#B2182B",
  colore_riduce = "#F4A3A3",
  colore_valori = "#7F1D1D"
)

grafico_xgb_nondefault = crea_waterfall_xgb(
  id_locale_xgb = id_nondefault_xgb,
  nome_file = "grafico_waterfall_conditional_shap_xgb_cost_sensitive_nondefault",
  colore_aumenta = "#2166AC",
  colore_riduce = "#BBD7EA",
  colore_valori = "#08306B"
)