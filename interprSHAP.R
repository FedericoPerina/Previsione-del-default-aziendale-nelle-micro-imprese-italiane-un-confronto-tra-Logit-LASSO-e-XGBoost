# Conditional SHAP values per modello LASSO con SMOTE
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(shapr)
library(pROC)

# Impostazioni generali
res_lasso_interpretazione = res_lasso_smote

modello_lasso = res_lasso_interpretazione$final_fit

lambda_lasso = 0.0001807

set.seed(123)

# Numero di osservazioni usate come background per stimare la distribuzione condizionata
n_background_shap = 500

# Numero massimo di coalizioni da usare nella stima SHAP
# Aumentalo se vuoi più precisione, ma sarà più lento
max_n_coalitions_shap = 50

# Numero di campioni Monte Carlo
n_MC_samples_shap = 50

# Approccio conditional SHAP
approach_shap = "gaussian"

# Preparazione dati X e y
x_shap_full = imputa_mediana(x)
x_shap_full = as.data.frame(x_shap_full)

x_shap_full[] = lapply(x_shap_full, as.numeric)

y_char = as.character(y)

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

# Funzione di predizione del LASSO
pred_lasso_raw = function(model, newdata) {
  
  newdata = as.data.frame(newdata)
  newdata[] = lapply(newdata, as.numeric)
  
  newx = as.matrix(newdata[, colnames(x_shap_full), drop = FALSE])
  
  pred = tryCatch(
    {
      as.numeric(
        predict(
          model,
          newx = newx,
          s = lambda_lasso,
          type = "response"
        )
      )
    },
    error = function(e) {
      as.numeric(
        predict(
          model,
          newx = newx,
          type = "response"
        )
      )
    }
  )
  
  return(pred)
}

# Controllo direzione della probabilità
pred_raw_full = pred_lasso_raw(
  model = modello_lasso,
  newdata = x_shap_full
)

print(summary(pred_raw_full))
print(tapply(pred_raw_full, y_factor, summary))

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

inverti_probabilita = auc_invertita > auc_raw

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

pred_default_full = pred_lasso_default(
  model = modello_lasso,
  newdata = x_shap_full
)

roc_finale = pROC::roc(
  response = y_factor,
  predictor = pred_default_full,
  levels = c("False", "True"),
  direction = "<",
  quiet = TRUE
)

# Creazione del background dataset per conditional SHAP
idx_true = which(y_factor == "True")
idx_false = which(y_factor == "False")

quota_true = length(idx_true) / nrow(x_shap_full)

n_true_background = round(n_background_shap * quota_true)
n_false_background = n_background_shap - n_true_background

n_true_background = min(n_true_background, length(idx_true))
n_false_background = min(n_false_background, length(idx_false))

idx_background = c(
  sample(idx_true, n_true_background),
  sample(idx_false, n_false_background)
)

idx_background = sample(idx_background)

x_background_shap = x_shap_full[idx_background, , drop = FALSE]

phi0_lasso = mean(
  pred_lasso_default(
    model = modello_lasso,
    newdata = x_background_shap
  ),
  na.rm = TRUE
)

# Scelta osservazioni da spiegare
set.seed(123)

n_explain_shap = 1000

idx_true = which(y_factor == "True")
idx_false = which(y_factor == "False")

n_true_explain = min(35, length(idx_true))
n_false_explain = min(n_explain_shap - n_true_explain, length(idx_false))

idx_explain = c(
  sample(idx_true, n_true_explain),
  sample(idx_false, n_false_explain)
)

idx_explain = sample(idx_explain)

osservazioni_shap_info = data.frame(
  id_originale = idx_explain,
  y = y_factor[idx_explain],
  pred_default = pred_default_full[idx_explain]
) %>%
  mutate(
    gruppo = ifelse(y == "True", "Default", "Non default"),
    explain_id = row_number()
  )

x_explain_shap = x_shap_full[
  osservazioni_shap_info$id_originale,
  ,
  drop = FALSE
]

# Selezione variabili principali del LASSO
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

pattern_esclusione = paste(
  c(
    "^zero_",
    "^one_",
    "^is_na_",
    "n_na",
    "NA_count",
    "na_count",
    "missing"
  ),
  collapse = "|"
)

coef_lasso_continue = coef_lasso_df %>%
  filter(
    !grepl(pattern_esclusione, variabile, ignore.case = TRUE),
    variabile %in% colnames(x_shap_full)
  )

# Variabili principali da lasciare singolarmente nella spiegazione
n_variabili_principali_shap = 12

variabili_principali_shap = coef_lasso_continue %>%
  slice_head(n = n_variabili_principali_shap) %>%
  pull(variabile)

# Gruppi SHAP: variabili principali + altre variabili
altre_variabili = setdiff(
  colnames(x_shap_full),
  variabili_principali_shap
)

gruppi_shap = c(
  as.list(variabili_principali_shap),
  list(altre_variabili = altre_variabili)
)

names(gruppi_shap) = c(
  variabili_principali_shap,
  "altre_variabili"
)

# Funzioni richieste da shapr per modello custom
predict_model_lasso_shapr = function(model, newdata) {
  
  pred_lasso_default(
    model = model,
    newdata = newdata
  )
}

get_model_specs_lasso = function(model) {
  
  labels = colnames(x_shap_full)
  
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

# Calcolo conditional SHAP values
set.seed(123)

spiegazione_shap_lasso = shapr::explain(
  model = modello_lasso,
  x_explain = x_explain_shap,
  x_train = x_background_shap,
  approach = approach_shap,
  phi0 = phi0_lasso,
  group = gruppi_shap,
  n_MC_samples = n_MC_samples_shap,
  max_n_coalitions = max_n_coalitions_shap,
  seed = 123,
  predict_model = predict_model_lasso_shapr,
  get_model_specs = get_model_specs_lasso,
  verbose = "basic"
)

# Estrazione risultati SHAP
shap_values = as.data.frame(
  shapr::get_results(
    spiegazione_shap_lasso,
    what = "shapley_est"
  )
)

colonne_phi = setdiff(
  names(shap_values),
  c("explain_id", "none")
)

shap_values$pred_ricostruita = shap_values$none +
  rowSums(shap_values[, colonne_phi, drop = FALSE])

shap_values = shap_values %>%
  left_join(
    osservazioni_shap_info %>%
      select(
        explain_id,
        id_originale,
        y,
        pred_default,
        gruppo
      ),
    by = "explain_id"
  )

cat("\nControllo additività:\n")
print(
  shap_values %>%
    select(
      explain_id,
      y,
      gruppo,
      pred_default,
      none,
      pred_ricostruita
    )
)

# Dataset lungo per grafici
shap_long = shap_values %>%
  select(
    explain_id,
    y,
    gruppo,
    pred_default,
    all_of(colonne_phi)
  ) %>%
  pivot_longer(
    cols = all_of(colonne_phi),
    names_to = "variabile",
    values_to = "shap_value"
  ) %>%
  mutate(
    shap_value_pp = shap_value * 100
  )

# Importanza globale conditional SHAP
importanza_conditional_shap = shap_long %>%
  group_by(variabile) %>%
  summarise(
    mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
    mean_abs_shap_pp = mean(abs(shap_value_pp), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_abs_shap))

# Grafico globale: mean absolute conditional SHAP
dati_importanza_shap_plot = importanza_conditional_shap %>%
  dplyr::filter(variabile != "altre_variabili") %>%
  dplyr::slice_head(n = 15) %>%
  dplyr::mutate(
    variabile_label = dplyr::recode(
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

grafico_importanza_shap = ggplot(
  dati_importanza_shap_plot,
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

grafico_importanza_shap

ggsave(
  filename = "grafico_importanza_conditional_shap_lasso_smote.png",
  plot = grafico_importanza_shap,
  width = 8.5,
  height = 6.2,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = "grafico_importanza_conditional_shap_lasso_smote.pdf",
  plot = grafico_importanza_shap,
  width = 8.5,
  height = 6.2,
  bg = "white"
)

# Grafico distribuzione conditional SHAP values
ordine_variabili = importanza_conditional_shap %>%
  filter(variabile != "altre_variabili") %>%
  slice_head(n = 15) %>%
  pull(variabile)

grafico_distribuzione_shap = shap_long %>%
  filter(
    variabile %in% ordine_variabili,
    variabile != "altre_variabili"
  ) %>%
  mutate(
    variabile = factor(variabile, levels = rev(ordine_variabili)),
    classe = factor(y, levels = c("False", "True"),
                    labels = c("Non default", "Default"))
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

grafico_distribuzione_shap

ggsave(
  filename = "grafico_distribuzione_conditional_shap_lasso_smote.png",
  plot = grafico_distribuzione_shap,
  width = 8,
  height = 6,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = "grafico_distribuzione_conditional_shap_lasso_smote.pdf",
  plot = grafico_distribuzione_shap,
  width = 8,
  height = 6,
  bg = "white"
)

# Scelta rapida osservazioni TRUE e FALSE per grafici locali SHAP
# Controllo quante osservazioni True/False ho nel subset SHAP
table(osservazioni_shap_info$y)

# Lista dei default nel subset SHAP, ordinati per score predetto decrescente
osservazioni_default = osservazioni_shap_info %>%
  filter(y == "True") %>%
  arrange(desc(pred_default)) %>%
  select(
    explain_id,
    id_originale,
    y,
    gruppo,
    pred_default
  )

# Lista dei non-default nel subset SHAP, ordinati per score predetto crescente
osservazioni_nondefault = osservazioni_shap_info %>%
  filter(y == "False") %>%
  arrange(pred_default) %>%
  select(
    explain_id,
    id_originale,
    y,
    gruppo,
    pred_default
  )

osservazioni_default
osservazioni_nondefault

id_default = osservazioni_default %>%
  dplyr::slice(1) %>%
  dplyr::pull(explain_id)

id_nondefault = osservazioni_nondefault %>%
  dplyr::slice(1) %>%
  dplyr::pull(explain_id)

id_default
id_nondefault

# Waterfall plot locali LASSO: una impresa default e una non-default
osservazioni_default_lasso = osservazioni_shap_info %>%
  dplyr::mutate(y_chr = as.character(y)) %>%
  dplyr::filter(y_chr == "True") %>%
  dplyr::arrange(desc(pred_default)) %>%
  dplyr::select(explain_id, id_originale, y, gruppo, pred_default)

osservazioni_nondefault_lasso = osservazioni_shap_info %>%
  dplyr::mutate(y_chr = as.character(y)) %>%
  dplyr::filter(y_chr == "False") %>%
  dplyr::arrange(pred_default) %>%
  dplyr::select(explain_id, id_originale, y, gruppo, pred_default)

if (nrow(osservazioni_default_lasso) == 0) {
  stop("Nel subset SHAP LASSO non ci sono osservazioni True/default.")
}

if (nrow(osservazioni_nondefault_lasso) == 0) {
  stop("Nel subset SHAP LASSO non ci sono osservazioni False/non-default.")
}

# Id interni per il grafico LASSO
id_default_lasso = osservazioni_default_lasso$explain_id[1]
id_nondefault_lasso = osservazioni_nondefault_lasso$explain_id[1]

# Id originali delle due imprese: questi servono per XGBoost
impresa_default = osservazioni_default_lasso$id_originale[1]
impresa_nondefault = osservazioni_nondefault_lasso$id_originale[1]

# Funzione per creare waterfall LASSO
crea_waterfall_lasso = function(id_locale, nome_file, colore_aumenta, colore_riduce, colore_valori) {
  
  info_locale = osservazioni_shap_info %>%
    dplyr::filter(explain_id == id_locale)
  
  if (nrow(info_locale) != 1) {
    stop("id_locale non valido: non trovo una sola osservazione in osservazioni_shap_info.")
  }
  
  waterfall_df = shap_long %>%
    dplyr::filter(explain_id == id_locale) %>%
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
  
  waterfall_top = waterfall_df
  
  baseline_pp = phi0_lasso * 100
  pred_finale_pp = info_locale$pred_default * 100
  
  waterfall_top = waterfall_top %>%
    dplyr::mutate(
      start = baseline_pp + dplyr::lag(cumsum(shap_value_pp), default = 0),
      end = baseline_pp + cumsum(shap_value_pp),
      xmin = pmin(start, end),
      xmax = pmax(start, end),
      direzione = ifelse(shap_value_pp >= 0, "Aumenta", "Riduce"),
      label_valore = paste0(
        ifelse(shap_value_pp >= 0, "+", ""),
        round(shap_value_pp, 2)
      )
    )
  
  ordine_steps = c(
    "Baseline",
    waterfall_top$variabile,
    "Predizione finale"
  )
  
  df_posizioni = data.frame(
    step = ordine_steps,
    y_pos = rev(seq_along(ordine_steps))
  )
  
  waterfall_top = waterfall_top %>%
    dplyr::mutate(step = variabile) %>%
    dplyr::left_join(df_posizioni, by = "step")
  
  df_start_end = data.frame(
    step = c("Baseline", "Predizione finale"),
    value = c(baseline_pp, pred_finale_pp)
  ) %>%
    dplyr::left_join(df_posizioni, by = "step") %>%
    dplyr::mutate(
      xmin = pmin(0, value),
      xmax = pmax(0, value)
    )
  
  limiti_x = range(
    c(
      0,
      baseline_pp,
      pred_finale_pp,
      waterfall_top$xmin,
      waterfall_top$xmax
    ),
    na.rm = TRUE
  )
  
  offset_x = diff(limiti_x) * 0.015
  
  if (!is.finite(offset_x) || offset_x == 0) {
    offset_x = 0.01
  }
  
  grafico_waterfall_shap_lasso = ggplot() +
    
    geom_vline(
      xintercept = 0,
      linewidth = 0.35,
      colour = "grey65"
    ) +
    
    geom_rect(
      data = df_start_end,
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
      data = waterfall_top,
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
      data = waterfall_top,
      aes(
        x = ifelse(shap_value_pp >= 0, xmax + offset_x, xmin - offset_x),
        y = y_pos,
        label = label_valore,
        hjust = ifelse(shap_value_pp >= 0, 0, 1)
      ),
      size = 3.2
    ) +
    
    geom_text(
      data = df_start_end,
      aes(
        x = xmax + offset_x,
        y = y_pos,
        label = round(value, 2)
      ),
      hjust = 0,
      size = 3.3,
      fontface = "bold"
    ) +
    
    scale_y_continuous(
      breaks = df_posizioni$y_pos,
      labels = df_posizioni$step
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
  
  print(grafico_waterfall_shap_lasso)
  
  ggsave(
    filename = paste0(nome_file, ".png"),
    plot = grafico_waterfall_shap_lasso,
    width = 11,
    height = 7,
    dpi = 400,
    bg = "white"
  )
  
  ggsave(
    filename = paste0(nome_file, ".pdf"),
    plot = grafico_waterfall_shap_lasso,
    width = 11,
    height = 7,
    bg = "white"
  )
  
  return(grafico_waterfall_shap_lasso)
}

# Creo i due waterfall LASSO
grafico_lasso_default = crea_waterfall_lasso(
  id_locale = id_default_lasso,
  nome_file = "grafico_waterfall_conditional_shap_lasso_smote_default",
  colore_aumenta = "#B2182B",
  colore_riduce = "#F4A3A3",
  colore_valori = "#7F1D1D"
)

grafico_lasso_nondefault = crea_waterfall_lasso(
  id_locale = id_nondefault_lasso,
  nome_file = "grafico_waterfall_conditional_shap_lasso_smote_nondefault",
  colore_aumenta = "#2166AC",
  colore_riduce = "#BBD7EA",
  colore_valori = "#08306B"
)