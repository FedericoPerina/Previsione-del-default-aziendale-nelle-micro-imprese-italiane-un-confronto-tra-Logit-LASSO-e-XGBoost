library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)

# Trasformazione logaritmica con mantenimento del segno
dati = dati %>%
  mutate(
    ebitda_to_sales_log = sign(ebitda_to_sales_2017) * log1p(abs(ebitda_to_sales_2017)),
    
    # Creazione robusta della variabile default per il grafico
    default_plot = case_when(
      tolower(as.character(default_2019)) %in% c("true", "1", "default", "yes") ~ "Default",
      tolower(as.character(default_2019)) %in% c("false", "0", "non default", "no") ~ "Non default",
      TRUE ~ NA_character_
    ),
    
    default_plot = factor(default_plot, levels = c("Non default", "Default"))
  )

table(dati$default_plot, useNA = "ifany")

# Istogramma variabile originale
p1 = ggplot(dati, aes(x = ebitda_to_sales_2017)) +
  geom_histogram(
    bins = 100,
    fill = "grey75",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Variabile originale",
    x = "EBITDA to sales",
    y = "Frequenza"
  ) +
  scale_x_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# Istogramma variabile trasformata
p2 = ggplot(dati, aes(x = ebitda_to_sales_log)) +
  geom_histogram(
    bins = 100,
    fill = "grey75",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Variabile trasformata",
    x = "EBITDA to sales log",
    y = "Frequenza"
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# Densità della variabile trasformata per default / non default
p3 = ggplot(
  dati %>% filter(!is.na(default_plot), !is.na(ebitda_to_sales_log)),
  aes(x = ebitda_to_sales_log, color = default_plot)
) +
  geom_density(
    linewidth = 1.1,
    adjust = 1.1
  ) +
  scale_color_manual(
    values = c("Non default" = "black", "Default" = "red")
  ) +
  labs(
    title = "Densità per stato di default",
    x = "EBITDA to sales log",
    y = "Densità",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Grafico finale
grafico_finale1 = p1 + p2 +
  plot_layout(ncol = 2)

grafico_finale1
p3

ggsave(
  filename = "distr_ebitda_to_sales.png",
  plot = p1,
  width = 5,
  height = 3.2,
  dpi = 300
)

ggsave(
  filename = "distr_ebitda_to_sales_log.png",
  plot = p2,
  width = 5,
  height = 3.2,
  dpi = 300
)

ggsave(
  filename = "distr_ebitda_to_sales_default.png",
  plot = p3,
  width = 5,
  height = 5,
  dpi = 300
)

controlla_score = function(res, titolo = "Distribuzione degli score") {
  
  out = res$output
  
  # Calcolo limiti 1%-99%
  limiti = quantile(out$predyp, c(0.00001, 0.99999), na.rm = TRUE)
  
  out_trim = out[
    out$predyp >= limiti[1] & out$predyp <= limiti[2],
  ]
  
  roc_ok = pROC::roc(
    response = out_trim$testy,
    predictor = out_trim$predyp,
    levels = c("False", "True"),
    direction = "<"
  )
  
  roc_reverse = pROC::roc(
    response = out_trim$testy,
    predictor = -out_trim$predyp,
    levels = c("False", "True"),
    direction = "<"
  )
  
  print(pROC::auc(roc_ok))
  print(pROC::auc(roc_reverse))
  
  out_trim$testy_label = ifelse(out_trim$testy == "False", "Non default", "Default")
  out_trim$testy_label = factor(out_trim$testy_label, levels = c("Non default", "Default"))
  
  p = ggplot(out_trim, aes(x = testy_label, y = predyp, fill = testy_label)) +
    geom_boxplot(
      width = 0.55,
      outlier.shape = NA,
      alpha = 0.85,
      linewidth = 0.35
    ) +
    scale_fill_manual(
      values = c(
        "Non default" = "#4E79A7",
        "Default" = "#E15759"
      )
    ) +
    labs(
      title = titolo,
      x = "Classe reale",
      y = "Score predetto"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      axis.text = element_text(size = 10),
      axis.title.y = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  print(p)
}
controlla_score(res_lasso_no_balance, "Distribuzione degli score - Lasso (No bilanciamento)")
controlla_score(res_lasso_under, "Distribuzione degli score - Lasso (Undersampling)")
controlla_score(res_lasso_smote, "Distribuzione degli score - Lasso (SMOTE)")

score_lasso = bind_rows(
  data.frame(
    modello = "No bilanciamento",
    classe_reale = res_lasso_no_balance$output$testy,
    score = res_lasso_no_balance$output$predyp
  ),
  data.frame(
    modello = "Undersampling",
    classe_reale = res_lasso_under$output$testy,
    score = res_lasso_under$output$predyp
  ),
  data.frame(
    modello = "SMOTE",
    classe_reale = res_lasso_smote$output$testy,
    score = res_lasso_smote$output$predyp
  )
)

score_lasso = score_lasso %>%
  mutate(
    classe_reale = ifelse(classe_reale == "False", "Non default", "Default"),
    classe_reale = factor(classe_reale, levels = c("Non default", "Default")),
    modello = factor(
      modello,
      levels = c("No bilanciamento", "Undersampling", "SMOTE")
    )
  )

limiti_score = quantile(score_lasso$score, c(0.01, 0.99), na.rm = TRUE)

grafico_score_lasso = ggplot(
  score_lasso,
  aes(x = classe_reale, y = score, fill = classe_reale)
) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    alpha = 0.85,
    linewidth = 0.35
  ) +
  facet_wrap(~ modello, nrow = 1) +
  coord_cartesian(ylim = limiti_score) +
  scale_fill_manual(
    values = c(
      "Non default" = "#4E79A7",
      "Default" = "#E15759"
    )
  ) +
  labs(
    x = NULL,
    y = "Score predetto"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    strip.text = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

grafico_score_lasso
ggsave(
  filename = "score_lasso_boxplot.png",
  plot = grafico_score_lasso,
  width = 8.5,
  height = 4,
  dpi = 300
)

estrai_score_xgb = function(res, nome_modello) {
  
  out = res$output
  
  data.frame(
    modello = nome_modello,
    classe_reale = ifelse(out$testy == "True", "Default", "Non default"),
    score = out$predyp
  )
}

score_xgb = bind_rows(
  estrai_score_xgb(res_xgb_no_balance2, "No bilanciamento"),
  estrai_score_xgb(res_xgb_under2, "Undersampling"),
  estrai_score_xgb(res_xgb_smote2, "SMOTE"),
  estrai_score_xgb(res_xgb_cost_sensitive2, "Cost-sensitive")
)

score_xgb$classe_reale = factor(
  score_xgb$classe_reale,
  levels = c("Non default", "Default")
)

score_xgb$modello = factor(
  score_xgb$modello,
  levels = c("No bilanciamento", "Undersampling", "SMOTE", "Cost-sensitive")
)

limiti_score_xgb = c(0, 1)

grafico_score_xgb = ggplot(
  score_xgb,
  aes(x = classe_reale, y = score, fill = classe_reale)
) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    alpha = 0.85,
    linewidth = 0.35
  ) +
  facet_wrap(~ modello, nrow = 2, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Non default" = "#4E79A7",
      "Default" = "#E15759"
    )
  ) +
  labs(
    x = NULL,
    y = "Score predetto"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    strip.text = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

grafico_score_xgb

ggsave(
  filename = "grafico_score_xgb.png",
  plot = grafico_score_xgb,
  width = 11,
  height = 5.5,
  dpi = 300
)

controlla_score(res_xgb_no_balance2)
controlla_score(res_xgb_under2)
controlla_score(res_xgb_smote2)
controlla_score(res_xgb_cost_sensitive2)

score_xgb_zoom = score_xgb %>%
  group_by(modello) %>%
  mutate(
    q01 = quantile(score, 0.01, na.rm = TRUE),
    q99 = quantile(score, 0.99, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(
    score >= q01,
    score <= q99
  )

grafico_score_xgb = ggplot(
  score_xgb_zoom,
  aes(x = classe_reale, y = score, fill = classe_reale)
) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    alpha = 0.85,
    linewidth = 0.35
  ) +
  facet_wrap(
    ~ modello,
    nrow = 2,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Non default" = "#4E79A7",
      "Default" = "#E15759"
    )
  ) +
  labs(
    x = NULL,
    y = "Score predetto"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

grafico_score_xgb

ggsave(
  filename = "grafico_score_xgb.png",
  plot = grafico_score_xgb,
  width = 8,
  height = 6,
  dpi = 300
)
