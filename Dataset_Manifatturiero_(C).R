### librerie
library(tidyverse)
# devtools::install_github("myles-lewis/nestedcv")
library(nestedcv)
library(caret)
library(ROSE)
library(ggplot2)
library(pROC)
library(dplyr)

# install.packages("pkgbuild")
# pkgbuild::check_build_tools(debug = TRUE)
# remove.packages("xgboost")
# install.packages("remotes")
# remotes::install_version("xgboost", version = "1.7.10.1", repos = "https://cloud.r-project.org")
# packageVersion("xgboost")

### carico i dati
dati = read.csv("dati/dati_C.csv")

### variabili categoriche e identificative--------------------------------------
# controllo dei missing
skimr::skim(dati)

table(dati$default_2019, useNA = "ifany")
prop.table(table(dati$default_2019, useNA = "ifany"))

# firm_id
dati = dati %>%
  select(-c(firm_id))

# ws_status_2017
table(dati[, "ws_status_2017"], dati[, "default_2019"], useNA = "ifany")
prop.table(
  table(dati[, "ws_status_2017"], dati[, "default_2019"], useNA = "ifany"),
  margin = 1) # non sembra influire sul default
dati = dati %>%
  select(-c(ws_status_2017))

# incorp_date
summary(as.Date(dati[, "incorp_date"]))
dati = dati %>%
  mutate(
    incorp_date = as.Date(incorp_date),
    incorp_year = as.integer(format(incorp_date, "%Y")),
    periodo_incorp = case_when(
      incorp_year < 1990 ~ "Prima del 1990",
      incorp_year >= 1990 & incorp_year < 2000 ~ "1990-1999",
      incorp_year >= 2000 & incorp_year < 2005 ~ "2000-2004",
      incorp_year >= 2005 & incorp_year < 2010 ~ "2005-2009",
      incorp_year >= 2010 & incorp_year < 2015 ~ "2010-2014",
      incorp_year >= 2015 ~ "Dal 2015 in poi",
      TRUE ~ NA_character_
    )
  )
tab_periodo = table(dati$periodo_incorp, dati$default_2019, useNA = "ifany")
tab_periodo
prop.table(tab_periodo, margin = 1) # non sembra influire sul default
dati = dati %>%
  select(-c(incorp_date, incorp_year, periodo_incorp))

# last_obs_date
summary(as.Date(dati[, "last_obs_date"]))
dati = dati %>%
  mutate(
    last_obs_date = as.Date(last_obs_date),
    last_obs_year = as.integer(format(last_obs_date, "%Y")),
    periodo_last_obs = case_when(
      last_obs_year < 2019 ~ "Prima del 2019",
      last_obs_year >= 2023 ~ "Dal 2019 in poi",
      TRUE ~ NA_character_
    )
  )
tab_periodo = table(dati$periodo_last_obs, dati$default_2019, useNA = "ifany")
tab_periodo
prop.table(tab_periodo, margin = 1) # corretta per definizione di default
rm(tab_periodo)
dati = dati %>%
  select(-c(last_obs_date, last_obs_year, periodo_last_obs))

# firm_status
table(dati[, "firm_status"], dati[, "default_2019"], useNA = "ifany")
prop.table(
  table(dati[, "firm_status"], dati[, "default_2019"], useNA = "ifany"),
  margin = 1) # corretta per definizione di default
dati = dati %>%
  select(-c(firm_status))

# firm_kind
table(dati[, "firm_kind"], dati[, "default_2019"], useNA = "ifany")
prop.table(
  table(dati[, "firm_kind"], dati[, "default_2019"], useNA = "ifany"),
  margin = 1)

dati$firm_kind_group = NA
dati$firm_kind_group[
  dati$firm_kind %in% c(
    "Limited liability company - SRL",
    "One-person company with limited liability - SRL",
    "Simplified limited liability company"
  )
] = "Societa a responsabilita limitata"
dati$firm_kind_group[
  dati$firm_kind %in% c(
    "Joint stock company - SPA",
    "One-person joint stock company - SPA",
    "Limited partnership with shares - SAPA"
  )
] = "Societa per azioni"
dati$firm_kind_group[
  dati$firm_kind %in% c(
    "Cooperative company with limited liability - SCARL",
    "Cooperative company with limited liability by shares - SCARLPA",
    "Cooperative company with limited liability, small - SCARL",
    "Consortium of cooperatives",
    "Limited liability consortium cooperative",
    "Small cooperative company",
    "Social cooperative company"
  )
] = "Societa cooperative"
dati$firm_kind_group[
  dati$firm_kind %in% c(
    "Consortium",
    "Consortium by shares",
    "Limited liability consortium"
  )
] = "Consorzi e forme consortili"

dati$firm_kind_group[
  dati$firm_kind %in% c(
    "General partnership - SNC",
    "Limited partnership - SAS"
  )
] = "Societa di persone"
dati$firm_kind_group[
  is.na(dati$firm_kind_group)
] = "Altre forme giuridiche"
tab_firm_kind = table(
  dati$firm_kind_group,
  dati$default_2019,
  useNA = "ifany"
)
tab_firm_kind
prop.table(tab_firm_kind, margin = 1)
addmargins(tab_firm_kind)
dati$firm_kind_group = factor(
  dati$firm_kind_group,
  levels = c(
    "Societa a responsabilita limitata",
    "Societa per azioni",
    "Societa cooperative",
    "Consorzi e forme consortili",
    "Societa di persone",
    "Altre forme giuridiche"
  )
)
tab_firm_kind = table(
  dati$firm_kind_group,
  dati$default_2019,
  useNA = "ifany"
)
tab_firm_kind
prop.table(tab_firm_kind, margin = 1)

rm(tab_firm_kind)
dati = dati %>%
  select(-c(firm_kind, firm_kind_group))

# firm_size
dati = dati %>%
  select(-c(firm_size))

# nace4_id
summary(as.factor(dati[, "nace4_id"]))
table(dati[, "nace4_id"], dati[, "default_2019"], useNA = "ifany")
prop.table(
  table(dati[, "nace4_id"], dati[, "default_2019"], useNA = "ifany"),
  margin = 1) # non sembra influire sul default

dati = dati %>%
  mutate(nace2_id = substr(nace4_id, 1, 3))
tab_nace2 = dati %>%
  group_by(nace2_id) %>%
  summarise(
    non_default = sum(default_2019 == "False", na.rm = TRUE),
    default = sum(default_2019 == "True", na.rm = TRUE),
    totale = n(),
    perc_non_default = non_default / totale,
    perc_default = default / totale
  ) %>%
  arrange(nace2_id)
print(tab_nace2, n = Inf)

rm(tab_nace2)
dati = dati %>%
  select(-c(nace4_id, nace2_id))

# nuts3_id
summary(as.factor(dati[, "nuts3_id"]))
dati = dati %>%
  mutate(
    macro_area = case_when(
      substr(nuts3_id, 3, 3) == "C" ~ "Nord-Ovest",
      substr(nuts3_id, 3, 3) == "F" ~ "Sud",
      substr(nuts3_id, 3, 3) == "H" ~ "Nord-Est",
      substr(nuts3_id, 3, 3) == "I" ~ "Centro",
      substr(nuts3_id, 3, 3) == "G" ~ "Isole",
      TRUE ~ NA_character_
    )
  )
tab_area = table(dati$macro_area, dati$default_2019, useNA = "ifany")
tab_area
prop.table(tab_area, margin = 1)
rm(tab_area)
dati = dati %>%
  select(-c(nuts3_id, macro_area))

# consolidation_id
summary(as.factor(dati[, "consolidation_id"]))
table(dati[, "consolidation_id"], dati[, "default_2019"], useNA = "ifany")
prop.table(
  table(dati[, "consolidation_id"], dati[, "default_2019"], useNA = "ifany"),
  margin = 1) # leggera sovrapresenza di TRUE su U2
dati = dati %>%
  select(-c(consolidation_id))

# bvd_indep_id
table(dati[, "bvd_indep_id"], dati[, "default_2019"], useNA = "ifany")
table(dati[, "bvd_indep_id"], dati[, "managers"], useNA = "ifany")
table(dati[, "managers"], dati[, "default_2019"], useNA = "ifany")
prop.table(
  table(dati[, "bvd_indep_id"], dati[, "default_2019"], useNA = "ifany"),
  margin = 1) # altissima sovrapresenza di TRUE su "-" (azienda senza shareholders rilevanti)
dati = dati %>%
  select(-c(bvd_indep_id))

# inno_startup
table(dati[, "inno_startup"], dati[, "default_2019"], useNA = "ifany")
prop.table(
  table(dati[, "inno_startup"], dati[, "default_2019"], useNA = "ifany"),
  margin = 1) # non sembra influire sul default
dati = dati %>%
  select(-c(inno_startup))

# inno_sme
table(dati[, "inno_sme"], dati[, "default_2019"], useNA = "ifany")
prop.table(
  table(dati[, "inno_sme"], dati[, "default_2019"], useNA = "ifany"),
  margin = 1) # non sembra influire sul default
dati = dati %>%
  select(-c(inno_sme))

# imp_exp
table(dati[, "imp_exp"], dati[, "default_2019"], useNA = "ifany")
prop.table(
  table(dati[, "imp_exp"], dati[, "default_2019"], useNA = "ifany"),
  margin = 1) # non sembra influire sul default
dati = dati %>%
  select(-c(imp_exp))

dati = dati %>%
  select(-c(city_name, city_id, postcode_id))

skimr::skim(dati)

# subsidiaries
summary(dati$subsidiaries)
table(as.factor(dati$subsidiaries), dati$default_2019, useNA = "ifany")
dati$subsidiaries_factor = ifelse(
  is.na(dati$subsidiaries),
  NA,
  ifelse(dati$subsidiaries >= 3, "3+", as.character(dati$subsidiaries))
)
dati$subsidiaries_factor = factor(
  dati$subsidiaries_factor,
  levels = c("0", "1", "2", "3+"),
  ordered = TRUE
)
dati = dati %>%
  select(-c(subsidiaries, subsidiaries_factor))

# corp_group
summary(dati$corp_group)
table(as.factor(dati$corp_group), dati$default_2019, useNA = "ifany")
dati$corp_group_factor = ifelse(
  is.na(dati$corp_group), NA, ifelse(dati$corp_group >= 7, "7+", as.character(dati$corp_group))
)
dati$corp_group_factor = factor(
  dati$corp_group_factor,
  levels = c("0", "2", "3", "4", "5", "6", "7+"),
  ordered = TRUE
)
dati = dati %>%
  select(-c(corp_group, corp_group_factor))

# shareholders
summary(dati$shareholders)
table(as.factor(dati$shareholders), dati$default_2019, useNA = "ifany")
dati$shareholders_factor = ifelse(
  is.na(dati$shareholders), NA, ifelse(dati$shareholders >= 6, "6+", as.character(dati$shareholders))
)
dati$shareholders_factor = factor(
  dati$shareholders_factor,
  levels = c("0", "1", "2", "3", "4", "5", "6+"),
  ordered = TRUE
)
dati = dati %>%
  select(-c(shareholders, shareholders_factor))

# managers
summary(dati$managers)
table(as.factor(dati$managers), dati$default_2019, useNA = "ifany")
dati = dati %>%
  select(-c(managers))

skimr::skim(dati)

### controllo quanti NA ci sono in ogni variabile
na_perc = colMeans(is.na(dati)) * 100 # percentuale di NA per ogni variabile
na_perc[order(na_perc, decreasing = TRUE)]

### Imputazione analitica delle variabili con vincoli---------------------------
imputa_analitica_one_shot = function(dati, p_low = 0.01, p_high = 0.99) {
  
  n_na_block = function(data, vars) {
    rowSums(is.na(data[, vars]))
  }
  
  bounds_from_formula = function(x, p_low = 0.01, p_high = 0.99) {
    x = x[is.finite(x) & !is.na(x)]
    c(
      low = quantile(x, p_low, na.rm = TRUE, names = FALSE),
      high = quantile(x, p_high, na.rm = TRUE, names = FALSE)
    )
  }
  
  in_bounds = function(x, b) {
    x >= b["low"] & x <= b["high"]
  }
  
  vars_sel = c(
    "roa_2017", "op_margin_2017", "assets_2017",
    "roe_2017", "profit_2017", "shareholders_funds_2017",
    "assets_turnover_2017", "rev_from_sales_2017",
    "curr_liab_to_assets_2017", "st_payables_2017", "payables_2017",
    "curr_ratio_2017", "curr_assets_2017",
    "solv_ratio_2017",
    "cash_flow_2017", "daw_2017",
    "added_value_per_emp_2017", "added_value_2017", "emp_2017",
    "net_work_cap_2017",
    "leverage_2017"
  )
  
  dati$n_na_selected = n_na_block(dati, vars_sel)
  
  min_assets = 1
  min_equity = 1
  min_payables = 1
  min_st_pay = 1
  min_ratio = 0.1
  
  # =========================
  # 1) ROA / OP_MARGIN / ASSETS
  # roa = op_margin / assets * 100
  # =========================
  nn = n_na_block(dati, c("roa_2017", "op_margin_2017", "assets_2017"))
  
  roa_ref = (dati$op_margin_2017 / dati$assets_2017) * 100
  idx_ref = !is.na(dati$op_margin_2017) & !is.na(dati$assets_2017) & abs(dati$assets_2017) > min_assets
  b_roa = bounds_from_formula(roa_ref[idx_ref], p_low, p_high)
  
  roa_calc = (dati$op_margin_2017 / dati$assets_2017) * 100
  idx = nn == 1 & is.na(dati$roa_2017) & !is.na(dati$op_margin_2017) & !is.na(dati$assets_2017) &
    abs(dati$assets_2017) > min_assets & in_bounds(roa_calc, b_roa)
  dati$roa_2017[idx] = roa_calc[idx]
  
  op_margin_ref = (dati$roa_2017 / 100) * dati$assets_2017
  idx_ref = !is.na(dati$roa_2017) & !is.na(dati$assets_2017)
  b_op_margin = bounds_from_formula(op_margin_ref[idx_ref], p_low, p_high)
  
  op_margin_calc = (dati$roa_2017 / 100) * dati$assets_2017
  idx = nn == 1 & is.na(dati$op_margin_2017) & !is.na(dati$roa_2017) & !is.na(dati$assets_2017) &
    in_bounds(op_margin_calc, b_op_margin)
  dati$op_margin_2017[idx] = op_margin_calc[idx]
  
  assets_ref = dati$op_margin_2017 / (dati$roa_2017 / 100)
  idx_ref = !is.na(dati$op_margin_2017) & !is.na(dati$roa_2017) & abs(dati$roa_2017) > min_ratio & assets_ref > 0
  b_assets_from_roa = bounds_from_formula(assets_ref[idx_ref], p_low, p_high)
  
  assets_calc = dati$op_margin_2017 / (dati$roa_2017 / 100)
  idx = nn == 1 & is.na(dati$assets_2017) & !is.na(dati$op_margin_2017) & !is.na(dati$roa_2017) &
    abs(dati$roa_2017) > min_ratio & assets_calc > 0 & in_bounds(assets_calc, b_assets_from_roa)
  dati$assets_2017[idx] = assets_calc[idx]
  
  # =========================
  # 2) ROE / PROFIT / SHAREHOLDERS_FUNDS
  # roe = profit / equity * 100
  # =========================
  nn = n_na_block(dati, c("roe_2017", "profit_2017", "shareholders_funds_2017"))
  
  roe_ref = (dati$profit_2017 / dati$shareholders_funds_2017) * 100
  idx_ref = !is.na(dati$profit_2017) & !is.na(dati$shareholders_funds_2017) & abs(dati$shareholders_funds_2017) > min_equity
  b_roe = bounds_from_formula(roe_ref[idx_ref], p_low, p_high)
  
  roe_calc = (dati$profit_2017 / dati$shareholders_funds_2017) * 100
  idx = nn == 1 & is.na(dati$roe_2017) & !is.na(dati$profit_2017) & !is.na(dati$shareholders_funds_2017) &
    abs(dati$shareholders_funds_2017) > min_equity & in_bounds(roe_calc, b_roe)
  dati$roe_2017[idx] = roe_calc[idx]
  
  profit_ref = (dati$roe_2017 / 100) * dati$shareholders_funds_2017
  idx_ref = !is.na(dati$roe_2017) & !is.na(dati$shareholders_funds_2017) & abs(dati$roe_2017) > min_ratio
  b_profit_from_roe = bounds_from_formula(profit_ref[idx_ref], p_low, p_high)
  
  profit_calc = (dati$roe_2017 / 100) * dati$shareholders_funds_2017
  idx = nn == 1 & is.na(dati$profit_2017) & !is.na(dati$roe_2017) & !is.na(dati$shareholders_funds_2017) &
    abs(dati$roe_2017) > min_ratio & in_bounds(profit_calc, b_profit_from_roe)
  dati$profit_2017[idx] = profit_calc[idx]
  
  equity_ref = dati$profit_2017 / (dati$roe_2017 / 100)
  idx_ref = !is.na(dati$profit_2017) & !is.na(dati$roe_2017) & abs(dati$roe_2017) > min_ratio
  b_equity_from_roe = bounds_from_formula(equity_ref[idx_ref], p_low, p_high)
  
  equity_calc = dati$profit_2017 / (dati$roe_2017 / 100)
  idx = nn == 1 & is.na(dati$shareholders_funds_2017) & !is.na(dati$profit_2017) & !is.na(dati$roe_2017) &
    abs(dati$roe_2017) > min_ratio & in_bounds(equity_calc, b_equity_from_roe)
  dati$shareholders_funds_2017[idx] = equity_calc[idx]
  
  # =========================
  # 3) ASSETS TURNOVER / REV / ASSETS
  # assets_turnover = rev / assets * 100
  # =========================
  nn = n_na_block(dati, c("assets_turnover_2017", "rev_from_sales_2017", "assets_2017"))
  
  at_ref = (dati$rev_from_sales_2017 / dati$assets_2017) * 100
  idx_ref = !is.na(dati$rev_from_sales_2017) & !is.na(dati$assets_2017) & abs(dati$assets_2017) > min_assets
  b_at = bounds_from_formula(at_ref[idx_ref], p_low, p_high)
  
  at_calc = (dati$rev_from_sales_2017 / dati$assets_2017) * 100
  idx = nn == 1 & is.na(dati$assets_turnover_2017) & !is.na(dati$rev_from_sales_2017) & !is.na(dati$assets_2017) &
    abs(dati$assets_2017) > min_assets & at_calc > 0 & in_bounds(at_calc, b_at)
  dati$assets_turnover_2017[idx] = at_calc[idx]
  
  rev_ref = (dati$assets_turnover_2017 / 100) * dati$assets_2017
  idx_ref = !is.na(dati$assets_turnover_2017) & !is.na(dati$assets_2017)
  b_rev_from_at = bounds_from_formula(rev_ref[idx_ref], p_low, p_high)
  
  rev_calc = (dati$assets_turnover_2017 / 100) * dati$assets_2017
  idx = nn == 1 & is.na(dati$rev_from_sales_2017) & !is.na(dati$assets_turnover_2017) & !is.na(dati$assets_2017) &
    rev_calc >= 0 & in_bounds(rev_calc, b_rev_from_at)
  dati$rev_from_sales_2017[idx] = rev_calc[idx]
  
  assets_ref = dati$rev_from_sales_2017 / (dati$assets_turnover_2017 / 100)
  idx_ref = !is.na(dati$rev_from_sales_2017) & !is.na(dati$assets_turnover_2017) & abs(dati$assets_turnover_2017) > min_ratio & assets_ref > 0
  b_assets_from_at = bounds_from_formula(assets_ref[idx_ref], p_low, p_high)
  
  assets_calc = dati$rev_from_sales_2017 / (dati$assets_turnover_2017 / 100)
  idx = nn == 1 & is.na(dati$assets_2017) & !is.na(dati$rev_from_sales_2017) & !is.na(dati$assets_turnover_2017) &
    abs(dati$assets_turnover_2017) > min_ratio & assets_calc > 0 & in_bounds(assets_calc, b_assets_from_at)
  dati$assets_2017[idx] = assets_calc[idx]
  
  # =========================
  # 4) curr_liab_to_assets / st_payables / payables
  # curr_liab_to_assets = st_payables / payables
  # =========================
  nn = n_na_block(dati, c("curr_liab_to_assets_2017", "st_payables_2017", "payables_2017"))
  
  cla_ref = dati$st_payables_2017 / dati$payables_2017
  idx_ref = !is.na(dati$st_payables_2017) & !is.na(dati$payables_2017) & dati$payables_2017 > min_payables
  b_cla = bounds_from_formula(cla_ref[idx_ref], p_low, p_high)
  
  cla_calc = dati$st_payables_2017 / dati$payables_2017
  idx = nn == 1 & is.na(dati$curr_liab_to_assets_2017) & !is.na(dati$st_payables_2017) & !is.na(dati$payables_2017) &
    dati$payables_2017 > min_payables & cla_calc >= 0 & cla_calc <= 1 & in_bounds(cla_calc, b_cla)
  dati$curr_liab_to_assets_2017[idx] = cla_calc[idx]
  
  st_pay_ref = dati$curr_liab_to_assets_2017 * dati$payables_2017
  idx_ref = !is.na(dati$curr_liab_to_assets_2017) & !is.na(dati$payables_2017) & dati$curr_liab_to_assets_2017 >= 0 & dati$curr_liab_to_assets_2017 <= 1
  b_st_pay_from_cla = bounds_from_formula(st_pay_ref[idx_ref], p_low, p_high)
  
  st_pay_calc = dati$curr_liab_to_assets_2017 * dati$payables_2017
  idx = nn == 1 & is.na(dati$st_payables_2017) & !is.na(dati$curr_liab_to_assets_2017) & !is.na(dati$payables_2017) &
    dati$curr_liab_to_assets_2017 >= 0 & dati$curr_liab_to_assets_2017 <= 1 &
    st_pay_calc >= 0 & in_bounds(st_pay_calc, b_st_pay_from_cla)
  dati$st_payables_2017[idx] = st_pay_calc[idx]
  
  pay_ref = dati$st_payables_2017 / dati$curr_liab_to_assets_2017
  idx_ref = !is.na(dati$st_payables_2017) & !is.na(dati$curr_liab_to_assets_2017) & dati$curr_liab_to_assets_2017 > 0
  b_pay_from_cla = bounds_from_formula(pay_ref[idx_ref], p_low, p_high)
  
  pay_calc = dati$st_payables_2017 / dati$curr_liab_to_assets_2017
  idx = nn == 1 & is.na(dati$payables_2017) & !is.na(dati$st_payables_2017) & !is.na(dati$curr_liab_to_assets_2017) &
    dati$curr_liab_to_assets_2017 > 0 & pay_calc >= 0 & in_bounds(pay_calc, b_pay_from_cla)
  dati$payables_2017[idx] = pay_calc[idx]
  
  # =========================
  # 5) current ratio / curr_assets / st_payables
  # curr_ratio = curr_assets / st_payables
  # =========================
  nn = n_na_block(dati, c("curr_ratio_2017", "curr_assets_2017", "st_payables_2017"))
  
  curr_ratio_ref = dati$curr_assets_2017 / dati$st_payables_2017
  idx_ref = !is.na(dati$curr_assets_2017) & !is.na(dati$st_payables_2017) & dati$st_payables_2017 > min_st_pay
  b_curr_ratio = bounds_from_formula(curr_ratio_ref[idx_ref], p_low, p_high)
  
  curr_ratio_calc = dati$curr_assets_2017 / dati$st_payables_2017
  idx = nn == 1 & is.na(dati$curr_ratio_2017) & !is.na(dati$curr_assets_2017) & !is.na(dati$st_payables_2017) &
    dati$st_payables_2017 > min_st_pay & curr_ratio_calc > 0 & in_bounds(curr_ratio_calc, b_curr_ratio)
  dati$curr_ratio_2017[idx] = curr_ratio_calc[idx]
  
  curr_assets_ref = dati$curr_ratio_2017 * dati$st_payables_2017
  idx_ref = !is.na(dati$curr_ratio_2017) & !is.na(dati$st_payables_2017) & dati$curr_ratio_2017 > 0
  b_curr_assets_from_cr = bounds_from_formula(curr_assets_ref[idx_ref], p_low, p_high)
  
  curr_assets_calc = dati$curr_ratio_2017 * dati$st_payables_2017
  idx = nn == 1 & is.na(dati$curr_assets_2017) & !is.na(dati$curr_ratio_2017) & !is.na(dati$st_payables_2017) &
    dati$curr_ratio_2017 > 0 & curr_assets_calc >= 0 & in_bounds(curr_assets_calc, b_curr_assets_from_cr)
  dati$curr_assets_2017[idx] = curr_assets_calc[idx]
  
  st_pay_ref = dati$curr_assets_2017 / dati$curr_ratio_2017
  idx_ref = !is.na(dati$curr_assets_2017) & !is.na(dati$curr_ratio_2017) & dati$curr_ratio_2017 > 0
  b_st_pay_from_cr = bounds_from_formula(st_pay_ref[idx_ref], p_low, p_high)
  
  st_pay_calc = dati$curr_assets_2017 / dati$curr_ratio_2017
  idx = nn == 1 & is.na(dati$st_payables_2017) & !is.na(dati$curr_assets_2017) & !is.na(dati$curr_ratio_2017) &
    dati$curr_ratio_2017 > 0 & st_pay_calc >= 0 & in_bounds(st_pay_calc, b_st_pay_from_cr)
  dati$st_payables_2017[idx] = st_pay_calc[idx]
  
  # =========================
  # 6) solv_ratio / equity / assets
  # solv_ratio = equity / assets * 100
  # =========================
  nn = n_na_block(dati, c("solv_ratio_2017", "shareholders_funds_2017", "assets_2017"))
  
  solv_ref = (dati$shareholders_funds_2017 / dati$assets_2017) * 100
  idx_ref = !is.na(dati$shareholders_funds_2017) & !is.na(dati$assets_2017) & abs(dati$assets_2017) > min_assets
  b_solv = bounds_from_formula(solv_ref[idx_ref], p_low, p_high)
  
  solv_calc = (dati$shareholders_funds_2017 / dati$assets_2017) * 100
  idx = nn == 1 & is.na(dati$solv_ratio_2017) & !is.na(dati$shareholders_funds_2017) & !is.na(dati$assets_2017) &
    abs(dati$assets_2017) > min_assets & in_bounds(solv_calc, b_solv)
  dati$solv_ratio_2017[idx] = solv_calc[idx]
  
  equity_ref = (dati$solv_ratio_2017 / 100) * dati$assets_2017
  idx_ref = !is.na(dati$solv_ratio_2017) & !is.na(dati$assets_2017)
  b_equity_from_solv = bounds_from_formula(equity_ref[idx_ref], p_low, p_high)
  
  equity_calc = (dati$solv_ratio_2017 / 100) * dati$assets_2017
  idx = nn == 1 & is.na(dati$shareholders_funds_2017) & !is.na(dati$solv_ratio_2017) & !is.na(dati$assets_2017) &
    in_bounds(equity_calc, b_equity_from_solv)
  dati$shareholders_funds_2017[idx] = equity_calc[idx]
  
  assets_ref = dati$shareholders_funds_2017 / (dati$solv_ratio_2017 / 100)
  idx_ref = !is.na(dati$shareholders_funds_2017) & !is.na(dati$solv_ratio_2017) & abs(dati$solv_ratio_2017) > min_ratio & assets_ref > 0
  b_assets_from_solv = bounds_from_formula(assets_ref[idx_ref], p_low, p_high)
  
  assets_calc = dati$shareholders_funds_2017 / (dati$solv_ratio_2017 / 100)
  idx = nn == 1 & is.na(dati$assets_2017) & !is.na(dati$shareholders_funds_2017) & !is.na(dati$solv_ratio_2017) &
    abs(dati$solv_ratio_2017) > min_ratio & assets_calc > 0 & in_bounds(assets_calc, b_assets_from_solv)
  dati$assets_2017[idx] = assets_calc[idx]
  
  # =========================
  # 7) cash_flow / profit / daw
  # cash_flow = profit + daw
  # =========================
  nn = n_na_block(dati, c("cash_flow_2017", "profit_2017", "daw_2017"))
  
  cf_ref = dati$profit_2017 + dati$daw_2017
  idx_ref = !is.na(dati$profit_2017) & !is.na(dati$daw_2017)
  b_cf = bounds_from_formula(cf_ref[idx_ref], p_low, p_high)
  
  cf_calc = dati$profit_2017 + dati$daw_2017
  idx = nn == 1 & is.na(dati$cash_flow_2017) & !is.na(dati$profit_2017) & !is.na(dati$daw_2017) &
    in_bounds(cf_calc, b_cf)
  dati$cash_flow_2017[idx] = cf_calc[idx]
  
  profit_ref = dati$cash_flow_2017 - dati$daw_2017
  idx_ref = !is.na(dati$cash_flow_2017) & !is.na(dati$daw_2017)
  b_profit_from_cf = bounds_from_formula(profit_ref[idx_ref], p_low, p_high)
  
  profit_calc = dati$cash_flow_2017 - dati$daw_2017
  idx = nn == 1 & is.na(dati$profit_2017) & !is.na(dati$cash_flow_2017) & !is.na(dati$daw_2017) &
    in_bounds(profit_calc, b_profit_from_cf)
  dati$profit_2017[idx] = profit_calc[idx]
  
  daw_ref = dati$cash_flow_2017 - dati$profit_2017
  idx_ref = !is.na(dati$cash_flow_2017) & !is.na(dati$profit_2017) & (dati$cash_flow_2017 - dati$profit_2017) >= 0
  b_daw_from_cf = bounds_from_formula(daw_ref[idx_ref], p_low, p_high)
  
  daw_calc = dati$cash_flow_2017 - dati$profit_2017
  idx = nn == 1 & is.na(dati$daw_2017) & !is.na(dati$cash_flow_2017) & !is.na(dati$profit_2017) &
    daw_calc >= 0 & in_bounds(daw_calc, b_daw_from_cf)
  dati$daw_2017[idx] = daw_calc[idx]
  
  # =========================
  # 8) added_value_per_emp / added_value / emp
  # =========================
  nn = n_na_block(dati, c("added_value_per_emp_2017", "added_value_2017", "emp_2017"))
  
  avpe_ref = dati$added_value_2017 / dati$emp_2017
  idx_ref = !is.na(dati$added_value_2017) & !is.na(dati$emp_2017) & dati$emp_2017 > 0
  b_avpe = bounds_from_formula(avpe_ref[idx_ref], p_low, p_high)
  
  avpe_calc = dati$added_value_2017 / dati$emp_2017
  idx = nn == 1 & is.na(dati$added_value_per_emp_2017) & !is.na(dati$added_value_2017) & !is.na(dati$emp_2017) &
    dati$emp_2017 > 0 & in_bounds(avpe_calc, b_avpe)
  dati$added_value_per_emp_2017[idx] = avpe_calc[idx]
  
  av_ref = dati$added_value_per_emp_2017 * dati$emp_2017
  idx_ref = !is.na(dati$added_value_per_emp_2017) & !is.na(dati$emp_2017) & dati$emp_2017 > 0
  b_av_from_avpe = bounds_from_formula(av_ref[idx_ref], p_low, p_high)
  
  av_calc = dati$added_value_per_emp_2017 * dati$emp_2017
  idx = nn == 1 & is.na(dati$added_value_2017) & !is.na(dati$added_value_per_emp_2017) & !is.na(dati$emp_2017) &
    dati$emp_2017 > 0 & in_bounds(av_calc, b_av_from_avpe)
  dati$added_value_2017[idx] = av_calc[idx]
  
  emp_ref = dati$added_value_2017 / dati$added_value_per_emp_2017
  idx_ref = !is.na(dati$added_value_2017) & !is.na(dati$added_value_per_emp_2017) & dati$added_value_per_emp_2017 > 0
  b_emp_from_avpe = bounds_from_formula(emp_ref[idx_ref], p_low, p_high)
  
  emp_calc = dati$added_value_2017 / dati$added_value_per_emp_2017
  idx = nn == 1 & is.na(dati$emp_2017) & !is.na(dati$added_value_2017) & !is.na(dati$added_value_per_emp_2017) &
    dati$added_value_per_emp_2017 > 0 & emp_calc >= 0 & in_bounds(emp_calc, b_emp_from_avpe)
  dati$emp_2017[idx] = round(emp_calc[idx])
  
  # =========================
  # 9) net_work_cap / curr_assets / st_payables
  # =========================
  nn = n_na_block(dati, c("net_work_cap_2017", "curr_assets_2017", "st_payables_2017"))
  
  nwc_ref = dati$curr_assets_2017 - dati$st_payables_2017
  idx_ref = !is.na(dati$curr_assets_2017) & !is.na(dati$st_payables_2017)
  b_nwc = bounds_from_formula(nwc_ref[idx_ref], p_low, p_high)
  
  nwc_calc = dati$curr_assets_2017 - dati$st_payables_2017
  idx = nn == 1 & is.na(dati$net_work_cap_2017) & !is.na(dati$curr_assets_2017) & !is.na(dati$st_payables_2017) &
    in_bounds(nwc_calc, b_nwc)
  dati$net_work_cap_2017[idx] = nwc_calc[idx]
  
  curr_assets_ref = dati$net_work_cap_2017 + dati$st_payables_2017
  idx_ref = !is.na(dati$net_work_cap_2017) & !is.na(dati$st_payables_2017) & (dati$net_work_cap_2017 + dati$st_payables_2017) >= 0
  b_curr_assets_from_nwc = bounds_from_formula(curr_assets_ref[idx_ref], p_low, p_high)
  
  curr_assets_calc = dati$net_work_cap_2017 + dati$st_payables_2017
  idx = nn == 1 & is.na(dati$curr_assets_2017) & !is.na(dati$net_work_cap_2017) & !is.na(dati$st_payables_2017) &
    curr_assets_calc >= 0 & in_bounds(curr_assets_calc, b_curr_assets_from_nwc)
  dati$curr_assets_2017[idx] = curr_assets_calc[idx]
  
  st_pay_ref = dati$curr_assets_2017 - dati$net_work_cap_2017
  idx_ref = !is.na(dati$curr_assets_2017) & !is.na(dati$net_work_cap_2017) & (dati$curr_assets_2017 - dati$net_work_cap_2017) >= 0
  b_st_pay_from_nwc = bounds_from_formula(st_pay_ref[idx_ref], p_low, p_high)
  
  st_pay_calc = dati$curr_assets_2017 - dati$net_work_cap_2017
  idx = nn == 1 & is.na(dati$st_payables_2017) & !is.na(dati$curr_assets_2017) & !is.na(dati$net_work_cap_2017) &
    st_pay_calc >= 0 & in_bounds(st_pay_calc, b_st_pay_from_nwc)
  dati$st_payables_2017[idx] = st_pay_calc[idx]
  
  # =========================
  # 10) leverage / assets / equity
  # leverage = assets / equity
  # =========================
  nn = n_na_block(dati, c("leverage_2017", "assets_2017", "shareholders_funds_2017"))
  
  lev_ref = dati$assets_2017 / dati$shareholders_funds_2017
  idx_ref = !is.na(dati$assets_2017) & !is.na(dati$shareholders_funds_2017) & abs(dati$shareholders_funds_2017) > min_equity
  b_lev = bounds_from_formula(lev_ref[idx_ref], p_low, p_high)
  
  lev_calc = dati$assets_2017 / dati$shareholders_funds_2017
  idx = nn == 1 & is.na(dati$leverage_2017) & !is.na(dati$assets_2017) & !is.na(dati$shareholders_funds_2017) &
    abs(dati$shareholders_funds_2017) > min_equity & in_bounds(lev_calc, b_lev)
  dati$leverage_2017[idx] = lev_calc[idx]
  
  assets_ref = dati$leverage_2017 * dati$shareholders_funds_2017
  idx_ref = !is.na(dati$leverage_2017) & !is.na(dati$shareholders_funds_2017) & (dati$leverage_2017 * dati$shareholders_funds_2017) > 0
  b_assets_from_lev = bounds_from_formula(assets_ref[idx_ref], p_low, p_high)
  
  assets_calc = dati$leverage_2017 * dati$shareholders_funds_2017
  idx = nn == 1 & is.na(dati$assets_2017) & !is.na(dati$leverage_2017) & !is.na(dati$shareholders_funds_2017) &
    assets_calc > 0 & in_bounds(assets_calc, b_assets_from_lev)
  dati$assets_2017[idx] = assets_calc[idx]
  
  equity_ref = dati$assets_2017 / dati$leverage_2017
  idx_ref = !is.na(dati$assets_2017) & !is.na(dati$leverage_2017) & abs(dati$leverage_2017) > min_ratio
  b_equity_from_lev = bounds_from_formula(equity_ref[idx_ref], p_low, p_high)
  
  equity_calc = dati$assets_2017 / dati$leverage_2017
  idx = nn == 1 & is.na(dati$shareholders_funds_2017) & !is.na(dati$assets_2017) & !is.na(dati$leverage_2017) &
    abs(dati$leverage_2017) > min_ratio & in_bounds(equity_calc, b_equity_from_lev)
  dati$shareholders_funds_2017[idx] = equity_calc[idx]
  
  dati
}

dati = imputa_analitica_one_shot(dati)
dati = imputa_analitica_one_shot(dati)
dati = imputa_analitica_one_shot(dati)

skimr::skim(dati)
na_perc = colMeans(is.na(dati)) * 100 # percentuale di NA per ogni variabile
na_perc[order(na_perc, decreasing = TRUE)]

which(na_perc > 50)
dati = dati %>%
  select(-c(which(na_perc > 50))) # elimino tutte le variabili con più del 50% di NA

### controllo quanti NA ci sono in ogni osservazione
dati$NA_count_oss = apply(dati, 1, function(x) sum(is.na(x)))
summary(dati$NA_count_oss)
table(dati$NA_count_oss, useNA = "ifany")
length(which(dati$NA_count_oss >= 20))
table(dati[which(dati$NA_count_oss >= 20), "default_2019"])
length(which(dati$NA_count_oss > 25))
table(dati[which(dati$NA_count_oss > 25), "default_2019"])
length(which(dati$NA_count_oss > 35))
table(dati[which(dati$NA_count_oss > 35), "default_2019"])

# elimino tutte le osservazioni con almeno 25 NA
dati = dati %>%
  filter(!(NA_count_oss > 25))
skimr::skim(dati)

### TRASFORMAZIONE VARIABILI----------------------------------------------------
skimr::skim(dati)
# ebitda_to_sales_2017
summary(dati$ebitda_to_sales_2017)
hist(dati$ebitda_to_sales_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$ebitda_to_sales_log = sign(dati$ebitda_to_sales_2017) * log(1+abs(dati$ebitda_to_sales_2017))
summary(dati$ebitda_to_sales_log)
hist(dati$ebitda_to_sales_log, breaks = 100)
plot(density(dati$ebitda_to_sales_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità EBITDA to sales log",
     xlab = "ebitda_to_sales_log")

lines(density(dati$ebitda_to_sales_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# rev_from_sales_2017
summary(dati$rev_from_sales_2017)
hist(dati$rev_from_sales_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$rev_from_sales_log = log(1+dati$rev_from_sales_2017)
summary(dati$rev_from_sales_log)
hist(dati$rev_from_sales_log, breaks = 100)
plot(density(dati$rev_from_sales_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità rev_from_sales_log",
     xlab = "rev_from_sales_log")

lines(density(dati$rev_from_sales_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

length(dati$rev_from_sales_log[which(dati$rev_from_sales_log == 0)]) # molti zero
dati$zero_rev_from_sales = as.integer(dati$rev_from_sales_2017 == 0)

# ebitda_2017
summary(dati$ebitda_2017)
hist(dati$ebitda_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$ebitda_log = sign(dati$ebitda_2017) * log(1+abs(dati$ebitda_2017))
summary(dati$ebitda_log)
hist(dati$ebitda_log, breaks = 100)
plot(density(dati$ebitda_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità ebitda_log",
     xlab = "ebitda_log")

lines(density(dati$ebitda_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# profit_2017
summary(dati$profit_2017)
hist(dati$profit_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$profit_log = sign(dati$profit_2017) * log(1+abs(dati$profit_2017))
summary(dati$profit_log)
hist(dati$profit_log, breaks = 100)
plot(density(dati$profit_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità profit_log",
     xlab = "profit_log")
lines(density(dati$profit_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# assets_2017
summary(dati$assets_2017)
hist(dati$assets_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$assets_log = log(1+dati$assets_2017)
summary(dati$assets_log)
hist(dati$assets_log, breaks = 100)
plot(density(dati$assets_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità assets_log",
     xlab = "assets_log")
lines(density(dati$assets_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# shareholders_funds_2017
summary(dati$shareholders_funds_2017)
hist(dati$shareholders_funds_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$shareholders_funds_log = sign(dati$shareholders_funds_2017) * log(1+abs(dati$shareholders_funds_2017))
summary(dati$shareholders_funds_log)
hist(dati$shareholders_funds_log, breaks = 100)
plot(density(dati$shareholders_funds_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità shareholders_funds_log",
     xlab = "shareholders_funds_log")
lines(density(dati$shareholders_funds_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# # net_fin_pos_2017
# summary(dati$net_fin_pos_2017)
# hist(dati$net_fin_pos_2017, breaks = 100)
# # trasformo in log per una distribuzione migliore
# dati$net_fin_pos_log = sign(dati$net_fin_pos_2017) * log(1+abs(dati$net_fin_pos_2017))
# summary(dati$net_fin_pos_log)
# hist(dati$net_fin_pos_log, breaks = 100)
# plot(density(dati$net_fin_pos_log[dati$default_2019 == "False"], na.rm = TRUE),
#      col = "black",
#      lwd = 2,
#      main = "Densità net_fin_pos_log",
#      xlab = "net_fin_pos_log")
# lines(density(dati$net_fin_pos_log[dati$default_2019 == "True"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)

# ros_2017
summary(dati$ros_2017)
hist(dati$ros_2017, breaks = 100)
plot(density(dati$ros_2017[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità ros_2017",
     xlab = "ros_2017")
lines(density(dati$ros_2017[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# roa_2017
summary(dati$roa_2017)
hist(dati$roa_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$roa_log = sign(dati$roa_2017) * log(1+abs(dati$roa_2017))
summary(dati$roa_log)
hist(dati$roa_log, breaks = 100)

# roe_2017
summary(dati$roe_2017)
hist(dati$roe_2017, breaks = 100)

# # debt_to_equity_2017
# summary(dati$debt_to_equity_2017)
# hist(dati$debt_to_equity_2017, breaks = 100)
# # trasformo in log per una distribuzione migliore
# dati$debt_to_equity_log = sign(dati$debt_to_equity_2017) * log(1+abs(dati$debt_to_equity_2017))
# summary(dati$debt_to_equity_log)
# hist(dati$debt_to_equity_log, breaks = 100)
# plot(density(dati$debt_to_equity_log[dati$default_2019 == "False"], na.rm = TRUE),
#      col = "black",
#      lwd = 2,
#      main = "Densità debt_to_equity_log",
#      xlab = "debt_to_equity_log")
# lines(density(dati$debt_to_equity_log[dati$default_2019 == "True"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
# 
# length(dati$debt_to_equity_log[which(dati$debt_to_equity_log == 0)]) # molti zero
# dati$debt_to_equity_zero = as.integer(dati$debt_to_equity_log == 0)

# # debt_to_ebitda_2017
# summary(dati$debt_to_ebitda_2017)
# hist(dati$debt_to_ebitda_2017, breaks = 100)
# # trasformo in log per una distribuzione migliore
# dati$debt_to_ebitda_log = sign(dati$debt_to_ebitda_2017) * log(1+abs(dati$debt_to_ebitda_2017))
# summary(dati$debt_to_ebitda_log)
# hist(dati$debt_to_ebitda_log, breaks = 100)
# plot(density(dati$debt_to_ebitda_log[dati$default_2019 == "False"], na.rm = TRUE),
#      col = "black",
#      lwd = 2,
#      main = "Densità debt_to_ebitda_log",
#      xlab = "debt_to_ebitda_log")
# lines(density(dati$debt_to_ebitda_log[dati$default_2019 == "True"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
# 
# length(dati$debt_to_equity_log[which(dati$debt_to_equity_log == 0)]) # molti zero
# dati$debt_to_equity_zero = as.integer(dati$debt_to_equity_log == 0)

# assets_turnover_2017
summary(dati$assets_turnover_2017)
hist(dati$assets_turnover_2017[which(dati$assets_turnover_2017 != 0)], breaks = 100)
plot(density(dati$assets_turnover_2017[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità assets_turnover_2017",
     xlab = "assets_turnover_2017")
lines(density(dati$assets_turnover_2017[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

length(dati$assets_turnover_2017[which(dati$assets_turnover_2017 == 0)]) # molti zero
dati$zero_assets_turnover = as.integer(dati$rev_from_sales_2017 == 0)

# emp_2017
table(dati$emp_2017)
hist(dati$emp_2017, breaks = 100)

length(dati$emp_2017[which(dati$emp_2017 == 0)]) # molti zero
dati$zero_emp = as.integer(dati$emp_2017 == 0)

# payables_2017
summary(dati$payables_2017)
hist(dati$payables_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$payables_log = log(1+dati$payables_2017)
summary(dati$payables_log)
hist(dati$payables_log, breaks = 100)
plot(density(dati$payables_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità payables_log",
     xlab = "payables_log")
lines(density(dati$payables_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# st_payables_2017
summary(dati$st_payables_2017)
hist(dati$st_payables_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$st_payables_log = log(1+dati$st_payables_2017)
summary(dati$st_payables_log)
hist(dati$st_payables_log, breaks = 100)

# rd_exp_2017
summary(dati$rd_exp_2017)
hist(dati$rd_exp_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$rd_exp_log = log(1+dati$rd_exp_2017)
summary(dati$rd_exp_log)
hist(dati$rd_exp_log[which(dati$rd_exp_log != 0)], breaks = 100)
dati = dati %>%
  select(-c(rd_exp_2017, rd_exp_log))

# ipr_assets_2017
summary(dati$ipr_assets_2017)
hist(dati$ipr_assets_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$ipr_assets_log = log(1+dati$ipr_assets_2017)
summary(dati$ipr_assets_log)
hist(dati$ipr_assets_log[which(dati$ipr_assets_log != 0)], breaks = 100)
dati = dati %>%
  select(-c(ipr_assets_2017, ipr_assets_log))

# clt_assets_2017
summary(dati$clt_assets_2017)
hist(dati$clt_assets_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$clt_assets_log = log(1+dati$clt_assets_2017)
summary(dati$clt_assets_log)
hist(dati$clt_assets_log[which(dati$clt_assets_log != 0)], breaks = 100)
dati = dati %>%
  select(-c(clt_assets_2017, clt_assets_log))

# inventories_2017
summary(dati$inventories_2017)
hist(dati$inventories_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$inventories_log = log(1+dati$inventories_2017)
summary(dati$inventories_log)
hist(dati$inventories_log, breaks = 100)
plot(density(dati$inventories_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità inventories_log",
     xlab = "inventories_log")
lines(density(dati$inventories_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

length(dati$inventories_log[which(dati$inventories_log == 0)]) # rischio troppi zero
dati$zero_inventories = as.integer(dati$inventories_log == 0)

# equity_invest_2017
summary(dati$equity_invest_2017)
hist(dati$equity_invest_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$equity_invest_log = log(1+dati$equity_invest_2017)
summary(dati$equity_invest_log)
hist(dati$equity_invest_log, breaks = 100)

length(dati$equity_invest_log[which(dati$equity_invest_log == 0)]) # troppi zero
dati = dati %>%
  select(-c(equity_invest_2017, equity_invest_log))

# intang_fix_assets_2017
summary(dati$intang_fix_assets_2017)
hist(dati$intang_fix_assets_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$intang_fix_assets_log = log(1+dati$intang_fix_assets_2017)
summary(dati$intang_fix_assets_log)
hist(dati$intang_fix_assets_log[which(dati$intang_fix_assets_log != 0)], breaks = 100)

length(dati$intang_fix_assets_log[which(dati$intang_fix_assets_log == 0)]) # rischio troppi zero
dati$zero_intang_fix_assets = as.integer(dati$intang_fix_assets_log == 0)

# tang_fix_assets_2017
summary(dati$tang_fix_assets_2017)
hist(dati$tang_fix_assets_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$tang_fix_assets_log = log(1+dati$tang_fix_assets_2017)
summary(dati$tang_fix_assets_log)
hist(dati$tang_fix_assets_log[which(dati$tang_fix_assets_log != 0)], breaks = 100)

length(dati$tang_fix_assets_log[which(dati$tang_fix_assets_log == 0)]) # molti zero
dati$zero_tang_fix_assets = as.integer(dati$tang_fix_assets_log == 0)

# fin_fix_assets_2017
summary(dati$fin_fix_assets_2017)
hist(dati$fin_fix_assets_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$fin_fix_assets_log = log(1+dati$fin_fix_assets_2017)
summary(dati$fin_fix_assets_log)
hist(dati$fin_fix_assets_log[which(dati$fin_fix_assets_log != 0)], breaks = 100)

length(dati$fin_fix_assets_log[which(dati$fin_fix_assets_log != 0)]) # rischio troppi zero
dati$zero_fin_fix_assets = as.integer(dati$fin_fix_assets_log == 0)

# curr_assets_2017
summary(dati$curr_assets_2017)
hist(dati$curr_assets_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$curr_assets_log = log(1+dati$curr_assets_2017)
summary(dati$curr_assets_log)
hist(dati$curr_assets_log, breaks = 100)
plot(density(dati$curr_assets_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità curr_assets_log",
     xlab = "curr_assets_log")
lines(density(dati$curr_assets_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# curr_liab_to_assets_2017
summary(dati$curr_liab_to_assets_2017)
hist(dati$curr_liab_to_assets_2017, breaks = 100)

length(dati$curr_liab_to_assets_2017[which(dati$curr_liab_to_assets_2017 == 1)]) # troppi 1
dati$one_curr_liab_to_assets = as.integer(dati$curr_liab_to_assets_2017 == 1)

# curr_ratio_2017
summary(dati$curr_ratio_2017)
hist(dati$curr_ratio_2017, breaks = 100)

# solv_ratio_2017
summary(dati$solv_ratio_2017)
hist(dati$solv_ratio_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$solv_ratio_log = sign(dati$solv_ratio_2017) * log(1+abs(dati$solv_ratio_2017))
summary(dati$solv_ratio_log)
hist(dati$solv_ratio_log, breaks = 100)
plot(density(dati$solv_ratio_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità solv_ratio_log",
     xlab = "solv_ratio_log")
lines(density(dati$solv_ratio_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# bt_profit_2017
summary(dati$bt_profit_2017)
hist(dati$bt_profit_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$bt_profit_log = sign(dati$bt_profit_2017) * log(1+abs(dati$bt_profit_2017))
summary(dati$bt_profit_log)
hist(dati$bt_profit_log, breaks = 100)
plot(density(dati$bt_profit_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità bt_profit_log",
     xlab = "bt_profit_log")
lines(density(dati$bt_profit_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# cash_flow_2017
summary(dati$cash_flow_2017)
hist(dati$cash_flow_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$cash_flow_log = sign(dati$cash_flow_2017) * log(1+abs(dati$cash_flow_2017))
summary(dati$cash_flow_log)
hist(dati$cash_flow_log, breaks = 100)
plot(density(dati$cash_flow_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità cash_flow_log",
     xlab = "cash_flow_log")
lines(density(dati$cash_flow_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# turnover_per_emp_2017
summary(dati$turnover_per_emp_2017)
hist(dati$turnover_per_emp_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$turnover_per_emp_log = log(1+dati$turnover_per_emp_2017)
summary(dati$turnover_per_emp_log)
hist(dati$turnover_per_emp_log, breaks = 100)

# added_value_2017
summary(dati$added_value_2017)
hist(dati$added_value_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$added_value_log = sign(dati$added_value_2017) * log(1+abs(dati$added_value_2017))
summary(dati$added_value_log)
hist(dati$added_value_log, breaks = 100)
plot(density(dati$added_value_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità added_value_log",
     xlab = "added_value_log")
lines(density(dati$added_value_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# added_value_per_emp_2017
summary(dati$added_value_per_emp_2017)
hist(dati$added_value_per_emp_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$added_value_per_emp_log = sign(dati$added_value_per_emp_2017) * log(1+abs(dati$added_value_per_emp_2017))
summary(dati$added_value_per_emp_log)
hist(dati$added_value_per_emp_log, breaks = 100)
plot(density(dati$added_value_per_emp_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità added_value_per_emp_log",
     xlab = "added_value_per_emp_log")
lines(density(dati$added_value_per_emp_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# net_work_cap_2017
summary(dati$net_work_cap_2017)
hist(dati$net_work_cap_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$net_work_cap_log = sign(dati$net_work_cap_2017) * log(1+abs(dati$net_work_cap_2017))
summary(dati$net_work_cap_log)
hist(dati$net_work_cap_log, breaks = 100)
plot(density(dati$net_work_cap_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità net_work_cap_log",
     xlab = "net_work_cap_log")
lines(density(dati$net_work_cap_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# lt_due_to_banks_2017
# lt_due_to_others_2017
# lt_due_to_suppliers_2017
debt_vars = c(
  "lt_due_to_banks_2017",
  "lt_due_to_others_2017",
  "lt_due_to_suppliers_2017"
)

for (v in debt_vars) {
  
  base = sub("_2017$", "", v)
  
  dati[[paste0(base, "_log")]] = ifelse(
    is.na(dati[[v]]),
    NA,
    log1p(dati[[v]])
  )
}

# value_of_prod_2017
summary(dati$value_of_prod_2017)
hist(dati$value_of_prod_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$value_of_prod_log = sign(dati$value_of_prod_2017) * log(1+abs(dati$value_of_prod_2017))
summary(dati$value_of_prod_log)
hist(dati$value_of_prod_log, breaks = 100)
plot(density(dati$value_of_prod_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità value_of_prod_log",
     xlab = "value_of_prod_log")
lines(density(dati$value_of_prod_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# rcr_2017
summary(dati$rcr_2017)
hist(dati$rcr_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$rcr_log = sign(dati$rcr_2017) * log(1+abs(dati$rcr_2017))
summary(dati$rcr_log)
hist(dati$rcr_log[which(dati$rcr_log != 0)], breaks = 100)

length(dati$rcr_log[which(dati$rcr_log == 0)]) # molti zero
dati$zero_rcr = as.integer(dati$rcr_log == 0)

# services_2017
summary(dati$services_2017)
hist(dati$services_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$services_log = log(1+dati$services_2017)
summary(dati$services_log)
hist(dati$services_log, breaks = 100)

# personnel_costs_2017
summary(dati$personnel_costs_2017)
hist(dati$personnel_costs_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$personnel_costs_log = log(1+dati$personnel_costs_2017)
summary(dati$personnel_costs_log)
hist(dati$personnel_costs_log, breaks = 100)

length(dati$personnel_costs_log[which(dati$personnel_costs_log == 0)]) # rischio troppi zero
dati$zero_personnel_costs = as.integer(dati$personnel_costs_log == 0)

# wages_2017
summary(dati$wages_2017)
hist(dati$wages_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$wages_log = log(1+dati$wages_2017)
summary(dati$wages_log)
hist(dati$wages_log, breaks = 100)

length(dati$wages_log[which(dati$wages_log == 0)]) # rischio troppi zero
dati$zero_wages = as.integer(dati$wages_log == 0)

# daw_2017
summary(dati$daw_2017)
hist(dati$daw_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$daw_log = log(1+dati$daw_2017)
summary(dati$daw_log)
hist(dati$daw_log, breaks = 100)

# op_margin_2017
summary(dati$op_margin_2017)
hist(dati$op_margin_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$op_margin_log = sign(dati$op_margin_2017) * log(1+abs(dati$op_margin_2017))
summary(dati$op_margin_log)
hist(dati$op_margin_log, breaks = 100)
plot(density(dati$op_margin_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità op_margin_log",
     xlab = "op_margin_log")
lines(density(dati$op_margin_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# liq_ratio_2017
summary(dati$liq_ratio_2017)
hist(dati$liq_ratio_2017, breaks = 100)

# leverage_2017
summary(dati$leverage_2017)
hist(dati$leverage_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$leverage_log = sign(dati$leverage_2017) * log(1+abs(dati$leverage_2017))
summary(dati$leverage_log)
hist(dati$leverage_log, breaks = 100)
plot(density(dati$leverage_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità leverage_log",
     xlab = "leverage_log")
lines(density(dati$leverage_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# g_profit_2017
summary(dati$g_profit_2017)
hist(dati$g_profit_2017, breaks = 100)
# trasformo in log per una distribuzione migliore
dati$g_profit_log = sign(dati$g_profit_2017) * log(1+abs(dati$g_profit_2017))
summary(dati$g_profit_log)
hist(dati$g_profit_log[which(dati$g_profit_log != 0)], breaks = 100)
plot(density(dati$g_profit_log[dati$default_2019 == "False"], na.rm = TRUE),
     col = "black",
     lwd = 2,
     main = "Densità g_profit_log",
     xlab = "g_profit_log")
lines(density(dati$g_profit_log[dati$default_2019 == "True"], na.rm = TRUE),
      col = "red",
      lwd = 2)

# Funzione per aggiungere le Super-Features
crea_super_features = function(df) {
  df = as.data.frame(df)
  
  df %>%
    mutate(
      # 1. Qualità degli utili (Aggiungiamo +0.001 per evitare divisioni per zero)
      quality_of_earnings = cash_flow_2017 / (ebitda_2017 + 0.001),
      
      # 2. Efficienza del Personale
      labor_efficiency = added_value_2017 / (personnel_costs_2017 + 0.001)
    )
}

# Applichiamo ai dati prima di passarli al modello
dati = crea_super_features(dati)

# Sostituiamo eventuali valori Infiniti nati dalle divisioni con NA (XGBoost li gestirà)
is.na(dati) = sapply(dati, is.infinite)


### funzione per imputare gli NA con mediana------------------------------------
imputa_mediana = function(x) {
  x = as.data.frame(x)
  for (j in seq_along(x)) {
    if (is.numeric(x[[j]]) || is.integer(x[[j]])) {
      med = median(x[[j]], na.rm = TRUE)
      
      if (is.finite(med)) {
        x[[j]][is.na(x[[j]])] = med
      } else {
        # colonna tutta NA nel fold
        x[[j]][is.na(x[[j]])] = 0
      }
    }
  }
  x = as.matrix(x)
  storage.mode(x) = "double"
  if (anyNA(x)) stop("modifyX ha lasciato NA in x")
  x
}
### creo la struttura dei dati per la nested CV---------------------------------
y = factor(dati$default_2019)
vars = names(dati)
vars_2017 = grep("_2017$", vars, value = TRUE) # variabili originali tipo variabile_2017
vars_log = grep("_log$", vars, value = TRUE) # variabili log tipo variabile_log
# nomi base
base_2017 = sub("_2017$", "", vars_2017)
base_log = sub("_log$", "", vars_log)
# variabili _2017 da eliminare se esiste la versione _log
vars_da_eliminare = vars_2017[base_2017 %in% base_log]
# nuovo dataset
x = dati[, !(names(dati) %in% vars_da_eliminare)]
# trasformo le character in factor
x[] = lapply(x, function(col) {
  if (is.character(col)) as.factor(col) else col
})
x = data.matrix(x)
x = subset(x, select = -default_2019) # tolgo l'intercetta

# Numero di core fisici
cores = 10 # parallel::detectCores(logical = FALSE)

### per testare i modelli su un campione
set.seed(123)
percentuale = 0.1
idx_sample = unlist(
  tapply(seq_along(y), y, function(indici) {
    sample(indici, size = ceiling(length(indici) * percentuale))
  })
)
x_small = x[idx_sample, ]
y_small = y[idx_sample]
table(y)
prop.table(table(y))
table(y_small)
prop.table(table(y_small))

### secondo campione
set.seed(456)

percentuale = 0.1

idx_sample_2 = unlist(
  tapply(seq_along(y), y, function(indici) {
    
    indici_disponibili = setdiff(indici, idx_sample)
    
    sample(
      indici_disponibili,
      size = ceiling(length(indici) * percentuale)
    )
  })
)

x_small_2 = x[idx_sample_2, ]
y_small_2 = y[idx_sample_2]

table(y_small_2)
prop.table(table(y_small_2))

# controllo che non ci siano osservazioni in comune
length(intersect(idx_sample, idx_sample_2))

### terzo campione test
set.seed(789)

percentuale_test = 0.1

idx_test = unlist(
  tapply(seq_along(y), y, function(indici) {
    
    indici_disponibili = setdiff(
      indici,
      c(idx_sample, idx_sample_2)
    )
    
    sample(
      indici_disponibili,
      size = ceiling(length(indici) * percentuale_test)
    )
  })
)

x_test = x[idx_test, ]
y_test = y[idx_test]

table(y_test)
prop.table(table(y_test))

# controllo che non ci siano osservazioni in comune con il primo campione
length(intersect(idx_sample, idx_test))

# controllo che non ci siano osservazioni in comune con il secondo campione
length(intersect(idx_sample_2, idx_test))

# controllo complessivo
length(intersect(c(idx_sample, idx_sample_2), idx_test))

### funzione per ricalibrare la soglia------------------------------------------
calibra_soglia_youden = function(modello) {
  
  out = modello$output
  
  y_true = factor(out$testy, levels = c("False", "True"))
  prob_true = out$predyp
  
  roc_obj = roc(
    response = y_true,
    predictor = prob_true,
    levels = c("False", "True"),
    direction = "<"
  )
  
  best_threshold = as.numeric(coords(
    roc_obj,
    x = "best",
    best.method = "youden",
    ret = "threshold"
  ))
  
  pred_calibrata = factor(
    ifelse(prob_true >= best_threshold, "True", "False"),
    levels = c("False", "True")
  )
  
  print(best_threshold)
  print(table(pred_calibrata))
  print(prop.table(table(pred_calibrata)))
  print(confusionMatrix(
    pred_calibrata,
    y_true,
    positive = "True"
  ))
  
  return(list(
    threshold = best_threshold,
    pred_calibrata = pred_calibrata,
    confusion_matrix = confusionMatrix(pred_calibrata, y_true, positive = "True"),
    roc = roc_obj
  ))
}

# ------------------------------------------------------------------------------
library(caret)
# set.seed(123)
outer_folds = createFolds(
  y = y,
  k = 3,
  returnTrain = FALSE
)

### Nested cross-validation con LASSO
res_lasso_no_balance = nestcv.glmnet(
  y = y,
  x = x,
  family = "binomial",
  modifyX = imputa_mediana,
  modifyX_useY = FALSE,
  alphaSet = 1,
  na.option = "pass",
  outer_folds = outer_folds,
  n_inner_folds = 3,
  cv.cores = cores,
  verbose = TRUE,
  trace.it = 2
)
# Risultati sintetici
summary(res_lasso_no_balance)
calibra_soglia_youden(res_lasso_no_balance)

### Nested cross-validation con LASSO, bilanciamento random under-sampling
res_lasso_under = nestcv.glmnet(
  y = y,
  x = x,
  family = "binomial",
  modifyX = imputa_mediana,
  modifyX_useY = FALSE,
  alphaSet = 1,
  na.option = "pass",
  balance = "randomsample",
  balance_options = list(
    minor = 1,
    major = 0.4
  ),
  outer_folds = outer_folds,
  n_inner_folds = 3,
  cv.cores = cores,
  verbose = TRUE,
  trace.it = 2
)
# Risultati sintetici
summary(res_lasso_under)
calibra_soglia_youden(res_lasso_under)

### Nested cross-validation con LASSO, bilanciamento SMOTE
res_lasso_smote = nestcv.glmnet(
  y = y,
  x = x,
  family = "binomial",
  modifyX = imputa_mediana,
  modifyX_useY = FALSE,
  alphaSet = 1,
  na.option = "pass",
  balance = "smote",
  balance_options = list(
    k = 5,
    over = NULL
  ),
  outer_folds = outer_folds,
  n_inner_folds = 3,
  cv.cores = cores,
  verbose = TRUE,
  trace.it = 2
)
# Risultati sintetici
summary(res_lasso_smote)
calibra_soglia_youden(res_lasso_smote)

controlla_score = function(res) {
  
  out = res$output
  
  print(table(out$testy))
  
  print(tapply(out$predyp, out$testy, summary))
  
  roc_ok = pROC::roc(
    response = out$testy,
    predictor = out$predyp,
    levels = c("False", "True"),
    direction = "<"
  )
  
  roc_reverse = pROC::roc(
    response = out$testy,
    predictor = -out$predyp,
    levels = c("False", "True"),
    direction = "<"
  )
  
  print(pROC::auc(roc_ok))
  print(pROC::auc(roc_reverse))
}
controlla_score(res_lasso_no_balance)
controlla_score(res_lasso_under)
controlla_score(res_lasso_smote)

out_smote = res_lasso_smote$output
summary(out_smote$predyp)
head(
  out_smote[order(out_smote$predyp, decreasing = TRUE), ],
  20
)
head(
  out_smote[order(out_smote$predyp, decreasing = FALSE), ],
  20
)

score_confronto = data.frame(
  no_balance = res_lasso_no_balance$output$predyp,
  under = res_lasso_under$output$predyp,
  smote = res_lasso_smote$output$predyp
)
cor(score_confronto, method = "spearman", use = "complete.obs")
