library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(purrr)
library(pracma)
library(patchwork)
library(RColorBrewer)
library(broom)
library(tidymodels)
library(rms)
library(ggsci)
Sys.setlocale("LC_TIME", "English")


###################################################################################################
#--------------------------------- D A T A S E T  R E A D I N G -----------------------------------
###################################################################################################

df_obitos <- read.csv2("df_obitos.csv", header = TRUE, sep = ";", dec = ",")
df_obitos$data <- lubridate::dmy(df_obitos$data)
View(df_obitos)

dataset_completo <- read.csv("dataset_completo.csv", header = TRUE, sep = ";", dec = ",")
colnames(dataset_completo) <- c("data", "20-49", "50-59", "60-69","70+", "<60", ">=60", 
                                "20-49_vac", "50-59_vac", "60-69_vac", "70+_vac", "<60_vac", ">=60_vac", 
                                "cobertura_20-49", "cobertura_50-59", "cobertura_60-69", "cobertura_70+",
                                "cobertura_<60", "cobertura_>=60", "rate_20-49",     
                                "rate_50-59", "rate_60-69", "rate_70+", "rate_<60", "rate_>=60",  
                                "ageAdj_20-49", "ageAdj_50-59", "ageAdj_60-69", "ageAdj_70+", "ageAdj_<60",     
                                "ageAdj_>=60")

dataset_completo$data <- lubridate::dmy(dataset_completo$data)
View(dataset_completo)


###################################################################################################
#-------------------------------------------  T A B L E S -----------------------------------------
###################################################################################################

#------------------------ Total Deaths - Vaccination roll-out 2021
df_obitos %>% 
  filter(faixa_etaria != "<60", faixa_etaria != ">=60", data>="2021-01-01") %>% 
  group_by(faixa_etaria, month) %>%
  summarise(total_deaths = sum(deaths)) %>% 
  spread(faixa_etaria, total_deaths)

#------------------------- Total Deaths - Baseline (jul-dec 2020)
df_obitos %>% 
  filter(faixa_etaria != "<60", faixa_etaria != ">=60", data >="2020-07-01", data<="2020-12-31") %>% 
  group_by(faixa_etaria, month) %>%
  summarise(total_deaths = sum(deaths)) %>% 
  spread(faixa_etaria, total_deaths) %>% 
  colSums()


#-------------------------- Mortality
df_obitos %>%  
  filter(faixa_etaria != "<60", faixa_etaria != ">=60", data >= "2021-01-01") %>% 
  group_by(faixa_etaria, month) %>%
  summarise(total_deaths = round(mean(deaths_pop),2)) %>% 
  spread(faixa_etaria, total_deaths)

#-------------------------- Mortality - Baseline (jul-dec 2020)
df_obitos %>%
  filter(faixa_etaria != "<60", faixa_etaria != ">=60", data >="2020-07-01", data<="2020-12-31") %>% 
  group_by(faixa_etaria, month) %>%
  summarise(total_deaths = mean(deaths_pop)) %>% 
  spread(faixa_etaria, total_deaths) %>% 
  select("BR", "20-49", "50-59", "60-69", "70+") %>% 
  colMeans() %>% round(., 2)


#-------------------------- Rate ratios

rate_BR <- df_obitos %>% 
  filter(faixa_etaria == "BR")
rate_BR <- rate_BR[, c(1,6)]
rate_fxEtaria <- dataset_completo[, c(20:25)]
filtro_rate_pop_BR <- bind_cols(rate_BR, rate_fxEtaria)
filtro_rate_pop_BR$month <- lubridate::month(filtro_rate_pop_BR$data)
View(filtro_rate_pop_BR)

rate_ratio <- filtro_rate_pop_BR %>% 
  filter(data >= "2021-01-01") %>% 
  group_by(month) %>% 
  mutate(`20-49` = mean(`rate_20-49`/`deaths_pop`),
         `50-59` = mean(`rate_50-59`/`deaths_pop`),
         `60-69` = mean(`rate_60-69`/`deaths_pop`),
         `70+` = mean(`rate_70+`/`deaths_pop`)) %>% 
  select(month, `20-49`, `50-59`, `60-69`, `70+`) %>% 
  unique() %>% round(., 2)
rate_ratio


#-------------------------- Vaccination coverage
df_vacinacao <- vacinacao %>%
  filter(uf != "" & uf != "XX") %>%
  mutate(state = uf, data = data_aplicacao, faixa_etaria = idade_grupo) %>%
  select(state, data, faixa_etaria, total, vacina_dose) %>%
  group_by(data, faixa_etaria) %>%
  filter(vacina_dose %in% c("D2","D_unica"), faixa_etaria != "0-4" &  faixa_etaria != "5-9" &
           faixa_etaria != "10-14" &  faixa_etaria != "15-19" ) %>%
  summarise(vaccinated = sum(total)) # WARNING:  it is necessary to modify the vaccine_dose filter if it is single or full vaccinate (single: only "D1", full: "D2","D_unica")
  
  aux_vac <- df_vacinacao %>% 
    spread(faixa_etaria, vaccinated) %>% 
    replace(is.na(.),0)
  aux_vac$data = lubridate::ymd(aux_vac$data)
  aux_vac <- aux_vac %>% filter(data <= "2021-12-31")
  
  aux_vac$`20-49` <- aux_vac$`20-29` + aux_vac$`30-39` + aux_vac$`40-49`
  aux_vac$`70+` <- aux_vac$`70-79` + aux_vac$`80+`
  aux_vac$`BR` <- aux_vac$`20-29` + aux_vac$`30-39`  + aux_vac$`40-49` + aux_vac$`50-59` +
    aux_vac$`60-69` + aux_vac$`70-79`  + aux_vac$`80+` 
  
vac <- aux_vac %>% select(data, "BR", "20-49", "50-59", "60-69", "70+") %>% 
    gather(., faixa_etaria, vacina, -data) %>% 
    mutate(population =  case_when(
      faixa_etaria == "BR" ~ total_BR$population,
      faixa_etaria == "20-49" ~ faixa$`20-49`,
      faixa_etaria == "50-59" ~ faixa$`50-59`,
      faixa_etaria == "60-69" ~ faixa$`60-69`,
      TRUE ~ faixa$`70+`),  
      month = lubridate::month(data),
      vacine_pop = 100*vacina/population) %>% 
    group_by(faixa_etaria, month) %>%
    summarise(vacinas = sum(vacine_pop)) %>% 
  spread(faixa_etaria, vacinas) %>%  cumsum() %>% round(., 2)
vac
View(vac)


###################################################################################################
#-------------------------------- Rate of Rate Ratios - RRR (DID estimator)-----------------------
###################################################################################################

faixa <- matrix(c(97706614, 23875072, 16732965, 13464087, 121581686, 30197052), nrow = 1, byrow = T)
colnames(faixa) <- c("20-49", "50-59", "60-69", "70+", "<60", ">=60")
faixa <- as.data.frame(faixa)
df_obitos$deaths <- as.double(df_obitos$deaths) 
total_BR_population <- 151778738

df_modelagem <- df_obitos %>% 
  filter(faixa_etaria != "<60", faixa_etaria != ">=60",
         data >= "2020-07-01") %>% 
  mutate(population =  case_when(
    faixa_etaria ==  "BR" ~  total_BR_population,
    faixa_etaria == "20-49" ~ faixa$`20-49`,
    faixa_etaria == "50-59" ~ faixa$`50-59`, 
    faixa_etaria == "60-69" ~ faixa$`60-69`, 
    TRUE ~ faixa$`70+`),
    month = as.factor(month),
    faixa_etaria = factor(faixa_etaria, 
                          levels = c("BR", "20-49", "50-59", "60-69", "70+")),
    control_trat = as.factor(if_else(year == 2020, 0, 1)), 
    mes = as.factor(paste0(month.abb[month],"-",year)),
    mes = fct_relevel(mes, "Dec-2020"),
    deaths = if_else(faixa_etaria == "BR", round(deaths_ageAdj*population/100000,0), deaths)
  )

#------------------------ DID/RR TEMPORAL

modelo_DID_temporalBR <- MASS::glm.nb(deaths ~ faixa_etaria + mes + mes*faixa_etaria + 
                                        offset(log(population)), link = log, data = df_modelagem)

tidy_DID_temporalBR <- tidy(modelo_DID_temporalBR, exponentiate = TRUE, conf.int = TRUE)



#------------------------ TABLES RESULTS
# Estimation
tidy_DID_temporalBR[c(23:nrow(tidy_DID_temporalBR)), c(1,2,6,7)] %>% 
  mutate(faixa_etaria = substr(term, start = 13, stop = (nchar(term)-12)),
         data = lubridate::my(if_else(nchar(term) == 29, 
                                      substr(term, start = 22, stop = nchar(term)),
                                      substr(term, start = 20, stop = (nchar(term)))
                                      ))) %>% 
  select(data, faixa_etaria, estimate) %>% 
  bind_rows(., data.frame(data = lubridate::my("Dec-2020"),
                          faixa_etaria = c("20-49", "50-59", "60-69", "70+"),
                          estimate = 1)) %>%
  arrange(., faixa_etaria, data) %>%
  spread(faixa_etaria, estimate)

# Lower bounds
tidy_DID_temporalBR[c(23:nrow(tidy_DID_temporalBR)), c(1,2,6,7)] %>% 
  mutate(faixa_etaria = substr(term, start = 13, stop = (nchar(term)-12)),
         data = lubridate::my(if_else(nchar(term) == 29, 
                                      substr(term, start = 22, stop = nchar(term)),
                                      substr(term, start = 20, stop = (nchar(term)))
         ))) %>% 
  select(data, faixa_etaria, conf.low) %>% 
  bind_rows(., data.frame(data = lubridate::my("Dec-2020"), 
                          faixa_etaria = c("20-49", "50-59", "60-69", "70+"),
                          conf.low = 1)) %>% 
  arrange(., faixa_etaria, data) %>% 
  spread(faixa_etaria, conf.low)

# Upper bounds
tidy_DID_temporalBR[c(23:nrow(tidy_DID_temporalBR)), c(1,2,6,7)] %>% 
  mutate(faixa_etaria = substr(term, start = 13, stop = (nchar(term)-12)),
         data = lubridate::my(if_else(nchar(term) == 29, 
                                      substr(term, start = 22, stop = nchar(term)),
                                      substr(term, start = 20, stop = (nchar(term)))
         ))) %>% 
  select(data, faixa_etaria, conf.high) %>% 
  bind_rows(., data.frame(data = lubridate::my("Dec-2020"), 
                          faixa_etaria = c("20-49", "50-59", "60-69", "70+"),
                          conf.high = 1)) %>% 
  arrange(., faixa_etaria, data) %>% 
  spread(faixa_etaria, conf.high)


###################################################################################################
#-----------------------  C O U N T E R F A C T U A L    A N A L Y S E  ---------------------------
###################################################################################################

dataset_completo_aux <- dataset_completo %>% 
  mutate(BR_ageAdj = `ageAdj_20-49` + `ageAdj_50-59` + `ageAdj_60-69` + `ageAdj_70+`)

#----------------------------- Tables

fit <- lm(`70+` ~ `BR_ageAdj`, data = dataset_completo_aux %>% filter(data <= as.Date("2021-02-12")))
IC_contrafatual_BR_ageAdj <- predict(fit, newdata = dataset_completo_aux, interval="predict")
colnames(IC_contrafatual_BR_ageAdj) <- c("fit_BR", "lwr_BR", "upr_BR")
resultado_BR_70 <- cbind(dataset_completo_aux, IC_contrafatual_BR_ageAdj)


fit <- lm(`60-69` ~ `BR_ageAdj`, data = dataset_completo_aux %>% filter(data <= as.Date("2021-03-27")))
IC_contrafatual_BR_ageAdj <- predict(fit, newdata = dataset_completo_aux, interval="predict")
colnames(IC_contrafatual_BR_ageAdj) <- c("fit_BR", "lwr_BR", "upr_BR")
resultado_BR_60_69 <- cbind(dataset_completo_aux, IC_contrafatual_BR_ageAdj)

fit <- lm(`50-59` ~ `BR_ageAdj`, data = dataset_completo_aux %>% filter(data <= as.Date("2021-05-06")))
IC_contrafatual_BR_ageAdj <- predict(fit, newdata = dataset_completo_aux, interval="predict")
colnames(IC_contrafatual_BR_ageAdj) <- c("fit_BR", "lwr_BR", "upr_BR")
resultado_BR_50_59 <- cbind(dataset_completo_aux, IC_contrafatual_BR_ageAdj)

fit <- lm(`20-49` ~ `BR_ageAdj`, data = dataset_completo_aux %>% filter(data <= as.Date("2021-05-10")))
IC_contrafatual_BR_ageAdj <- predict(fit, newdata = dataset_completo_aux, interval="predict")
colnames(IC_contrafatual_BR_ageAdj) <- c("fit_BR", "lwr_BR", "upr_BR")
resultado_BR_20_49 <- cbind(dataset_completo_aux, IC_contrafatual_BR_ageAdj)


#---------------------------- Age group 20-49
# preventable deaths
resultado_BR_20_49 %>% filter(data > "2021-05-10") %>% 
  mutate(fit_BR = if_else(fit_BR < 0, 0, round(fit_BR,0)),
         excess_BR = if_else(round((fit_BR - `20-49`),0) < 0, 
                                  round((fit_BR - `20-49`),0), 0),
         month = lubridate::month(data),
         ) %>% 
  group_by(month) %>% 
  select(excess_BR, fit_BR, `20-49`) %>% 
  summarise(excess_BR = sum(excess_BR),
            `20-49_obsDeaths` = sum(`20-49`),
            `Estim_BR` = sum(fit_BR),
            ) %>% colSums()


# prevented deaths
resultado_BR_20_49 %>% filter(data > "2021-05-10") %>% 
  mutate(fit_BR = if_else(fit_BR < 0, 0, round(fit_BR,0)),
         averted_BR = if_else(round((fit_BR - `20-49`),0) > 0, 
                                          round((fit_BR - `20-49`),0), 0),
         month = lubridate::month(data)
  ) %>% 
  group_by(month) %>% 
  select(averted_BR) %>% 
  summarise(averted_BR = sum(averted_BR)
            ) %>% colSums()


#---------------------------- Age group 50-59
# preventable deaths
resultado_BR_50_59 %>% filter(data > "2021-05-06") %>% 
  mutate(fit_BR = if_else(fit_BR < 0, 0, round(fit_BR,0)),
         excess_BR = if_else(round((fit_BR - `50-59`),0) < 0, 
                                          round((fit_BR - `50-59`),0), 0),
         month = lubridate::month(data)
         ) %>% 
  group_by(month) %>% 
  select(excess_BR, fit_BR, `50-59`) %>% 
  summarise(excess_BR = sum(excess_BR),
            `50-59_obsDeaths` = sum(`50-59`),
            `Estim_BR` = sum(fit_BR),
            ) %>% colSums()


# prevented deaths
resultado_BR_50_59 %>% filter(data > "2021-05-06") %>% 
  mutate(fit_BR = if_else(fit_BR < 0, 0, round(fit_BR,0)),
         averted_BR = if_else(round((fit_BR - `50-59`),0) > 0, 
                                  round((fit_BR - `50-59`),0), 0),
         month = lubridate::month(data)
  ) %>% 
  group_by(month) %>% 
  select(averted_BR) %>% 
  summarise(averted_BR = sum(averted_BR),
            ) %>% colSums()


#---------------------------- Age group 60-69
# preventable deaths
resultado_BR_60_69 %>% filter(data > "2021-03-27") %>% 
  mutate(fit_BR = if_else(fit_BR < 0, 0, round(fit_BR,0)),
         excess_BR = if_else(round((fit_BR - `60-69`),0) < 0, 
                                          round((fit_BR - `60-69`),0), 0),
         month = lubridate::month(data)
  ) %>% 
  group_by(month) %>% 
  select(excess_BR, fit_BR, `60-69`) %>% 
  summarise(excess_BR = sum(excess_BR),
            `60-69_obsDeaths` = sum(`60-69`),
            `Estim_BR` = round(sum(fit_BR),0),
            ) %>% colSums()


# prevented deaths
resultado_BR_60_69 %>% filter(data > "2021-03-27") %>% 
  mutate(fit_BR = if_else(fit_BR < 0, 0, round(fit_BR,0)),
         averted_BR = if_else(round((fit_BR - `60-69`),0) > 0, 
                                  round((fit_BR - `60-69`),0), 0),
         month = lubridate::month(data)
  ) %>% 
  group_by(month) %>% 
  select(averted_BR) %>% 
  summarise(averted_BR = sum(averted_BR)
            ) %>% colSums()


#---------------------------- Age group FAIXA 70+
# preventable deaths
resultado_BR_70 %>% filter(data > "2021-02-12") %>% 
  mutate(fit_BR = if_else(fit_BR < 0, 0, round(fit_BR,0)),
         excess_BR = if_else(round((fit_BR - `70+`),0) < 0, 
                                          round((fit_BR - `70+`),0), 0),
         month = lubridate::month(data)
  ) %>% 
  group_by(month) %>% 
  select(excess_BR, fit_BR, `70+`) %>% 
  summarise(excess_BR = sum(excess_BR),
            `70_obsDeaths` = sum(`70+`),
            `Estim_BR` = round(sum(fit_BR),0),
            ) %>% colSums()

# prevented deaths
resultado_BR_70 %>% filter(data > "2021-02-12") %>% 
  mutate(fit_BR = if_else(fit_BR < 0, 0, round(fit_BR,0)),
         averted_BR = if_else(round((fit_BR - `70+`),0) > 0, 
                                  round((fit_BR - `70+`),0), 0),
         month = lubridate::month(data)
  ) %>% 
  group_by(month) %>% 
  select(averted_BR) %>% 
  summarise(averted_BR = sum(averted_BR),
           ) %>% colSums()


#-----------------------------------------------------------------------------------

# Distâncias dias cobertura vacinal 75%

dataset_completo$data[which(round(dataset_completo$`cobertura_20-49`,2)>=0.75)][1] # "2021-07-30"
dataset_completo$data[which(round(dataset_completo$`cobertura_50-59`,2)>=0.75)][1] # "2021-06-23"
dataset_completo$data[which(round(dataset_completo$`cobertura_60-69`,2)>=0.75)][1] # "2021-04-30"
dataset_completo$data[which(round(dataset_completo$`cobertura_70+`,2)>=0.75)][1] # 2021-03-25"


lubridate::ymd("2021-07-30") - lubridate::ymd("2021-01-15") # Time difference of 196 days
lubridate::ymd("2021-06-23") - lubridate::ymd("2021-01-15") # Time difference of 159 days
lubridate::ymd("2021-04-30") - lubridate::ymd("2021-01-15") # Time difference of 105 days
lubridate::ymd("2021-03-25") - lubridate::ymd("2021-01-15") # Time difference of 69 days

# Brazil
lubridate::ymd("2021-08-12") - lubridate::ymd("2021-01-15") # Time difference of 209 days


