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
#--------------------------------- D A T A S E T  A G R E G A T I O N -----------------------------
###################################################################################################

#----------------- Data reading 

# Deaths - SIVEP
obitos <- read.csv("srag_adults_covid_hosp_2022-07-11.csv", header = TRUE, sep = ",", dec = ",")

# Vaccination by state - SI-PNI
vacinacao <- read.csv("vw_vacc_date_state_age_dose_2022-02-02.csv", header = TRUE, sep = ",")

# Population by age group and state
piramide_etaria <- read.csv2("df_population_city_sex.csv", sep = ",", header = TRUE)

# Age-adjusted
df_ageSex_adjusted <- read.csv2("who_pop_std_rates.csv", sep = ",", header = TRUE)


# Filtreing deaths and creating age group
df_obitos <- NULL

# Filtreing deaths and creating age group%>% count(EVOLUCAO)
data_obitos <- obitos %>% 
  mutate(state = SG_UF, data = date_desf, deaths = EVOLUCAO) %>%
  mutate(faixa_etaria = case_when(NU_IDADE_N < 10  ~ "0-9",
                                  NU_IDADE_N >= 10 & NU_IDADE_N < 20 ~ "10-19",
                                  NU_IDADE_N >= 20 & NU_IDADE_N < 50 ~ "20-49",
                                  NU_IDADE_N >= 50 & NU_IDADE_N < 60 ~ "50-59",
                                  NU_IDADE_N >= 60 & NU_IDADE_N < 70 ~ "60-69",
                                  TRUE ~ "70+")) %>% 
  select(state, data, faixa_etaria, deaths) %>%
  group_by(data, faixa_etaria) %>% 
  filter(deaths == "Death") %>% 
  mutate(deaths = 1) %>%
  summarise(newDeaths = sum(deaths)) 


aux_1 <- data_obitos %>% 
  spread(faixa_etaria, newDeaths) %>% 
  replace(is.na(.),0) %>% 
  mutate(`<60` = `20-49` + `50-59`,
         `>=60` = `60-69` + `70+`) 
aux_1 <- aux_1[-c(1),]
aux_1$data = lubridate::ymd(aux_1$data)


aux_2 <- data.frame(data = seq(from = lubridate::ymd(as.Date(aux_1$data[1])),
                               to = lubridate::ymd(as.Date("2021/12/31")),
                               by = "day"), valor = 0)

aux_3 <- left_join(aux_2, aux_1, by = c("data") )%>% 
  replace(is.na(.),0) 
aux_3 <- aux_3[,-c(2)]


# Filtering single dose
data_vacinacao <- vacinacao %>%
  filter(uf != "" & uf != "XX") %>%
  mutate(state = uf, data = data_aplicacao, faixa_etaria = idade_grupo) %>%
  select(state, data, faixa_etaria, total, vacina_dose) %>%
  group_by(data, faixa_etaria) %>%
  filter(vacina_dose == "D1", faixa_etaria != "0-4" &  faixa_etaria != "5-9" &
           faixa_etaria != "10-14" &  faixa_etaria != "15-19" ) %>%
  summarise(vaccinated = sum(total))


aux_4 <- data_vacinacao %>% 
  spread(faixa_etaria, vaccinated) %>% 
  replace(is.na(.),0)
aux_4$data = lubridate::ymd(aux_4$data)

aux_4$`20-49` <- aux_4$`20-29` + aux_4$`30-39` + aux_4$`40-49`
aux_4$`70+` <- aux_4$`70-79` + aux_4$`80+`
aux_4$`<60` <- aux_4$`20-29` + aux_4$`30-39`  + aux_4$`40-49` + aux_4$`50-59`
aux_4$`>=60` <- aux_4$`60-69` + aux_4$`70-79`  + aux_4$`80+` 
aux_4 <- aux_4 %>% select(data, "20-49", "50-59", "60-69", "70+", "<60", ">=60")

aux_4 <- as.data.frame(aux_4)
names(aux_4) <- c("data","a1","a2","a3","a4","a5","a6")


# Cumulative by age group
aux_5 <- aux_4 %>%  
  mutate("20-49_vac" = cumsum(a1),
         "50-59_vac" = cumsum(a2),
         "60-69_vac" = cumsum(a3),
         "70+_vac" = cumsum(a4),
         "<60_vac" = cumsum(a5),
         ">=60_vac" = cumsum(a6))

aux_5 <- aux_5 %>% 
  select(data, "20-49_vac", "50-59_vac", "60-69_vac", 
         "70+_vac", "<60_vac", ">=60_vac")


# Join  dataset de óbitos e vacinação são a partir do começo das mortes
dataset_joined <- left_join(aux_3, aux_5, by = c("data")) %>% 
  replace(is.na(.),0)


# Calculating moving average deaths
dataset_joined$`20-49` <- round(movavg(dataset_joined$`20-49`, n = 15, type = 's'), 0)
dataset_joined$`50-59` <- round(movavg(dataset_joined$`50-59`, n = 15, type = 's'), 0)
dataset_joined$`60-69` <- round(movavg(dataset_joined$`60-69`, n = 15, type = 's'), 0)
dataset_joined$`70+` <- round(movavg(dataset_joined$`70+`, n = 15, type = 's'), 0)
dataset_joined$`<60` <- round(movavg(dataset_joined$`<60`, n = 15, type = 's'), 0)
dataset_joined$`>=60` <- round(movavg(dataset_joined$`>=60`, n = 15, type = 's'), 0)


# Population by age group
faixa_etaria <- piramide_etaria %>% 
  select(sexo, idade_grupo, population) %>% 
  group_by(idade_grupo) %>% 
  summarise( population =  sum(population)) %>% 
  spread(idade_grupo, population) 

faixa <- faixa_etaria %>%
  mutate(`20-49` = `20-29` + `30-39` + `40-49`,
         `70+` =  `70-79` + `80+`,
         `<60` = `20-29` + `30-39` + `40-49` + `50-59`,
         `>=60` = `60-69` + `70-79` + `80+`) %>% 
  select(`20-49`, `50-59`, `60-69`, `70+`, `<60`, `>=60`)

total_BR <- faixa_etaria %>%
  mutate(population = `20-29` + `30-39` + `40-49` + `50-59` + `60-69` + `70-79` + `80+`) %>% 
  select(population)


# Reference population
df_ageSex_adjusted$pop_rate_perc = as.numeric(df_ageSex_adjusted$pop_rate_perc)
pop_ageAdjusted <- df_ageSex_adjusted %>% 
  mutate(faixa_etaria =  case_when(
    age_group == "0-4" | age_group == "5-9" | 
      age_group == "10-14" | age_group == "15-19" ~ "<20",
    age_group == "20-24" | age_group == "25-29" | 
      age_group == "30-34" | age_group == "35-39" |
      age_group == "40-44" | age_group == "45-49" ~ "20-49",
    age_group == "50-54" | age_group == "55-59" ~ "50-59",
    age_group == "60-64" | age_group == "65-69" ~ "60-69",
    TRUE ~ "70+")) %>% 
  select(faixa_etaria, pop_rate_perc) %>% 
  group_by(faixa_etaria) %>% 
  summarise(pop_rate_perc = sum(pop_rate_perc))

total_ageAdjusted <- pop_ageAdjusted %>% 
  filter(faixa_etaria != "<20") %>% 
  summarise(pop_rate_perc = sum(pop_rate_perc))
total_ageAdjusted[[1]]

pop_ageAdjusted <- pop_ageAdjusted %>% 
  spread(faixa_etaria, pop_rate_perc) %>% 
  mutate(`<60` = `20-49` + `50-59`,
         `>=60` = `60-69` + `70+`) %>% 
  select(`20-49`, `50-59`, `60-69`, `70+`, `<60`, `>=60`)


# Vaccine coverage, mortality and  deaths-ageadjusted
dataset_completo <- dataset_joined %>% mutate(
  `cobertura_20-49` = `20-49_vac`/faixa$`20-49`,
  `cobertura_50-59` = `50-59_vac`/faixa$`50-59`,
  `cobertura_60-69` = `60-69_vac`/faixa$`60-69`,
  `cobertura_70+` = `70+_vac`/faixa$`70+`,
  `cobertura_<60` = `<60_vac`/faixa$`<60`,
  `cobertura_>=60` = `>=60_vac`/faixa$`>=60`,
  
  `rate_20-49` = 100000*`20-49`/faixa$`20-49`,
  `rate_50-59` = 100000*`50-59`/faixa$`50-59`,
  `rate_60-69` = 100000*`60-69`/faixa$`60-69`,
  `rate_70+` = 100000*`70+`/faixa$`70+`,
  `rate_<60` = 100000*`<60`/faixa$`<60`,
  `rate_>=60` = 100000*`>=60`/faixa$`>=60`,
  
  `ageAdj_20-49` = 100000*(`20-49`/faixa$`20-49`)*
    (pop_ageAdjusted$`20-49`/total_ageAdjusted[[1]]),
  `ageAdj_50-59` = 100000*(`50-59`/faixa$`50-59`)*
    (pop_ageAdjusted$`50-59`/total_ageAdjusted[[1]]),
  `ageAdj_60-69` = 100000*`60-69`/faixa$`60-69`*
    (pop_ageAdjusted$`60-69`/total_ageAdjusted[[1]]),
  `ageAdj_70+` = 100000*(`70+`/faixa$`70+`)*
    (pop_ageAdjusted$`70+`/total_ageAdjusted[[1]]),
  `ageAdj_<60` = 100000*(`<60`/faixa$`<60`)*
    (pop_ageAdjusted$`<60`/total_ageAdjusted[[1]]),
  `ageAdj_>=60` = 100000*(`>=60`/faixa$`>=60`)*
    (pop_ageAdjusted$`>=60`/total_ageAdjusted[[1]])
  
)


dataset_completo$data = lubridate::ymd(dataset_completo$data)

conjunto_obitos <- dataset_completo[,c(1:7)] %>%
  gather(., faixa_etaria, deaths, -data)

conjunto_obitos_rate <- dataset_completo[,c(1,20:25)] %>%
  gather(., faixa_etaria, deaths_pop, -data)

conjunto_vacina_doses <- dataset_completo[,c(1,8:13)] %>%
  gather(., faixa_etaria, vacina, -data)

conjunto_vacina_coverage <- dataset_completo[,c(1,14:19)] %>%
  gather(., faixa_etaria, cobertura, -data)

conjunto_obitos_ageAdjusted <- dataset_completo[,c(1,26:31)] %>%
  gather(., faixa_etaria, deaths_ageAdj, -data)


conjunto_obitos_rate$faixa_etaria = conjunto_obitos$faixa_etaria 
conjunto_vacina_doses$faixa_etaria = conjunto_obitos$faixa_etaria 
conjunto_vacina_coverage$faixa_etaria = conjunto_obitos$faixa_etaria 
conjunto_obitos_ageAdjusted$faixa_etaria = conjunto_obitos$faixa_etaria 

join_1 <- left_join(conjunto_obitos, conjunto_obitos_ageAdjusted, by = c("data","faixa_etaria"))
join_2 <- left_join(conjunto_vacina_doses, conjunto_vacina_coverage, by = c("data","faixa_etaria"))
conjunto_dados <- left_join(join_1, join_2, by = c("data","faixa_etaria"))

df_obitos <- left_join(conjunto_dados, conjunto_obitos_rate, by = c("data","faixa_etaria"))


df_filtro_brasil <- df_obitos %>% 
  filter(faixa_etaria != "<60", faixa_etaria != ">=60") %>% 
  group_by(data) %>% 
  summarise(deaths = sum(deaths),
            deaths_ageAdj = sum(deaths_ageAdj),
            deaths_pop = 100000*(sum(deaths)/total_BR$population),
            vacina = sum(vacina),
            cobertura = cumsum(sum(vacina/total_BR$population))) %>% 
  mutate(faixa_etaria = "BR") %>% 
  select("data", "faixa_etaria", "deaths", "deaths_ageAdj", "vacina","cobertura", "deaths_pop" )

df_obitos <- bind_rows(df_filtro_brasil, df_obitos)
df_obitos$data = lubridate::ymd(df_obitos$data)


###################################################################################################
#-----------------------  C O U N T E R F A C T U A L    A N A L Y S I S  ---------------------------
###################################################################################################
dataset_completo_aux <- dataset_completo %>% 
  mutate(BR_ageAdj = `ageAdj_20-49` + `ageAdj_50-59` + `ageAdj_60-69` + `ageAdj_70+`)
c0 <- lubridate::ymd("2021-01-15") 


#---------------------------------- Age group 70+
fit <- lm(`70+` ~ `BR_ageAdj`, data = dataset_completo_aux %>% filter(data <= as.Date("2021-02-12")))
IC_contrafatual_BR_ageAdj <- predict(fit, newdata = dataset_completo_aux, interval="predict")
colnames(IC_contrafatual_BR_ageAdj) <- c("fit_BR", "lwr_BR", "upr_BR")
resultado_BR_70 <- cbind(dataset_completo_aux, IC_contrafatual_BR_ageAdj)


datas.4 <- data.frame(corte = c(lubridate::ymd(as.Date("2021-02-12"))))
df_resultado_BR_70 <- 
  resultado_BR_70 %>%
  as_tibble() %>% 
  mutate(
    area_low = pmin(`70+`, fit_BR),
    area_high = pmax(`70+`, fit_BR),
    fill_area = if_else(`70+` > fit_BR, TRUE, FALSE) # fill_area = `70+`>= fit_BR
  )

CT4 <- df_resultado_BR_70 %>% 
  ggplot() +
  geom_ribbon(data = df_resultado_BR_70 %>% filter(data >= ymd(as.Date("2021-02-12"))),
              aes(x = data, ymin = area_low, ymax = area_high, fill = fill_area), alpha = 0.3) +
  geom_line(aes(x = data, y = fit_BR), color = "black", size = 0.5) +
  geom_line(data = resultado_BR_70 %>% filter(data < as.Date("2021-02-12")),
            aes(x = data, y = fit_BR), color = "white", size = 1) + 
  geom_line(data = resultado_BR_70 %>% filter(data < as.Date("2021-02-12")),
            aes(x = data, y = fit_BR), color = "gray55", size = 0.5) + 
  geom_line(aes(x = data, y = `70+`), color = "#802268FF", size = 0.8) +
  geom_vline(aes(xintercept = c0), linetype = "dashed" , color = "black", alpha = 0.5) +
  geom_vline(data = datas.4, aes(xintercept = corte), linetype = "dashed" , color = "black", alpha = 0.9) +
  geom_text(data = datas.4, aes(x = corte + 85, y = 2500,
                                label = c("70+ vaccination")),
            color = "gray15", size = 3, angle = 0) +
  scale_x_date(labels = date_format("%b/%y")) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(guide = "none", values = c("TRUE" = "#ED00007F", "FALSE" = "#009966")) +  #c("TRUE" = "#ED00007F", "FALSE" = "#42B5407F")
  theme_classic() +
  ylab("Reported deaths 70+") + xlab("") +
  theme(axis.title.y.left = element_text(color = "grey8"),
        axis.text.y.left = element_text(color = "grey8"),
        axis.title.y.right = element_text(color = "grey8"),
        axis.text.y.right = element_text(color = "grey8"),
        axis.text.x = element_text(color = "grey8"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))



#---------------------------------- Age group 60-69
fit <- lm(`60-69` ~ `BR_ageAdj`, data = dataset_completo_aux %>% filter(data <= as.Date("2021-03-27")))
IC_contrafatual_BR_ageAdj <- predict(fit, newdata = dataset_completo_aux, interval="predict")
colnames(IC_contrafatual_BR_ageAdj) <- c("fit_BR", "lwr_BR", "upr_BR")
resultado_BR_60_69 <- cbind(dataset_completo_aux, IC_contrafatual_BR_ageAdj)


datas.3 <- data.frame(corte = c(lubridate::ymd(as.Date("2021-03-27"))))
df_resultado_BR_60_69 <- 
  resultado_BR_60_69 %>% 
  mutate(
    area_low = pmin(`60-69`, fit_BR),
    area_high = pmax(`60-69`, fit_BR),
    fill_area = if_else(`60-69` > fit_BR, TRUE, FALSE)
  )

CT3 <- df_resultado_BR_60_69 %>% 
  ggplot() +
  geom_ribbon(data = df_resultado_BR_60_69 %>% filter(data >= ymd(as.Date("2021-03-27"))),
              aes(x = data, ymin = area_low, ymax = area_high, fill = fill_area), alpha = 0.3) +
  geom_line(aes(x = data, y = fit_BR), color = "black", size = 0.5) +
  geom_line(data = resultado_BR_60_69 %>% filter(data < as.Date("2021-03-27")),
            aes(x = data, y = fit_BR), color = "white", size = 1) + 
  geom_line(data = resultado_BR_60_69 %>% filter(data < as.Date("2021-03-27")),
            aes(x = data, y = fit_BR), color = "gray55", size = 0.5) + 
  geom_line(aes(x = data, y = `60-69`), color = "#5DB1DDFF", size = 0.8) +
  geom_vline(aes(xintercept = c0), linetype = "dashed" , color = "black", alpha = 0.5) +
  geom_vline(data = datas.3, aes(xintercept = corte), linetype = "dashed" , color = "black", alpha = 0.9) +
  geom_text(data = datas.3, aes(x = corte + 90, y = 850,
                                label = c("60-69 vaccination")),
            color = "gray15", size = 3, angle = 0) +
  scale_x_date(labels = date_format("%b/%y")) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(guide = "none", values = c("TRUE" = "#ED00007F", "FALSE" = "#009966")) +
  theme_classic() +
  ylab("Reported deaths 60-69") + xlab("") +
  theme(axis.title.y.left = element_text(color = "grey8"),
        axis.text.y.left = element_text(color = "grey8"),
        axis.title.y.right = element_text(color = "grey8"),
        axis.text.y.right = element_text(color = "grey8"),
        axis.text.x = element_text(color = "grey8"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))


#----------------------------------  Age group 50-49
fit <- lm(`50-59` ~ `BR_ageAdj`, data = dataset_completo_aux %>% filter(data <= as.Date("2021-05-06")))
IC_contrafatual_BR_ageAdj <- predict(fit, newdata = dataset_completo_aux, interval="predict")
colnames(IC_contrafatual_BR_ageAdj) <- c("fit_BR", "lwr_BR", "upr_BR")
resultado_BR_50_59 <- cbind(dataset_completo_aux, IC_contrafatual_BR_ageAdj)


datas.2 <- data.frame(corte = c(lubridate::ymd(as.Date("2021-05-06"))))
df_resultado_BR_50_59 <- 
  resultado_BR_50_59 %>% 
  mutate(
    area_low = pmin(`50-59`, fit_BR),
    area_high = pmax(`50-59`, fit_BR),
    fill_area = if_else(`50-59` > fit_BR, TRUE, FALSE)
  )

CT2 <- df_resultado_BR_50_59 %>% 
  ggplot() +
  geom_ribbon(data = df_resultado_BR_50_59 %>% filter(data >= ymd(as.Date("2021-05-06"))),
              aes(x = data, ymin = area_low, ymax = area_high, fill = fill_area), alpha = 0.3) +
  geom_line(aes(x = data, y = fit_BR), color = "black", size = 0.5) +
  geom_line(data = resultado_BR_50_59 %>% filter(data < as.Date("2021-05-06")),
            aes(x = data, y = fit_BR), color = "white", size = 1) + 
  geom_line(data = resultado_BR_50_59 %>% filter(data < as.Date("2021-05-06")),
            aes(x = data, y = fit_BR), color = "gray55", size = 0.5) + 
  geom_line(aes(x = data, y = `50-59`), color = "#BA6338FF", size = 0.8) +
  geom_vline(aes(xintercept = c0), linetype = "dashed" , color = "black", alpha = 0.5) +
  geom_vline(data = datas.2, aes(xintercept = corte), linetype = "dashed", color = "black", alpha = 0.9) +
  geom_text(data = datas.2, aes(x = corte + 90, y = 510,
                                label = c("50-59 vaccination")),
            color = "gray15", size = 3, angle = 0) +
  scale_x_date(labels = date_format("%b/%y")) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(guide = "none", values = c("TRUE" = "#ED00007F", "FALSE" = "#009966")) +
  theme_classic() +
  ylab("Reported deaths 50-59") + xlab("") +
  theme(axis.title.y.left = element_text(color = "grey8"),
        axis.text.y.left = element_text(color = "grey8"),
        axis.title.y.right = element_text(color = "grey8"),
        axis.text.y.right = element_text(color = "grey8"),
        axis.text.x = element_text(color = "grey8"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))



#---------------------------------- Age group 20-49
fit <- lm(`20-49` ~ `BR_ageAdj`, data = dataset_completo_aux %>% filter(data <= as.Date("2021-05-10")))
IC_contrafatual_BR_ageAdj <- predict(fit, newdata = dataset_completo_aux, interval="predict")
colnames(IC_contrafatual_BR_ageAdj) <- c("fit_BR", "lwr_BR", "upr_BR")
resultado_BR_20_49 <- cbind(dataset_completo_aux, IC_contrafatual_BR_ageAdj)


datas.1 <- data.frame(corte = c(lubridate::ymd(as.Date("2021-05-10"))))
df_resultado_BR_20_49 <- 
  resultado_BR_20_49 %>% 
  mutate(
    area_low = pmin(`20-49`, fit_BR),
    area_high = pmax(`20-49`, fit_BR),
    fill_area = if_else(`20-49` > fit_BR, TRUE, FALSE)
  )



CT1 <- df_resultado_BR_20_49 %>% 
  ggplot() +
  geom_ribbon(data = df_resultado_BR_20_49 %>% filter(data >= ymd(as.Date("2021-05-10"))),
              aes(x = data, ymin = area_low, ymax = area_high, fill = fill_area), alpha = 0.3) +
  geom_line(aes(x = data, y = fit_BR), color = "black", size = 0.5) +
  geom_line(data = resultado_BR_20_49 %>% filter(data < as.Date("2021-05-10")),
            aes(x = data, y = fit_BR), color = "white", size = 1) + 
  geom_line(data = resultado_BR_20_49 %>% filter(data < as.Date("2021-05-10")),
            aes(x = data, y = fit_BR), color = "gray55", size = 0.5) + 
  geom_line(aes(x = data, y = `20-49`), color = "#6BD76BFF", size = 0.8) +
  geom_vline(aes(xintercept = c0), linetype = "dashed" , color = "black", alpha = 0.5) +
  geom_vline(data = datas.1, aes(xintercept = corte), linetype = "dashed" , color = "black", alpha = 0.9) +
  geom_text(data = datas.1, aes(x = corte + 90, y = 510,
                                label = c("20-49 vaccination")),
            color = "gray15", size = 3, angle = 00) +
  scale_x_date(labels = date_format("%b/%y")) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(guide = "none", values = c("TRUE" = "#ED00007F", "FALSE" = "#009966")) +
  theme_classic() +
  ylab("Reported deaths 20-49") + xlab("") +
  theme(axis.title.y.left = element_text(color = "grey8"),
        axis.text.y.left = element_text(color = "grey8"),
        axis.title.y.right = element_text(color = "grey8"),
        axis.text.y.right = element_text(color = "grey8"),
        axis.text.x = element_text(color = "grey8"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))


CT4 + CT3 + CT2 + CT1 + plot_annotation(tag_levels = "A")






