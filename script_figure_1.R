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
#-------------------------------- FIGURES - DEATHS + VACCINE COVERAGE -----------------------------
###################################################################################################

# Extracting the dates in which the percentage of vaccination coverage was reached
c0 <- lubridate::ymd("2021-01-15") # Inicio da vacinação
c75 <- df_filtro_brasil$data[which(df_filtro_brasil$cobertura > 0.75)][1]

corte_cobertura_1 <- data.frame(corte = c(c0, c75))
scaleFactor <- max(df_filtro_brasil$deaths)/max(df_filtro_brasil$cobertura)

p1 <- df_filtro_brasil %>%
  ggplot() +
  geom_rect(aes(xmin = lubridate::as_date("2021-02-15"),  ymin = 0, ymax = Inf, 
                xmax =  lubridate::as_date("2021-08-11")),
            fill = "lightyellow") +
  geom_rect(aes(xmin = lubridate::as_date("2021-08-12"),  ymin = 0, ymax = Inf, 
                xmax =  lubridate::as_date("2021-12-27")),
            fill = "aliceblue", ) +
  geom_area(aes(x = data, y = deaths), color = "gray90", alpha = 0.2 ) +
  geom_line(aes(x = data, y = scaleFactor*cobertura), size = 1, color = "blue") +
  geom_vline(data = corte_cobertura_1, aes(xintercept = corte), linetype = "dashed" , color = "grey5", alpha = 0.5) +
  geom_text(aes(x = lubridate::as_date("2021-06-30")-50, y = 3700,
                label = c("Gamma")), color = "grey5", size = 3.4, alpha = 0.8) +
  geom_text(aes(x = lubridate::as_date("2021-09-15") + 35, y = 3700,
                label = c("Delta")), color = "grey5", size = 3.4, alpha = 0.8) +
  geom_text(data = corte_cobertura_1,
            aes(x = lubridate::as_date(corte) + 53, y = 3300,
                label = c("Vaccination campaign", "")),
            color = "grey5", size = 3.4, alpha = 0.8) +
  geom_text(data = corte_cobertura_1,
            aes(x = lubridate::as_date(corte) + 15, y = 3300,
                label = c("", "75 %")),
            color = "grey5", size = 3, alpha = 0.8) +
  scale_y_continuous(sec.axis = sec_axis(~.*(1/scaleFactor*1), 
                                         name = "Vaccination coverage (%)",
                                         labels = scales::percent_format(),
                                         breaks = seq(0,1, by = 0.25)),
                     breaks = seq(0,3100, by = 500),
                     labels = comma) +
  scale_x_date(labels = date_format("%b/%y"), breaks = "2 month") + #breaks = "2 month", waiver()
  ylab("Reported deaths") + xlab("") + labs(subtitle = "") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "grey8"),
        axis.text.y.left = element_text(color = "grey8"),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
        axis.text.x = element_text(color = "grey8"),
        plot.background = element_rect(color = "white"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))



colores <- c("#6BD76BFF", "#BA6338FF", "#5DB1DDFF", "#802268FF")
names(colores) <- c("20-49", "50-59", "60-69", "70+")
c0 <- lubridate::ymd("2021-01-15")

p2 <- df_obitos %>% filter(faixa_etaria %in% c("20-49", "50-59", "60-69", "70+"), data > "2020-12-31") %>% 
  mutate(faixa_etaria = factor(faixa_etaria, levels = c("70+", "60-69", "50-59", "20-49"))) %>% 
  ggplot() +
  geom_area(aes(x = data, y = deaths, fill = faixa_etaria)) +
  geom_vline(aes(xintercept = c0), linetype = "dashed" , color = "black", alpha = 0.5) +
  geom_text(aes(x = lubridate::as_date(c0)+108, y = 3400,
                label = c("Vaccination campaign")), color = "grey5", size = 3.4, alpha = 0.8) +
  scale_fill_manual(guide = "none", values = colores) +
  scale_x_date(labels = date_format("%b/%y"), breaks = waiver()) + #breaks = "2 month"
  scale_y_continuous(labels = comma) +
  ylab("Reported deaths") + xlab("") + labs(subtitle = "") +
  labs(subtitle = "", color = "\n", fill = "\n") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "grey8"),
        axis.text.y.left = element_text(color = "grey8"),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
        axis.text.x = element_text(color = "grey8"),
        plot.background = element_rect(color = "white"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        legend.position = "top")



p3 <- df_obitos %>% 
  filter(faixa_etaria %in% c("20-49", "50-59", "60-69", "70+"), data > "2020-12-31") %>% 
  group_by(data) %>% 
  mutate(proprocao_mortes = deaths/sum(deaths)) %>% 
  filter(data > "2020-03-20") %>% 
  ggplot() +
  geom_line(aes(x = data, y = proprocao_mortes, color = faixa_etaria), size = 1) +
  geom_vline(aes(xintercept = c0), linetype = "dashed" , color = "black", alpha = 0.5) +
  geom_text(aes(x = lubridate::as_date(c0)+108, y = 0.75,
                label = c("Vaccination campaign")), color = "grey5", size = 3.4, alpha = 0.8) +
  scale_color_manual(values = colores) +
  scale_x_date(labels = date_format("%b/%y"), breaks = waiver()) + #breaks = "2 month"
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Proportion of the reported deaths") + xlab("") + labs(subtitle = "") +
  labs(subtitle = "", color = "\n") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "grey8"),
        axis.text.y.left = element_text(color = "grey8"),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
        axis.text.x = element_text(color = "grey8"),
        plot.background = element_rect(color = "white"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        legend.position = "top")


filtro_20_49 <- df_obitos %>% 
  filter(faixa_etaria == "20-49") %>% 
  mutate(cobertura = round(cobertura,4))
c75.1 <- filtro_20_49$data[which(filtro_20_49$cobertura > 0.75)][1]

filtro_50_59 <- df_obitos %>% 
  filter(faixa_etaria == "50-59") %>% 
  mutate(cobertura = round(cobertura,4))
c75.2 <- filtro_50_59$data[which(filtro_50_59$cobertura > 0.75)][1]

filtro_60_69 <- df_obitos %>% 
  filter(faixa_etaria == "60-69") %>% 
  mutate(cobertura = round(cobertura,4))
c75.3 <- filtro_60_69$data[which(filtro_60_69$cobertura > 0.75)][1]

filtro_70 <- df_obitos %>% 
  filter(faixa_etaria == "70+") %>% 
  mutate(cobertura = round(cobertura,4))
c75.4 <- filtro_70$data[which(filtro_70$cobertura > 0.75)][1]


p4 <- df_obitos %>% 
  filter(faixa_etaria %in% c("20-49", "50-59", "60-69", "70+"), data > "2020-12-31") %>% 
  mutate(cobertura = if_else(faixa_etaria == "20-49" & cobertura > 1, 1, cobertura)) %>% 
  ggplot() +
  geom_line(aes(x = data, y = cobertura, color = faixa_etaria), size = 1) +
  geom_hline(aes(yintercept = 0.75), linetype = "dashed") +
  geom_vline(aes(xintercept = c0), linetype = "dashed" , color = "black", alpha = 0.5) +
  geom_segment(aes(x = c75.1, y = -0.01, xend =  c75.1, yend = 0.75), linetype = "dashed" , color = "#6BD76BFF", alpha = 0.5) +
  geom_segment(aes(x = c75.2, y = -0.01, xend =  c75.2, yend = 0.75), linetype = "dashed" , color = "#BA6338FF", alpha = 0.5) +
  geom_segment(aes(x = c75.3, y = -0.01, xend =  c75.3, yend = 0.75), linetype = "dashed" , color = "#5DB1DDFF", alpha = 0.5) +
  geom_segment(aes(x = c75.4, y = -0.01, xend =  c75.4, yend = 0.75), linetype = "dashed" , color = "#802268FF", alpha = 0.5) +
  geom_text(aes(x = lubridate::as_date(c0)+108, y = 1.1,
                label = c("Vaccination campaign")), color = "grey5", size = 3.4, alpha = 0.8) +
  scale_x_date(labels = date_format("%b/%y"), breaks = waiver()) + #breaks = "2 month"
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0,100, by = 0.25),) +
  scale_color_manual(values = colores) +
  ylab("Single dose vaccination coverage(%)") + xlab("") + labs(subtitle = "") +
  labs(subtitle = "", color = "\n") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "grey8"),
        axis.text.y.left = element_text(color = "grey8"),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
        axis.text.x = element_text(color = "grey8"),
        plot.background = element_rect(color = "white"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        legend.position = "top")

#--------------------  Figure 1
p1 /(p2 + p3 + p4) + plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")




