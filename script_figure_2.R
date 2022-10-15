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
#---------------------------------------- R A T E   R A T I O S -----------------------------------
###################################################################################################

rate_BR <- df_obitos %>% 
  filter(faixa_etaria == "BR")
rate_BR <- rate_BR[, c(1,6)]
rate_fxEtaria <- dataset_completo[, c(20:25)]
filtro_rate_pop_BR <- bind_cols(rate_BR, rate_fxEtaria)
View(filtro_rate_pop_BR)

#--------------------  Creating Figure 2A
colores <- c("#6BD76BFF", "#BA6338FF", "#5DB1DDFF", "#802268FF")
names(colores) <- c("20-49", "50-59", "60-69", "70+")

c0 <- lubridate::ymd("2021-01-15") 
datebreaks <- c(as.Date("2020-07-11"), as.Date("2020-10-31"), as.Date("2021-01-31"),
                as.Date("2021-04-30"), as.Date("2021-07-31"), as.Date("2021-10-31"), 
                as.Date("2021-12-31"))

L1 <- filtro_rate_pop_BR %>% filter(data>="2020-07-01") %>% 
  ggplot() +
  geom_line(aes(x = data, y = `rate_70+`/`deaths_pop`, color = '70+'), size = 1) +
  geom_line(aes(x = data, y = `rate_60-69`/`deaths_pop`, color = '60-69'), size = 1) +
  geom_line(aes(x = data, y = `rate_50-59`/`deaths_pop`, color = '50-59'), size = 1) +
  geom_line(aes(x = data, y = `rate_20-49`/`deaths_pop`, color = '20-49'), size = 1) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  geom_vline(aes(xintercept = c0), linetype = "dashed" , color = "black", alpha = 0.5) +
  geom_text(aes(x = lubridate::as_date(c0) + 116, y = 7,
                label = c("Vaccination campaign")), color = "grey5", size = 3.4, alpha = 0.8) +
  scale_x_date(labels = date_format("%b/%y"), breaks = datebreaks) + #breaks = "2 month", waiver()
  scale_color_manual(name = '',
                     breaks = c('20-49', '50-59', '60-69', '70+'),
                     values = c('20-49' = "#6BD76BFF",'50-59' = "#BA6338FF", '60-69' = "#5DB1DDFF", '70+' = "#802268FF")) +
  scale_y_continuous(breaks = seq(0,35, by = 1)) +
  theme_classic() +
  ylab("Rate Ratio") + xlab("") +
  theme(axis.title.y.left = element_text(color = "grey8"),
        axis.text.y.left = element_text(color = "grey8"),
        axis.title.y.right = element_text(color = "grey8"),
        axis.text.y.right = element_text(color = "grey8"),
        axis.text.x = element_text(color = "grey8"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        legend.position = "none")


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


#------------------------ DID/RR CLASSIC

modelo_DID_classicoBR <- MASS::glm.nb(deaths ~ faixa_etaria + control_trat + control_trat*faixa_etaria + 
                                        offset(log(population)), link = log, data = df_modelagem)

tidy_DID_classicoBR <- tidy(modelo_DID_classicoBR, exponentiate = TRUE, conf.int = TRUE)

#------------------------ DID/RR TEMPORAL

modelo_DID_temporalBR <- MASS::glm.nb(deaths ~ faixa_etaria + mes + mes*faixa_etaria + 
                                        offset(log(population)), link = log, data = df_modelagem)

tidy_DID_temporalBR <- tidy(modelo_DID_temporalBR, exponentiate = TRUE, conf.int = TRUE)


#------------------------ RRR dataframe to plot the results

# Referência BR
df_DIDclassico_ref.BR <- tidy_DID_classicoBR[c(7:nrow(tidy_DID_classicoBR)), c(1,2,6,7)] %>% 
  mutate(faixa_etaria = substr(term, start = 13, stop = (nchar(term)-14))) %>% 
  select(faixa_etaria, estimate, conf.low, conf.high)


df_DIDtemporal_ref.BR <- tidy_DID_temporalBR[c(23:nrow(tidy_DID_temporalBR)), c(1,2,6,7)] %>% 
  mutate(faixa_etaria = substr(term, start = 13, stop = (nchar(term)-12)),
         data = lubridate::my(if_else(nchar(term) == 29, 
                                      substr(term, start = 22, stop = nchar(term)),
                                      substr(term, start = 20, stop = (nchar(term)))
         ))) %>% 
  select(data, faixa_etaria, estimate, conf.low, conf.high) %>% 
  bind_rows(., data.frame(data = lubridate::my("Dec-2020"), 
                          faixa_etaria = c("20-49", "50-59", "60-69", "70+"),
                          estimate = 1, conf.low = 1, conf.high = 1)) %>% 
  arrange(., faixa_etaria, data)


#--------------------  Creating Figure 2B
c0 <- lubridate::ymd("2021-01-15") 
datebreaks <- c(as.Date("2020-07-01"), as.Date("2020-10-11"), as.Date("2021-01-01"),
                as.Date("2021-04-01"), as.Date("2021-07-01"), as.Date("2021-10-01"), 
                as.Date("2021-12-01"))

L2 <- df_DIDtemporal_ref.BR %>% 
  ggplot() +
  geom_line(aes(x = data, y = estimate, color = faixa_etaria), size = 0.5) +
  geom_point(aes(x = data, y = estimate, color = faixa_etaria), size = 1) +
  geom_errorbar(aes(x = data, ymin = conf.low, ymax = conf.high, color = faixa_etaria), width = 0.04) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  geom_vline(aes(xintercept = c0), linetype = "dashed" , color = "black", alpha = 0.5) +
  geom_text(aes(x = lubridate::as_date(c0) + 108, y = 3.5,
                label = c("Vaccination campaign")), color = "grey5", size = 3.4, alpha = 0.8) +
  scale_color_manual(values = colores) +
  scale_x_date(labels = date_format("%b/%y"), breaks = datebreaks) + #"3 months"
  
  ylab("Rate of Rate Ratios (95% CI)") + xlab("") +
  theme_classic()  +
  labs(subtitle = "", color = "\n") +
  theme(axis.title.y.left = element_text(color = "grey8"),
        axis.text.y.left = element_text(color = "grey8"),
        axis.title.y.right = element_text(color = "grey8"),
        axis.text.y.right = element_text(color = "grey8"),
        axis.text.x = element_text(color = "grey8"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        legend.position = "none")

#--------------------  Figure 2-AB
(L1 + plot_layout(guides = "collect")  & theme(legend.position = "bottom")) +  
  L2 + plot_annotation(tag_levels = "A")  


