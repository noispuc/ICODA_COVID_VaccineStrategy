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

#--------------------  Figure 3
CT4 + CT3 + CT2 + CT1 + plot_annotation(tag_levels = "A")


