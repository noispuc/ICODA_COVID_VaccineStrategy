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
#------------------------- FIGURES SINGLE AND FULL VACCIONATION COVERAGE --------------------------
###################################################################################################

colores <- c("#6BD76BFF", "#BA6338FF", "#5DB1DDFF", "#802268FF")
names(colores) <- c("20-49", "50-59", "60-69", "70+")
c0 <- lubridate::ymd("2021-01-15")

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

#----------------------- Figure first dose vaccination
c1 <- df_obitos %>% 
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
  ylab("First dose vaccination \n coverage(%)") + xlab("") + labs(subtitle = "") +
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
  mutate(cobertura_full = round(cobertura_full,4))
c75.1 <- filtro_20_49$data[which(filtro_20_49$cobertura_full > 0.75)][1]

filtro_50_59 <- df_obitos %>% 
  filter(faixa_etaria == "50-59") %>% 
  mutate(cobertura_full = round(cobertura_full,4))
c75.2 <- filtro_50_59$data[which(filtro_50_59$cobertura_full > 0.75)][1]

filtro_60_69 <- df_obitos %>% 
  filter(faixa_etaria == "60-69") %>% 
  mutate(cobertura_full = round(cobertura_full,4))
c75.3 <- filtro_60_69$data[which(filtro_60_69$cobertura_full > 0.75)][1]

filtro_70 <- df_obitos %>% 
  filter(faixa_etaria == "70+") %>% 
  mutate(cobertura_full = round(cobertura_full,4))
c75.4 <- filtro_70$data[which(filtro_70$cobertura_full > 0.75)][1]

#----------------------- Figure second or single dose vaccionation
c2 <- df_obitos %>% 
  filter(faixa_etaria %in% c("20-49", "50-59", "60-69", "70+"), data > "2020-12-31") %>% 
  mutate(cobertura_full = if_else(faixa_etaria == "20-49" & cobertura_full > 1, 1, cobertura_full)) %>% 
  ggplot() +
  geom_line(aes(x = data, y = cobertura_full, color = faixa_etaria), size = 1) +
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
  ylab("Second or single vaccination \n coverage(%)") + xlab("") + labs(subtitle = "") +
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

(c1 + c2) + plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")


