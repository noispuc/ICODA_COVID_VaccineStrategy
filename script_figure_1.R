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
