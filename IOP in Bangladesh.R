library(tidyverse)
library(patchwork)
library(scales)
library(ggrepel)
library(haven)

df <- read_sav("Intraocular Pressure (IOP) in Bangladesh/Data IOP.sav")

#Create plot of BMI vs IOPRight
plot1 <- df |> 
  drop_na(BMI) |> 
  ggplot(aes(x = BMI, y = IOPRight)) +
  geom_point(aes(color = as.factor(IOpGr_rt))) +
  labs(color = "IOP Classification") +
  scale_color_hue(labels = c("Normal", "Raised")) +
  theme(
    legend.position = "bottom",
  ) +
  geom_hline(yintercept=21.5)
plot1

#Create plot of BMI vs IOPLeft
plot2 <- df |> 
  drop_na(BMI) |> 
  ggplot(aes(x = BMI, y = IOPLeft)) +
  geom_point(aes(color = as.factor(IOPGr_lt))) +
  labs(color = "IOP Classification") +
  scale_color_hue(labels = c("Normal", "Raised")) +
  theme(
    legend.position = "bottom",
  ) +
  geom_hline(yintercept=21.5)
plot2

bmi_iop <- plot1 + plot2
bmi_iop

plot3 <- df |> 
  drop_na(SBPavg) |> 
  ggplot(aes(x = SBPavg, y = IOPRight)) +
  geom_point(aes(color = as.factor(IOpGr_rt))) +
  labs(color = "IOP Classification") +
  scale_color_hue(labels = c("Normal", "Raised")) +
  theme(
    legend.position = "bottom",
  ) +
  geom_hline(yintercept=21.5)
plot3

#Create plot of BMI vs IOPLeft
plot4 <- df |> 
  drop_na(SBPavg) |> 
  ggplot(aes(x = SBPavg, y = IOPLeft)) +
  geom_point(aes(color = as.factor(IOPGr_lt))) +
  labs(color = "IOP Classification") +
  scale_color_hue(labels = c("Normal", "Raised")) +
  theme(
    legend.position = "bottom",
  ) +
  geom_hline(yintercept=21.5)
plot4

sbp_iop <- plot3 + plot4
sbp_iop

plot5 <- df |> 
  drop_na(DBPavg) |> 
  ggplot(aes(x = DBPavg, y = IOPRight)) +
  geom_point(aes(color = as.factor(IOpGr_rt))) +
  labs(color = "IOP Classification") +
  scale_color_hue(labels = c("Normal", "Raised")) +
  theme(
    legend.position = "bottom",
  ) +
  geom_hline(yintercept=21.5)
plot5

#Create plot of BMI vs IOPLeft
plot6 <- df |> 
  drop_na(DBPavg) |> 
  ggplot(aes(x = DBPavg, y = IOPLeft)) +
  geom_point(aes(color = as.factor(IOPGr_lt))) +
  labs(color = "IOP Classification") +
  scale_color_hue(labels = c("Normal", "Raised")) +
  theme(
    legend.position = "bottom",
  ) +
  geom_hline(yintercept=21.5)
plot6

dbp_iop <- plot5 + plot6
dbp_iop

#Check correlation
df <- df |> 
  drop_na(DBPavg, SBPavg, BMI)
bmi_iop_corr <- cor(df$BMI, df$IOP_Total)
dbp_iop_corr <- cor(df$DBPavg, df$IOP_Total)
sbp_iop_corr <- cor(df$SBPavg, df$IOP_Total)

#Create correlation matrix
df_corr <- cor(df)
df_corr
