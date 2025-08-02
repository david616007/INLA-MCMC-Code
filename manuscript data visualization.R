setwd("D:/Documents/R/proyek")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

df <- read.csv("data manuscript.csv", sep = ";")

df$Mean <- round(df$Mean,3)

ggplot(df, aes(x = Year, y = Mean, group = Sub.district, 
                          color = Sub.district)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(2016, 2022, by = 1)) +
  scale_y_continuous(breaks = c(0.5,1,1.5,2)) +
  labs(x = "Year",
       y = "Relative Risk Estimation",
       color = "Sub-district") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 5),   # Resize legend text
        legend.title = element_text(size = 10))

df_mcmc <- read.csv("hasil mcmc_2.csv", sep = ";")
df_long <- df_mcmc %>%
  pivot_longer(cols = starts_with("IN"), 
               names_to = "Year", 
               values_to = "RelativeRisk") %>%
  mutate(Year = as.integer(sub("IN", "20", Year)))

# Plotting
ggplot(df_long, aes(x = Year, y = RelativeRisk, group = ID, color = ID)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 2016:2022) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2)) +
  labs(x = "Year",
       y = "Relative Risk Estimation",
       color = "Sub-district") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 5),   # Resize legend text
        legend.title = element_text(size = 10))

df_lengkong <- df %>% filter(Sub.district == "Lengkong")

ggplot(df_lengkong, aes(x = Year, y = Mean)) +
  geom_ribbon(aes(ymin = X0.025quant, ymax = X0.975quant), 
              fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1.2) +
  scale_x_continuous(breaks = seq(2016, 2022, by = 1)) +
  scale_y_continuous(breaks = c(0.5,1,1.5,2)) +
  labs(title = "Relative Risk Estimation with Confidence Interval (Lengkong)",
       x = "Year",
       y = "Relative Risk Estimation") +
  theme_minimal()

df2 <- read.csv("2017 CI manuscript.csv", sep = ";")
df2_long <- df2 %>%
  pivot_longer(cols = c(MCMC, INLA),  # Columns to pivot
               names_to = "Method",   # New column for method names
               values_to = "Value")   # New column for values

ggplot(df2_long, aes(x = Method, y = Value, fill = Method)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Method",
       y = "Confidence Interval Difference Range") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend


df3 <- read.csv("ins CI manuscript.csv",sep = ";")
df3_long <- df3 %>%
  pivot_longer(cols = c(MCMC, INLA),  # Columns to pivot
               names_to = "Method",   # New column for method names
               values_to = "Value")   # New column for values
ggplot(df3_long, aes(x = Method, y = Value, fill = Method)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("MCMC" = "purple", "INLA" = "yellow")) +
  labs(x = "Method",
       y = "Confidence Interval Difference Range") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend
