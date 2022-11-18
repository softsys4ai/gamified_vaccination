#*******************************************************************************
#*******************************************************************************
# Housekeeping ----
#*******************************************************************************
#*******************************************************************************

rm(list = ls())

library(tidyverse)
library(readxl)
library(readxl)
library(tidyverse)
library(data.table)
library(corrplot)
library(mice)
library(VIM)
library(stargazer)
library(ggthemes)
library(hrbrthemes)
library(ggsci)
library(systemfonts)
library(showtext)
library(viridis)
library(ggpubr)
library(kableExtra)
library(png)
library(factoextra)
library(mice)
library(VIM)

getwd()

theme_set(theme_light(base_size = 18, base_family = "Poppins"))
font_add_google("Poppins", "Poppins")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()

#*******************************************************************************
#*******************************************************************************
# Data ----
#*******************************************************************************
#*******************************************************************************

df <- read.csv("Data/df.csv")
# View(df)
#*******************************************************************************
#*******************************************************************************
## Data Exploration ----
#*******************************************************************************
#*******************************************************************************

# Histogram of Missing Values
df5 <- t(data.frame(colSums(is.na(df))))
barplot(df5, las = 2, main = "Canada Vax Game - Histogram of Missing Values")


# Bor plot of categories: Source, ad_code, FB_audience_target
# Source
source_bar_plot <- data.frame(table(df$source))
ggplot(data = source_bar_plot, aes(x = reorder(Var1, -Freq), fill = Var1, y = Freq)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Source", y = "Frequency", fill = "Source") +
  theme_minimal() + 
  scale_fill_viridis(discrete = T) + 
  theme(
    legend.position = c(0.5, 0.35),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12), 
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)

# FB_audience_target
fb_bar_plot <- data.frame(table(df$FB_audience_target))
ggplot(data = fb_bar_plot, aes(x = reorder(Var1, -Freq), fill = Var1, y = Freq)) + 
  geom_bar(stat = "identity") +
  labs(x = "FB Audience Target", y = "Frequency", fill = "FB Target") +
  theme_minimal() + 
  scale_fill_uchicago() +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12), 
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)

# Age
age_bar_plot <- data.frame(table(df$Demographics_age))
age_bar_plot$ranges <- c("15-18", "18-24", "25-34", 
                         "35-44", "45-54", "55-64", 
                         "65-74", "75-84", "85+")
age_bar_plot
p1 <- ggplot(data = age_bar_plot, 
             aes(x = reorder(ranges, -Freq), fill = ranges, y = Freq)) + 
  geom_bar(stat = "identity") +
  labs(x = "Age", y = "Frequency", fill = "Age") +
  theme_minimal() + 
  scale_fill_uchicago() +
  theme(
    legend.position = c(0.8, 0.9),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12), 
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)

# Gender
gender_bar_plot <- data.frame(table(df$Demographics_gend))
gender_bar_plot$genders <- c("Female", "Male", "Non-binary")

p2 <- ggplot(data = gender_bar_plot, 
             aes(x = reorder(genders, -Freq), fill = genders, y = Freq)) + 
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Frequency", fill = "Gender") +
  theme_minimal() + 
  scale_fill_uchicago() +
  theme(
    legend.position =  "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12), 
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)

# Ethnicity 1
ethnic_bar_plot <- data.frame(table(df$Demographics_ethn1))
ethnic_bar_plot$ethnic <- c("Aboriginal", "Asian", "Black", 
                            "Latin", "White", "Mid. Eastern", 
                            "Mixed", "NR")
p3 <- ggplot(data = ethnic_bar_plot, 
             aes(x = reorder(ethnic, -Freq), fill = ethnic, y = Freq)) + 
  geom_bar(stat = "identity") +
  labs(x = "Ethnicity 1", y = "Frequency") +
  theme_minimal() + 
  scale_fill_uchicago() +
  theme(
    legend.position =  "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12), 
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)

# Ethnicity 2
ethnic2_bar_plot <- data.frame(table(df$Demographics_ethn2))
ethnic2_bar_plot$ethnic <- c("Aboriginal", "Asian", "Black", 
                             "Latin", "White", "Mid. Eastern", 
                             "Mixed", "NR")
p4 <- ggplot(data = ethnic2_bar_plot, 
             aes(x = reorder(ethnic, -Freq), fill = ethnic, y = Freq)) + 
  geom_bar(stat = "identity") +
  labs(x = "Ethnicity 2", y = "Frequency") +
  theme_minimal() + 
  scale_fill_uchicago() +
  theme(
    legend.position =  "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12), 
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)

ggarrange(p1, p2, p3, p4, 
          common.legend = T)

#*******************************************************************************
# Scatter plot for Numerical Variables
#*******************************************************************************

p1 <- ggplot(data = df, aes(x = df$True.False_Knowledge_Questions_KNOW_AVG, 
                            y = df$Willingness_Range_VAXINTENT_BINARY)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = T, 
              fill = "lightblue", 
              color = "darkblue") + 
  labs(x = "Knowledge Score", 
       y = "Vaccine Intent") +
  theme_minimal() + 
  scale_fill_uchicago() +
  theme(
    legend.position =  "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 8), 
    axis.text.y = element_text(family = "Roboto Mono", size = 8),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)





# Line Chart for numerical variables
df_complete_vaxintent_binary <- df[!is.na(df$Willingness_Range_VAXINTENT_BINARY),]
df_complete_vaxintent_binary$Willingness_Range_VAXINTENT_BINARY <- as.character(df_complete_vaxintent_binary$Willingness_Range_VAXINTENT_BINARY)
df_no_vaxintent <- dplyr::select(df_complete_vaxintent_binary, -Willingness_Range_VAXINTENT_BINARY)

p2 <- ggplot(data = #df_complete_vaxintent_binary, 
               df,
             aes(x = True.False_Knowledge_Questions_KNOW_AVG, 
                 y = Policy_Support_POLSUP_AVG)) + 
  # geom_point(data = df_no_vaxintent, colour = 'grey70') + 
  geom_point(alpha = 0.2) + 
  geom_smooth(fill = "lightblue", 
              color = "darkblue") +
  # facet_wrap(~Willingness_Range_VAXINTENT_BINARY) + 
  labs(x = "Knowledge Score", 
       y = "Policy Support Score") +
  theme_minimal() + 
  scale_fill_uchicago() +
  theme(
    legend.position =  "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 8), 
    axis.text.y = element_text(family = "Roboto Mono", size = 8),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)

p3 <- ggplot(data = #df_complete_vaxintent_binary, 
               df,
             aes(x = True.False_Knowledge_Questions_KNOW_AVG, 
                 y = Willingness_Range_VAXINTENT)) + 
  # geom_point(data = df_no_vaxintent, colour = 'grey70') + 
  geom_point(alpha = 0.2) + 
  geom_smooth(fill = "lightblue", 
              color = "darkblue") +
  # facet_wrap(~Willingness_Range_VAXINTENT_BINARY) + 
  labs(x = "Knowledge Score", 
       y = "Vaccine Intent") +
  theme_minimal() + 
  scale_fill_uchicago() +
  theme(
    legend.position =  "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 8), 
    axis.text.y = element_text(family = "Roboto Mono", size = 8),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)


p4 <- ggplot(data = #df_complete_vaxintent_binary, 
               df,
             aes(x = True.False_Knowledge_Questions_KNOW_AVG, 
                 y = df$Policy_Support_POLSUP_AVG_BINARY)) + 
  # geom_point(data = df_no_vaxintent, colour = 'grey70') + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = T, 
              fill = "lightblue", 
              color = "darkblue") +
  # facet_wrap(~Willingness_Range_VAXINTENT_BINARY) + 
  labs(x = "Knowledge Score", 
       y = "Policy Support Score") +
  theme_minimal() + 
  scale_fill_uchicago() +
  theme(
    legend.position =  "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 8), 
    axis.text.y = element_text(family = "Roboto Mono", size = 8),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)


p5 <- ggplot(data = #df_complete_vaxintent_binary, 
               df,
             aes(x = True.False_Knowledge_Questions_KNOW_AVG, 
                 y = df$Emotion_proportion_HAP_PRO)) + 
  # geom_point(data = df_no_vaxintent, colour = 'grey70') + 
  geom_point(alpha = 0.2) + 
  geom_smooth(
              fill = "lightblue", 
              color = "darkblue") +
  # facet_wrap(~Willingness_Range_VAXINTENT_BINARY) + 
  labs(x = "Knowledge Score", 
       y = "Happiness Score") +
  theme_minimal() + 
  scale_fill_uchicago() +
  theme(
    legend.position =  "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 8), 
    axis.text.y = element_text(family = "Roboto Mono", size = 8),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)


p6 <- ggplot(data = #df_complete_vaxintent_binary, 
               df,
             aes(x = True.False_Knowledge_Questions_KNOW_AVG, 
                 y = df$Willingness_Range_SHARINTENT)) + 
  # geom_point(data = df_no_vaxintent, colour = 'grey70') + 
  geom_point(alpha = 0.2) + 
  geom_smooth(
              fill = "lightblue", 
              color = "darkblue") +
  # facet_wrap(~Willingness_Range_VAXINTENT_BINARY) + 
  labs(x = "Knowledge Score", 
       y = "Sharing Intent") +
  theme_minimal() + 
  scale_fill_uchicago() +
  theme(
    legend.position =  "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 8), 
    axis.text.y = element_text(family = "Roboto Mono", size = 8),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F)

ggarrange(p2, p3, p1, p4, p5, p6,
          nrow = 3, ncol = 2, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          vjust = 3, 
          hjust = -0.5, 
          heights = 0.7)

# Policy Support vs Vax Intent

# ggplot(data = df, aes(x = df$Policy_Support_POLSUP_AVG, 
#                       y = df$Willingness_Range_VAXINTENT)) + 
#   geom_line()




























