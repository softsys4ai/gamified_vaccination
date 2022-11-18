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
library(plotly)
library(xtable)

getwd()

theme_set(theme_light(base_size = 18, base_family = "Poppins"))
# font_add_google("Poppins", "Poppins")
# font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()


#*******************************************************************************
#*******************************************************************************
## Data ----
#*******************************************************************************
#*******************************************************************************

df_complete <- read.csv("Data/df_complete.csv")
df_complete$X <- NULL


#*******************************************************************************
#*******************************************************************************
## Correlation Analysis ----
#*******************************************************************************
#*******************************************************************************

cor_data <- df_complete[, c("HAP_PRO", "ANG_PRO", "EANX_PRO", "SKP_PRO", 
                            "POLSUP_AVG", "KNOW_AVG", "SHARINTENT", 
                            "VAXINTENT", "age")]

colnames(cor_data) <- c("Happiness", "Angry", "Anxious", "Skeptic", 
                        "Policy Support", "Knowledge Score", "Sharing Behaviour", 
                        "Vaxine Intent", "Age")


M <- cor(cor_data)
corrplot(M, method = "color",
         type = "lower", 
        # order = "hclust", 
         tl.col = "black", 
         tl.srt = 45)

#*******************************************************************************
#*******************************************************************************
## Clustering ----
#*******************************************************************************
#*******************************************************************************

df_cluster_unscaled <- df_complete[, c("HAP_PRO", "ANG_PRO", "EANX_PRO", "SKP_PRO", 
                            "POLSUP_AVG", "KNOW_AVG", "SHARINTENT", 
                            "VAXINTENT", "age", "Actual_Sharing", 
                            "ethn1", "ethn2", "gend")]

df_cluster <- scale(df_cluster_unscaled)

# Determining Ideal Number of Clusters
### Elbow Method 
fviz_nbclust(df_cluster, kmeans, # nstart=100, 
             method = "wss") + geom_vline(xintercept = 4, linetype = 1) + 
  labs(subtitle = "Elbow Method")

### Silhouette Method 
fviz_nbclust(df_cluster, kmeans, # nstart=100, 
             method = "silhouette") + geom_vline(xintercept = 4, linetype = 1) + 
  labs(subtitle = "Silhouette Method")

### Gap Stats Method 
fviz_nbclust(df_cluster, kmeans, nstart= 25, nboot = 10, 
             method = "gap_stat") + geom_vline(xintercept = 4, linetype = 1) + 
  labs(subtitle = "Gap Statistic Method")


# Clustering Analysis
clust <- kmeans(df_cluster, centers = 4, nstart = 100)
summary(clust)


kmeans_table <- data.frame(clust$size, clust$centers)
kmeans_df <- data.frame(Cluster = clust$cluster, df_cluster)

head(kmeans_df[10:12])

df_cluster_for_labelling <- as.data.frame(df_cluster)

# Visualiztion of clusters based on first two eigen vectors
fviz_cluster(clust, data = df_cluster, 
             geom = "point", 
             ellipse.type = "norm",
             palette = "jco",
            ellipse.alpha = 0.1,
            pointsize = .5,
            shape = 19,
            main = ""          ) +
            theme_minimal() +
           # scale_fill_viridis(discrete = T) +
            theme(
              # legend.position = c(0.5, 0.35),
              axis.title = element_text(size = 16),
              axis.text.x = element_text(family = "Roboto Mono", size = 12),
              axis.text.y = element_text(family = "Roboto Mono", size = 12),
              panel.grid = element_blank()) +
            theme_light(base_size = 18, base_family = "Poppins")
          


# 3-D plot of data based on Clusters

df_cluster_unscaled$km_cluster <- c(clust$cluster)

plot_ly(x = df_cluster_unscaled$KNOW_AVG, 
        y = df_cluster_unscaled$VAXINTENT,
        z = df_cluster_unscaled$HAP_PRO, 
        type = "scatter3d", 
        mode = "markers", 
        color = as.factor(df_cluster_unscaled$km_cluster)) %>%
  layout(title = "",
        scene = list(xaxis = list(title = "Knoweldge Score"),
                     yaxis = list(title = "Vaccine Intent"),
                     zaxis = list(title = "Happiness Score")))


# Summary Statistics of each cluster

df_cluster_summarized <- df_cluster_unscaled %>%
                              group_by(km_cluster) %>%
                              summarise("Knoweldge Score" = mean(KNOW_AVG),
                                        "Vaccine Intent" = mean(VAXINTENT),
                                        "Policy Support" = mean(POLSUP_AVG), 
                                        "Happiness Score" = mean(HAP_PRO),
                                        # "Anger Score" = mean(ANG_PRO), 
                                        # "Anxiety Score" = mean(EANX_PRO), 
                                        # "Skepticism Score" = mean(SKP_PRO), 
                                         "Count" = n()) %>% 
                              mutate_all(round, 3)

# Converting summary statistics to Latex table
xtable(df_cluster_summarized)


# Exploring each cluster further

# Gender
p1 <- df_cluster_unscaled %>%
        mutate(Gender = ifelse(gend == 1, "Female", 
                               ifelse(gend == 2, "Male",
                                      "Non-Binary"))) %>%
        group_by(km_cluster, Gender) %>%
        summarise(count = n()) %>%
        ggplot(aes(x = km_cluster, y = count, fill = Gender)) +
        geom_col(position = "dodge", color = "white") +
        scale_fill_brewer(palette = "Set1") +
        labs(y = "Count", x = "Clusters") +
        theme_minimal() +
        theme(legend.position = "top") +
        labs(x = "Clusters", y = "Count", fill = "Gender") +
        scale_fill_uchicago() +
        theme(
          legend.position = "top",
          axis.title = element_text(size = 16),
          axis.text.x = element_text(family = "Roboto Mono", size = 12), 
          axis.text.y = element_text(family = "Roboto Mono", size = 12),
          panel.grid = element_blank()) +
        theme_light(base_size = 18, base_family = "Poppins") #+ guides(fill = F)
      

# Age
p2 <- df_cluster_unscaled %>%
  mutate(Age = ifelse(age == 1, "15-18", 
                         ifelse(age == 2, "18-24",
                                ifelse(age == 3, "25-34",
                                       ifelse(age == 4, "35-44", 
                                              ifelse(age == 5, "45-54",
                                                     ifelse(age == 6, "55-64",
                                                            ifelse(age == 7, "65-74",
                                                                   ifelse(age == 8,"75-84",
                                                                          "85+"))))))))) %>%
  group_by(km_cluster, Age) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = km_cluster, y = count, fill = Age)) +
  geom_col(position = "dodge", color = "white") +
 #  scale_fill_brewer(palette = "Set2") +
  labs(y = "Count", x = "Clusters") +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(x = "Clusters", y = "Count", fill = "Age") +
  # scale_fill_uchicago() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12), 
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") #+ guides(fill = F)


p2 <- set_palette(p2, "jco")


# Ethnicity 

p3 <- df_cluster_unscaled %>%
  mutate(Ethnicity = ifelse(ethn1 == 1, "Aboriginal", 
                      ifelse(ethn1 == 2, "Asian",
                             ifelse(ethn1 == 3, "Black",
                                    ifelse(ethn1 == 4,  "Latin", 
                                           ifelse(ethn1 == 5, "White",
                                                  ifelse(ethn1 == 6,  "Mid. Eastern", 
                                                         ifelse(ethn1 == 7, "Mixed","NR")))))))) %>%
  group_by(km_cluster, Ethnicity) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = km_cluster, y = count, fill = Ethnicity)) +
  geom_col(position = "dodge", color = "white") +
  #  scale_fill_brewer(palette = "Set2") +
  labs(y = "Count", x = "Clusters") +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(x = "Clusters", y = "Count", fill = "Ethnicity") +
  # scale_fill_uchicago() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12), 
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") #+ guides(fill = F)


p3 <- set_palette(p3, "Dark2")




# Vaccine Intended
p4 <- df_cluster_unscaled %>%
  mutate(Vaccine = ifelse(VAXINTENT == 1, "Very Unlikely", 
                            ifelse(VAXINTENT == 2, "Unlikely",
                                   ifelse(VAXINTENT == 3, "Somewhat Likely",
                                          ifelse(VAXINTENT == 4, "Likely", 
                                                 "Very Likely"))))) %>%
  mutate(Vaccine = factor(Vaccine, 
                          levels = c("Very Unlikely", "Unlikely", "Somewhat Likely", 
                                     "Likely", "Very Likely"))) %>%
  group_by(km_cluster, Vaccine) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = km_cluster, y = count, fill = Vaccine)) +
  geom_col(position = "dodge", color = "white") +
  #  scale_fill_brewer(palette = "Set2") +
  labs(y = "Count", x = "Clusters") +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(x = "Clusters", y = "Count", fill = "Vaccine Intent") +
  # scale_fill_uchicago() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12), 
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") #+ guides(fill = F)


p4 <- set_palette(p4, "locuszoom")

ggarrange(p1, p2, p3, p4, 
          labels = c("A", "B", "C", "D"))


















































