rm(list = ls())

library(tidyverse)
library(readxl)
library(ggstatsplot)
library(stargazer)


# Data ====
data <- read_excel("G:/My Drive/Research/UofSC Research/Data/__Dataset 1 Canada COVID-19 Game 2020.xlsx")
#View(data)
timestamps <- read_excel("G:/My Drive/Research/UofSC Research/Data/timestamps.xlsx")
#View(timestamps)

# Wrangling
colnames(data) <- c("session_id", "source", "ad_code", 
                            "FB_audience_target", "Right_6", 
                            paste("Emotion_proportion", data[1, 6:9], sep = "_"), 
                            paste("Policy_Support", data[1, 10:16], sep = "_"), 
                            paste("Repeated_Question", data[1, 17:40], sep = "_"), 
                            paste("True/False_Knowledge_Questions", data[1, 41:64], sep = "_"),
                            paste("Willingness_Range", data[1, 65:66], sep = "_"),
                            paste("Demographics", data[1, 67:70], sep = "_"), 
                            "Sharing_Behaviour_Actual_Sharing")

data <- data[-c(1), ]
data <- data.frame(data)

colnames(timestamps)[1] <- "session_id"
data$session_id <- as.numeric(data$session_id)

merged <- left_join(data, timestamps, by = "session_id")
#View(merged)

merged <- merged %>%
                select(timestamp, everything()) %>%
                mutate(Date = as.Date(timestamp)) %>%
                mutate(post = ifelse(Date >= "2020-12-01", 1, 0)) %>%
                select(Date, post,  everything()) 

# DiD (Time) =====

DiD_time <- merged %>%  
                filter(!is.na(post)) %>%
                select(Date, post, source, ad_code, FB_audience_target, 
                       Emotion_proportion_HAP_PRO, Emotion_proportion_ANG_PRO, 
                       Emotion_proportion_ANX_PRO, Emotion_proportion_SKP_PRO, 
                       True.False_Knowledge_Questions_KNOW_AVG, 
                       Willingness_Range_SHARINTENT, Willingness_Range_VAXINTENT, 
                       Demographics_age, Demographics_ethn1, Demographics_ethn2, Demographics_gend, 
                       Sharing_Behaviour_Actual_Sharing)

colnames(DiD_time) <- c("Date", "Post", "Source", "Ad_Code", "FB_Target", "HAP_PRO", 
                        "ANG_PRO", "ANX_PRO", "SKP_PRO", "Know_AVG", "Share_Int", "Vax_Int", 
                        "Age", "Ethn1", "Ethn2", "Sex", "Share_Act")
DiD_time$Know_AVG <- as.numeric(DiD_time$Know_AVG)

#View(DiD_time)


colnames(DiD_time)

ggbetweenstats(
  data = DiD_time, 
  x = Source, 
  y = Know_AVG, 
  plot.type = "violin", 
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  annotation.args  = list(title = "Differences in movie length by mpaa ratings for different genres"), 
  pairwise.comparisons = F, 
  title = "Average Knowledge Score per Platform", 
  xlab = "Platform", 
  ylab = "Average Knowledge Score", 
  caption = "For each platform, Violin and Jitter plot can be observed. Points were clustered for clarity.  ", 
  output = "plot_0418"
  
)

DiD_time$Vax_Int <- as.numeric(DiD_time$Vax_Int)

ggplot(data = DiD_time, aes(x = as.factor(Vax_Int), y = Know_AVG)) + 
  geom_boxplot(aes(col = Know_AVG))

DiD_time <- DiD_time %>%
              mutate(Vax_Int_Factor = ifelse(Vax_Int == 1, "Not at all Likely", 
                                             ifelse(Vax_Int == 2, "Unlikely", 
                                                  ifelse(Vax_Int == 3, "Somewhat Likely", 
                                                         ifelse(Vax_Int == 4, "Likely", 
                                                                "Very much Likely")))))
              

DiD_time$Vax_Int_Factor <- factor(DiD_time$Vax_Int_Factor, ordered = T, 
                                  levels = c("Not at all Likely", "Unlikely", "Somewhat Likely", "Likely", 
                                             "Very much Likely"))

ggbetweenstats(
  data = DiD_time, 
  x = Vax_Int_Factor, 
  y = Know_AVG, 
  point.args = list(size = 2), 
  title = "Average Knowledge Score per Response to Vaccination Intent", 
  xlab = "Vaccination Intent", 
  ylab = "Knowledge Score"
)

viz_timeseries <- DiD_time %>%
                    group_by(Date) %>%
                    summarize(count = n())

library(scales)

ggplot(viz_timeseries, aes(x = Date, y = count)) + geom_point(col = "darkred", size = 2) +
  geom_line(col = "darkred", size = 1.2) + 
  geom_vline(xintercept = viz_timeseries$Date[98], size = 1.2, color = "darkblue") + 
  xlab("Date of Session") + ylab("Number of Games") + 
  ggtitle("Time Series Plot of Number of Games per Date") + 
  theme_bw() + 
  theme(axis.title = element_text(face = "bold"))





# DiD Regression Analysis =====


DiD_time <- merged %>%
                    filter(!is.na(merged$Willingness_Range_VAXINTENT))

DiD_time_reg <- DiD_time[, -c(20:43)]

sort(sapply(DiD_time, function(x) sum(is.na(x))))

DiD_time_reg_treat <- DiD_time %>%
                filter( !is.na(True.False_Knowledge_Questions_Masks.in.public) & 
                                            !is.na(True.False_Knowledge_Questions_Hydroxychloroquine) & 
                                            !is.na(True.False_Knowledge_Questions_Children.may.spread) & 
                                            !is.na(True.False_Knowledge_Questions_Remdesivir)) %>%
                mutate(outcome = as.numeric(Willingness_Range_VAXINTENT))

DiD_time_reg_control<- DiD_time %>%
  filter(is.na(True.False_Knowledge_Questions_Masks.in.public) & 
            is.na(True.False_Knowledge_Questions_Hydroxychloroquine) & 
            is.na(True.False_Knowledge_Questions_Children.may.spread) & 
            is.na(True.False_Knowledge_Questions_Remdesivir)) %>%
  mutate(outcome = as.numeric(Willingness_Range_VAXINTENT))



viz_timeseries_reg_control <- DiD_time_reg_control %>%
  group_by(Date) %>%
  summarize(count = n(), 
            outcome = sum(outcome))


viz_timeseries_reg_treat <- DiD_time_reg_treat %>%
  group_by(Date) %>%
  summarize(count = n(), 
            outcome = sum(outcome))


ggplot(viz_timeseries_reg_control, aes(x = Date, y = outcome)) + geom_point(col = "darkred", size = 2) +
  geom_line(col = "darkred", size = 1.2) + 
  geom_vline(xintercept = viz_timeseries$Date[98], size = 1.2, color = "darkblue") + 
  xlab("Date of Session") + ylab("Sum of Scores for Vaccination Intent") + 
  ggtitle("Treatment Group - Time Series Plot of Sum of Scores for Vaccination Intent per Date") + 
  theme_bw() + 
  theme(axis.title = element_text(face = "bold"))


ggplot(viz_timeseries_reg_treat, aes(x = Date, y = outcome)) + geom_point(col = "darkred", size = 2) +
  geom_line(col = "darkred", size = 1.2) + 
  geom_vline(xintercept = viz_timeseries$Date[98], size = 1.2, color = "darkblue") + 
  xlab("Date of Session") + ylab("Sum of Scores for Vaccination Intent") + 
  ggtitle("Control Group - Time Series Plot of Sum of Scores for Vaccination Intent per Date") + 
  theme_bw() + 
  theme(axis.title = element_text(face = "bold"))





merged <- left_join(viz_timeseries_reg_treat, viz_timeseries_reg_control, by = "Date")


ggplot(data = merged, aes(x = Date)) + geom_point(aes(y = outcome.y), col = "steelblue", size = 2) +
  geom_line(aes(y = outcome.y), col = "steelblue", size = 1.2) + 
  geom_vline(xintercept = viz_timeseries$Date[98], size = 1.2, color = "darkblue") + 
  xlab("Date of Session") + ylab("Sum of Scores for Vaccination Intent") + 
  ggtitle("Treatment Group vs Control Group Comparison") + 
  theme_bw() + 
  theme(axis.title = element_text(face = "bold")) + 
  geom_point(aes(y = outcome.x), col = "darkred", size = 2) +
  geom_line(aes(y = outcome.x), col = "darkred", size = 1.2)







model1 <- glm(data = DiD_time_reg, 
              outcome ~ treatment + post + treatment * post, family = "binomial")

model2 <- glm(data = DiD_time_reg, 
              outcome ~ treatment + post + treatment * post + 
                as.numeric(Demographics_age) + as.numeric(Demographics_gend) + as.numeric(Demographics_ethn1), family = "binomial")



model3 <- glm(data = DiD_time_reg, 
              outcome ~ treatment + post + treatment * post + 
                as.numeric(Demographics_age) + as.numeric(Demographics_gend) + as.numeric(Demographics_ethn1)
               + as.numeric(Sharing_Behaviour_Actual_Sharing)  , family = "binomial")


model4 <- glm(data = DiD_time_reg, 
            outcome ~ treatment + post + treatment * post + 
              as.numeric(Demographics_age) + as.numeric(Demographics_gend) + as.numeric(Demographics_ethn1)
             + as.numeric(Sharing_Behaviour_Actual_Sharing) + source , family = "binomial")

summary(model)
colnames(DiD_time_reg)
table(DiD_time_reg$treatment)



stargazer(model1, model2, model3, model4, type = "latex", 
          covariate.labels = c("Treatment", "Post Cut-Off", "Treatment x Post Cut-Off", 
                               "Gender: Male", "Age", "Sharing Behaviour", "Platform: Google Ads", "Platform: mTurk", 
                               "Platform: Nanos", "Platform: Partners", "Platform: Post Media", "Platform: Redirect", 
                               "Platform: RIWI", "Platform: Snapchat", "Platform: TikTok", "Platform: Twitter", 
                               "constant"),
          dep.var.labels = "Vaccination Intent", 
          dep.var.caption = "Difference in Difference Regression", 
          omit = c("Constant"), 
          column.sep.width = "30pt", 
          title = "Regression Analysis of Knowledge Questions Impact on Vaccination Intent")



stargazer(model1, model2, model3, type = "latex", 
          covariate.labels = c("Treatment", "Post Cut-Off", "Treatment x Post Cut-Off", 
                               "Gender: Male", "Age", "Sharing Behaviour", 
                               "constant"),
          dep.var.labels = "Vaccination Intent", 
          dep.var.caption = "Difference in Difference Regression", 
          omit = c("Constant"), 
          column.sep.width = "30pt", 
          title = "Regression Analysis of Knowledge Questions Impact on Vaccination Intent")




##################### Imbens

DiD_time_reg <- DiD_time_reg %>%
                      mutate(treatment = -1 * treatment)

model1 <- glm(data = DiD_time_reg, 
              outcome ~ treatment  + 
                as.numeric(Demographics_age) + as.numeric(Demographics_gend) + as.numeric(Demographics_ethn1)
              + as.numeric(Sharing_Behaviour_Actual_Sharing)  , family = "binomial")

model2 <- glm(data = sample_n(DiD_time_reg, 45000), 
              outcome ~ treatment  + 
                as.numeric(Demographics_age) + as.numeric(Demographics_gend) + as.numeric(Demographics_ethn1)
              + as.numeric(Sharing_Behaviour_Actual_Sharing)  , family = "binomial")


model3 <- glm(data = sample_n(DiD_time_reg, 45000), 
              outcome ~ treatment  + 
                as.numeric(Demographics_age) + as.numeric(Demographics_gend) + as.numeric(Demographics_ethn1)
              + as.numeric(Sharing_Behaviour_Actual_Sharing)  , family = "binomial")



stargazer(model1, model2, model3, type = "latex", 
          covariate.labels = c("Treatment",
                               "Gender: Male", "Age", "Sharing Behaviour", "platform", "Emotional Scores", 
                               "constant"),
          dep.var.labels = "Vaccination Intent", 
          dep.var.caption = "Difference in Difference Regression - Questions as Backdoor", 
          omit = c("Constant"), 
          column.sep.width = "30pt", 
          title = "Regression Analysis of Knowledge Questions Impact on Vaccination Intent")


















