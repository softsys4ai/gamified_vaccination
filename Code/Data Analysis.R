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
library(tableone)
library(Matching)
library(readxl)

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

df <- read.csv("Data/df.csv")
df$X <- NULL

df_timestamped <- read.csv("Data/df_timestamped.csv")
df_timestamped$X <- NULL


#*******************************************************************************
#*******************************************************************************
## Logistic Regression ----
#*******************************************************************************
#*******************************************************************************

model1 <- glm(data = df, 
              df$Willingness_Range_VAXINTENT_BINARY ~ 
                df$True.False_Knowledge_Questions_KNOW_AVG)

model2 <- glm(data = df, 
              df$Willingness_Range_VAXINTENT_BINARY ~ 
                df$True.False_Knowledge_Questions_KNOW_AVG + df$Emotion_proportion_HAP_PRO )

model3 <- glm(data = df, 
              df$Willingness_Range_VAXINTENT_BINARY ~ 
                df$True.False_Knowledge_Questions_KNOW_AVG + df$Emotion_proportion_HAP_PRO +
                df$Policy_Support_POLSUP_AVG)

model4 <- glm(data = df, 
              df$Willingness_Range_VAXINTENT_BINARY ~ 
                df$True.False_Knowledge_Questions_KNOW_AVG + df$Emotion_proportion_HAP_PRO +  
                df$Policy_Support_POLSUP_AVG + df$Willingness_Range_SHARINTENT_BINARY)

model5 <- glm(data = df, 
              df$Willingness_Range_VAXINTENT_BINARY ~ 
                df$True.False_Knowledge_Questions_KNOW_AVG + df$Emotion_proportion_HAP_PRO +  
                df$Policy_Support_POLSUP_AVG + df$Willingness_Range_SHARINTENT_BINARY + 
                df$Demographics_age )

# We should also add interaction between variables. 
# model6 

stargazer(model1, model2, model3, model4, model5, 
          type="latex",
         #  style="aer",
         # column.labels=c("Vaccination Change in Intent"),
          dep.var.labels="Vaccination Intent",
         covariate.labels = c("Knowlege Score", "Happiness Score", "Policy Support",
                              "Sharing Intent",
                              "Age")
         )



ggplot(df, aes(x=True.False_Knowledge_Questions_KNOW_AVG, y = Willingness_Range_VAXINTENT_BINARY )) +
  geom_point() + 
  # add logit curve
  stat_smooth(method="glm", method.args=list(family="binomial"), se= TRUE) 


#*******************************************************************************
#*******************************************************************************
## Propensity Score Matching ----
#*******************************************************************************
#*******************************************************************************

# treatment variable is in a form of 0, 1
# There are a bunch of indepentent x varibles: Age, Sex, blood pressure, etc. 

#TODO Adding dot plot of outcome of PSM

df_propensity <- df_complete
df_propensity$treatment <- ifelse((df_propensity$Antibiotics == 0) | 
                                    (df_propensity$Antibodies == 0) |
                                    (df_propensity$Restricting.access == 0),
                           1, 0)

treatment <- df_propensity$treatment

#covariates we will use (shorter list than you would use in practice)
xvars <- c( # "source", "ad_code", "FB_audience_target", 
  "HAP_PRO", "POLSUP_AVG", "KNOW_AVG",
  "SHARINTENT"    ,                 "VAXINTENT" ,                    
  "age"          ,                  "ethn1",
  "ethn2"   ,                       "gend" ,                         
  "Actual_Sharing")

#look at a table 1
table1<- CreateTableOne(vars=xvars,strata="treatment", data=df_propensity, test=FALSE)
## include standardized mean difference (SMD)
print(table1,smd=TRUE)

#fit a propensity score model. logistic regression

psmodel<-glm(treatment~  HAP_PRO+ POLSUP_AVG+  KNOW_AVG +
              SHARINTENT     +                  VAXINTENT  +                    
              age,
             family=binomial(),data=df_propensity)

#show coefficients etc
summary(psmodel)
#create propensity score
pscore<-psmodel$fitted.values


#do greedy matching on logit(PS) using Match with a caliper

logit <- function(p) {log(p)-log(1-p)}
psmatch<-Match(Tr=df_propensity$treatment,M=1,X=logit(pscore),replace=FALSE,caliper=.2)
matched<-df_propensity[unlist(psmatch[c("index.treated","index.control")]), ]
xvars <- c( # "source", "ad_code", "FB_audience_target", 
  "HAP_PRO", "POLSUP_AVG", "KNOW_AVG",
  "SHARINTENT"    ,                 "VAXINTENT" ,                    
  "age")

#get standardized differences
matchedtab1<-CreateTableOne(vars=xvars, strata ="treatment", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

#outcome analysis
y_trt<-matched$VAXINTENT[matched$treatment==1]
y_con<-matched$VAXINTENT[matched$treatment==0]

#pairwise difference
diffy<-y_trt-y_con

#paired t-test
propensity_ttest <- t.test(diffy)
propensity_ttest$p.value

p <- print(matchedtab1, printToggle = T, noSpaces = T)
kable(p, format = 'latex')




#*******************************************************************************
#*******************************************************************************
## Regression Discontinuity Design ----
#*******************************************************************************
#*******************************************************************************
#* Since We do not have a continous outcome variable, we can use Policy Support Avg
#* instead which is somewhat continous. The next challenge for regression discontinuity 
#* can be the randomization of treatment
# 
# df_RDD <- df %>%
#             dplyr::select(source, ad_code, FB_audience_target, Emotion_proportion_HAP_PRO, 
#                    Policy_Support_POLSUP_AVG,Policy_Support_POLSUP_AVG_BINARY ,
#                    True.False_Knowledge_Questions_KNOW_AVG, 
#                    Willingness_Range_SHARINTENT, 
#                    Willingness_Range_VAXINTENT, 
#                    Willingness_Range_VAXINTENT_BINARY, 
#                    Willingness_Range_SHARINTENT_BINARY, 
#                    Demographics_gend, Demographics_ethn1, Demographics_ethn2, Demographics_age, 
#                    Sharing_Behaviour_Actual_Sharing)
# 
# 
# ggplot(data = df_RDD, aes(x = df_RDD$True.False_Knowledge_Questions_KNOW_AVG, 
#                       y = df_RDD$Policy_Support_POLSUP_AVG)) +
#   geom_smooth() +
#   geom_point()
# 
# ggplot(data = df_RDD, aes(x = df_RDD$True.False_Knowledge_Questions_KNOW_AVG, 
#                           y = df_RDD$Willingness_Range_VAXINTENT)) +
#   geom_smooth() +
#   geom_point()
# 
# 
# ggplot(data = df_RDD, aes(x = df_RDD$Policy_Support_POLSUP_AVG, 
#                           y = df_RDD$Willingness_Range_VAXINTENT)) +
#   geom_smooth() +
#   geom_point()
# 
# 
# summary(lm(df = df_RDD, df_RDD$Policy_Support_POLSUP_AVG ~ 
#              df$True.False_Knowledge_Questions_KNOW_AVG))
# 
# 
# RDD_Complete <- lm(data = df_RDD, df_RDD$Policy_Support_POLSUP_AVG ~ df_RDD$True.False_Knowledge_Questions_KNOW_AVG + 
#              df_RDD$source + df_RDD$ad_code + df_RDD$FB_audience_target + df_RDD$Willingness_Range_SHARINTENT +
#              df_RDD$Willingness_Range_VAXINTENT_BINARY + df_RDD$Demographics_gend + 
#              df_RDD$Demographics_ethn1 + df_RDD$Demographics_ethn2 + df_RDD$Demographics_age + 
#              df_RDD$Emotion_proportion_HAP_PRO)
# 
# 
# stargazer(RDD_Complete, type = "text", 
#           omit = c("source", "ad_code", "FB_audience_target"))


# Code Snippet for Conference 2022
# Subset of questions for RDD 
# my_RDD_subset <- data.frame(as.numeric(df$True.False_Knowledge_Questions_Persistent.Symptoms), 
#                             as.numeric(df$True.False_Knowledge_Questions_Cold.Weather), 
#                             as.numeric(df$True.False_Knowledge_Questions_Antibodies))
# 
# lapply(my_RDD_subset, class)
# 
# View(my_RDD_subset)
# 
# df <- df %>%
#   mutate(Knowl_Subset_RDD = ifelse(rowSums(is.na(my_RDD_subset)) ==  3 , 1, 0))

# df<- df %>%
#   mutate(Knowl_subset_INST = rowMeans(my_RDD_subset))
# 
# 
# 
# 
# df5 <- t(data.frame(colSums(is.na(mysubset1))))
# barplot(df5, las = 2, main = "Subset1 - Histogram of Missing Values")
# 
# 
# df5 <- t(data.frame(colSums(is.na(mysubset2))))
# barplot(df5, las = 2, main = "Subset1 - Histogram of Missing Values")
# 
# 
# df5 <- t(data.frame(colSums(is.na(mysubset))))
# barplot(df5, las = 2, main = "Subset1 - Histogram of Missing Values")
# 
# 
# ggplot(mysubset1, aes(x=True.False_Knowledge_Questions_KNOW_AVG, y=binary_vax_intent)) +
#   geom_point(aes(col = as.factor(mysubset1$Knowl_Subset_RDD))) + 
#   # add logit curve
#   stat_smooth(method="glm", method.args=list(family="binomial"), se= TRUE)
# 
# 
# 
# 
# 
# mymodel4 <- glm(data = mysubset1, mysubset1$binary_vax_intent ~
#                   mysubset1$True.False_Knowledge_Questions_KNOW_AVG + 
#                   mysubset1$Knowl_Subset_RDD +
#                   mysubset1$True.False_Knowledge_Questions_KNOW_AVG*mysubset1$Knowl_Subset_RDD, 
#                 family = "binomial" )
# 
# summary(mymodel4)
# 
# class(mysubset1$Knowl_Subset_RDD)
# 
# library(stargazer)
# 
# # view regression discontinuity model
# stargazer(mymodel4,type="text",style="aer",
#           column.labels=c("Y~X+I(X>Cutoff)+X*I(X>Cutoff)"),
#           dep.var.labels="Regression Discontinuity",
#           covariate.labels = c("Knowledege Questions", "Randomized Subset", "Knowledge Questions * Randomize Subset"),
#           omit.stat=c("f","ser","rsq","n","adj.rsq"),
#           intercept.bottom=F)
#********************************************************************************


#*******************************************************************************
#*******************************************************************************
## Instrumental Variable Regression ----
#*******************************************************************************
#*******************************************************************************
# 
# library(AER)
# 
# 
# 
# 
# mymodel5 <- ivreg(data = mysubset1, 
#                   mysubset1$binary_vax_intent ~
#                     mysubset1$True.False_Knowledge_Questions_KNOW_AVG | mysubset1$Knowl_subset_INST,
#                   family = "binomial" )
# 
# 
# mymodel6 <-      ivreg(data = mysubset1, 
#                        mysubset1$binary_vax_intent ~
#                          mysubset1$True.False_Knowledge_Questions_KNOW_AVG + 
#                          mysubset1$source + 
#                          # mysubset1$ad_code + 
#                          mysubset1$FB_audience_target + 
#                          mysubset1$Demographics_age + 
#                          mysubset1$Demographics_ethn1 + 
#                          mysubset1$Emotion_proportion_HAP_PRO | mysubset1$Knowl_subset_INST,
#                        family = "binomial" )             
# 
# summary(mymodel5)
# summary(mymodel6)
# 
# 
# stargazer(mymodel5, mymodel6,type="html",style="aer",
#           column.labels=c("IV Reg", "IV Reg with Controls"),
#           omit=c("Constant"),
#           dep.var.labels=c("Vaccine Intent"),
#           model.names=F,omit.stat=c("ser","rsq","n","adj.rsq"),
#           intercept.bottom=F)
# 


#*******************************************************************************
#*******************************************************************************
# Conference 2022 ----
#*******************************************************************************
#*******************************************************************************

rm(list = ls())

library(tidyverse)
library(readxl)
library(ggstatsplot)
library(stargazer)

#*******************************************************************************
#*******************************************************************************
### Data ----
#*******************************************************************************
#*******************************************************************************


# 
DiD_time <- df
#View(data)
data <- read_excel("Data/__Dataset 1 Canada COVID-19 Game 2020.xlsx")
timestamps <- read_excel("Data/timestamps.xlsx")
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

merged <- left_join(timestamps, data, by = "session_id")
View(merged)

merged <- merged %>%
 # select(timestamp, everything()) %>%
  mutate(Date = as.Date(timestamp)) %>%
  mutate(post = ifelse(Date >= as.Date("2020-12-01"), 1, 0)) 

#*******************************************************************************
#*******************************************************************************
### Difference in Difference (Time) -----
#*******************************************************************************
#*******************************************************************************

df_timestamped <- read.csv("Data/df_timestamped.csv")
df_timestamped$X <- NULL

df_timestamped$timestamp <- as.Date(df_timestamped$timestamp)

df_timestamped$treatment <- ifelse(!is.na(df_timestamped$True.False_Knowledge_Questions_Bill.Gates.conspiracy) 
                                   # & 
                                   #  !is.na(df_timestamped$True.False_Knowledge_Questions_Antibodies) &
                                   #  !is.na(df_timestamped$True.False_Knowledge_Questions_Restricting.access)
                                   , 1, 0)

df_timestamped$post <- ifelse(df_timestamped$timestamp > as.Date("2020-11-08"), 1, 0)
df_timestamped_treatment <- df_timestamped %>%
                                filter(treatment == 1)

df_timestamped_control <- df_timestamped %>%
                                filter(treatment == 0)

sum_vax_treatment <- df_timestamped_treatment %>%
  group_by(timestamp) %>%
  dplyr::summarise(mean_vaxintent = mean(Willingness_Range_VAXINTENT, 
                                         na.rm = T), 
                   sum_vaxintent = sum(Willingness_Range_VAXINTENT, na.rm = T)) %>%
  mutate(treatment = 1)


sum_vax_control <- df_timestamped_control %>%
  group_by(timestamp) %>%
  dplyr::summarise(mean_vaxintent = mean(Willingness_Range_VAXINTENT, 
                                         na.rm = T), 
                   sum_vaxintent = sum(Willingness_Range_VAXINTENT, na.rm = T)) %>%
  mutate(treatment = 0)


merged <- rbind(sum_vax_treatment, sum_vax_control)
merged$treatment <- as.factor(merged$treatment)

write.csv(merged, "Data/timeseries.csv")

ggplot(data = merged , aes(x = timestamp, 
           y = mean_vaxintent, 
           color = treatment)) + 
  geom_line(lwd = 1.5)+
  labs(x = "Date", y = "Mean of Vaccine Intent", 
       color = "Treatment") +
  theme_minimal() + 
  scale_fill_viridis(discrete = T) + 
  theme(
    legend.position = c(0.5, 0.35),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12), 
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()) +
  theme_light(base_size = 18, base_family = "Poppins") + guides(fill = F) +
  geom_vline(xintercept=as.numeric(merged$timestamp[70]),
             linetype=4, colour="black")



merged$post <- ifelse(merged$timestamp > as.Date("2020-11-08"), 1, 0)

DiD_time_model <- glm(df_timestamped$Willingness_Range_VAXINTENT_BINARY ~ 
                        df_timestamped$post + 
                        df_timestamped$treatment + 
                        df_timestamped$post:df_timestamped$treatment,
                    family = binomial(), data = df_timestamped)

summary(DiD_time_model)

stargazer(DiD_time_model, 
          type="latex",
          #  style="aer",
          # column.labels=c("Vaccination Change in Intent"),
          dep.var.labels="Change in Vaccination Intent",
          covariate.labels = c("Post November 10th", "Treatment Records",
                               "Post November 10th:Treatment Records"),
          notes=c("True Coef on X = 2"))



###########################################################################################################################

# Calculations for the Conference
DiD_time <- merged %>%
  filter(!is.na(post)) 
# %>%
#   select(Date, post, source, ad_code, FB_audience_target,
#          Emotion_proportion_HAP_PRO, Emotion_proportion_ANG_PRO,
#          Emotion_proportion_ANX_PRO, Emotion_proportion_SKP_PRO,
#          True.False_Knowledge_Questions_KNOW_AVG,
#          Willingness_Range_SHARINTENT, Willingness_Range_VAXINTENT,
#          Demographics_age, Demographics_ethn1, Demographics_ethn2, Demographics_gend,
#          Sharing_Behaviour_Actual_Sharing)

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



#*******************************************************************************
#*******************************************************************************
### Difference in Difference Regression Analysis -----
#*******************************************************************************
#*******************************************************************************

DiD_time <- merged %>%
  filter(!is.na(merged$Willingness_Range_VAXINTENT))

DiD_time_reg <- DiD_time

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
              DiD_time_reg$Willingness_Range_VAXINTENT ~ treatment + post + treatment * post, family = "binomial")

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


#*******************************************************************************
#*******************************************************************************
### Difference in Difference (Imbens) ----
#*******************************************************************************
#*******************************************************************************

DiD_time_reg <- DiD_time_reg %>%
  mutate(treatment = -1 * treatment)


DiD_time_reg$outcome <- ifelse(DiD_time_reg$Willingness_Range_VAXINTENT > 3, 1, 0)
DiD_time_reg$treatment <- as.numeric(DiD_time_reg$True.False_Knowledge_Questions_KNOW_AVG)

model1 <- glm(data = DiD_time_reg, 
              outcome ~ treatment + 
                as.numeric(Demographics_age) + as.numeric(Demographics_gend) + as.numeric(Demographics_ethn1)  , family = "binomial")





stargazer(model1, type = "latex", 
          covariate.labels = c("Treatment Records",
                               "Type II Controls", "Treatment Records:Type II Controls", 
                               "constant"),
          dep.var.labels = "Vaccination Change in Intent", 
          dep.var.caption = "Difference in Difference Regression - Questions as Backdoor", 
          omit = c("Constant"), 
          column.sep.width = "30pt", 
          title = "Regression Analysis of Knowledge Questions Impact on Vaccination Intent")





















