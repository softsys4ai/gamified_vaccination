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
library(readxl)

getwd()

theme_set(theme_light(base_size = 18, base_family = "Poppins"))
# font_add_google("Poppins", "Poppins")
# font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()

#*******************************************************************************
#*******************************************************************************
# Data ----
#*******************************************************************************
#*******************************************************************************

can_vax_game <- read_excel("Data/__Dataset 1 Canada COVID-19 Game 2020.xlsx")
# View(can_vax_game)

df <- can_vax_game


timestamps <- read_excel("Data/timestamps.xlsx")


#*******************************************************************************
#*******************************************************************************
## Data Preparation ----
#*******************************************************************************
#*******************************************************************************

df5 <- t(data.frame(colSums(is.na(df))))
barplot(df5, las = 2, main = "Canada Vax Game - Histogram of Missing Values")

colnames(df) <- c("session_id", "source", "ad_code", 
                            "FB_audience_target", "Right_6", 
                            paste("Emotion_proportion", df[1, 6:9], sep = "_"), 
                            paste("Policy_Support", df[1, 10:16], sep = "_"), 
                            paste("Repeated_Question", df[1, 17:40], sep = "_"), 
                            paste("True/False_Knowledge_Questions", df[1, 41:64], sep = "_"),
                            paste("Willingness_Range", df[1, 65:66], sep = "_"),
                            paste("Demographics", df[1, 67:70], sep = "_"), 
                            "Sharing_Behaviour_Actual_Sharing")

df <- df[-c(1), ]
df <- data.frame(df)





table(df$True.False_Knowledge_Questions_KNOW_AVG)
table(df$Willingness_Range_VAXINTENT)

for (i in 1:ncol(df)){print(paste0(i, " ---> ", colnames(df)[i]))}

# Making df smaller to better work with it (repeated questions removed)
df <- df[, c(1:16, 41:71)]
# View(df)

# Fixing data types issue

lapply(df, class)

df[6:47] <- lapply(df[6:47], as.numeric)

df$Willingness_Range_VAXINTENT_BINARY <- ifelse(df$Willingness_Range_VAXINTENT >= 3, 1, 0)
df$Willingness_Range_SHARINTENT_BINARY <- ifelse(df$Willingness_Range_SHARINTENT >= 3, 1, 0)
df$Policy_Support_POLSUP_AVG_BINARY <- ifelse(df$Policy_Support_POLSUP_AVG >= 3, 1, 0)



# View(df)

write.csv(df, "Data/df.csv")


# Time Stamped Data

# head(df)
# head(timestamps)
# colnames(timestamps)[1] <- "session_id"
# df_timestamped <- left_join(timestamps, df)
# write.csv(df_timestamped, "Data/df_timestamped.csv")

#*******************************************************************************
#*******************************************************************************
# Imputation ----
#*******************************************************************************
#*******************************************************************************

# Histogram of Missing Values
barplot(t(data.frame(colSums(is.na(df)))),
        las = 2, main = "Canada Vax Game - Histogram of Missing Values")

# Throwing away about 80K rows of data for which even knowledge questions are 
# full of NAs. 

df_no_NA_PROs <- df[rowSums(is.na(df[ , c(6:9)])) == 0 , ]
df_no_NA_PROs <- df_no_NA_PROs[, -c(1:5, 48:50)]
colnames(df_no_NA_PROs) <- c( "HAP_PRO" ,                                  
                              "ANG_PRO"  ,                                 
                              "EANX_PRO"  ,                                 
                              "SKP_PRO"    ,                               
                              "Group.Sizes" ,                                  
                              "Mandatory.masks" ,                              
                              "Negative.test"    ,                             
                              "Non.essential.businesses"  ,                    
                              "Recurring.shutdowns"        ,                   
                              "Social.distancing"           ,                  
                              "POLSUP_AVG"                   ,                 
                              "The5G.Conspiracy"               ,  
                              "Antibiotics"                 ,  
                              "Antibodies"                   , 
                              "Bill.Gates.conspiracy"       ,  
                              "Children.are.immune"          , 
                              "Children.may.spread"         ,  
                              "Cloth.Masks.Equal.Transmission",
                              "Cold.Weather"  ,                
                              "COVID.19.Negative.Test"  ,      
                              "COVID.19.vs.Flu"          ,     
                              "Herd.Immunity"           ,      
                              "Hydroxychloroquine"       ,     
                              "Masks.and.breathing"       ,    
                              "Masks.in.public"           ,    
                              "Mosques.in.Canada"          ,   
                              "NML.Conspiracy"              ,  
                              "Packages"                     , 
                              "Persistent.Symptoms"           ,
                              "Remdesivir"                    ,
                              "Restricting.access"            ,
                              "Russian.Vaccine"               ,
                              "Stress"                        ,
                              "Trudeau.mandatory.vaccines"    ,
                              "KNOW_AVG"                      ,
                              "SHARINTENT"                     ,            
                              "VAXINTENT"                       ,           
                              "age"                              ,               
                              "ethn1"                             ,              
                              "ethn2"                              ,             
                              "gend"                                ,            
                              "Actual_Sharing" )


barplot(t(data.frame(colSums(is.na(df_no_NA_PROs)))),
        las = 2, main = "Canada Vax Game - Histogram of Missing Values")

# md.pattern(df_no_NA_PROs)
# 
# aggr_plot <- aggr(df_no_NA_PROs, col=c('navyblue','red'), numbers=TRUE,
#                   sortVars=TRUE,
#                   labels=names(df_no_NA_PROs), cex.axis=.7, gap=3, 
#                   ylab=c("Histogram of missing data","Temp"))

methods(mice)

df_no_NA_PROs$age <- NULL
df_no_NA_PROs$ethn1 <- NULL
df_no_NA_PROs$ethn2 <- NULL
df_no_NA_PROs$gend <- NULL
df_no_NA_PROs$Actual_Sharing <- NULL
# 
# colnames(df_no_NA_PROs) <- c("HAP_PRO", "ANG_PRO", "EANX_PRO", "SKP_PRO", "Group_Size", "Mand_Masks", "Neg_Test", "Non_Ess_Bus_",      
#                              "Recur_Shutd_", "Soc_Dist_", "Pol_Sup_AVG", "The5G_Cons_", "Antibio_", "Antibod_", "B_Gates_Cons_",
#                              "Child_Immune", "Child_Spread", "Cloth_Masks_Trans_", "Cold_Weather", "C19_Neg_Test",        
#                              "C19vsFlu", "Herd_Immun_", "Hydrox_", "Masks&Breath_",           
#                               "Masks_in_Public", "Mosq_in_CAN", "NML_Conspiracy", "Packages",                      
#                                "Persistent_Symptoms", "Remdesivir", "Restricting_access", "Russian_Vaccine",               
#                                "Stress", "Trudeau_Man_Vax", "Know_Avg", "Share_Intent",                    
#                                "Vax_Intent")

tempData <- mice(df_no_NA_PROs,m=2,maxit=10,meth='pmm',seed=500)
summary(tempData)


completedData <- complete(tempData,1)
completedData$Recurring.shutdowns <- NULL

# View(completedData)

barplot(t(data.frame(colSums(is.na(completedData)))),
        las = 2, main = "Canada Vax Game - Histogram of Missing Values")



# densityplot(tempData)
stripplot(tempData, pch = 20, cex = 1.2)


write.csv(completedData, "Data/df_complete.csv")

