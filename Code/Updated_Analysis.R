

rm(list = ls())

library(tidyverse)
library(readxl)
getwd()


can_vax_game <- read_excel("C:/Users/momaleki/Desktop/Research/UofSC Research/Data/__Dataset 1 Canada COVID-19 Game 2020.xlsx")

View(can_vax_game)



df5 <- t(data.frame(colSums(is.na(can_vax_game))))

barplot(df5, las = 2, main = "Canada Vax Game - Histogram of Missing Values")




colnames(can_vax_game) <- c("session_id", "source", "ad_code", 
                            "FB_audience_target", "Right_6", 
                            paste("Feelings", can_vax_game[1, 6:36], sep = "_"), 
                            paste("Policy_Support", can_vax_game[1, 41:47], sep = "_"), 
                            paste("Repeated_Question", can_vax_game[1, 48:78], sep = "_"), 
                            paste("True/False", can_vax_game[1, 79:109], sep = "_"),
                            "Know_Avg",
                            paste("Willingness_Range", can_vax_game[1, 111:112], sep = "_"),
                            paste("Demographics", can_vax_game[1, 113:116], sep = "_"), 
                            "Sharing_Behaviour")





can_vax_game <- can_vax_game[-c(1), ]

can_vax_game <- data.frame(can_vax_game)















































