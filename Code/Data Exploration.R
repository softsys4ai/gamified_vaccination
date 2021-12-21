


rm(list = ls())

library(readxl)
library(tidyverse)
library(data.table)
library(corrplot)
library(mice)
library(VIM)

# Reading in the data ===================================================================================================================

column_info <- read_excel("C:/Users/momaleki/Desktop/Research/UofSC Research/Data/Column labels & details for Dataset 1 and later.xlsx")

game_contents_2to4 <- read_excel("C:/Users/momaleki/Desktop/Research/UofSC Research/Data/Game content for Datasets 2-4 [CAN_US Vaccine 2021].xlsx")





usa_vax_demo <- read_excel("C:/Users/momaleki/Desktop/Research/UofSC Research/Data/Data-20211017T163240Z-001/Data/Dataset 4/2021-07-28_Vax US demo.xlsx")

usa_vax_session <- read_excel("C:/Users/momaleki/Desktop/Research/UofSC Research/Data/Data-20211017T163240Z-001/Data/Dataset 4/2021-07-28_Vax US sessions.xlsx")

usa_vax_game <- read_excel("C:/Users/momaleki/Desktop/Research/UofSC Research/Data/Data-20211017T163240Z-001/Data/Dataset 2/Dataset 2 USA Vaccine Game Nov-Dec 2020.xlsx")




# Info about columns  ========================================================================================================================


View(column_info)
View(game_contents_2to4)


# USA =========================================================================================================================================
## US Data Exploration ========================================================================================================================

View(usa_vax_demo)
View(usa_vax_game)
View(usa_vax_session)



## US Data Wrangling ==========================================================================================================================


### USA Vax Demo ===============================================================================================================
colnames(usa_vax_demo)

colnames(usa_vax_demo) <- c("session_id", paste("consent", usa_vax_demo[2, 2:3], sep = "_"), 
                            paste("age", usa_vax_demo[2, 4:11], sep = "_"), 
                            paste("ethnicity_us", usa_vax_demo[2, 12:17], sep = "_"), 
                            paste("gender", usa_vax_demo[2, 18:22], sep = "_"))


usa_vax_demo <- usa_vax_demo[3:dim(usa_vax_demo)[1], ]


usa_vax_demo[ , 1:ncol(usa_vax_demo)] <- lapply(usa_vax_demo[ , 1:ncol(usa_vax_demo)], as.numeric)



head(usa_vax_demo)

df <- t(data.frame(colSums(is.na(usa_vax_demo))))



barplot(df, las = 2)



### USA Vax Session ================================================================================


colnames(usa_vax_session)


colnames(usa_vax_session) <- c("session_id", paste("Feelings", usa_vax_session[2, 2:16], sep = "_"), 
                            paste("Policy_Support", usa_vax_session[2, 17:22], sep = "_"), 
                            paste("Political_Affiliation", usa_vax_session[2, 23:38], sep = "_"), 
                            paste("True/False", usa_vax_session[2, 39:50], sep = "_"))




View(usa_vax_session)


usa_vax_session <- usa_vax_session[-c(1:2), ]

usa_vax_session


usa_vax_session[ , 1:ncol(usa_vax_session)] <- lapply(usa_vax_session[ , c(17:23)], as.numeric)

head(usa_vax_session)


df1 <- t(data.frame(colSums(is.na(usa_vax_session))))

barplot(df1, las = 2)





### Merging Demo with Vax Session  =========================================================================================


session_demo <- merge(usa_vax_demo, usa_vax_session, by = "session_id")

View(session_demo)



# write.csv(session_demo, "mydf.csv")


# session_demo[ ,  c(1:22, 38:44)] <- lapply(usa_vax_game[ , c(1:22, 38:44)], as.numeric)



# lapply(session_demo[,c(4:11)], class)


hist(as.numeric(session_demo[, c(4)]))

colnames(session_demo)





model1 <- lm(as.numeric(session_demo$"Political_Affiliation_Political affiliation") ~ 
               
             as.numeric(session_demo$"ethnicity_us_White/European") + as.numeric(session_demo$"age_25-34") + as.numeric(session_demo$"age_18-24") +
             as.numeric(session_demo$"age_45-54") + as.numeric(session_demo$"age_35-44")  + as.numeric(session_demo$"age_65-74") +
              as.numeric(session_demo$"age_75-84") + as.numeric(session_demo$"age_85+") +
               
               as.numeric(session_demo$"gender_Male"))





summary(model1)

mytest <- lapply(session_demo[, c(4:22 , 38:44)], as.numeric)

mytest <- data.frame(mytest)

colnames(mytest) <- c("18_24", "25_34", "35_44", "45_54", "55_64", "65_74", "75_84" ,
                      "85+", "Ind", "black", "his", "NR", "mixed", "white", "gabst", "fem", 
                      "NR2", "male", "two", 
                      "gov", "vacc", "mix", "buis", "school", "pass", "affil")

mytest

M <- cor(mytest, use="pairwise.complete.obs")
corrplot::corrplot(M, order = "hclust")


### USA Vaccine Game ============================================================

#### Data Wrangling ========================================================================


colnames(usa_vax_game)
dim(usa_vax_demo)
dim(usa_vax_game)
dim(usa_vax_session)

usa_vax_game1 <- usa_vax_game
colnames(usa_vax_game1) <- c("session_id", "version", "ENCODE", paste("Vaccine_attitudes", usa_vax_game1[2, 4:11], sep = "_"), 
                             paste("Feelings", usa_vax_game1[2, 12:28], sep = "_"), 
                             paste("Emotional_proportions", usa_vax_game1[2, 29:32], sep = "_"), 
                             paste("Repeated_Q", usa_vax_game1[2, 33:50], sep = "_"))


usa_vax_game1 <- usa_vax_game1[-c(1:3), ]

usa_vax_game1[ , c(4:11, 29:50)] <- lapply(usa_vax_game1[ , c(4:11, 29:50)], as.numeric)

View(usa_vax_game1)
head(usa_vax_game1)

# Counting numebr of NAs
df2 <- t(data.frame(colSums(is.na(usa_vax_game1))))

barplot(df2, las = 2)
summary(usa_vax_game1)

# Number of NAs for Numerical values

df3 <- t(data.frame(colSums(is.na(usa_vax_game1[ , c(4:11, 29:50)]))))

barplot(df3, las = 2, main = "NA values for Numerical Values")

# Complete Cases in Rows
temp <- usa_vax_game1[rowSums(is.na(usa_vax_game1[ , c(4:11, 29:32)])) < 10 , ]
M <- cor(temp[, c(4:11, 29:32)])
corrplot(M, method = "square" ,tl.pos = "n")

lapply(usa_vax_game1[ , c(4:11, 29:50)], class)

# Correlation 
M <- cor(usa_vax_game1[ , c(4:11)])

corrplot(M, method = "cluster", tl.pos = "n")



View(temp[, c(4:11, 29:33)])

temp <- temp[, c(4:11, 29:32)]
summary(temp)
View(temp)

df5 <- t(data.frame(colSums(is.na(temp))))

barplot(df5, las = 2, main = "NA values for Selected Numerical Values in USA Vaccine Game")

#### Inputation ==========================================================

library(cluster)

test <- temp

for (i in 1:ncol(test)){
  
          
          k <- which(is.na(test[ , i]), arr.ind = TRUE) 
          test[k[, 1], i] <- rowMeans(test, na.rm = TRUE)[k[, 1]]

}


View(test)


#### Cor Plot =================================================================

M <- cor(test)
corrplot(M, method = "square", tl.pos = "n")




myclusters <- kmeans(test, centers = 5)
summary(myclusters)

clustplot




#### Regression Analysis ========================================================

colnames(test)[9] <- "Emotional Proportions_HAP_PRO"
colnames(test)[10] <- "Emotional Proportions_ANG_PRO"
colnames(test)[11] <- "Emotional Proportions_ANX_PRO"
colnames(test)[12] <- "Emotional Proportions_SKP_PRO"

test_HAP <- test[ ,c(1:9)]


model_HAP <- lm(data = test_HAP, test_HAP$`Emotional Proportions_HAP_PRO` ~ .)

summary(model_HAP)





#### Clustering ==================================================================





library(factoextra)

fviz_nbclust(scale(test), kmeans, nstart=100, method = "wss") + 
  geom_vline(xintercept = 5, linetype = 1)



clust <- kmeans(scale(test), centers = 5, nstart = 100)

summary(clust)
kmeans_table <- data.frame(clust$size, clust$centers)
kmeans_df <- data.frame(Cluster = clust$cluster, test)

head(kmeans_df[10:12])


fviz_cluster(clust, data = scale(test), geom = c("point"),ellipse.type = "euclid")


clust



# What does the shape of the Oval mean in the output of fviz_cluster?
# Check for NA values in only the numerical columns. 70%




#### test ===========




clust1 <- kmeans(scale(test), centers = 3, nstart = 100)

summary(clust1)
kmeans_table1 <- data.frame(clust1$size, clust1$centers)
kmeans_df1 <- data.frame(Cluster = clust1$cluster, test)

head(kmeans_df1[10:12])


fviz_cluster(clust1, data = scale(test), geom = c("point"),ellipse.type = "euclid")

clust1










# Canada =====================================================================================================================




can_vax_game  <- read_excel("C:/Users/momaleki/Desktop/Research/UofSC Research/Data/Data-20211017T163240Z-001/Data/Dataset 1/Dataset 1 Canada COVID-19 Game 2020.xlsx")

can_vax_demo <- read_excel("C:/Users/momaleki/Desktop/Research/UofSC Research/Data/Data-20211017T163240Z-001/Data/Dataset 3/2021-05-27_Vax CAN demo.xlsx")

can_vax_session <- read_excel("C:/Users/momaleki/Desktop/Research/UofSC Research/Data/Data-20211017T163240Z-001/Data/Dataset 3/2021-05-27_Vax CAN session.xlsx")


dim(can_vax_demo)

## CA Data Exploration ========================================================================================================================

View(can_vax_demo)
View(can_vax_game)
View(can_vax_session)

View(can_vax_demo)
View(usa_vax_demo)


### Canada Vax Demo ===============================================================================================================

# It needs some melting of the table and additional analysis. 



### CAN Vax Session ================================================================================

#### Data Wrangling ===================================================================
colnames(can_vax_session)


colnames(can_vax_session) <- c("session_id", paste("True/False", can_vax_session[2, 2:16], sep = "_"), 
                               paste("Repeat_True/False", can_vax_session[2, 17:31], sep = "_"), 
                               paste("Feelings", can_vax_session[2, 32:46], sep = "_"), 
                               paste("Policy_Support", can_vax_session[2, 47:52], sep = "_"),
                               paste("Political_Preference", can_vax_session[2, 53], sep = "_"),
                               paste("Willingness_Range", can_vax_session[2, 54:55], sep = "_"))




View(can_vax_session)


can_vax_session <- can_vax_session[-c(1:2), ]

can_vax_session




head(can_vax_session)

# Number of NAs in the data
df1 <- t(data.frame(colSums(is.na(can_vax_session))))

barplot(df1, las = 2, 
        main = "Canada Vaccination Session - Missing Values per Column")


dim(can_vax_session)



# Changing the True False values

for (i in 1:nrow(can_vax_session)){
  for (j in 1:ncol(can_vax_session)){
    
    if (is.na(can_vax_session[i, j])){
      
      print(NA)
      
    } else if (can_vax_session[i,j] == "correct"){

      

      can_vax_session[i, j] <- "1"

    } else if (can_vax_session[i,j] == "incorrect"){
      
      

      can_vax_session[i, j] <- "0"

    } else if (can_vax_session[i,j] == "abstain"){
      
      
      
      can_vax_session[i, j] <- NA
      
    }
  
    
  }
}


# Subsetting the dataset into the ones that have fewer NAs


can_session_less_NAs <- can_vax_session[, c(1:16, 53:55)]



View(can_session_less_NAs)


# Number of NAs in the data
df1 <- t(data.frame(colSums(is.na(can_session_less_NAs))))

barplot(df1, las = 2, 
        main = "Subset of Canada Vaccination Session - Missing Values per Column")


dim(can_session_less_NAs)



temp <- can_session_less_NAs[rowSums(is.na(can_session_less_NAs[ , c(1:ncol(can_session_less_NAs))])) < 10 , ]
View(temp)


for (i in 1:ncol(temp)){
  
  print(colnames(temp)[i])
}


# Number of NAs in the data
df1 <- t(data.frame(colSums(is.na(temp))))

barplot(df1, las = 2, 
        main = "Temp Canada Vaccination Session - Missing Values per Column")



#### Cor Plot =================================================================






temp[, 1:ncol(temp)] <- lapply(temp[ , 1:ncol(temp)], as.numeric)

temp$session_id <- NULL

# Calculating the difference column 

temp$Willingness_Diff <- temp$`Willingness_Range_Vaccine intention after` - temp$`Willingness_Range_Vaccine intention before`


M <- cor(temp, use="pairwise.complete.obs")
corrplot(M, method = "square", tl.pos = "n")





#### Imputation ================================================================


library(mice)

md.pattern(temp)


library(VIM)

library(VIM)
aggr_plot <- aggr(temp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                   labels=names(temp), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Temp"))



methods(mice)


test <- temp

test <- data.frame(test)

colnames(test) <- paste0(1:ncol(test))

tempData <- mice(temp,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)


completedData <- complete(tempData,1)

View(completedData)

colnames(completedData) <- colnames(temp)

temp <- completedData

densityplot(tempData)

stripplot(tempData, pch = 20, cex = 1.2)

#### Clustering =================================================================


test <- temp

library(factoextra)

fviz_nbclust(scale(test), kmeans, nstart=100, method = "wss") + 
  geom_vline(xintercept = 5, linetype = 1)



clust <- kmeans(scale(test), centers = 5, nstart = 100)

summary(clust)
kmeans_table <- data.frame(clust$size, clust$centers)
kmeans_df <- data.frame(Cluster = clust$cluster, test)

head(kmeans_df[10:12])


fviz_cluster(clust, data = scale(test), geom = c("point"),ellipse.type = "euclid")


clust


#### Regression Analysis ======================================================

model1 <- lm(data = temp, temp$`True/False_Vaccine Refusal` ~ .)

summary(model1)

temp$`True/False_Bill Gates conspiracy` <- NULL
temp$`True/False_Side Effects` <- NULL
temp$`Willingness_Range_Vaccine intention after` <- NULL
temp$`Willingness_Range_Vaccine intention before` <- NULL

model2 <- lm(data = temp, temp$Willingness_Diff ~ .)
summary(model2)







### CAN Vaccine Game ============================================================

df5 <- t(data.frame(colSums(is.na(can_vax_game))))

barplot(df5, las = 2, main = "Canada Vax Game - Histogram of Missing Values")






#### Data Wrangling ========================================================================



can_vax_game

colnames(can_vax_game)

View(can_vax_game)



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




table(can_vax_game$source)


ggplot(data = can_vax_game) + geom_histogram(aes(x = can_vax_game$source, 
                                                 fill = can_vax_game$source), 
                                             stat = "count")



ggplot(data = can_vax_game) + geom_histogram(aes(x = FB_audience_target, 
                                                 fill = FB_audience_target), 
                                             stat = "count")



ggplot(data = can_vax_game) + geom_histogram(aes(x = ad_code), 
                                             stat = "count")





# Subsetting the dataset into the ones that have fewer NAs


can_game_less_NAs <- can_vax_game[, c(75:117)]



View(can_game_less_NAs)


# Number of NAs in the data
df1 <- t(data.frame(colSums(is.na(can_game_less_NAs))))

barplot(df1, las = 2, 
        main = "Subset of Canada Vaccination Session - Missing Values per Column")


dim(can_game_less_NAs)



temp <- can_game_less_NAs[rowSums(is.na(can_game_less_NAs[ , c(1:ncol(can_game_less_NAs))])) < 20 , ]
View(temp)

df1 <- t(data.frame(colSums(is.na(temp))))

barplot(df1, las = 2, 
        main = "Subset of Canada Vaccination Session - Missing Values per Column")

temp[, 1:ncol(temp)] <- lapply(temp[ , 1:ncol(temp)], as.numeric)


temp <- temp[,colSums(is.na(temp)) < 30000]











View(temp)


df1 <- t(data.frame(colSums(is.na(temp))))

barplot(df1, las = 2, 
        main = "Subset of Canada Vaccination Session - Missing Values per Column")


dim(temp)




for (i in 1:ncol(temp)){
  
  print(colnames(temp)[i])
}


#### CorPlot ===================================================================





M <- cor(temp, use="pairwise.complete.obs")
corrplot(M, method = "square", tl.pos = "n")


#### Imputation ================================================================


library(mice)

md.pattern(temp)


library(VIM)

library(VIM)
aggr_plot <- aggr(temp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(temp), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Temp"))



methods(mice)





data <- temp


smp_size <- floor(0.80 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]

df1 <- t(data.frame(colSums(is.na(train))))

barplot(df1, las = 2, 
        main = "Subset of Canada Vaccination Session - Missing Values per Column")



test <- data[-train_ind, ]

df1 <- t(data.frame(colSums(is.na(test))))

barplot(df1, las = 2, 
        main = "Subset of Canada Vaccination Session - Missing Values per Column")



n <- 5000
fab_test <- apply (test, 2, function(x) {x[sample( c(1:n), floor(n/10))] <- NA; x} )

df1 <- t(data.frame(colSums(is.na(fab_test))))

barplot(df1, las = 2, 
        main = "Subset of Canada Vaccination Session - Missing Values per Column")


#### Random Forest ==========================================================
#import the package
library(randomForest)
# Perform training:
rf_classifier = randomForest(Species ~ ., data=training, ntree=100, mtry=2, importance=TRUE)

# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier,validation1[,-5])
table(observed=validation1[,5],predicted=prediction_for_table)

#### Logistic Regression  ==========================================================
#import the package

# Perform training:
rf_classifier = glm(Species ~ ., data=training, ntree=100, mtry=2, importance=TRUE)

# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier,validation1[,-5])
table(observed=validation1[,5],predicted=prediction_for_table)


#### Imputation using MICE Package =======================================

# pmm 
tempData <- mice(temp,m=5,maxit=10,meth='pmm',seed=500)
summary(tempData)


completedData <- complete(tempData,1)

View(completedData)

densityplot(tempData)

stripplot(tempData, pch = 20, cex = 1.2)



# Imputation trying with various method. 

# Out of different Imputation methods which one dominates, and comparing results. 


#### Imputation using Machine Learning Models =======================================


































































