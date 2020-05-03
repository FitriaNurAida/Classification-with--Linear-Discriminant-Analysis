# Importing Data #
df <- read.csv("E:/My Dictionary/Using R/Data/House_Price_2.csv")
View(df)
str(df)

############################## Preprocessing Data ############################################
summary(df)
boxplot(df$n_hot_rooms) #vaiable n_hot_rooms has outliers data
pairs(~Sold+rainfall, data = df) #there is an outlier
barplot(table(df$airport))
barplot(table(df$bus_ter)) #this variable is useless because it contains only 1 category

# Outlier Treatment #
summary(df$n_hot_rooms)
quantile(df$n_hot_rooms, 0.99) #P99
uv = 3*quantile(df$n_hot_rooms, 0.99) ; uv #3*P99
df$n_hot_rooms[df$n_hot_rooms > uv] <- uv #replace all values above 3*P99 with 3*P99, it is used for higher outlier
summary(df$n_hot_rooms)

summary(df$rainfall)
lv = 0.3*quantile(df$rainfall, 0.01) ; lv 
df$rainfall[df$rainfall<lv] <-lv ##replace all values above 0.3*P1 with 0.3*P1, it is used for lower outlier
summary(df$rainfall)

# Handling Missing Value #
mean(df$n_hos_beds)
mean(df$n_hos_beds,na.rm = TRUE) #without NA value
which(is.na(df$n_hos_beds)) #Mencari letak NA
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds,na.rm = TRUE) #Imputing mean
summary(df$n_hos_beds)
which(is.na(df$n_hos_beds))

# Variable Transformation #
df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4 #dimension reduction
View(df)
df2 <- df[,-6:-9] #because we have had new variable svg_dist then we remove variable dist
df <- df2
rm(df2)
df <-df[,-13] #because variabel 13 has 1 category so it is useless

# Dummy Variable #
install.packages("dummies")
library(dummies)
df <- dummy.data.frame(df)
View(df)
df <- df[,-8] #if the variable has 2 categories, there will be 1 dummy variable, because -> the number of dummy variable = the number of categories - 1
df <- df[,-13] #if the variable has 2 categories, there will be 1 dummy variable, because -> the number of dummy variable = the number of categories - 1

########################## Linear Discriminant Analysis  ###########################
install.packages("MASS")
library(MASS)
lda.fit = lda(Sold~., data = df)
lda.fit
lda.pred = predict(lda.fit, df)
View(lda.pred)
lda.pred$posterior # first column represents the probability of returning to 0, second column represents the probability of returning to 1
lda.pred$posterior[1:10,]
lda.pred$class #prediction of class responses
lda.pred$class[1:10]

cm <- table(lda.pred$class, df$Sold); cm #Confusion Matrix
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm); Accuracy #akurasi = 67,79%

## NOTE : LDA has an assumption that the continuous variables are normaly distributed

################################### SPLIT DATA ######################################
set.seed(0)
install.package("caTools")
library(caTools)
split <- sample.split(df,SplitRatio = 0.8)
train_set <- subset(df,split == TRUE)
test_set <- subset(df,split == FALSE)
dim(test_set)
trainX <- train_set[,-16] #without variable Sold
testX <- test_set[,-16] #without variable Sold
trainy <- train_set$Sold
testy <- test_set$Sold

################################### MODELING ########################################
trainlda.fit = lda(Sold~., data = train_set)
test.pred = predict(trainlda.fit, test_set)
View(test.pred)
test.class = test.pred$class

# Confusion Matrix #
cm <- table(test.class, test_set$Sold); cm #Confusion Matrix
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm); Accuracy #akurasi = 66.67%
