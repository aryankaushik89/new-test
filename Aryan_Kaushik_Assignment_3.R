# Assignment 3 | Aryan Kaushik | ID:500196172 | CMTH 642 | Dr. Ceni Babaoglu

wine_quality <- read.csv("winequality-white.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE) 

# QUESTION 1: Is there missing data?
sum(is.na(wine_quality)) #Output is 0, therefore there are no missing values. 


# QUESTION 2: What is the correlation beteen the attributes other than wine quality?
corr <- cor(subset(wine_quality, select = c(-quality)))

# To Plot and visualize the correlation.
install.packages('PerformanceAnalytics', dep=TRUE)
library(PerformanceAnalytics)
chart.Correlation(wine_quality[1:11])


# QUESTION 3: Graph the distribution of Wine Quality.
hist(wine_quality$quality, xlab = "Wine Quality", border="black", col="yellow",
ylim=c(0,2198), main = "Distribution of wine quality")

# QUESTION 4: Reduce the levels of rating for quality to three levels as high, medium and low.

wine_quality$quality = cut(wine_quality$quality,3,labels=c('Low','Medium','High'))
#The cut function is used to convert a numeric variable into a factor.
#The labels argument allows you to specify the levels of the factors.

# QUESTION 5: Normalize the Dataset.
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
wine_quality_n <- as.data.frame(lapply(wine_quality[1:11], normalize))

# QUESTION 6: Divide the Dataset into traing and tesing groups. (65% and 35% respectively)
wine_quality_train <- wine_quality_n[1:3184,]
wine_quality_test <- wine_quality_n[3185:4898,]

wine_quality_train_labels <- wine_quality[1:3184, 12]
wine_quality_test_labels <- wine_quality[3185:4898, 12]

# QUESTION 7: Use the KNN algorithm to predict the quality of wine using its attributes.
wine_quality_test_pred <- knn(train = wine_quality_train, test = wine_quality_test,
cl = wine_quality_train_labels, k=10)

CrossTable(x=wine_quality_test_labels, y=wine_quality_test_pred, prop.chisq=FALSE)

# QUESTION 8: Evalute the model performance.
#(We define accuracy as the percentage of cases correctly predicted. 
#In our case, our model correctly predicted low 212 times, medium 1038 times, and High only 1 time. 
# Thus, our accuracy is:(212+1038+1)/1714 = 0.7298 or 73%. 

knn_accuracy <- (212+1038+1)/1714
knn_accuracy
# [1] 0.7298716

#-------------------------------------END OF ASSIGNMENT 3------------------------------------#
