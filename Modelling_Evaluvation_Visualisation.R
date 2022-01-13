library(utils)
library(e1071)
library(caTools)
library(stats)
library(graphics)

#Reading the Data
df <- as.data.frame(read.csv("https://raw.githubusercontent.com/Mahesh95802/Spam-Filter-using-Naive-Bayes/main/Processed_naivebayes.csv"))
#head(df)

split <- sample.split(df, SplitRatio = 0.8)
train_data <- subset(df, split == TRUE)
test_data <- subset(df, split == FALSE)
model <- naiveBayes(Label_Spam ~ .,train_data)
predictions <- predict(model, newdata = test_data)

#Confusion Matrix
cf <- table(factor(predictions), factor(test_data$Label_Spam))
print("Confusion Matrix: ")
print(cf)

#Evaluation Measures
tp <- cf["no","no"]
tn <- cf["yes","yes"]
fp <- cf["no","yes"]
fn <- cf["yes","no"]

#Precision, Recall, F1 Score, Accuracy
precision <- tp/(tp+fp)
recall <- tp/(tp+fn)
f1score <- (2*precision*recall)/(precision+recall)
accuracy <- (tp+tn)/(tp+fp+tn+fn)
print(paste("Precision : ",precision))
print(paste("Recall : ",recall))
print(paste("F1 Score : ",f1score))
print(paste("Accuracy: ",accuracy))

#Visualization
par(mfrow=c(2,2))

#Observed Values Barplot
o_spam <- sum((test_data$Label_Spam == "yes"))
o_not_spam <- length(test_data$Label_Spam) - o_spam
barplot(c(o_spam,o_not_spam),names.arg=c("Spam","Not Spam"),
        xlab="Observations",ylab="No. of Observations",main="Observed Spam/Not-Spam BarPlot")

#Predicted Values Barplot
p_spam <- sum((predictions == "yes"))
p_not_spam <- length(predictions) - p_spam
barplot(c(p_spam,p_not_spam),names.arg=c("Spam","Not Spam"),
        xlab="Predictions",ylab="No. of Predictions",main="Predicted Spam/Not-Spam BarPlot")

#Overall Accuracy Barplot
correct_predictions <- sum((test_data$Label_Spam == predictions))
wrong_predictions <- sum((test_data$Label_Spam != predictions))
barplot(c(correct_predictions,wrong_predictions),names.arg=c("Correct","Wrong"),
        xlab="Predictions",ylab="No. of Predictions",main="Accuracy BarPlot")

#Accuracy PieChart
accuracy_percentage <- round(100 * c(correct_predictions,wrong_predictions) / sum(c(correct_predictions,wrong_predictions)), 1)
pie(c(correct_predictions,wrong_predictions), 
    c(paste("Correct",accuracy_percentage[1]),paste("Wrong",accuracy_percentage[2])), 
    main = "Accuracy PieChart")
