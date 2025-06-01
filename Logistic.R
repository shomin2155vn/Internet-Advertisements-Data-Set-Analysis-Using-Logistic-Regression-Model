install.packages("ROCR")  # Only run once
library(ROCR)
#Load and store the loaded data into a variable
ads <- read.csv("add.csv")
ads <- ads[, -1]
#Inspect the data set in a tabular format
View(ads)
colnames(ads) [1] <- "Height"
colnames(ads) [2] <- "Width"
colnames(ads) [3] <- "aspect_ratio"
colnames(ads) [4:1558] <- paste0("url_term_", 3:1557)
colnames(ads) [1559] <- "Type"
ads <- as.data.frame(lapply(ads, function(x) {
  x[grepl("^\\s*\\?$", x)] <- NA
  return(x)
}))

na_per_column <- colSums(is.na(ads))
print(na_per_column[na_per_column > 0])

# Delete the row has NA value in the 4th columns
ads <- ads[!is.na(ads[[4]]), ]

# Convert the first two columns to numeric, handling any non-numeric values
ads[[1]] <- as.numeric(as.character(ads[[1]]))
ads[[2]] <- as.numeric(as.character(ads[[2]]))
# Calculate the mean of each column, ignoring NA values
mean_col_1 <- mean(ads[[1]], na.rm = TRUE)
mean_col_2 <- mean(ads[[2]], na.rm = TRUE)
# Replace NA values in the first column with its mean
ads[[1]][is.na(ads[[1]])] <- mean_col_1
# Replace NA values in the second column with its mean
ads[[2]][is.na(ads[[2]])] <- mean_col_2
ads$`aspect_ratio` <- ads$Width / ads$Height
ads[, 1] <- as.numeric(ads[, 1])
ads[, 2] <- as.numeric(ads[, 2])
ads[, 3] <- as.numeric(ads[, 3])
ads[, 4] <- as.numeric(ads[, 4])
na_counts <- sapply(ads, function(x) sum(is.na(x))) 
print(na_counts)
print(na_counts[na_counts > 0])
ads$Type <- ifelse(ads$Type == "ad.", 1, 0)

hist(ads[, 1], main = "Histogram of Height", xlab = "Height", col = "lightblue", border = "black", ylim = c(0, 2000))


hist(ads[, 2], main = "Histogram of Width", xlab = "Width", col = "lightblue", border = "black", ylim = c(0, 2000))


hist(ads[, 3], main = "Histogram of Aspect Ratio", xlab = "Aspect Ratio", col = "lightblue", border = "black", ylim = c(0, 3000))


hist(ads[, 4], main = "Histogram of url_term_3", xlab = "url_term_3", col = "lightblue", border = "black", ylim = c(0, 3000))


hist(ads[, 5], main = "Histogram of url_term_4", xlab = "url_term_4", col = "lightblue", border = "black", ylim = c(0, 4000))


hist(ads[, 6], main = "Histogram of url_term_5", xlab = "url_term_5", col = "lightblue", border = "black", ylim = c(0, 4000))


hist(ads[, 7], main = "Histogram of url_term_6", xlab = "url_term_6", col = "lightblue", border = "black", ylim = c(0, 4000))


hist(ads[, 8], main = "Histogram of url_term_7", xlab = "url_term_7", col = "lightblue", border = "black", ylim = c(0, 4000))


hist(ads[, 9], main = "Histogram of url_term_8", xlab = "url_term_8", col = "lightblue", border = "black", ylim = c(0, 4000))


hist(ads[, 1559], main = "Histogram of Type", xlab = "Type", col = "lightblue", border = "black", ylim = c(0,4000))

boxplot(ads[, 1] ~ ads$Type, main = "Boxplot of Height", xlab = "Type", ylab = "Height", col = c("lightblue", "lightgreen"))
boxplot(ads[, 2] ~ ads$Type, main = "Boxplot of Width", xlab = "Type", ylab = "Width", col = c("lightblue", "lightgreen"))
boxplot(ads[, 3] ~ ads$Type, main = "Boxplot of Aspect Ratio", xlab = "Type", ylab = "Aspect Ratio", col = c("lightblue", "lightgreen"))
boxplot(ads[, 4] ~ ads$Type, main = "Boxplot of url_term_3", xlab = "Type", ylab = "url_term_3", col = c("lightblue", "lightgreen"))
boxplot(ads[, 5] ~ ads$Type, main = "Boxplot of url_term_4", xlab = "Type", ylab = "url_term_4", col = c("lightblue", "lightgreen"))
boxplot(ads[, 6] ~ ads$Type, main = "Boxplot of url_term_5", xlab = "Type", ylab = "url_term_5", col = c("lightblue", "lightgreen"))
boxplot(ads[, 7] ~ ads$Type, main = "Boxplot of url_term_6", xlab = "Type", ylab = "url_term_6", col = c("lightblue", "lightgreen"))
boxplot(ads[, 8] ~ ads$Type, main = "Boxplot of url_term_7", xlab = "Type", ylab = "url_term_7", col = c("lightblue", "lightgreen"))
boxplot(ads[, 9] ~ ads$Type, main = "Boxplot of url_term_8", xlab = "Type", ylab = "url_term_8", col = c("lightblue", "lightgreen"))
boxplot(ads[, 10] ~ ads$Type, main = "Boxplot of url_term_9", xlab = "Type", ylab = "url_term_9", col = c("lightblue", "lightgreen"))
library(corrplot)
ads_cor_matrix = cor(ads[,1:10])
round(ads_cor_matrix,3)
corrplot(cor(ads_cor_matrix),method = 'color')

#shuffing rows of the dataset
ads <- ads[sample(nrow(ads)), ]
rownames(ads) <- NULL
#set seed for reproducibility
set.seed(42)
#Create training and testing sets
train_index <- 1:floor(0.7*nrow(ads))
train_ads <- ads[train_index,]
test_ads <- ads[-train_index,]
# Remove columns with near-zero variance

library(caret)
nzv <- nearZeroVar(train_ads, saveMetrics = TRUE)
train_ads <- train_ads[, !nzv$nzv]
train_ads$url_term_64 <- NULL
train_ads$url_term_455 <- NULL
full_model <- glm(Type ~ ., data = train_ads, family = "binomial")
# Apply stepwise selection
step_model <- step(full_model, direction = "both")
# View the summary of the reduced model
summary(step_model)
#Predict on test_data
test_pred <- predict(step_model, newdata = test_ads, type = "response")
test_class <- ifelse(test_pred >= 0.5, 1, 0)
confusionMatrix(factor(test_class, levels = c("1", "0")), factor(test_ads$Type, levels = c("1", "0")))
#ROC curver
#Transfer test_pred into vectors
pred_vector <- as.vector(test_pred)
#Create prediction object from prediction vector and experimental vector
labels <- factor(test_ads$Type, levels = c("1", "0")) 
ROCpred <- prediction(pred_vector, labels)
ROCperf <- performance(ROCpred, "tpr", "fpr")
#plot ROC curve
plot(ROCperf, col = "blue", main = "ROC Curve of Logistic Regression",
    xlab = "False Positive Rate (1- Specificity)",
    ylab = "True Positive Rate (Sensitivity)")
abline(a = 0, b =1, col = "red", lty =2)
#Calculate AUC
as.numeric(performance(ROCpred,"auc")@y.values)