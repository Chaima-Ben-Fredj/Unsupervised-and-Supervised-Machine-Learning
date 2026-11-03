library(ggplot2)
library(dplyr)
library(caret)
library(DataExplorer)
library(tidyverse)
library(skimr)
library(patchwork)
library(plotly)
library(RANN)
library(corrplot)
library(pheatmap)
library(e1071)
library(randomForest)
library(gbm) 
library(rpart.plot)
library(pROC)



                 #---------------------EDA--------------------#

ds <- read.csv("C:/Users/PCS/Desktop/DSE/second trimester/stat learning/Supervised learning/heart.csv")
head(ds)
str(ds)

colnames(ds)
summary(ds)
colSums(is.na(ds))


#reports summary for our dataset
#skim(ds)

#we will keep the following palette here for future references in our code
palette_colors <- c("deepskyblue3", "deepskyblue4", "dodgerblue3","skyblue","dodgerblue4",
                    "cyan4","navyblue","cadetblue4","lightskyblue4","lightblue3",
                    "gray39","lightcyan4","powderblue","turquoise","steelblue4",
                    "tan","salmon","rosybrown1","thistle1")

#------- Univariate Analysis-------------

#let's first check the distribution of our numerical variables
ds %>%
  select(Age, RestingBP, Cholesterol,FastingBS,MaxHR,Oldpeak,HeartDisease) %>%  
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~key, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Numerical Variables")



# plotting heart diseas percentages
percent_data <- ds %>%
  group_by(HeartDisease) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

ggplot(percent_data, aes(x = factor(HeartDisease), y = Count, fill = factor(HeartDisease))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  ggtitle("Distribution of Heart Disease") +
  xlab("Heart Disease (0 = No, 1 = Yes)") +
  ylab("Count") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "deepskyblue4"),guide = "none") +  
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 12)    
  )
  



palette_colors <- c("deepskyblue3", "deepskyblue4", "dodgerblue3","skyblue","dodgerblue4",
                    "cyan4","navyblue","cadetblue4","lightskyblue4","lightblue3",
                    "gray39","lightcyan4","powderblue","turquoise","steelblue4",
                    "tan","salmon","rosybrown1","thistle1")



# plotting of sex column data

percentsex <- ds %>%
  group_by(Sex) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

ggplot(percentsex, aes(x = factor(Sex), y = Count, fill = factor(Sex))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  ggtitle("Distribution of Sex") +
  xlab("Sex (F = Female, M = Male)") +
  ylab("Count") +
  theme_minimal() +
  scale_fill_manual(values = c("F" = "skyblue", "M" = "deepskyblue4"),guide = "none") +  
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 12)    
  )


# detecting outliersusing boxplots
ds_long <- ds %>%
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value")

ggplot(ds_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = palette_colors[13], color="black") +
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Boxplots for Numerical Variables") +
  xlab("Variable") +
  ylab("Value") +
  theme_minimal()


#Understanding if there are real outliers in the RestingBP column
RestingBP_zero_rows <- ds %>% filter(RestingBP == 0)
num_RestingBP_zero <- nrow(RestingBP_zero_rows)
cat("Number of rows where RestingBP = 0:", num_RestingBP_zero, "\n")
print(RestingBP_zero_rows)

#there's one row with RestingBP=0 which is an outlier because the RestingBP cannot be 0 so we have to remove this row
ds_clean<- ds %>% filter(RestingBP != 0)
head(ds_clean)

#now let's handle the cholesterol values if they are outliers or data errors because medically cholesterol cannot be 0
Cholesterol_zero_rows <- ds %>% filter(Cholesterol == 0)
num_Cholesterol_zero <- nrow(Cholesterol_zero_rows)
cat("Number of rows where Cholesterol is 0:", num_Cholesterol_zero, "\n")
head(Cholesterol_zero_rows)
#let's see how many columns among those with cholesterol 0 actually have HeartDisease
Cholesterol_zero_withHD <- ds %>% filter(Cholesterol == 0 & HeartDisease==1)
num_Cholesterol_withHD <- nrow(Cholesterol_zero_withHD)
cat("Number of rows where Cholesterol is 0 with a Heart Disease :", num_Cholesterol_withHD, "\n")
head(Cholesterol_zero_withHD)
#we remark that we have many cholesterol values and most of them have heart disease, 
#so we might need to treat these values so that our prediction models work properly 
#because medically usually high levels of cholesterol are associated with heart diseases 
#so it's not possible to have 0 levels of cholesterol

#we will use knn to assign values from the nearest neghbors to cholesterol with 0 values 
#first let's start by assing the value NA for the cholesterol values of 0
ds_clean$Cholesterol[ds_clean$Cholesterol == 0] <- NA
head(ds_clean)

#let's first convert the categorical variables to numerical variables
ds_numeric <- ds_clean  
ds_numeric <- ds_numeric %>%
  mutate(
    Sex = recode(Sex, "M" = 1, "F" = 0),
    ExerciseAngina = recode(ExerciseAngina, "Y" = 1, "N" = 0),
    ChestPainType = as.numeric(as.factor(ChestPainType)),
    RestingECG = as.numeric(as.factor(RestingECG)),
    ST_Slope = as.numeric(as.factor(ST_Slope))
  )

# let's  Check the rows where Cholesterol is NA
Cholest <- ds_numeric %>% filter(is.na(Cholesterol))
head(Cholest)
binary_cols <- ds_numeric %>% select(FastingBS, ExerciseAngina, HeartDisease)
non_binary_cols <- ds_numeric %>% select(-FastingBS, -ExerciseAngina, -HeartDisease)

# we will apply KNN imputation only to non-binary columns
preprocess_model <- preProcess(non_binary_cols, method = "knnImpute", k = 5)
ds_imputed_non_binary <- predict(preprocess_model, non_binary_cols)

# Combine the imputed non-binary columns with the binary columns
ds_imputed <- bind_cols(ds_imputed_non_binary, binary_cols)
head(ds_imputed)

# Rescaling non_binary columns back to original values
mean_values <- preprocess_model$mean
std_values <- preprocess_model$std
ds_rescaled_non_binary <- ds_imputed_non_binary %>%
  mutate(across(all_of(names(mean_values)), ~ . * std_values[cur_column()] + mean_values[cur_column()]))
ds_rescaled <- bind_cols(ds_rescaled_non_binary, binary_cols)
head(ds_rescaled)

#getting the categorical variables back in order to perform the scatter plot again
chest_pain_mapping <- levels(as.factor(ds_clean$ChestPainType))  
resting_ecg_mapping <- levels(as.factor(ds_clean$RestingECG))   
st_slope_mapping <- levels(as.factor(ds_clean$ST_Slope))        

# let's Convert the numerical values back to categorical
ds_rescaled <- ds_rescaled %>%
  mutate(
    ChestPainType = factor(ChestPainType, levels = 1:length(chest_pain_mapping), labels = chest_pain_mapping),
    RestingECG = factor(RestingECG, levels = 1:length(resting_ecg_mapping), labels = resting_ecg_mapping),
    ST_Slope = factor(ST_Slope, levels = 1:length(st_slope_mapping), labels = st_slope_mapping)
  )

head(ds_rescaled)

unique(ds_rescaled$ChestPainType)
unique(ds_rescaled$RestingECG)
unique(ds_rescaled$ST_Slope)

# Convert binary columns (age and exercise angine) back to original format
ds_rescaled <- ds_rescaled %>%
  mutate(
    Sex = recode(Sex, `1` = "M", `0` = "F"),
    ExerciseAngina = recode(ExerciseAngina, `1` = "Y", `0` = "N")
  )
head(ds_rescaled)

# Creating new boxplots after data cleaning
ds_rescaled_long <- ds_rescaled %>%
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value")

ggplot(ds_rescaled_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = palette_colors[13], color="black") +
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Boxplots for Numerical Variables (After Handling Outliers)") +
  xlab("Variable") +
  ylab("Value") +
  theme_minimal()
#------------ End of Univariate Analysis



#--------- Bivariate Analysis

#------ heart disease vs resting ecg
ggplot(ds_rescaled, aes(x = RestingECG, fill = factor(HeartDisease))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "powderblue", "1" = "deepskyblue4")) + 
  ggtitle("Heart Disease Proportion by Resting ECG") +
  theme_minimal()


#------ heart disease vs chest pain type
ggplot(ds_rescaled, aes(x = ChestPainType, fill = factor(HeartDisease))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "powderblue", "1" = "deepskyblue4")) +  
  ggtitle("Heart Disease Proportion by Chest Pain Type") +
  theme_minimal()

#------ heart disease vs sex
ggplot(ds_rescaled, aes(x = Sex, fill = factor(HeartDisease))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "powderblue", "1" = "deepskyblue4")) +  
  ggtitle("Heart Disease Proportion by Sex") +
  theme_minimal()


#------ heart disease vs exercise angina
ggplot(ds_rescaled, aes(x = ExerciseAngina, fill = factor(HeartDisease))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "powderblue", "1" = "deepskyblue4")) +  
  ggtitle("Heart Disease Proportion by Exercise Angina") +
  theme_minimal()

#------ heart disease vs fasting blood sugar
ggplot(ds_rescaled, aes(x = FastingBS, fill = factor(HeartDisease))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "powderblue", "1" = "deepskyblue4")) +  
  ggtitle("Heart Disease Proportion by Fasting BS") +
  theme_minimal()


#------------End of Bivariate Analysis--------------------



#--------------MultiVariate Analysis-----------------

ds_encoded <- ds_rescaled %>%
  mutate(across(c(Sex, ExerciseAngina, ST_Slope), ~ as.numeric(as.factor(.))))

cor_matrix <- cor(ds_encoded %>% select(where(is.numeric)), use = "complete.obs")
print(cor_matrix)

# heatmap 

pheatmap(
  cor_matrix,
  color = colorRampPalette(c("darkgrey", "white", "dodgerblue4"))(100),
  border_color = "white",
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  clustering_method = "complete",
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  display_numbers = TRUE,
  number_color = "black",
  fontsize_number = 8,
  main = "Correlation Heatmap",
  angle_col = 45,
  treeheight_row = 0,
  treeheight_col = 0
)

#----------------End of Multivariate Analysis---------------



#######################################################################################"

                     #------------------Building Models-----------------#


# 1)splitting the data

table(ds_rescaled$HeartDisease)
prop.table(table(ds_rescaled$HeartDisease))

set.seed(10)  
splitIndex <- createDataPartition(ds_rescaled$HeartDisease, p = 0.8, list = FALSE)


train_data <- ds_rescaled[splitIndex, ]
test_data <- ds_rescaled[-splitIndex, ]

dim(train_data)
dim(test_data)

prop.table(table(train_data$HeartDisease))
prop.table(table(test_data$HeartDisease))

# 2)Preprocessing

str(train_data)

# transforming categorical variables
train_data$Sex <- as.factor(train_data$Sex)
train_data$ExerciseAngina <- as.factor(train_data$ExerciseAngina)
train_data$ChestPainType <- as.factor(train_data$ChestPainType)
train_data$RestingECG <- as.factor(train_data$RestingECG)
train_data$ST_Slope <- as.factor(train_data$ST_Slope)

test_data$Sex <- as.factor(test_data$Sex)
test_data$ExerciseAngina <- as.factor(test_data$ExerciseAngina)
test_data$ChestPainType <- as.factor(test_data$ChestPainType)
test_data$RestingECG <- as.factor(test_data$RestingECG)
test_data$ST_Slope <- as.factor(test_data$ST_Slope)

# Creating dummy variables and applying to train and test data
dummies_model <- dummyVars(HeartDisease ~ ., data = train_data)

train_transformed <- predict(dummies_model, newdata = train_data)
test_transformed <- predict(dummies_model, newdata = test_data)

train_transformed <- as.data.frame(train_transformed)
test_transformed <- as.data.frame(test_transformed)

train_transformed$HeartDisease <- train_data$HeartDisease
test_transformed$HeartDisease <- test_data$HeartDisease

head(test_transformed)
#scaling numerical variables
numeric_columns <- c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")

train_transformed[numeric_columns] <- scale(train_transformed[numeric_columns])

test_transformed[numeric_columns] <- scale(
  test_transformed[numeric_columns],
  center = attr(scale(train_transformed[numeric_columns]), "scaled:center"),
  scale = attr(scale(train_transformed[numeric_columns]), "scaled:scale")
)

str(train_transformed)
str(test_transformed)

train_transformed$HeartDisease <- factor(train_transformed$HeartDisease, levels = c(1, 0),labels = c("Yes", "No"))
test_transformed$HeartDisease <- factor(test_transformed$HeartDisease, levels = c(1, 0),labels = c("Yes", "No"))

summary(train_transformed)
summary(test_transformed)
head(train_transformed)
head(test_transformed)

#cross validation
set.seed(10)
cv_control <- trainControl(method = "cv", 
                           number = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

# 3) Training models

#Logistic Regression
set.seed(10)
model_lr <- train(
  HeartDisease ~ ., 
  data = train_transformed,
  method = "glm", 
  family = "binomial",
  trControl = cv_control,
  metric = "ROC"  
)
model_lr

LR_pred <- predict(model_lr, test_transformed)
conf_matrix <- confusionMatrix(LR_pred, test_transformed$HeartDisease)
conf_matrix
#Accuracy : 0.7596 

# Confusion matrix plot (actual vs actual prediction)
cm_data <- as.data.frame(conf_matrix$table)
ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "floralwhite", size = 6) +
  scale_fill_gradient(low = "darkgrey", high = "dodgerblue4") +
  labs(title = "Logistic Regression Confusion Matrix Heatmap",
       x = "Actual Condition",
       y = "Predicted Condition",
       fill = "Count") +
  theme_minimal()
  

#Support Vector Machine (SVM)
set.seed(10) 

model_svm <- train(
  HeartDisease ~ ., 
  data = train_transformed,
  kernel="plonomial", 
  trControl = cv_control,
  metric = "ROC",
  preProcess = c("center", "scale"),
  tuneLength = 10 
)
model_svm
svm_pred <- predict(model_svm, test_transformed)
svmconfmatrix <-confusionMatrix(svm_pred, test_transformed$HeartDisease)
svmconfmatrix
#Accuracy : 0.8443 
# Confusion matrix plot (actual vs actual prediction)
cm_data <- as.data.frame(svmconfmatrix$table)
ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "floralwhite", size = 6) +
  scale_fill_gradient(low = "darkgrey", high = "dodgerblue4") +
  labs(title = "SVM Confusion Matrix Heatmap",
       x = "Actual Condition",
       y = "Predicted Condition",
       fill = "Count") +
  theme_minimal()

#Decision Tree
set.seed(10)

model_dt <- train(
  HeartDisease ~ ., 
  data = train_transformed,
  method = "rpart",
  trControl = cv_control,
  tuneGrid = data.frame(cp = 0.001),  
  metric = "ROC"
)

rpart.plot(model_dt$finalModel)
png("decision_tree_plot.png", width = 2000, height = 1500, res = 300)
rpart.plot(model_dt$finalModel)
dev.off()

dt_pred <- predict(model_dt, test_transformed)
conf_matrix_dt <- confusionMatrix(dt_pred, test_transformed$HeartDisease)
conf_matrix_dt
#Accuracy : 0.7459 
# Decision Tree ROC
dt_probs <- predict(model_dt, test_transformed, type = "prob")[,"Yes"]
roc_dt <- roc(test_transformed$HeartDisease, dt_probs)
plot(roc_dt, col = "darkolivegreen2", main = "Decision Tree ROC Curve")
text(0.5, 0.3, paste("AUC =", round(auc(roc_dt), 3)), col = "deepskyblue4")
abline(a = 0, b = 1, lty = 2, col = "gray")

# Confusion matrix plot (actual vs actual prediction)
cm_data <- as.data.frame(conf_matrix_dt$table)
ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "floralwhite", size = 6) +
  scale_fill_gradient(low = "darkgrey", high = "dodgerblue4") +
  labs(title = "Decision Tree Confusion Matrix Heatmap",
       x = "Actual Condition",
       y = "Predicted Condition",
       fill = "Count") +
  theme_minimal()

#Random Forest
set.seed(10)

model_rf <- train(
  HeartDisease ~ ., 
  data = train_transformed,
  method = "rf",
  trControl = cv_control, 
  metric = "ROC",     
  importance = TRUE        
)

model_rf
rf_pred <- predict(model_rf, test_transformed)

conf_matrix_rf <- confusionMatrix(rf_pred, test_transformed$HeartDisease)
print(conf_matrix_rf)

# Confusion matrix plot (actual vs actual prediction)
cm_data <- as.data.frame(conf_matrix_rf$table)
ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "floralwhite", size = 6) +
  scale_fill_gradient(low = "darkgrey", high = "dodgerblue4") +
  labs(title = "Random Forest Confusion Matrix Heatmap",
       x = "Actual Condition",
       y = "Predicted Condition",
       fill = "Count") +
  theme_minimal()

#  Accuracy : 0.8607 

#comparing the models

#creating a function that allows extracting confusion matrix values 
get_conf_matrix <- function(pred, actual) {
  cm <- confusionMatrix(pred, actual)
  data.frame(
    True_Negative = cm$table[1,1],
    False_Positive = cm$table[1,2],
    False_Negative = cm$table[2,1],
    True_Positive = cm$table[2,2],
    Accuracy = round(cm$overall["Accuracy"], 3)  
  )
}

cm_lr <- get_conf_matrix(LR_pred, test_transformed$HeartDisease)
cm_svm <- get_conf_matrix(svm_pred, test_transformed$HeartDisease)
cm_dt <- get_conf_matrix(dt_pred, test_transformed$HeartDisease)
cm_rf <- get_conf_matrix(rf_pred, test_transformed$HeartDisease)

# Combine into one table
combined_cm <- rbind(
  data.frame(Model = "Logistic Regression", cm_lr),
  data.frame(Model = "SVM", cm_svm),
  data.frame(Model = "Decision Tree", cm_dt),
  data.frame(Model = "Random Forest", cm_rf)
)

combined_cm <- combined_cm[, c("Model", "Accuracy", "True_Negative", "False_Positive", 
                               "False_Negative", "True_Positive")]

print(combined_cm)


#ROC curves

# Logistic Regression
lr_probs <- predict(model_lr, test_transformed, type = "prob")[,"Yes"]
roc_lr <- roc(test_transformed$HeartDisease, lr_probs)
plot(roc_lr, col = "turquoise", main = "Logistic Regression ROC Curve")
text(0.5, 0.3, paste("AUC =", round(auc(roc_lr), 3)), col = "deepskyblue4")
abline(a = 0, b = 1, lty = 2, col = "gray")

# SVM
svm_probs <- predict(model_svm, test_transformed, type = "prob")[,"Yes"]
roc_svm <- roc(test_transformed$HeartDisease, svm_probs)
plot(roc_svm, col = "salmon", main = "SVM ROC Curve")
text(0.5, 0.3, paste("AUC =", round(auc(roc_svm), 3)), col = "deepskyblue4")
abline(a = 0, b = 1, lty = 2, col = "gray")



# Random Forest
rf_probs <- predict(model_rf, test_transformed, type = "prob")[,"Yes"]
roc_rf <- roc(test_transformed$HeartDisease, rf_probs)
plot(roc_rf, col = "purple", main = "Random Forest ROC Curve")
text(0.5, 0.3, paste("AUC =", round(auc(roc_rf), 3)), col = "deepskyblue4")
abline(a = 0, b = 1, lty = 2, col = "gray")

# comparison
plot(roc_lr, col = "turquoise", main = "All ROC Curves")
lines(roc_svm, col = "salmon")
lines(roc_dt, col = "darkolivegreen2")
lines(roc_rf, col = "purple")
legend("bottomright", 
       legend = c("Logistic Regression", "SVM", "Decision Tree", "Random Forest"),
       col = c("turquoise", "salmon", "darkolivegreen2", "purple"), lty = 1)


