##Reading  Data
getwd()
news <- read.csv("fake_news_dataset.csv",header=TRUE)
head(news)
sum(is.na(news)) # I need to create NA

#Creating NA
#install.packages("missMethods")
library(missMethods)
set.seed(412)
news_mcar = delete_MCAR(ds = news, p = 0.1) # To generate 10% missing completely at random data for each column
colSums(is.na(news_mcar)) # Every variable has 10% missing at random.
write.csv(news_mcar, 'news_missing.csv', row.names = FALSE) # To save the final data. row.names = FALSE to exclude indexes.

news_data <- read.csv("news_missing.csv",header=T)
news_data <- news_data[,-c(1,2,4,6)]


##Explanatory Data Analysis (EDA)
dim(news_data) 
head(news_data) 
str(news_data) # to check whether there are inconsistencies in structure of variables

news_data$author <- as.factor(news_data$author)
news_data$state <- as.factor(news_data$state) 
news_data$source <- as.factor(news_data$source)
news_data$category <- as.factor(news_data$category)
news_data$has_images <- as.factor(news_data$has_images)
news_data$has_videos <- as.factor(news_data$has_videos)
news_data$political_bias <- as.factor(news_data$political_bias)
news_data$fact_check_rating <- as.factor(news_data$fact_check_rating)
news_data$is_satirical <- as.factor(news_data$is_satirical)
news_data$label <- as.factor(news_data$label)
str(news_data)
table(news_data$author) # It can be seen the factor levels and their frequencies


summary(news_data) 

#install.packages("psych")
library(psych)
describe(news_data) #detailed version of summary function for numeric variables


library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

data <- news_data %>%
  filter(!is.na(sentiment_score),!is.na(word_count),!is.na(char_count),!is.na(readability_score),!is.na(clickbait_score),
         !is.na(num_shares),!is.na(num_comments),!is.na(trust_score),!is.na(source_reputation),!is.na(plagiarism_score))

numeric <-data[,sapply(data,is.numeric)]
corr_matrix <- cor(numeric)
corrplot(corr_matrix, method = "color", 
         addCoef.col = "black",   
         tl.col = "black",       
         number.cex = 0.6,
         main = "Correlation Matrix")


#Now,let's try to detect the type of missingness.
library(mice)
library(naniar)
md.pattern(news_data) #All variables have missing values and we have 400 NA for each variable.
md.pairs(news_data)$mm #We have lots of common missing rows between variables.


##**Research Questions**
#*NA values in the categorical variables have been temporarily removed to prevent looking as third category(NA) during visualization.
#*Research Question 1 = Which authors' news are the mostly classified as Fake news and which with Real news?*
table(news_data$author, news_data$label) # As we can see, Alex Johnson has the highest number of fake news with 352 news and Emily Davis has the highest number of real news with 333.

author_article_counts <- news_data %>%
  filter(!is.na(label),!is.na(author)) %>%   
  group_by(author, label) %>%
  summarise(count = n())


ggplot(author_article_counts, aes(x = reorder(author, count), y = count, fill = label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.3, color = "black", size = 4) +  # Text just above the bars
  labs(title = "Number of Fake Vs Real News by Author",
       x = "Authors", y = "Number of News", fill = "News Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



#*Research Question 2 = Is there a relationship between the category of news and fact check rating ?*
table(news_data$category,news_data$fact_check_rating) #There is slightly difference between categories in fake news


filtered_data <- news_data %>%
  filter(!is.na(category), !is.na(fact_check_rating))

ggplot(filtered_data, aes(x = category, fill = fact_check_rating)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Distribution of Fact Check Rating by News Category",
    x = "News Category",
    y = "Proportion",
    fill = "Fact Check Rating"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#*Research Question 3 = How does the number of comment and shares affect classifying news?*
filtered_data2 <- news_data %>%
  filter(!is.na(label))

ggplot(filtered_data2, aes(x = num_comments, y = num_shares)) +
  geom_point(color = "blue") +
  facet_wrap(~ label) +
  labs(title = "Trellis Plot: Number of Comments vs Number of Shares by News' Label",
       x = "Number of Comments",
       y = "Number of Shares")


p <- ggplot(filtered_data2, aes(x = label, y = num_comments, fill = label)) +
  geom_boxplot() +
  labs(title = "Number of Comments by News Label", x = "News Label", y = "Number of Comments") +
  theme_minimal()

p2 <- ggplot(filtered_data2, aes(x = label, y = num_shares, fill = label)) +
  geom_boxplot() +
  labs(title = "Number of Shares by News Label", x = "News Label", y = "Number of Shares") +
  theme_minimal()

library(gridExtra)
library(DescTools) # for mode
grid.arrange(p, p2, ncol = 2) 

filtered_data2 %>%
  group_by(label) %>%
  summarise(
    mean_comments = mean(num_comments, na.rm = TRUE),
    median_comments = median(num_comments, na.rm = TRUE),
    mode_comments = Mode(num_comments, na.rm = TRUE))

filtered_data2 %>%
  group_by(label) %>%
  summarise(
    mean_shares = mean(num_shares, na.rm = TRUE),
    median_shares = median(num_shares, na.rm = TRUE),
    mode_shares = Mode(num_shares, na.rm = TRUE))


#*Research Question 4 = How does average reputation of news sources vary across political bias categories?*
filtered_data3 <- news_data %>%
  filter(!is.na(political_bias),!is.na(source_reputation),!is.na(source))


hm_data <- filtered_data3 %>%
  group_by(source, political_bias) %>%
  summarise(mean_reputation = mean(source_reputation))

ggplot(hm_data, aes(x = factor(political_bias, levels=c("Left","Center","Right")), y = source, fill = mean_reputation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_reputation, 1)), size = 3, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Heatmap: Average Source Reputation by Source and Political Bias",
    x = "Political Bias",
    y = "Source"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#*Research Question 5 = What are the effects of having images and videos on readability score?*
summary_data <- news_data %>%
  filter(!is.na(has_images), !is.na(has_videos), !is.na(readability_score)) %>%
  group_by(has_images, has_videos) %>%
  summarise(mean_readability = mean(readability_score), .groups = "drop")

ggplot(summary_data, aes(x = has_images, y = mean_readability, color = has_videos, group = has_videos)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Interaction Plot: Image & Video Effect on Readability",
    x = "Has Image",
    y = "Mean Readability Score",
    color = "Has Video"
  ) +
  theme_minimal()

filtered_data4 <- news_data %>%
  filter(!is.na(has_images),!is.na(has_videos))


ggplot(filtered_data4 , aes(x = has_images, y = readability_score, fill = has_videos)) +
  geom_boxplot() +
  facet_wrap(~ has_videos) +
  labs(
    title = "Readability Score by Image and Video Presence",
    x = "Has Image",
    y = "Readability Score"
  ) +
  theme_minimal()


#*Research Question 6 = Do satirical news tend to have higher clickbait scores than non-satirical ones?*
filtered_data5 <- news_data %>%
  filter(!is.na(is_satirical), !is.na(clickbait_score)) 

ggplot(filtered_data5, aes(x = is_satirical, y = clickbait_score)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(
    title = "Clickbait Score by Satirical vs Non-Satirical News",
    x = "Is Satirical?",
    y = "Clickbait Score"
  ) +
  theme_minimal()

filtered_data5 %>%
  group_by(is_satirical) %>%
  summarise(
    mean_clickbait = mean(clickbait_score, na.rm = TRUE),
    median_clickbait = median(clickbait_score, na.rm = TRUE),
    sd_clickbait = sd(clickbait_score, na.rm = TRUE),
    Q1_clickbait = quantile(clickbait_score, 0.25, na.rm = TRUE),
    Q3_clickbait = quantile(clickbait_score, 0.75, na.rm = TRUE),
    count = n()
  )


##**Exploration of the Missingness Mechanism**
vis_miss(news_data)


library(mice)
init = mice(news_data, maxit = 0, printFlag = FALSE)
default_methods = init$method
default_predMat = init$predictorMatrix
print(default_methods) 
imputed_data = mice(news_data, method = default_methods, m = 3, maxit = 3, seed = 120)
completed_data = complete(imputed_data, 1)
sum(is.na(completed_data))

summary(news_data)
summary(completed_data)


table(news_data$label, useNA = "ifany") / nrow(news_data)
table(completed_data$label) / nrow(completed_data)

mean(news_data$source_reputation, na.rm = TRUE)
mean(completed_data$source_reputation)

median(news_data$source_reputation, na.rm = TRUE)
median(completed_data$source_reputation)

pl1 <- ggplot(news_data, aes(x = clickbait_score)) +
  geom_density(color = "red", na.rm = TRUE) +
  ggtitle("Original Cliclbait Score Distribution")


pl2 <- ggplot(completed_data, aes(x = clickbait_score)) +
  geom_density(color = "blue") +
  ggtitle("Imputed Cliclbait Score Distribution")

grid.arrange(pl1, pl2, ncol = 2)


##**Feature Engineering**
completed_data$text_density <- completed_data$char_count / completed_data$word_count #average number of characters per word

completed_data$interaction_rate <- (completed_data$num_comments / (completed_data$num_shares + completed_data$num_comments)) * 100  #What % of total interaction #was commenting?

dim(completed_data)
head(completed_data)
str(completed_data)
summary(completed_data)

numeric <-completed_data[,sapply(completed_data,is.numeric)]
corr_matrix <- cor(numeric)
corrplot(corr_matrix, method = "color",
 addCoef.col = "black",
 tl.col = "black",
 number.cex = 0.6,
 main = "Correlation Matrix")

library(dplyr)
completed_data_cleaned <- completed_data %>%
  dplyr::select(-num_comments,-num_shares,-word_count,-char_count) #To avoid multicollinearity problem.

# Min-Max Scaling function
min_max_scale <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

numeric_cols <- completed_data_cleaned[, sapply(completed_data_cleaned, is.numeric)]
completed_data_scaled <- as.data.frame(lapply(numeric_cols, min_max_scale))
completed_data_scaled <- cbind(completed_data_cleaned[, !sapply(completed_data_cleaned, is.numeric)], completed_data_scaled)
head(completed_data_scaled)



##**Confirmatory Data Analysis(CDA)**
#*Question 1 = Which authors' news are the mostly classified as Fake news and which with Real news?*
#*H0=There is no relationship between author and label of the news (They are independent.)*
#*H1=There is relationship between author and label of the new*
cont_table <- table(completed_data$author, completed_data$label)


chisq_result <- chisq.test(cont_table)#Chi-square test checks whether two categorical variables are related or not.
chisq_result$expected #Expected cell frequencies should be ≥ 5 in at least 80% of cells, and none should be < 1
#Both variables are categorical and observations are independent.
print(chisq_result) #p-value:0.1792 > 0.05 ,so do not reject H0.We can conclude that there is no relationship between author and label of the news.


#*Question 2=Is there a relationship between the category of news and fact check rating ?*
#*H0=There is no relationship between category of news and fact check rating (They are independent.)*
#*H1=There is relationship between category of news and fact check rating*
cont_table2 <- table(completed_data$category, completed_data$fact_check_rating)
chisq_result2 <- chisq.test(cont_table2) #Chi-square test checks whether two categorical variables are related or not. 
print(chisq_result2) #P value is 0.1344 > 0.05 , so we can conclude that there is no significant relationship between category of news and fact check rating.



#*Question 3=How does the number of comment and shares affect classifying news?*
#*H0=There is no interaction effect between comments and shares.*
#*H1=There is interaction effect between comments and shares.*

#*H0=There is no relationship between  number of shares and label of the news (They are independent.)*
#*H1=There is relationship between  number of shares and label of the news*
library(ltm)
biserial.cor(completed_data$num_shares, completed_data$label) # 0.024 is close to 0, which indicates a very weak positive relationship

#*H0=There is no relationship between  number of comments and label of the news (They are independent.)*
#*H1=There is relationship between  number of comments and label of the news*
biserial.cor(completed_data$num_comments, completed_data$label) # -0.0106 is very close to 0, which indicates a very weak positive relationship. 

model_interaction <- glm(label ~ num_comments * num_shares, 
                         data = completed_data, 
                         family = binomial)

summary(model_interaction)

#*Question 4=How does average reputation of news sources vary across political bias categories?*
#*H0=There is no significant difference in the average reputation of news sources and political bias categories.*
#*H1=There is significant difference in the average reputation of news sources and political bias categories.*

kruskal_test <- kruskal.test(source_reputation ~ political_bias, data = completed_data)
kruskal_test #p-value:0.6128 > 0.05 ,so do not reject H0 that states there is no significant difference in source reputation between political bias categories.




#*Question 5=What is the effect of having images and videos on readability score?*
#*H0=There is no significant difference in readability scores between the groups created by the presence of images and videos.*
#*H1=There is significant difference in readability scores between the groups created by the presence of images and videos.*

test <- kruskal.test(readability_score ~ interaction(has_images, has_videos), data = completed_data)
test #p-value:0.000186 < 0.05, so reject H0 states that no difference in readability scores between the groups created by the presence of images and videos.

#To find which group or groups differ from others, use post-hoc tests such as Dunn's test
library(dunn.test)
dunn_test_result <- dunn.test(completed_data$readability_score, completed_data$image_video_interaction_term, method = "bonferroni")
print(dunn_test_result)


#*Question 6=Do satirical news ten? to have higher clickbait scores than non-satirical ones?*
#*H0=There is no significant difference in the clickbait scores between satirical and non-satirical news*
#*H1=There is significant difference in the clickbait scores between satirical and non-satirical news*

mann_whitney <- wilcox.test(clickbait_score ~ is_satirical, data = completed_data)
print(mann_whitney)
#p-value:0.1872 > 0.05 ,so do not reject H0. There is no strong evidence to say that satirical ones have higher clickbait scores.




##**Test-train Split and Cross Validation**
set.seed(2025)

library(caret)
train_index <- createDataPartition(completed_data_scaled$label, p = 0.8, list = FALSE)
train_data <- completed_data_scaled[train_index, ]
test_data <- completed_data_scaled[-train_index, ]
completed_data_scaled

cv_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE, savePredictions = "final")



##**Modeling**
logit_completed_data <-  completed_data_scaled #we need to add small terms to continuous variables and we need to protect original data too.


summary(logit_completed_data)
# To ensure all continuous variables are positive(for logit transformation cannot handle 0 or negative values)
logit_completed_data$readability_score <- logit_completed_data$readability_score +1e-4
logit_completed_data$trust_score <- logit_completed_data$trust_score +1e-4
logit_completed_data$source_reputation <- logit_completed_data$source_reputation +1e-4
logit_completed_data$clickbait_score <- logit_completed_data$clickbait_score +1e-4
logit_completed_data$plagiarism_score <- logit_completed_data$plagiarism_score +1e-4
logit_completed_data$sentiment_score <- logit_completed_data$sentiment_score +1e-4
logit_completed_data$interaction_rate <- logit_completed_data$interaction_rate +1e-4
logit_completed_data$text_density <- logit_completed_data$text_density +1e-4


set.seed(2025)
train_logit <- logit_completed_data[train_index, ] 
test_logit <- logit_completed_data[-train_index, ]

prop.table(table(logit_completed_data$label)) # It is balanced.
prop.table(table(train_logit$label)) # The same balance seems to follow in train dataset.


logit_model_full <- train(label ~ ., data = train_logit,method = "glm",family = "binomial",trControl = cv_control)
summary(logit_model_full$finalModel) # Those that have * for p value have significant effect.
exp(coef(logit_model_full$finalModel)) #Odds-ratios


logit_model_reduced <- train(label ~ readability_score+ interaction_rate +   source_reputation+plagiarism_score+category+text_density+ fact_check_rating+political_bias+source+state+author+is_satirical ,
data = train_logit,method = "glm",family = "binomial",trControl = cv_control)
summary(logit_model_reduced$finalModel) #Adding continuous predictors that have linear relationship with logit

logit_model_reduced_2 <- train(label ~ political_bias+source+state+author,
data = train_logit,method = "glm",family = "binomial",trControl = cv_control)
summary(logit_model_reduced$finalModel) #Adding only the terms that are significant 

results1 <- logit_model_full$results %>% mutate(Model = "Full Model")
results2 <- logit_model_reduced$results %>% mutate(Model = "Reduced Model")
results3 <- logit_model_reduced_2$results %>% mutate(Model = "Second Reduced Model")

result <- bind_rows(results1,results2,results3)
dplyr::select(result, Model, Accuracy, Kappa)



##*Model Assumptions*
#Multicollinearity check
library(car)
vif(logit_model_full$finalModel) #Only the VIF of log_text_density is 10 , rest of the VIF values are smaller than 10. So if we do not use log_text_density, there is no multicollinearity problem anymore.

#Outlier check 
residuals <- residuals(logit_model_full$finalModel, type = "deviance")
plot(residuals, main = "Deviance Residuals") #There is no outlier

#Linearity of logit for continuous predictors 
library(mgcv)
gam_model <- gam(label ~  s(interaction_rate) + s(text_density) +
                   s(readability_score)  + s(trust_score) + 
                   s(source_reputation) + s(clickbait_score) + s(plagiarism_score) +
                    s(sentiment_score), 
                 data = logit_completed_data, family = binomial)

summary(gam_model)  # If edf > 1 for any s(), it violates linearity
#trust_score,clickbait_score and log_interaction_rate violates linearity assumption. I am planning not to use them by modelling


#*Prediction*
train_pred <- predict(logit_model_reduced_2, newdata = train_logit)
test_pred <- predict(logit_model_reduced_2, newdata = test_logit)



#*Performance Evaluation*
confusionMatrix(train_pred, train_logit$label)
#When we look at accuracy, we can say that model correctly classifies about 56% of the instances.
#Kappa score (0.11) indicates the model has only slight agreement beyond random chance, so model's overall reliability is weak.
#Model identifies about 60% of actual Fake news (sensitivity) and 51% of the Real news (specificity) correctly labeled.


confusionMatrix(test_pred, test_logit$label)
#When we look at accuracy, we can say that model correctly classifies about 51% of the instances.
#Model identifies about 55% of actual Fake news (sensitivity) and 48% of the Real news (specificity) correctly.
#Compared to the test set (51% accuracy), model appears to do better on the training set (56% accuracy).The model performs poorly on unseen data, model does not generalize well.


#Support Vector Machine
svm_completed_data <-  completed_data_scaled 
set.seed(2025)
train_svm <- svm_completed_data[train_index,]
test_svm <- svm_completed_data[-train_index,]

grid <- expand.grid(
  sigma = c(0.01, 0.05, 0.1),
  C = c(0.25, 0.5, 1))

model_svm <- train(label ~ ., 
                   data = train_svm,
                   method = "svmRadial",
                   trControl = cv_control,
                   preProcess = c("center", "scale"),
                   tuneGrid = grid,
                   metric = "Accuracy")

## Prediction
train_pred <- predict(model_svm, newdata = train_svm)
test_pred <- predict(model_svm, newdata = test_svm)

confusionMatrix(train_pred, train_svm$label)
confusionMatrix(test_pred, test_svm$label)


#Random Forest
#install.packages("randomForest")
library(randomForest)
rf_completed_data <-  completed_data_scaled 
set.seed(2025)
train_rf <- rf_completed_data[train_index,]
test_rf <- rf_completed_data[-train_index,]

rf_grid <- expand.grid(
  mtry = c(3, 5, 7))


model_rf <- train(label ~ ., data = train_svm, method = "rf", trControl = cv_control,
                  tuneGrid = rf_grid, metric = "ROC")


## Prediction
train_pred <- predict(model_rf, newdata = train_rf)
test_pred <- predict(model_rf, newdata = test_rf)

confusionMatrix(train_pred, train_rf$label)
confusionMatrix(test_pred, test_rf$label)



#XGBoost
library(xgboost)
xgb_completed_data <-  completed_data_scaled 
set.seed(2025)
train_xgb <- xgb_completed_data[train_index,]
test_xgb <- xgb_completed_data[-train_index,]

xgb_grid <- expand.grid(
  nrounds = c(100, 200),         
  max_depth = c(3, 5, 7),       
  eta = c(0.05, 0.1),            
  gamma = 0,                    
  colsample_bytree = 0.8,       
  min_child_weight = 1,         
  subsample = 0.8)

set.seed(2025)
model_xgb <- train(label ~ ., 
                   data = train_xgb,
                   method = "xgbTree",
                   trControl = cv_control,    
                   tuneGrid = xgb_grid,
                   metric = "ROC")


## Prediction
train_pred <- predict(model_xgb, newdata = train_xgb)
test_pred <- predict(model_xgb, newdata = test_xgb)

confusionMatrix(train_pred, train_xgb$label)
confusionMatrix(test_pred, test_xgb$label)


#Artificial Neural Networks
library(keras)
library(nnet)
library(caret)
library(dplyr)
library(pROC)
library(tensorflow)

ann_completed_data <-  completed_data_scaled 
set.seed(2025)
train_ann <- ann_completed_data[train_index,]
test_ann <- ann_completed_data[-train_index,]

ann_grid <- expand.grid(size = c(3, 5, 7, 9, 11),
                        decay = c(0.001, 0.01, 0.1))

model_ann <- train(label~. ,data=train_ann, method="nnet", trControl=cv_control,
                   tuneGrid = ann_grid, trace=FALSE, maxit=500, metric="ROC")
model_ann$bestTune
best_model_ann <- model_ann$results %>%
  filter(size == model_ann$bestTune$size,
         decay == model_ann$bestTune$decay)


## Prediction
train_pred <- predict(model_ann, newdata = train_ann)
test_pred <- predict(model_ann, newdata = test_ann)

confusionMatrix(train_pred, train_ann$label)
confusionMatrix(test_pred, test_ann$label)
