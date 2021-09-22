# Churn

## choose a reference period ~ reference date: 01/01/2019
churn_study_period <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE < as.Date("1/1/2019",
                            format = "%d/%m/%Y"),
         TIC_DATE > as.Date("01/10/2018",
                            format = "%d/%m/%Y"))

## choose an holdout period after each reference date (28/02/2019)
## on which evaluate the churn model:
churn_holdout <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE < as.Date("28/02/2019",
                            format = "%d/%m/%Y"),
         TIC_DATE > as.Date("01/01/2019",
                            format = "%d/%m/%Y"))

## identify customer who did not churn 
no_churner <- unique(churn_holdout$ID_CLI)


## choose the length of a lookback period (3 months) before the reference date:
churn_recency <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))

## create 3 new variable/dataframes (recency, frequency, monetary) to be used in the model
churn_recency$RECENCY <- difftime(as.Date("01/01/2019",
                                          format = "%d/%m/%Y"),         
                                  churn_recency$LAST_PURCHASE_DATE,
                                  units = "days")

churn_frequency <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(TOT_PURCHASE))

churn_monetary <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO),
            SPESA = IMPORTO_LORDO - SCONTO) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(IMPORTO_LORDO))

churn <- merge(churn_recency, churn_frequency, by = "ID_CLI")
churn <- merge(churn, churn_monetary, by = "ID_CLI") %>%
  select(ID_CLI,
         RECENCY,
         SPESA, 
         TOT_PURCHASE)

## create a new binary variable (0/1) where 1 = churn and 0 = no churn
churn$CHURN <- 1
for (i in c(1:nrow(churn))){
  if (churn$ID_CLI[i] %in% no_churner) churn$CHURN[i] <- 0
}
churn$CHURN <- as.factor(churn$CHURN)
table(churn$CHURN)

## choose a set of variables that will help predict whether the client will churn 
## or not in the lookback period (RECENCY; SPESA; TOT_PURCHASE;REGION;LAST_COD_FID;
## TYP_JOB)

churn <- left_join(churn, df_2_cli_account_clean[, c("ID_CLI", "TYP_JOB")], by = "ID_CLI")
churn <- left_join(churn, df_1_cli_fid_clean[, c("ID_CLI", "LAST_COD_FID")], by = "ID_CLI") 
region <- left_join(df_2_cli_account_clean[, c("ID_CLI", "ID_ADDRESS")],
                    df_3_cli_address_clean[, c("ID_ADDRESS", "REGION")], by = "ID_ADDRESS") 
churn <- left_join(churn, region, by = "ID_CLI")
churn <- churn[, -8]
head(churn)

## Model

## split the entire dataset in train and test dataset.
churn <- na.omit(churn)
train_index <- createDataPartition(churn$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)
train <- churn[train_index,]
test <- churn[-train_index,]
table(train$CHURN)

## choose models to be evaluated:
# 1) Recursive Partitioning And Regression Trees
# 2) Random Forest
# 3) Logistic Regression 
# 4) Lasso 

## 1) Regression Trees
tree <- rpart(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB,
              data = train)
rpart.plot(tree)
summary(tree) 

## 2) Random Forest
tree_rf <- randomForest(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB,
                        data = train, ntree = 100)
print(tree_rf)

## 3) Logistic Regression
logistic <- train(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB,
                 data = train,
                 method = "glm")
summary(logistic)

## 4) Lasso
lasso <- train(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB,
            data = train,
            method = "glmnet",
            family = "binomial")
lasso
plot(lasso)

## Prediction of the variable churn with the different models
pred <- predict(tree, test[, -5], type = "class")
p1 <- unlist(pred)
confusionMatrix(p1, test$CHURN)

pred_rf <- predict(tree_rf, test[,-5], type = "class")
confusionMatrix(pred_rf, test$CHURN)

pred_logistic <- predict(logistic, test[, -5], type = "raw")
confusionMatrix(pred_logistic, test$CHURN)

pred_lasso <- predict(lasso, test[,-5], type = "raw")
confusionMatrix(pred_lasso, test$CHURN)

## show all confusion matrix in the same dataframe accuracy
accuracy <- as.data.frame(t(cbind(confusionMatrix(pred_lasso, test$CHURN)$overall[1],
      confusionMatrix(pred_logistic, test$CHURN)$overall[1],
      confusionMatrix(pred_rf, test$CHURN)$overall[1],
      confusionMatrix(pred, test$CHURN)$overall[1])))

accuracy <- as.data.frame(cbind(c("Lasso", "Logistic","Random Forest","Tree"),
                                accuracy))

## rename the columns of the accuracy dataframe
colnames(accuracy) <- c("Models", "Accuracy")

## plot the accuracy of the four different models
accuracy_plot <- (ggplot(data = accuracy,
       aes(x = Models,
           y = Accuracy,
           fill = Models)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0.681, 0.693)) +
  theme_minimal() +
  guides(fill = FALSE) +
  labs(title = "Accuracy",
       x = "Models",
       y = " ") +
    scale_fill_manual(values = c('#ffd147','#0795ff','red','forestgreen'))+
    theme(plot.title = element_text(size=15,hjust = 0.5),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10, hjust=1),
          legend.text = element_text(size=10),
          legend.title = element_text(size=10))
)

accuracy_plot

## evaluation
p_tree = predict(tree, test[,-5], "prob")[,1]
p_rf = predict(tree_rf, test[,-5], "prob")[,1]
p_log = predict(logistic, test[,-5], "prob")[,1]
p_lasso = predict(lasso, test[,-5], "prob")[,1]

## show all evaluations in the same dataframe data_class
data_class = as.data.frame(cbind(p_tree, p_rf, p_log, p_lasso))
data_class = cbind(data_class, test$CHURN)
colnames(data_class) <- c("p_tree", "p_rf", "p_log", "p_lasso", "churn")
head(data_class)

## create lift curve for every models
lift_tree = gain_lift(data = data_class, score = 'p_tree', target = 'churn')
lift_rf = gain_lift(data = data_class, score = 'p_rf', target = 'churn')
lift_log = gain_lift(data = data_class, score = 'p_log', target = 'churn')
lift_lasso = gain_lift(data = data_class, score = 'p_lasso', target = 'churn')

## model tree is useless, it's interesting to use models rf and log, notice that
## model lasso is almost the same as logistic model 

## use the created models to predict churn probability on the remaining dates
new_churn_study_period <- df_7_tic_clean_final %>%
                        filter(DIREZIONE == 1,
                               TIC_DATE < as.Date("30/04/2019",
                                                  format = "%d/%m/%Y"),
                               TIC_DATE > as.Date("28/02/2019",
                                                  format = "%d/%m/%Y"))
new_churn_recency <- new_churn_study_period %>%
                  filter(DIREZIONE == 1) %>%
                  group_by(ID_CLI) %>%
                  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))

new_churn_recency$RECENCY <- difftime(as.Date("30/04/2019",
                                          format = "%d/%m/%Y"),          
                                  new_churn_recency$LAST_PURCHASE_DATE,
                                  units = "days")

new_churn_frequency <- new_churn_study_period %>%
                    filter(DIREZIONE == 1) %>%
                    group_by(ID_CLI) %>%
                    summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
                    arrange(desc(TOT_PURCHASE))

new_churn_monetary <- new_churn_study_period %>%
                    filter(DIREZIONE == 1) %>%
                    group_by(ID_CLI) %>%
                    summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
                              SCONTO = sum(SCONTO),
                              SPESA = IMPORTO_LORDO - SCONTO) %>%
                    ungroup() %>%
                    as.data.frame() %>%
                    arrange(desc(IMPORTO_LORDO))

new_churn <- merge(new_churn_recency, new_churn_frequency, by = "ID_CLI")
new_churn <- merge(new_churn, new_churn_monetary, by = "ID_CLI") %>%
          select(ID_CLI,
                 RECENCY,
                 SPESA, 
                 TOT_PURCHASE)

new_churn <- left_join(new_churn, df_2_cli_account_clean[, c("ID_CLI", "TYP_JOB")],
                       by = "ID_CLI")  
new_churn <- left_join(new_churn, df_1_cli_fid_clean[, c("ID_CLI", "LAST_COD_FID")],
                       by = "ID_CLI") 
region <- left_join(df_2_cli_account_clean[, c("ID_CLI", "ID_ADDRESS")],
                    df_3_cli_address_clean[, c("ID_ADDRESS", "REGION")],
                    by = "ID_ADDRESS") 
new_churn <- left_join(new_churn, region, by = "ID_CLI")
new_churn <- new_churn[, -7]
new_churn <- na.omit(new_churn)

## use logistic model to predict probability to churn
new_churn$prob_to_churn <- predict(logistic, new_churn, type = "prob")[,2]

head(new_churn)
