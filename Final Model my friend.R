#Midterm - Project 

library(dplyr)
library(ggplot2)
library(tidyverse)
library(zoo)
#Question 1 - Description of the stocks 
stocks <- read.csv("C:/Users/venus/Downloads/ukvol.csv")
stocks 

#I decided to reframe the dataset because I struggled with its initial form
stocks_GLEN<-stocks%>%
  select(Date=GLEN,Open,High, Low, Close,Volume)%>%
  mutate(Ticker="GLEN")

stocks_VOD <- stocks %>% 
  select(Date = VOD, Open = `Open.1`, High = `High.1`, Low = `Low.1`, 
         Close = `Close.1`, Volume = `Volume.1`) %>% 
  mutate(Ticker = "VOD")

stocks_OCDO <- stocks %>% 
  select(Date = OCDO, Open = `Open.2`, High = `High.2`, Low = `Low.2`, 
         Close = `Close.2`, Volume = `Volume.2`) %>% 
  mutate(Ticker = "OCDO")

stocks_BHL <- stocks %>% 
  select(Date = BHL, Open = `Open.3`, High = `High.3`, Low = `Low.3`, 
         Close = `Close.3`, Volume = `Volume.3`) %>% 
  mutate(Ticker = "BHL")

stocks_APTA <- stocks %>% 
  select(Date = APTA, Open = `Open.4`, High = `High.4`, Low = `Low.4`, 
         Close = `Close.4`, Volume = `Volume.4`) %>% 
  mutate(Ticker = "APTA")

tidy_stocks <- bind_rows(stocks_GLEN, stocks_VOD, stocks_OCDO, stocks_BHL, stocks_APTA)
head(tidy_stocks)

tidy_stocks<-tidy_stocks%>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y"))

dim(tidy_stocks)
sum(is.na(tidy_stocks))
str(tidy_stocks)

         
tidy_stocks<-tidy_stocks%>%
  group_by(Ticker)%>%
  mutate(Return=(Close-lag(Close))/lag(Close))%>%
  ungroup()

tidy_stocks


tidy_stocks<-tidy_stocks%>%
  group_by(Ticker)%>%
  mutate(Volume_volatility=rollapply(Volume,width=25, FUN=sd, fill=NA, align="right"))%>%
  ungroup()
tidy_stocks

#Basic descriptive statistics 
desc_stats<-tidy_stocks%>%
  group_by(Ticker)%>%
  summarise(
    Mean_Close=mean(Close,na.rm=TRUE),
    Mean_Volume=mean(Volume,na.rm=TRUE),
    Mean_Return=mean(Return,na.rm=TRUE),
    Mean_Volatility=mean(Volume_volatility,na.rm=TRUE),
  )


print(desc_stats)

head(tidy_stocks$Date)
str(tidy_stocks$Date)

#Time series plot, trading volumes and closing prices over time 
#Outliers 
tidy_stocks_clean<-tidy_stocks%>%
  filter(!is.na(Date)&!is.na(Volume))

tidy_stocks_clean

ggplot(tidy_stocks_clean,aes(x = Date, y=Volume, color=Ticker))+
  geom_line()+
  facet_wrap(~Ticker, scales="free_y")+
  labs(title = "Volume against time", x="Date",y="volume")+
  theme_minimal()

ggplot(tidy_stocks_clean,aes(x = Date, y=Close, color=Ticker))+
  geom_line()+
  facet_wrap(~Ticker, scales="free_y")+
  labs(title = "Closing price across time", x="date",y="Closing price")+
  theme_minimal()

boxplot(log(Volume)~Ticker,data=tidy_stocks_clean,
        main="boxplot",
        xlab="Stocks",
        ylab="Logarithm Volume",
        col="pink")
#Log volume is more informative 
#VOD has a lot of outliers

#Also had a higher median volume and a broader distribution, which align with the fact that it is a large and frequently traded company.

#Model fit - Choice OLS + Ridge 


######Model Fitting

library(car)

ols_modeling <- function(ticker, data) {
  set.seed(17)
  ticker_data <- data %>% filter(Ticker == ticker)

  ticker_data <- ticker_data %>%
    mutate(Log_Volume = log(Volume))

  train_index <- sample(1:nrow(ticker_data), size = 0.7 * nrow(ticker_data))
  train_data <- ticker_data[train_index, ]
  test_data <- ticker_data[-train_index, ]
  predictor_vars <- c("Open", "High", "Low", "Close", "Return", "Volume_volatility")
  
  train_data_std <- train_data %>%
    mutate(across(all_of(predictor_vars), ~as.numeric(scale(.))))
  train_means <- sapply(train_data[predictor_vars], mean, na.rm = TRUE)
  train_sds <- sapply(train_data[predictor_vars], sd, na.rm = TRUE)
  test_data_std <- test_data
  for (var in predictor_vars) {
    test_data_std[[var]] <- (test_data[[var]] - train_means[[var]]) / train_sds[[var]]
  }
  
  ols_model <- lm(Log_Volume ~ Open + High + Low + Close + Return + Volume_volatility,
                  data = train_data_std)
  
  rsquared<-summary(ols_model)$r.squared
  
  vif_values<-vif(ols_model)
  print(paste("Stock:",ticker,"vif"))
  print(vif_values)
  
  train_predictions <- exp(predict(ols_model, newdata = train_data_std))
  test_predictions <- exp(predict(ols_model, newdata = test_data_std))
  
  rmse_in_sample <- sqrt(mean((train_data$Volume - train_predictions)^2, na.rm = TRUE))
  rmse_out_sample <- sqrt(mean((test_data$Volume - test_predictions)^2, na.rm = TRUE))
  
  print(paste("stock", ticker, "RMSE in sample:", rmse_in_sample, "RMSE out sample", rmse_out_sample))
  return(data.frame(Ticker = ticker, RMSE_train_osl = rmse_in_sample, RMSE_test_ols = rmse_out_sample,R2=rsquared))
  
}

tickers <- c("GLEN", "VOD", "OCDO", "BHL", "APTA")

results_ols <- do.call(rbind, lapply(tickers, function(t) ols_modeling(t, tidy_stocks_clean)))

print(results_ols)

#This model has high VIF so we know we need to drop some variables 


#Overall i think those are not very good results, we also have to few variables in my opinion
cor_matrix <- cor(tidy_stocks_clean %>% select(Volume, Open, High, Low, Close, Return, Volume_volatility), use = "complete.obs")
print(cor_matrix)
#indeed very few variables seems to be of interest 

#Now attempting RIDGE

install.packages("glmnet")
library("glmnet")
ridge_modeling <- function(ticker, data) {
  set.seed(17)

  ticker_data <- data %>% filter(Ticker == ticker)
  ticker_data <- ticker_data %>%
    mutate(Log_Volume = log(Volume))
  
  train_index <- sample(1:nrow(ticker_data), size = 0.7 * nrow(ticker_data))
  train_data <- ticker_data[train_index, ]
  test_data <- ticker_data[-train_index, ]
  
  predictor_vars <- c("Open", "High", "Low", "Close", "Return", "Volume_volatility")
  for (var in predictor_vars) {
    train_data[[var]][is.na(train_data[[var]])] <- mean(train_data[[var]], na.rm = TRUE)
    test_data[[var]][is.na(test_data[[var]])] <- mean(train_data[[var]], na.rm = TRUE)  
  }

  train_means <- sapply(train_data[predictor_vars], mean, na.rm = TRUE)
  train_sds <- sapply(train_data[predictor_vars], sd, na.rm = TRUE)
  train_data_std <- train_data
  for (var in predictor_vars) {
    train_data_std[[var]] <- (train_data[[var]] - train_means[[var]]) / train_sds[[var]]
  }
  
  test_data_std <- test_data
  for (var in predictor_vars) {
    test_data_std[[var]] <- (test_data[[var]] - train_means[[var]]) / train_sds[[var]]
  }
  
  X_train <- as.matrix(train_data_std %>% select(all_of(predictor_vars)))
  y_train <- train_data_std$Log_Volume
  X_test <- as.matrix(test_data_std %>% select(all_of(predictor_vars)))
  y_test <- test_data_std$Log_Volume
  
  X_train <- X_train[complete.cases(X_train), ]
  y_train <- y_train[complete.cases(y_train)]
  
  ridge_model <- cv.glmnet(X_train, y_train, alpha = 0.2)
  choice_lambda <- ridge_model$lambda.min 
  
  train_predictions_log <- predict(ridge_model, newx = X_train, s = choice_lambda)
  test_predictions_log <- predict(ridge_model, newx = X_test, s = choice_lambda)

  sigma_squared <- mean((train_predictions_log - y_train)^2, na.rm = TRUE)
  
  train_predictions <- exp(train_predictions_log + 0.5 * sigma_squared)
  test_predictions <- exp(test_predictions_log + 0.5 * sigma_squared)
  
  rmse_in_sample <- sqrt(mean((train_data$Volume - train_predictions)^2, na.rm = TRUE))
  rmse_out_sample <- sqrt(mean((test_data$Volume - test_predictions)^2, na.rm = TRUE))
  ss_t <- sum((y_test - mean(y_test))^2)
  ss_r <- sum((y_test - test_predictions_log)^2)
  rsquared <- 1 - (ss_r / ss_t)
  
  print(paste("stock", ticker, "RMSE in sample:", rmse_in_sample, "RMSE out sample", rmse_out_sample, "R^2:", rsquared))
  
  return(data.frame(Ticker = ticker, RMSE_in_ridge = rmse_in_sample, RMSE_out_ridge = rmse_out_sample, Rsqrd = rsquared))
}


tickers <- c("GLEN", "VOD", "OCDO", "BHL", "APTA")
ridge_results <- do.call(rbind, lapply(tickers, function(t) ridge_modeling(t, tidy_stocks_clean)))
print(ridge_results)

#Barely improve the model, no matter what the alpha is, i tried 0.2, 0.5 and 1 (Lasso)

#########
#Now attempting to improve the model 

#We create new variables: 
tidy_stocks_clean <- tidy_stocks_clean %>%
  group_by(Ticker) %>%
  arrange(Date) %>%
  mutate(
    Lag_Volume = lag(Volume, n = 1),
    Volume_Close_Interaction = Volume * Close 
  ) %>%
  ungroup()

library(car)

ols_modeling <- function(ticker, data) {
  set.seed(17)

  ticker_data <- data %>% filter(Ticker == ticker)
  ticker_data <- ticker_data %>%
    mutate(Log_Volume = log(Volume))
  
  train_index <- sample(1:nrow(ticker_data), size = 0.7 * nrow(ticker_data))
  train_data <- ticker_data[train_index, ]
  test_data <- ticker_data[-train_index, ]
  
  predictor_vars <- c("Close", "Return", "Volume_volatility","Lag_Volume", "Volume_Close_Interaction")
  
  train_data_std <- train_data %>%
    mutate(across(all_of(predictor_vars), ~as.numeric(scale(.))))
  
  train_means <- sapply(train_data[predictor_vars], mean, na.rm = TRUE)
  train_sds <- sapply(train_data[predictor_vars], sd, na.rm = TRUE)
 
  test_data_std <- test_data
  for (var in predictor_vars) {
    test_data_std[[var]] <- (test_data[[var]] - train_means[[var]]) / train_sds[[var]]
  }
  

  ols_model <- lm(Log_Volume ~ Close + Return + Volume_volatility + Lag_Volume + Volume_Close_Interaction,
                  data = train_data_std)
  
  rsquared<-summary(ols_model)$r.squared
  
  vif_value<-vif(ols_model)
  print(paste("stock:",ticker,"vif"))
  print(vif_value)

  train_predictions <- exp(predict(ols_model, newdata = train_data_std))
  test_predictions <- exp(predict(ols_model, newdata = test_data_std))
  
  rmse_in_sample <- sqrt(mean((train_data$Volume - train_predictions)^2, na.rm = TRUE))
  rmse_out_sample <- sqrt(mean((test_data$Volume - test_predictions)^2, na.rm = TRUE))
  
  print(paste("Ticker:", ticker, "RMSE in sample:", rmse_in_sample, "RMSE out sample:", rmse_out_sample))
  return(data.frame(Ticker = ticker, RMSE_Train_OLS = rmse_in_sample, RMSE_Test_OLS = rmse_out_sample,R2=rsquared))
  
  
}

tickers <- c("GLEN", "VOD", "OCDO", "BHL", "APTA")
OLS_bis <- do.call(rbind, lapply(tickers, function(t) ols_modeling(t, tidy_stocks_clean)))
print(OLS_bis)

#No more multicollinearity
#RMSE still high for VOD and APTA 
#So for APTA, there is a large spread, which suggest that it behaves differently from the other stocks. so other model 
#Problem for VOD might be outliers 

###################
#We improve the model by removing the outliers
VOD_data <- tidy_stocks_clean %>% filter(Ticker == "VOD")

Q1 <- quantile(vod_data$Volume, 0.25, na.rm = TRUE)
Q3 <- quantile(vod_data$Volume, 0.75, na.rm = TRUE)
IQR_value <- IQR(vod_data$Volume, na.rm = TRUE)
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
VOD_data_clean <- VOD_data %>%
  filter(Volume >= lower_bound & Volume <= upper_bound)

VOD_data_clean <- VOD_data_clean %>%
  arrange(Date) %>% 
  mutate(
    Lag_Volume = lag(Volume, n = 1),  
    Volume_Close_Interaction = Volume * Close  
  )

ols_vod_modeling <- function(data) {
  set.seed(17)  
  
  data <- data %>% mutate(Log_Volume = log(Volume))

  train_index <- sample(1:nrow(data), size = 0.7 * nrow(data))
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  predictor_vars <- c("Close", "Return", "Volume_volatility", "Lag_Volume", "Volume_Close_Interaction")
  
  train_data_std <- train_data %>%
    mutate(across(all_of(predictor_vars), ~as.numeric(scale(.))))

  train_means <- sapply(train_data[predictor_vars], mean, na.rm = TRUE)
  train_sds <- sapply(train_data[predictor_vars], sd, na.rm = TRUE)
  

  test_data_std <- test_data
  for (var in predictor_vars) {
    test_data_std[[var]] <- (test_data[[var]] - train_means[[var]]) / train_sds[[var]]
  }
  
  ols_model <- lm(Log_Volume ~ Close + Return + Volume_volatility + Lag_Volume + Volume_Close_Interaction,
                  data = train_data_std)
  
  rsquared <- summary(ols_model)$r.squared
  
  train_predictions <- exp(predict(ols_model, newdata = train_data_std))
  test_predictions <- exp(predict(ols_model, newdata = test_data_std))
  rmse_in_sample <- sqrt(mean((train_data$Volume - train_predictions)^2, na.rm = TRUE))
  rmse_out_sample <- sqrt(mean((test_data$Volume - test_predictions)^2, na.rm = TRUE))

  print(paste(" RMSE in:", rmse_in, "RMSE out:", rmse_out, "R^2:", rsquared))
  
  return(data.frame(Ticker = "VOD", RMSE_in_ols = rmse_in_sample, RMSE_out_ols = rmse_out_sample, R2 = rsquared))
}

VOD_ols_results <- ols_vod_modeling(VOD_data_clean)
print(VOD_ols_results)


#######Let's see for APTA

apta_data <- tidy_stocks_clean %>% filter(Ticker == "APTA")

Q1 <- quantile(apta_data$Volume, 0.25, na.rm = TRUE)
Q3 <- quantile(apta_data$Volume, 0.75, na.rm = TRUE)
IQR_value <- IQR(apta_data$Volume, na.rm = TRUE)

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

apta_data_clean <- apta_data %>%
  filter(Volume >= lower_bound & Volume <= upper_bound)

apta_data_clean <- apta_data_clean %>%
  arrange(Date) %>% 
  mutate(
    Lag_Volume = lag(Volume, n = 1),
    Volume_Close_Interaction = Volume * Close
  ) %>%
  mutate(
    Lag_Volume = ifelse(is.na(Lag_Volume), mean(Lag_Volume, na.rm = TRUE), Lag_Volume)
  )

ols_apta_modeling <- function(data) {
  set.seed(17)
  
  data <- data %>% mutate(Log_Volume = log(Volume))

  train_index <- sample(1:nrow(data), size = 0.7 * nrow(data))
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  predictor_vars <- c("Close", "Return", "Volume_volatility", "Lag_Volume", "Volume_Close_Interaction")

  train_data_std <- train_data %>%
    mutate(across(all_of(predictor_vars), ~as.numeric(scale(.))))
  train_means <- sapply(train_data[predictor_vars], mean, na.rm = TRUE)
  train_sds <- sapply(train_data[predictor_vars], sd, na.rm = TRUE)
  test_data_std <- test_data
  for (var in predictor_vars) {
    test_data_std[[var]] <- (test_data[[var]] - train_means[[var]]) / train_sds[[var]]
  }
  
  ols_model <- lm(Log_Volume ~ Close + Return + Volume_volatility + Lag_Volume + Volume_Close_Interaction,
                  data = train_data_std)
  
  rsquared <- summary(ols_model)$r.squared
  
  train_predictions <- exp(predict(ols_model, newdata = train_data_std))
  test_predictions <- exp(predict(ols_model, newdata = test_data_std))
  
  rmse_in_sample <- sqrt(mean((train_data$Volume - train_predictions)^2, na.rm = TRUE))
  rmse_out_sample <- sqrt(mean((test_data$Volume - test_predictions)^2, na.rm = TRUE))
  
  print(paste("RMSE in sample:", rmse_in_sample, "RMSE out sample:", rmse_out_sample, "|R2:", rsquared))
  
  return(data.frame(Ticker = "APTA", RMSE_in_ols = rmse_in_sample, RMSE_out_OLS = rmse_out_sample, R2 = rsquared))
}

apta_ols_results <- ols_apta_modeling(apta_data_clean)
print(apta_ols_results)

