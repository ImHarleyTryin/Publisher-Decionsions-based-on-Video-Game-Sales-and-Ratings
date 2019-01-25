library(caTools)
library(tidyverse)
library(fastDummies)
library(caret)

## Make count columns for Developer and Co_Developer
Clean_Video_Games_Data$Dev_Count <- as.numeric(ave(Clean_Video_Games_Data$Developer, Clean_Video_Games_Data$Developer, FUN = length))

## Make dummy columns for each Genre
Clean_Video_Games_Data <- dummy_cols(Clean_Video_Games_Data, select_columns = c("Genre"))

## set seed
set.seed(8)

## Split data into testing and training
split <- sample.split(Clean_Video_Games_Data$Global_Sales, SplitRatio = 0.8)
Games_Train <- subset(Clean_Video_Games_Data, split == TRUE)
Games_Test <- subset(Clean_Video_Games_Data, split == FALSE)

## Manual Selection
Games_Mod <- lm(Global_Sales ~ Genre + Critic_Score + User_Score + Rating + Top_20_Devs + Dev_Count , data = Games_Train)
summary(Games_Mod)

# There are possibly better methods of model selection that are done automatically

## Set the scope of the model fit
MaxFit <- lm(Global_Sales ~ Genre + Critic_Score + User_Score + Developer + Dev_Count + Rating + Top_20_Devs + Platform, data = Games_Train)
FitStart <- lm(Global_Sales ~ 1, data = Games_Train)

## Forward selection
Forward_mod <- step(FitStart, direction = "forward", scope = formula(MaxFit))
summary(Forward_mod)

## Backward Selection

Backward_mod <- step(MaxFit, direction = "backward")
summary(Backward_mod)

anova(Games_Mod, Forward_mod, Backward_mod)

# Of the three models, the model made with forward selection is comparitively the best

## prediction on test set
Pred_Test <- predict(Forward_mod, Games_Test)

## testing and plotting
error <- Pred_Train - Games_Test$Global_Sales
sqrt(mean(error^2))


