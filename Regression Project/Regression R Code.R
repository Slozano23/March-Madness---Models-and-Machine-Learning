library(GGally)
library(tidyverse)

cbb <- read_csv("C:\\Users\\yllw1\\OneDrive\\Desktop\\M&M Learning\\Regression project 1\\cbb.csv")

#response variable creation
cbb=cbb%>%
  mutate(win_perc=W/G)
pairs=cbb%>%
  select("3P_O","2P_O","3P_D","2P_D","BARTHAG","win_perc")

#Pairs plots, one with win percentage
ggpairs(cbb[3:7])
ggpairs(pairs)

#Training and Testing Data
trainingInd=sample(1:3523,2466)
train_data=cbb[trainingInd,]
test_data=cbb[-trainingInd,]

#Fitting linear model
lin_mod=lm(win_perc~BARTHAG,data=train_data)
summary(lin_mod)
train_data%>%
  ggplot(aes(x=BARTHAG,y=win_perc))+
  geom_point()+
  geom_smooth(method="lm")

write.csv(cbb, "cbb.csv")
#5

#%in% checks if the conference name matches ANY of the values in the list
#Returns TRUE if the team is in ACC, B10, B12, P12, or SEC
#Returns FALSE for all other conferences
cbb$power_5 = ifelse(cbb$CONF %in% c("ACC", "B10", "B12", "P12", "SEC"), "Power 5", "Non-Power 5")

#If TRUE (team is in Power 5 conference) then assigns "Power 5"
#If FALSE (team is not in Power 5 conference) then assigns "Non-Power 5"
# Create power_5 variable in test data
test_data$power_5 = ifelse(test_data$CONF %in% c("ACC", "B10", "B12", "P12", "SEC"), "Power 5", "Non-Power 5")

#fit the parallel lines model
parallel_mod = lm(win_perc ~ BARTHAG + power_5, data=train_data)
summary(parallel_mod)

#plot
train_data %>%
  ggplot(aes(x=BARTHAG, y=win_perc, color=power_5)) +
  geom_point() +
  geom_parallel_slopes() +
  theme_minimal() +
  labs(x="BARTHAG", y="Win Percentage", title="Parallel Lines Model: Win % vs BARTHAG by Conference Type")


#6
# (0.2122085-0.1581129) + 0.6759296x    Power 5 equation
# (0.2122085+0.6759296) + 0.6759296x    Non Power 5 equation

#HW 5
#1
library(caret)
library(rpart)  #library for CART
library(rpart.plot)

trctrl_1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

caretTree  <-  train(win_perc ~ BARTHAG + power_5 + ADJ_T + SEED + WAB, 
                     data = train_data, 
                     method = "rpart",
                     trControl=trctrl_1,
                     tuneGrid = expand.grid(cp=seq(0.001, 0.1, 0.001))
)
plot(caretTree)


rpart.plot(caretTree$finalModel)


#2
trctrl_2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10)

caretLM<-  train(win_perc ~ BARTHAG + power_5 + ADJ_T + SEED + WAB, 
                 data = train_data, 
                 method = "lm",
                 trControl=trctrl_2
)
summary(caretLM)

#3


caretLM$results

caretTree$results[caretTree$results$cp==caretTree$bestTune[1,1], ]

"The LM had a lower RMSE"

#4) Discuss.  Which variable(s) were most used in the splits? Were there any 
#variables that you were surprised about were more or less useful than you thought?
#Any variables used by the tree that had p-values > 0.05? Any other thoughts?
"I am not really suprised what variables showed up the most and least, I kind of
expeceted them to be used as much as they are. The variable used were WAB (Wins above Bubble)
seed, power_5 (in a power 5 conference), ADJ_T (Adjusted Tempo of possesion), 
and BARTHAG (power 5 rating)"

#-------------------------------



get_rmse_safely <- function(model_object, model_name) {
  tryCatch({
    if (model_name == "tree") {
      return(model_object$results[model_object$results$cp == model_object$bestTune[1,1], "RMSE"])
    } else {
      return(model_object$results$RMSE)
    }
  }, error = function(e) {
    message("Error extracting RMSE for ", model_name, ": ", e$message)
    return(NA)

# handle missing values by removing rows with NAs
train_data_complete <- train_data %>%
  drop_na()  # Remove any rows with NA values

# also clean the test data
test_data_complete <- test_data %>%
  drop_na()


# modified scatter plot matrix that excludes SEED to avoid high cardinality error
pairs_plot <- train_data_complete %>%
  select(win_perc, BARTHAG, ADJ_T) %>%  # Removed SEED
  GGally::ggpairs()
print(pairs_plot)

# cross-validation control
trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 10)

# fit polynomial regression model with BARTHAG^2 using the cleaned data
poly_model <- train(
  win_perc ~ poly(BARTHAG, 2) + power_5 + ADJ_T + SEED + POSTSEASON,
  data = train_data_complete,
  method = "lm",
  trControl = trctrl
)


summary(poly_model)

# extract RMSE from the results
poly_model$results



# Create comparison data frame
model_comparison <- data.frame(
  Model = c("Linear Model", "Decision Tree", "Polynomial"),
  RMSE = c(
    get_rmse_safely(caretLM, "lm"),
    get_rmse_safely(caretTree, "tree"),
    poly_model$results$RMSE
  )
)
print(model_comparison)

# Visualize the polynomial fit
ggplot(train_data_complete, aes(x = BARTHAG, y = win_perc, color = power_5)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  theme_minimal() +
  labs(
    title = "Polynomial Regression: Win % vs BARTHAG??",
    x = "BARTHAG",
    y = "Win Percentage",
    color = "Conference Type"
  )











