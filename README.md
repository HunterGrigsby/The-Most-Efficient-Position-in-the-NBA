# progress report 4

# problem 1: Categorical Variable
nba_fg_vector <- NBA2024$FGP
NBA2024$FGP
 
# Assuming nba_fg_vector contains numeric values

categorical_variable <- cut(nba_fg_vector,
                            breaks = c(0, 0.300, 0.400, 0.500, 1.000),
                            labels = c("low", "average","high", "very high"))
print(categorical_variable)

# A: change numerical variable into categorical variable
category_vector <- c(NBA2024$Pos)
custom_levels <- c("PG", "SG", "SF","PF","C" )
factor_variable <- factor(category_vector, levels = custom_levels)

print(factor_variable)

#B: 
MLR_NBA <- lm(FGP ~ Pos + `3PP`, data = NBA2024)
summary(MLR_NBA)

# problem 2: interaction term
# A:
interaction_term <- lm(FGP ~ Pos * PTS * `3PP`, NBA2024)
summary(interaction_term)

# problem 3: machine learning
# A:
# Sampling size: select train (80%) and test (20%) randomly
split_size <- 0.7
sample_size <- floor(split_size * nrow(NBA2024))
set.seed(10)
train_indices <- sample(seq_len(nrow(NBA2024)), size = sample_size)
# Recognizing Train vs Test
NBA2024$type = ifelse(1:nrow(NBA2024) %in% train_indices, "train", "test")
# Splitting dataframe
train <- NBA2024[train_indices, ]
test <- NBA2024[-train_indices, ]  

#B:

sqrt(sum(train$FGP - train$`3PP`) ** 2 / nrow(train)) # RMSE
summary(MLR_NBA)$r.squared # R^2
summary(MLR_NBA)$adj.r.squared # Adjusted Rˆ2

# C
test$output <- predict(MLR_NBA, test[, c("FGP", "Pos","3PP")])
test$output

# D:
sqrt(sum(test$FGP - test$`3PP`) ** 2 / nrow(test)) # RMSE
test.m <- lm(FGP ~ Pos + `3PP`, data = test)
summary(test.m)$r.squared # R^2
summary(test.m)$adj.r.squared # Adjusted Rˆ2

full_model <- lm(FGP~ Pos + `3PP` + FTP + PTS, data = NBA2024)
stepwise_model <- step(full_model)

summary(full_model, scientific = TRUE)

full <- lm(FGP ~ factor(Pos)  + `3PP` + FTP + PTS, data = NBA2024)
summary(full)

install.packages("car")
library(car)
vif(full)


ci95 = predict(full, NBA2024, interval = "confidence")
ci95


# Fit the linear regression model
model <- lm(FGP ~ Pos + `3PP` + FTP + PTS, data = NBA2024)

# Print the coefficients
summary(model)

# Interpretation of coefficients
