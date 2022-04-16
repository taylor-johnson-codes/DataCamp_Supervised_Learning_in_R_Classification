# regression involves predicting an outcome (y: dependent variable) using 1+ predictors (x)
# linear regression fits a straight line that best captures the relationship between the x and y terms
# for a binary y outcome (e.g. 0 and 1), logistic regression function says for any input of x, the output is always between 0 and 1
# Because a logistic regression model estimates the probability of the outcome, it is up to you to determine the threshold at which the probability implies action. 
# m <- glm(y ~ x1 + x2, data = dataset, family = "binomial")
# prob <- predict(m, test_dataset, type = "response")
# pred <- ifelse(prob > 0.5, 1, 0)

library(pROC)

# THE DATASET ISN'T LOADED HERE SO THE CODE WON'T WORK

# Examine the dataset to identify potential independent variables
str(donors)

# Explore the dependent variable
table(donors$donated)

# Build the donation model
donation_model <- glm(donated ~ bad_address + interest_religion + interest_veterans, 
                      data = donors, family = "binomial")

# Summarize the model results
summary(donation_model)

# Estimate the donation probability
donors$donation_prob <- predict(donation_model, type = "response")

# Find the donation probability of the average prospect
mean(donors$donated)

# Predict a donation if probability of donation is greater than average
donors$donation_pred <- ifelse(donors$donation_prob > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donated == donors$donation_pred)


# Area Under the Curve (AUC) when plotting ROC curve
# AUC results are usually between 0.5 and 1, where 1 is the best model

# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)


# glm() will automatically dummy-code any factor-type variables

# Convert the wealth rating to a factor
donors$wealth_levels <- factor(donors$wealth_rating, levels = c(0, 1, 2, 3), labels = c("Unknown", "Low", "Medium", "High"))

# Use relevel() to change reference category
donors$wealth_levels <- relevel(donors$wealth_levels, ref = "Medium")

# See how our factor coding impacts the model
summary(glm(donated ~ wealth_levels, data = donors, family = "binomial"))


# R will exclude any cases with NA values when building a regression model.
# One workaround is to replace, or impute, the missing values with an estimated value. 
# After doing so, you may also create a missing data indicator to model the possibility that cases with missing data are different in some way from those without.

# Find the average age among non-missing values
summary(donors$age)

# Impute missing age values with the mean age
donors$imputed_age <- ifelse(is.na(donors$age), round(mean(donors$age, na.rm = TRUE), 2), donors$age)

# Create missing value indicator for age
donors$missing_age <- ifelse(is.na(donors$age), 1, 0)

# Build a recency, frequency, and money (RFM) model
rfm_model <- glm(donated ~ recency * frequency + money, data = donors, family = "binomial")

# Summarize the RFM model to see how the parameters were coded
summary(rfm_model)

# Compute predicted probabilities for the RFM model
rfm_prob <- predict(rfm_model, data = donors, type = "response")

# Plot the ROC curve for the new model
ROC <- roc(donors$donated, rfm_prob)
plot(ROC, col = "red")
auc(ROC)


# In the absence of subject-matter expertise, stepwise regression can assist with the search for the most important predictors of the outcome of interest.
# In this exercise, you will use a forward stepwise approach to add predictors to the model one-by-one until no additional benefit is seen

# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = donors, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = donors, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# Estimate the stepwise donation probability
step_prob <- predict(step_model, type = "response")

# Plot the ROC of the stepwise model
ROC <- roc(donors$donated, step_prob)
plot(ROC, col = "red")
auc(ROC)