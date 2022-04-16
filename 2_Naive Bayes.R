# Bayesian methods: a branch of statistics that estimates probabilities in light of historic data
# Naive Bayes is an algorithm that applies Bayesian methods to estimate the conditional probability of an outcome
# naive_bayes(location ~ time_of_day, data = location_history) - predict users' location based on time of day

library(naivebayes)

# THE DATASET ISN'T LOADED HERE SO THE CODE WON'T WORK

# Compute P(A) 
p_A <- nrow(subset(where9am, location == "office")) / nrow(where9am)

# Compute P(B)
p_B <- nrow(subset(where9am, daytype == "weekday")) / nrow(where9am)

# Compute the observed P(A and B)
p_AB <- nrow(subset(where9am, location == "office" & daytype == "weekday")) / nrow(where9am)

# Compute P(A | B) and print its value
p_A_given_B <- p_AB / p_B
p_A_given_B


# Build the location prediction model
locmodel <- naive_bayes(location ~ daytype, data = where9am)

# Predict Thursday's 9am location
predict(locmodel, thursday9am)

# Predict Saturdays's 9am location
predict(locmodel, saturday9am)


# Typing the name of the model object provides the a priori (overall) and conditional probabilities of each of the model's predictors. 
# If one were so inclined, you might use these for calculating posterior (predicted) probabilities by hand.
# Alternatively, R will compute the posterior probabilities for you if the type = "prob" parameter is supplied to the predict() function.

# Examine the location prediction model
locmodel

# Obtain the predicted probabilities for Thursday at 9am
predict(locmodel, thursday9am, type = "prob")

# Obtain the predicted probabilities for Saturday at 9am
predict(locmodel, saturday9am, type = "prob")


# Understanding the idea of event independence will become important as you learn more about how "naive" Bayes got its name.
# Independent events: Knowing the outcome of one event does not help predict the other.

# The Naive Bayes algorithm got its name because it makes a "naive" assumption about event independence.
# It's purpose of making this assumption: The joint probability calculation is simpler for independent events.

# Build a NB model of location
locmodel <- naive_bayes(location ~ daytype + hourtype, data = locations)

# Predict Brett's location on a weekday afternoon
predict(locmodel, weekday_afternoon)

# Predict Brett's location on a weekday evening
predict(locmodel, weekday_evening)


# While Brett was tracking his location over 13 weeks, he never went into the office during the weekend. Consequently, the joint probability of P(office and weekend) = 0.
# Explore how this impacts the predicted probability that Brett may go to work on the weekend in the future. Additionally, you can see how using the Laplace correction will allow a small chance for these types of unforeseen circumstances.
# (multiplying anything by 0 equals 0; Laplace fixes this issue in models)

# Observe the predicted probabilities for a weekend afternoon
predict(locmodel, weekend_afternoon, type = "prob")

# Build a new model using the Laplace correction
locmodel2 <- naive_bayes(location ~ daytype + hourtype, data = locations, laplace = 1)

# Observe the new predicted probabilities for a weekend afternoon
predict(locmodel2, weekend_afternoon, type = "prob")


# binning technique: creating categories from numeric data; divide a range of numbers into a series of sets called "bins"
# e.g. morning/afternoon/evening; cold/warm/hot