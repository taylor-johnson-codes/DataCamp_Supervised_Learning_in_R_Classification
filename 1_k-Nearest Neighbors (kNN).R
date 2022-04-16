# kNN is an algorithm that uses the principal of nearest neighbors to classify unlabeled examples
# "k" is a variable that specifies the number of neighbors to consider when making the classification (default value for k is 1)
# by default, R's knn function searches a dataset for the observation most similar to the newly observed one
# knn(training_data, testing_data, training_labels) - knn() is in the class library
# knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

library(class)

# THE DATASET ISN'T LOADED HERE SO THE CODE WON'T WORK

# Create a vector of labels
sign_types <- c(signs$sign_type)

# Classify the next sign observed
knn(train = signs[-1], test = next_sign, cl = sign_types)

# Examine the structure of the signs dataset
str(signs)

# Count the number of signs of each type
table(signs$sign_type)

# Check r10's average red level by sign type
aggregate(r10 ~ sign_type, data = signs, mean)

# Use kNN to identify the test road signs
sign_types <- signs$sign_type
signs_pred <- knn(train = signs[-1], test = test_signs[-1], cl = sign_types)

# Create a confusion matrix of the predicted versus actual values
signs_actual <- test_signs$sign_type
table(signs_pred, signs_actual)

# Compute the accuracy
mean(signs_pred == signs_actual)

# Compute the accuracy of the baseline model (default k = 1)
k_1 <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types)
mean(signs_actual == k_1)

# Modify the above to set k = 7
k_7 <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types, k = 7)
mean(signs_actual == k_7)

# Set k = 15 and compare to the above
k_15 <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types, k = 15)
mean(signs_actual == k_15)


# When multiple nearest neighbors hold a vote, it can sometimes be useful to examine whether the voters were unanimous or widely separated.
# For example, knowing more about the voters' confidence in the classification could allow an autonomous vehicle to use caution in the case there is any chance at all that a stop sign is ahead.

# Use the prob parameter to get the proportion of votes for the winning class
sign_pred <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types, k = 7, prob = TRUE)

# Get the "prob" attribute from the predicted classes
sign_prob <- attr(sign_pred, "prob")

# Examine the first several predictions
head(sign_pred)

# Examine the proportion of votes for the winning class
head(sign_prob)


# Before applying kNN to a classification task, it is common practice to rescale the data using a technique like min-max normalization.