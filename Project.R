data <- read.csv("DiscrDat.csv", header = TRUE, sep = " ")

summary(data)

nrow(data)
# sample size = 350


################### SEPARATING TRAINING SET AND THE TEST SET ###################
# Age groups:
# 0: < 30
# 1: 30 <=
# 3: Unknown

# Training set is where the age is known
training_set <- data[data$Age != 3,]
# Test set is the rest of the data
test_set <- data[data$Age == 3,]

colnames(training_set)

# Splitting the training set by age group
training_set_group1 <- training_set[training_set$Age == 0, 1:4]
training_set_group2 <- training_set[training_set$Age == 1, 1:4]


################### PLOTTING THE VARIABLES ###################

par(mfrow = c(2, 2))
qqplot(training_set_group1$Book, training_set_group2$Book, pch = 16)
qqplot(training_set_group1$Electr, training_set_group2$Electr, pch = 16)
qqplot(training_set_group1$Cloth, training_set_group2$Cloth, pch = 16)
qqplot(training_set_group1$House, training_set_group2$House, pch = 16)
par(mfrow = c(1, 1))

################### ESTIMATING THE PARAMETERS OF THE MODEL ###################

# Number of observations in group 1
nrow(training_set_group1)
# Total number of observations
nrow(training_set)

# Proportion of group 1
# We use this to estimate p
p_hat <- nrow(training_set_group1) / nrow(training_set)
p_hat

# covariance matrix of training_set
# We use this to estimate Sigma
Sigma_hat <- cov(training_set[,1:4])
Sigma_hat


# mean of each dimension of group1
# We use this to estimate mu1
mu1_hat <- colMeans(training_set_group1)
mu1_hat


# mean of each dimension of group2
# We use this to estimate mu2
mu2_hat <- colMeans(training_set_group2)
mu2_hat


################### CLASSIFYING OBSERVATIONS ###################

classify <- function(mu_1, mu_2, Sigma, p, y) {
    # mu_1: population mean of class 1
    # mu_2: population mean of class 2
    # Sigma: covariance matrix
    # p: probability of class 1
    # y: observation(s) to classify

    # y is given as a matrix where the rows are the dimensions 
    # and the columns are the observations
    y <- t(y)

    Sigma_inv <- solve(Sigma)

    xi <- Sigma_inv %*% (mu_1 - mu_2)
    m <- (mu_1 + mu_2) / 2
    c <- log(p / (1 - p))

    # For reasons of efficiency, we made this function work for 
    # multiple observations at once

    # result vector
    res <- c(rep(0, ncol(y)))

    for(i in seq_len(ncol(y))) {
        # logical vector with 0 for class 1 and 1 for class 2
        res[i] <- t(xi) %*% (y[,i] - m) + c < 0
    }

    return(as.numeric(res))
}


# Classifying the training set
training_set_classification <- classify(mu1_hat, mu2_hat, Sigma_hat, p_hat, training_set[,1:4])

# How many we get correctly
sum(training_set_classification == training_set$Age)
nrow(training_set)

sum(training_set_classification == training_set$Age) / nrow(training_set)


# Some analyses
lm_thing <- lm(training_set_classification ~ training_set$Age)
summary(lm_thing)


# plot fitted residuals
plot(lm_thing)


t.test(training_set_classification, training_set$Age)
t.test(training_set_classification == training_set$Age, 
rep(0, length(training_set_classification)))



# Applying to test set
test_set_classification <- classify(mu1_hat, mu2_hat, Sigma_hat, p_hat, test_set[,1:4])

test_set_classification
