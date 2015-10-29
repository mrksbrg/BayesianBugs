library(rjags)
# Bayesian regression model: incoming bug reports during a specific month
#
# summer: 1 = July, August (holiday season)
# test: 1 = formal verification phase ongoing
# release: 1 = the product has been released

# PROJECT SV5_0
bugs_sv5 <- read.csv("ProjectX.csv")
bugs_sv5 <- read.csv("ProjectX.1.csv")
bugs_sv5 <- read.csv("ProjectX.2.csv")

#define the model
model.txt <- "model {
#likelihood
y[1] ~ dpois(lambda[1])
log(lambda[1]) <- intercept + size_coeff*sys_size[1] + is_summer[1]*summer_coeff + in_test[1]*test_coeff + released[1]*rel_coeff + x[1]*beta
for (i in 2:length(y)) {
#likelihood
y[i] ~ dpois(lambda[i])
log(lambda[i]) <- intercept + alpha*log(lambda[i-1])+size_coeff*sys_size[i] + is_summer[i]*summer_coeff + in_test[i]*test_coeff + released[i]*rel_coeff + x[i]*beta
}

#priors
intercept ~ dnorm(0,0.01)
size_coeff ~ dnorm(0,0.01)
summer_coeff ~ dnorm(-5,0.1) # Lower no. reports in summer
test_coeff ~ dnorm(25,0.1) # Many more bugs when in formal testing phase
rel_coeff ~ dnorm(5,0.1) # More bugs when the product has been released
alpha ~ dnorm(0,0.1)
beta ~ dnorm(0,0.1) 

# Do some predictions

# 13 predictions
log(lambda_pred_30) <- intercept + alpha*log(2) + size_coeff*1.05 + 30*beta
pred_30 ~ dpois(lambda_pred_30)
log(lambda_pred_35) <- intercept + alpha*log(1) + size_coeff*1.059 + 35*beta
pred_35 ~ dpois(lambda_pred_35)
log(lambda_pred_40) <- intercept + alpha*log(51) + size_coeff*1.067 + 40*beta
pred_40 ~ dpois(lambda_pred_40)
log(lambda_pred_45) <- intercept + alpha*log(27) + size_coeff*1.076 + 45*beta
pred_45 ~ dpois(lambda_pred_45)
log(lambda_pred_50) <- intercept + alpha*log(24) + size_coeff*1.084 + 1*summer_coeff + 50*beta
pred_50 ~ dpois(lambda_pred_50)
log(lambda_pred_55) <- intercept + alpha*log(28) + size_coeff*1.093 + 55*beta
pred_55 ~ dpois(lambda_pred_55)
log(lambda_pred_60) <- intercept + alpha*log(118) + size_coeff*1.1 + 1*test_coeff + 60*beta
pred_60 ~ dpois(lambda_pred_60)
log(lambda_pred_65) <- intercept + alpha*log(279) + size_coeff*1.1 + 1*test_coeff + 65*beta
pred_65 ~ dpois(lambda_pred_65)
log(lambda_pred_70) <- intercept + alpha*log(561) + size_coeff*1.1 + 1*test_coeff + 70*beta
pred_70 ~ dpois(lambda_pred_70)
log(lambda_pred_75) <- intercept + alpha*log(232) + size_coeff*1.1 + 1*test_coeff + 75*beta
pred_75 ~ dpois(lambda_pred_75)
log(lambda_pred_80) <- intercept + alpha*log(70) + size_coeff*1.1 + 1*rel_coeff + 80*beta
pred_80 ~ dpois(lambda_pred_80)
log(lambda_pred_85) <- intercept + alpha*log(6) + size_coeff*1.1 + 1*rel_coeff + 85*beta
pred_85 ~ dpois(lambda_pred_85)
log(lambda_pred_90) <- intercept + alpha*log(3)+ size_coeff*1.1 + 1*rel_coeff + 90*beta
pred_90 ~ dpois(lambda_pred_90)

}"

data <- list(sys_size=bugs_sv5$size, is_summer=bugs_sv5$summer, in_test=bugs_sv5$test, released=bugs_sv5$release, y=bugs_sv5$bugCount, x=bugs_sv5$month)
inits <- list("intercept"=10)

model.jags <- jags.model(textConnection(model.txt),
                         data=data, inits=inits, n.adapt=1000)

model_vars<- c("intercept", "alpha", "beta", "size_coeff", "summer_coeff", "test_coeff", "rel_coeff")
pred_vars <- c("pred_30", "pred_35", "pred_40",  "pred_45", "pred_50", "pred_55", "pred_60", "pred_65", "pred_70", "pred_75", "pred_80", "pred_85", "pred_90")
samples <- coda.samples(model.jags, c(model_vars,pred_vars) , n.iter=1e6)
samples_matrix <- as.matrix(samples)
print(summary(samples_matrix))
plot(samples)
qq <- quantile(samples_matrix)
quantile(samples_matrix[,"pred_35"], c(0.45, 0.55))
