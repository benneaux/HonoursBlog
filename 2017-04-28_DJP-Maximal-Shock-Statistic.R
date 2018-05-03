require(KFAS)
library(magrittr)
# The first part of this code comes from the examples in the KFAS package.

# Example of local level model for Nile series
# This fits a basic univariate model (i.e. Y~1) where the variances of both
# the error terms (Q and H) are estimated. In the original paper they report
# that H = 15099 and Q = 1469, which you will see are the reported values in
# the model objects

# Specify the model framework: a basic model with the measurement and
# level variances not specified as we will estimate them in the next step.

modelNile<-SSModel(Nile ~ SSMtrend(1, Q = list(matrix(NA))),
                   H=matrix(NA))

# Now we fit the State Space model using the framework above.
modelNile<-fitSSM(inits=c(log(var(Nile)), log(var(Nile))),
                  model=modelNile,
                  method='BFGS',
                  control=list(REPORT=1, trace=1))$model

# Check H and Q
modelNile$H # DJP -> 15099
modelNile$Q # DJP -> 1469

### Filtering and state smoothing ##############################################

# Now we fit the Kalman Filter Smoother to the model

out<-KFS(modelNile,
         filtering = c("state"), # At least state. Not sure what else we could do.
         smoothing = c("state", # State errors
                       "disturbance", # Measurment errors
                       "mean"), # Need this to use rstandard("measurement") below
         simplify = FALSE) # need this to produce the N matrices.

#### Maximal Shock Statistic ###################################################

# Using the output from the KFS() call, we can now calculate the shock stats.
# From the equation 14 (definition of rho*^2) on page 801

v <- (out$v) # Prediction errors
F.mat = as.numeric(out$F) # Prediction error variances
rho1 = (v^2*F.mat^(-1))

r = out$r[1,] # Weighted sums of innovations
N.mat = as.numeric(out$N) # Covariances of Weighted sums of innovations
rho2 = (r^2 * N.mat^(-1))

# rho1 is shorter (100L) than rho2 (101L). I think that the package defines
# the terms in a way that means we should remove the first point in order to
# match them up but I could be wrong. Also, when I confirm that the calculations
# are correct with the plot (marked with (*) below) the lines match up this way.

# Equation 14 (The code is still a bit messy still a bit messy)
vals= c(0,c(rho1 + rho2[-1])[-1]) %>%
  ts(start = 1872, end =1970)
# PLOT 1: Figure 4 (page 799)
plot(vals, type = "l")

### Measurement and Level Shocks ###############################################

# rho2 is the Level Shocks
# (*)
# PLOT 2: Figure 3b (page 799) - Level Shocks
plot(ts(rho2[2:100], start = 1872, end = 1970), type = "l")
abline(v=1899)
abline(h = qchisq(0.95,df =2), col ="red")

# Measuremnent Shocks

# This one was a bit different because it doesn't come from the componets of the
# maximal shock statistic as clearly as the level shocks do.

y = out$model$y # data
muhat = out$muhat # level estimates
# run below to see
#####
# plot(y)
# lines(muhat, col = "red")
#####

H.mat = out$model$H[1] # variance of level (sigma_eps = 15099 in the paper)
V.mu = out$V_mu[1,,] # variance of level estimates

rho3 = (y - muhat)/(sqrt(H.mat - V.mu))

# PLOT 3: Figure 3a (page 799) - Measurement Shocks

plot(ts(rho3[2:100]^2, start = 1872, end = 1970), type = "l")
abline(v=c(1877,1913), col = "blue", lty = 2)
abline(h = qchisq(0.95,df =2), col ="red")

# PLOT 4: NOT Figure 3a: The other part of the maximal shock statistic equation
# (i.e. not rho2, which gives the level shocks) gives 'recursive' residuals. I
# think these are the residuals that most other people use when assessing KF SS
# models. They are t-distributed I think. They look kind of like the measurement
# shocks, but with a different term in the denomenator.

plot(ts(rho1[2:100], start = 1872, end = 1970), type = "l")
# lines(rho3^2, col = "red") # uncomment to compare with the measurement shocks.
abline(h = qchisq(0.95,df =2), col ="red") # more peaks are significant: one in
                                           # 1899 (?) and  1917 (?).


### RESIDUALS in KFAS ##########################################################

# KFAS adds code to the rstandard() function that let's you pull out the
# standardised residuals from the model object. It lets you select from
# a number of different types: recursive, pearson and state. From below, you can
# see that they match

plot(cbind(
  recursive = abs(rstandard(out))^2, # PLOT 4.
  measurement = abs(rstandard(out, "pearson")^2), # PLOT 3
  state = abs(rstandard(out, "state"))^2), # PLOT 2
  main = "recursive and auxiliary residuals")

### Confirm plots ##############################################################


# Confirm PLOT 2
plot(ts(rho2[2:100], start = 1872, end = 1970), type = "l")
lines(ts(rstandard(out, "state")^2, start = 1872, end = 1970), col = "red")

# Confirm PLOT 3
plot(ts(rho3[2:100]^2, start = 1872, end = 1970), type = "l")
lines(ts(rstandard(out, "pearson")^2, start = 1871, end = 1970), col = "red")

# Confirm PLOT 4:
plot(ts(rho1[2:100], start = 1872, end = 1970), type = "l")
lines(ts(rstandard(out, "recursive")^2, start = 1871, end = 1970),col = "red")


##### Additional info ##########################################################

# When I was trying to figure out the measurement shocks I copied some of the
# code from the rstandard() function in base R (stats package).
# They KFAS authors call them 'Pearson Residuals' because:
## "For Gaussian models, these coincide with the standardized smoothed ε
## disturbance residuals, and for generalized linear models these coincide
## with the standardized Pearson residuals." (see rstandard.KFS() for more).

# pearson_standardized <- function(object) {
#   n <- attr(object$model, "n") # length
#   res <- object$model$y - object$muhat # Diff between observed and expected val.
#   # Note: errors will occur if NA values are present or sum of resids is < 0.
#   for(t in 1:n){
#     D <- sqrt(object$model$H[[1]] - object$V_mu[[t]])
#     res[[t]] <- res[[t]] / D
#   }
#   res^2
# }
#
# rho3b = pearson_standardized(out)
# plot(rho3b)
# lines(rho3^2, lty = 2, col = "red")
