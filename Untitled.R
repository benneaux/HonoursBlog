modelNile <- SSModel(Nile ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA))
modelNile <- fitSSM(inits = c(log(var(Nile)),log(var(Nile))), model = modelNile,
                    method = "BFGS")$model
# Filtering and state smoothing
out <- KFS(modelNile, smoothing = c("state", "mean", "disturbance"))

plot(cbind(
  recursive = rstandard(out, "recursive"),
  irregular = rstandard(out, "pearson")^2,
  state = rstandard(out, "state")^2,
  main = "recursive and auxiliary residuals"))

