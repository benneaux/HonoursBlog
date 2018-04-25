require(KFAS)

# Example of local level model for Nile series
modelNile<-SSModel(Nile~SSMtrend(1,Q=list(matrix(NA))),H=matrix(NA))
modelNile
modelNile<-fitSSM(inits=c(log(var(Nile)),log(var(Nile))),model=modelNile,
                  method='BFGS',control=list(REPORT=1,trace=1))$model

# Can use different optimisation:
# should be one of “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
modelNile<-SSModel(Nile~SSMtrend(1,Q=list(matrix(NA))),H=matrix(NA))
modelNile
modelNile<-fitSSM(inits=c(log(var(Nile)),log(var(Nile))),model=modelNile,
                  method='L-BFGS-B',control=list(REPORT=1,trace=1))$model

# Filtering and state smoothing
out<-KFS(modelNile,filtering = c("state","signal"),smoothing=c("state", "signal", "disturbance"), simplify = FALSE)
inn = mvInnovations(out)
test = drop(t(out$v)*1/(out$F))*out$v
test2 = drop(t(out$r)*1/(c(out$N)))*out$r
u = drop(1/out$F)*out$v - drop(out$K)*out$r[1:100]
m = drop(1/out$F) + drop(out$K)*c(out$N)*drop(out$K)
vals=drop(test + test2[-1])
plot(vals[-1], type = "l")
plot(test2[-1], type = "l")

plot(abs(rstandard(out)[-1]), type = "l")
plot(m[2:101], type = "l")

ts.plot(cbind(out$model$y, fitted(out)),lty = 1:2, col = 1:2,
        main = 'Observations and smoothed signal with and without seasonal component')
lines(signal(out, states = c('regression', 'trend'))$signal, col = 4, lty = 1)
legend('bottomleft',
       legend = c('Observations', 'Smoothed signal','Smoothed level'),
       col = c(1, 2, 4), lty = c(1, 2, 1))
x = signal(out, states = c('regression', 'trend'))
