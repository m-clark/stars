library(mgcv)
set.seed(1234)
d = gamSim(1)
head(d)

library(ggplot2)
d %$% qplot(x2,y, geom='smooth')


modmat0 = gam(y~x0 + x1 + s(x2), data=d, fit=F)
str(modmat0, 1)
head(modmat0$X)
cor(modmat0$X[,-1], d$x2) #last column is the linear
lmtest  = lm(d$y~.-1, data=data.frame(modmat0$X))  # unpenalized fit
gamtest = gam(y~x0 + x1 + s(x2), data=d, fit=T)

qplot(d$x2,fitted(lmtest), geom='smooth') + geom_point(aes(y=d$y))
visreg::visreg(gamtest, scale='response')

# modmat = data.frame(modmat0$X[,c(1, ncol(modmat0$X), 2:(ncol(modmat0$X)-1))])
modmat = modmat0$X
head(modmat)

# library(brms)
# library(parallel)
#
# prior = get_prior(y~.-1, data=data.frame(modmat, y=d$y))   # see brm note on intercept
# prior = c(set_prior("normal(0,10)", class = "b", coef='X.Intercept.'),
#           set_prior("normal(0,10)", class = "b", coef='V2'),
#           set_prior("normal(0,5)", class = "b", coef='V3'),
#           set_prior("normal(0,5)", class = "b", coef='V4'),
#           set_prior("normal(0,5)", class = "b", coef='V5'),
#           set_prior("normal(0,5)", class = "b", coef='V6'),
#           set_prior("normal(0,5)", class = "b", coef='V7'),
#           set_prior("normal(0,5)", class = "b", coef='V8'),
#           set_prior("normal(0,5)", class = "b", coef='V9'),
#           set_prior("normal(0,5)", class = "b", coef='V10'))
#
# bayesmod0 = brm(y~.-1, data=data.frame(modmat, y=d$y),
#                 iter=6000, warmup=1000, thin=20, chains=4, cores=4,
#                 prior=prior)
# qplot(d$x2,fitted(bayesmod0)[,1], geom='smooth') + geom_point(aes(y=d$y))
#
# str(bayesmod0,1)
#
# modcode0 = make_stancode(y~.-1, data=data.frame(modmat, y=d$y), prior=prior)
#


library(rstan)
standat = list(X=as.matrix(modmat), Y=d$y, K=ncol(modmat), N=nrow(modmat))

modcode =
"
data {
  int<lower=1> N;                      // number of observations
  vector[N] Y;                         // response variable
  int<lower=1> K;                      // number of columns
  matrix[N, K] X;                      // design matrix
}

transformed data {
  matrix[N,3] Xlin;                    // parametric part
  matrix[N,K-3] Z;                     // basis function

  Xlin <- X[,1:3];
  Z <- X[,4:K];
}

parameters {
  vector[3] b;                         // fixed effects
  vector[K-3] g;                       // penalized coef
  real<lower=0> sigma;                 // residual SD
  real<lower=0> tau;                   // sd for g
}

transformed parameters {

}

model {
  vector[N] eta;  // linear predictor

  // compute linear predictor
  eta <- Xlin * b + Z * g;

  // prior specifications
  sigma ~ student_t(3, 0, 5);
  tau ~ student_t(3, 0, 1);
  b ~ normal(0, 10);
  g ~ normal(0, tau);

  // likelihood contribution
  Y ~ normal(eta, sigma);
}

generated quantities {
  real<lower=0> lambda;
  vector[N] Ypred;

  lambda <- sigma^2/tau^2;   # smoothing parameter
  Ypred  <- Xlin * b + Z * g;
}
"

modbayes_codecheck = stan(model_code=modcode, data=standat,
                          iter=1, chains=1)
# print(modbayes_codecheck, pars=c('b','g', 'sigma', 'tau', 'lambda'), digits=3)
# traceplot(modbayes_codecheck)
# pairs(modbayes_codecheck, pars=c('b','g'))
# pairs(modbayes_codecheck, pars=c('sigma','tau'))

# greater than 1 min+
modbayes = stan(model_code=modcode, fit=modbayes_codecheck, data=standat,
                iter=6000, warmup=1000, thin=20, chains=4, cores=4) #                control=list(adapt_delta=.85, max_treedepth=12))
print(modbayes, pars=c('b','g', 'sigma', 'tau', 'lambda'), digits=3)
coef(gamtest)

cbind(get_posterior_mean(modbayes, c('b','g'))[c(1,3:10, 2),5], coef(gamtest), coef(lmtest))

library(shinystan)
Y = d$y  # for posterior predictive check
YpredMeans = get_posterior_mean(modbayes, 'Ypred')[,5]
launch_shinystan(modbayes)


# visual comparisons
qplot(fitted(gamtest), Y, xlim=c(0,20))
qplot(YpredMeans, Y, xlim=c(0,20))
qplot(YpredMeans, fitted(gamtest))  # very odd pattern divergence
qplot(YpredMeans, geom='density') +
  geom_density(aes(x=fitted(gamtest)), color='blue') +
  geom_density(aes(x=Y), color='red')

qplot(YpredMeans, Y, geom='line', data=d) +
  geom_point(aes(y=Y), alpha=.2) +
  geom_line(aes(y=fitted(gamtest)), color='red')
summary(gamtest)$r.sq
cor(get_posterior_mean(modbayes, 'Ypred')[,5], d$y)^2


# check identity
modmatTest = predict(gamtest, 'lpmatrix', newdata=d)
head(modmatTest)
head(modmat0$X)
identical(modmat0$X,modmatTest)
