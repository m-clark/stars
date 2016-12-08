functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // centered population-level design matrix
  vector[K] X_means;  // column means of X before centering
  int ns;  // number of splines terms
  int knots[ns];  // number of knots per spline
  // design matrix of spline s(x0)
  matrix[N, knots[1]] Zs_1;
  // design matrix of spline s(x1)
  matrix[N, knots[2]] Zs_2;
  // design matrix of spline s(x2)
  matrix[N, knots[3]] Zs_3;
  // design matrix of spline s(x3)
  matrix[N, knots[4]] Zs_4;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K] b;  // population-level effects
  real temp_Intercept;  // temporary Intercept
  // parameters of spline s(x0)
  vector[knots[1]] zs_1;
  real<lower=0> sds_1;
  // parameters of spline s(x1)
  vector[knots[2]] zs_2;
  real<lower=0> sds_2;
  // parameters of spline s(x2)
  vector[knots[3]] zs_3;
  real<lower=0> sds_3;
  // parameters of spline s(x3)
  vector[knots[4]] zs_4;
  real<lower=0> sds_4;
  real<lower=0> sigma;  // residual SD
}
transformed parameters {
  vector[knots[1]] s_1;
  vector[knots[2]] s_2;
  vector[knots[3]] s_3;
  vector[knots[4]] s_4;
  vector[N] eta;  // linear predictor
  s_1 <- sds_1 * zs_1;
  s_2 <- sds_2 * zs_2;
  s_3 <- sds_3 * zs_3;
  s_4 <- sds_4 * zs_4;
  // compute linear predictor
  eta <- X*b + Zs_1*s_1 + Zs_2*s_2 + Zs_3*s_3 + Zs_4*s_4 + temp_Intercept;
}
model {
  // prior specifications
  b ~ normal(0,10);
  zs_1 ~ normal(0, 1);
  sds_1 ~ student_t(3, 0, 10);
  zs_2 ~ normal(0, 1);
  sds_2 ~ student_t(3, 0, 10);
  zs_3 ~ normal(0, 1);
  sds_3 ~ student_t(3, 0, 10);
  zs_4 ~ normal(0, 1);
  sds_4 ~ student_t(3, 0, 10);
  sigma ~ student_t(3, 0, 10);
  // likelihood contribution
  if (!prior_only) {
    Y ~ normal(eta, sigma);
  }
}
generated quantities {
  real b_Intercept;  // population-level intercept
  b_Intercept <- temp_Intercept - dot_product(X_means, b);
}