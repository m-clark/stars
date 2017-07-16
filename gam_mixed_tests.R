library(microbenchmark); library(mgcv); library(lme4)


# mod_lme4 = lmer(Reaction ~ Days + (1|Subject) + (0 + Days|Subject), sleepstudy, REML=F)
# mod_gamm = gam(Reaction ~ Days + s(Subject, bs='re') + s(Days, Subject, bs='re'), data=sleepstudy, method='ML')

testdat = gamSim(6, n=1000)
str(testdat)

# lme4 faster, method doesn't really matter
# microbenchmark(lme4 = lmer(Reaction ~ Days + (1|Subject) + (0 + Days|Subject), sleepstudy, REML=F),
#                gam = gam(Reaction ~ Days + s(Subject, bs='re') + s(Days, Subject, bs='re'), data=sleepstudy, method='ML'))
microbenchmark(lme4 = lmer(Reaction ~ Days + (1|Subject) + (0 + Days|Subject), sleepstudy, REML=T),
               gam = gam(Reaction ~ Days + s(Subject, bs='re') + s(Days, Subject, bs='re'), data=sleepstudy, method='REML'))

# GAM penalized ML faster
microbenchmark(lme4 = lmer(y ~ x0 + (1|fac) + (0 + x0|fac), testdat),
               gam = gam(y ~ x0 + s(fac, bs='re') + s(x0, fac, bs='re'), data=testdat), times=20)

# lme4 faster
library(gamm4)
microbenchmark(lme4 = lmer(y ~ x0 + (1|fac) + (0 + x0|fac), testdat),
               gam = gamm4(y ~ x0, data=testdat,random=~(1|fac) + (0 + x0|fac)), times=20)
