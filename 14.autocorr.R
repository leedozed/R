datadir = "http://econ.korea.ac.kr/~chirokhan/book/data"
Death = read.csv(file.path(datadir, "deathrate.csv"))
# head(Death, h=10)
model = deathrate ~ drink + smoke + aged + vehipc + factor(year)
ols = lm(model, data = Death)
coeftest(ols)
coeftest(ols, vcov = vcovHC)

#클러스터로 인한 자기상관 해결
# install.packages('multiwayvcov')
library(multiwayvcov)
# CC0
coeftest(ols, vcov = cluster.vcov, cluster = ~region, df_correction = F)
coeftest(ols, vcov = cluster.vcov, cluster = ~region, df_correction = T)
#                    Estimate Std. Error t value  Pr(>|t|)
# (Intercept)      -0.2241278  1.0191647 -0.2199 0.8261175
# drink             0.0063935  0.0141033  0.4533 0.6506992
# smoke             0.0332761  0.0193888  1.7163 0.0873495 .
# aged              0.4026956  0.0135248 29.7745 < 2.2e-16 ***
# vehipc            1.4079470  1.7245757  0.8164 0.4150444
# factor(year)2009 -0.3787601  0.0765451 -4.9482 1.374e-06 ***
# factor(year)2010 -0.3509959  0.0989481 -3.5473 0.0004644 ***


# Newey-West 추정량: 시계열 자료의 자기상관 보정
coeftest(ols, vcov = vcovHAC)
#                    Estimate Std. Error t value  Pr(>|t|)
# (Intercept)      -0.2241278  0.9584196 -0.2339 0.8152909
# drink             0.0063935  0.0128423  0.4978 0.6190273
# smoke             0.0332761  0.0187477  1.7749 0.0771189 .
# aged              0.4026956  0.0121669 33.0976 < 2.2e-16 ***
# vehipc            1.4079470  1.8109463  0.7775 0.4376168
# factor(year)2009 -0.3787601  0.0695324 -5.4472 1.219e-07 ***
# factor(year)2010 -0.3509959  0.1038244 -3.3807 0.0008384 ***

set.seed(101)
x = rep(c(-1,1), 50)
u = rnorm(100)
y = 1 + x + u
library(lmtest)
dwtest(y ~ x)
