library(Ecdat)

data(Housing, package = 'Ecdat')
ols = lm(log(price) ~ log(lotsize) + bedrooms + bathrms, data = Housing)
summary(ols)

# sandwich: R에서 분산 관련 라이브러리
# install.packages("sandwich")
library(sandwich)
# lmtest 내 coeftest가 포함됨, OLS 변동 test 함수가 포함됨 패키지
# install.packages("lmtest")
library(lmtest)

# HC를 보정함
# HC 보정 이후 b값은 동일하지만 std는 증가함 -> t값 감소 -> 기각 확률 감소

coeftest(ols, vcov=vcovHC, type = 'HC0')
#              Estimate Std. Error t value  Pr(>|t|)
# (Intercept)  6.622221   0.247946 26.7084 < 2.2e-16 ***
# log( lotsize) 0.456821   0.030110 15.1717 < 2.2e-16 ***
# bedrooms     0.089237   0.017850  4.9993 7.781e-07 ***
# bathrms      0.236758   0.026392  8.9707 < 2.2e-16 ***

coeftest(ols, vcov=vcovHC, type = 'HC3')
#              Estimate Std. Error t value  Pr(>|t|)
# (Intercept)  6.622221   0.250780 26.4065 < 2.2e-16 ***
# log(lotsize) 0.456821   0.030464 14.9954 < 2.2e-16 ***
# bedrooms     0.089237   0.018122  4.9243 1.125e-06 ***
# bathrms      0.236758   0.026834  8.8231 < 2.2e-16 ***


#  그래프로 이분산 여부 확인
plot(ols$residuals ~ Housing$bedrooms)

# 이분산의 표준오차 test
se = sqrt(diag(vcov(ols)))
hse0 = sqrt(diag(vcovHC(ols, type = "HC0")))
hse1 = sqrt(diag(vcovHC(ols, type = "HC1")))
hse2 = sqrt(diag(vcovHC(ols, type = "HC2")))
hse3 = sqrt(diag(vcovHC(ols, type = "HC3")))
hse4 = sqrt(diag(vcovHC(ols, type = "HC4")))

# round: 소수점 5자리에서 반올림
round(cbind(se, hse0, hse1, hse2, hse3, hse4), 5)
#                   se    hse0    hse1    hse2    hse3    hse4
# (Intercept)  0.24123 0.24795 0.24886 0.24936 0.25078 0.25090
# log(lotsize) 0.02898 0.03011 0.03022 0.03029 0.03046 0.03049
# bedrooms     0.01651 0.01785 0.01792 0.01798 0.01812 0.01822
# bathrms      0.02448 0.02639 0.02649 0.02661 0.02683 0.02709

# p354. 지역별 사망률
datadir = "http://econ.korea.ac.kr/~chirokhan/book/data"
Death = read.csv(file.path(datadir, "deathrate.csv"))
model = deathrate ~ drink + smoke + aged + vehipc + factor(year)
ols = lm(model, data = Death)
coeftest(ols)
# 이분산성 HC3 반영
coeftest(ols, vcov = vcovHC)

# install.packages('car')
library(car)
lht(ols, c("drink", "smoke"))
#   Res.Df    RSS Df Sum of Sq      F  Pr(>F)
# 1    253 97.062
# 2    251 94.613  2    2.4493 3.2488 0.04046 *
waldtest(ols, c('drink', 'smoke'), vcov = vcovHC)
#이분산을 고려한 결과 3.25 => 2.99로 감소
#   Res.Df Df      F  Pr(>F)
# 1    251
# 2    253 -2 2.9862 0.05227 .

# 이분산 F 검정 동일함
lht(ols, c("drink", "vehipc"), vcov = vcovHC)
lht(ols, c("drink", "vehipc"), white.adjust = 'hc3')
waldtest(ols, c('drink', 'vehipc'), vcov = vcovHC)

