dataidr = "http:\\econ.korea.ac.kr/~chirokhan/book/data"
ivdata = read.csv(file.path(datadir, "ivdata.csv"))
names = (ivdata)
nrow(ivdata)

# 일반 OLS
library(lmtest)
coeftest(lm(y~x1+x2, data = ivdata))

# 1st
stage1 = lm(x2~x1+z2a, data = ivdata)
coeftest(stage1)
# x1 통제 후 z2a의 t value가 3이상으로 강한 상관관계
#              Estimate Std. Error t value  Pr(>|t|)
# (Intercept)  6.337438   0.936309  6.7685 9.946e-10 ***
# x1          -0.131912   0.079058 -1.6685   0.09843 .
# z2a         21.840080   4.043760  5.4009 4.720e-07 ***

# 1st의 수정된 x2hat 값을 입력 및 생성
ivdata$x2hat = fitted(stage1)

# 2nd
stage2 = lm(y~x1+x2hat, data = ivdata)
coeftest(stage2)
#             Estimate Std. Error t value  Pr(>|t|)
# (Intercept)  1.88266    2.25002  0.8367 0.4048008
# x1           0.41694    0.11089  3.7600 0.0002905 ***
# x2hat        0.68666    0.22994  2.9863 0.0035752 **

#완전 자동 도구변수 추정
install.packages("AER")
library("AER")

tsls = ivreg(y~x1+x2|x1+z2a, data = ivdata)
coeftest(tsls)

#p.414 
#1) 내생성 있는 변수와 도구 변수 사이 강한 상관성 검정
#IV 변수 검정
coeftest(stage1)
coeftest(stage1, vcov = vcovHC) #내생성 검증 포함

stage1a = lm(x2 ~ x1+z2a+z2b, data = ivdata)
coeftest(stage1a)

# F test
waldtest(stage1a, x2~x1)

#2) 설명변수가 내생적인지 검정하기
stage1 = lm(x2 ~ x1+z2a, data = ivdata)
#p.416 1st 회귀식의 잔차 V 추출 후 v2hat 변수에 입력
ivdata$v2hat = resid(stage1)
# 추정된 v2hat과 y의 회귀식에서 계수가 유의미하면 v와 u가 상관관계를 가진다
coeftest(lm(y ~ x1+x2+v2hat, data = ivdata))

# 3) 도구변수 외생성 검정
#귀무가설 = 모두 도구변수는 외생적이다
#2sls 진행
tsls = ivreg(y~x1+x2|x1+z2a+z2b, data = ivdata)
#2sls의 잔차항을 uhat의 추정 후 입력
ivdata$uhat = tsls$resid
#추정된 u값을 외생변수(도구변수)와 OLS 진행
aux = lm(uhat~x1+z2a+z2b,data=ivdata)
#표본크기 X aux의 R제곱 = 카이제곱 분포
stat = nrow(ivdata)*summary(aux)$r.sq
stat
qchisq(.95,1)
1-pchisq(stat,1)

# p.421 교육수익률
data(Schooling, package="Ecdat")
nrow(Schooling)
names(Schooling)

# 1) OLS 추정 - 능력 변수 누락
model = lwage76 ~ ed76 + exp76 + smsa76
ols0 = lm(model, data=Schooling)
coeftest(ols0)
# ed76        0.0878104  0.0035681  24.610 < 2.2e-16 ***
# 교육 1년 증가 시 임금 8.8% 상승
confint(ols0, "ed76")
#95% 신뢰구간 (0.0808, 0.095)

# 2) IQ 능력의 대리변수로 사용
ols1 = lm(update(model, .~.+iqscore), data=Schooling)
coeftest(ols1)

# 3) 도구변수 추정
# 좌변을 ed76으로 변경 후 우변의 ed76 제거 후 momed 추가
# 1st regression
stage1 = lm(update(model, ed76~.-ed76+momed), data=Schooling)
coeftest(stage1)
# momed        0.2132285  0.0123062  17.3268 < 2.2e-16 ***
# momed의 t값 17로 강한 도구변수

# 2nd regression / 완전자동의 AER 패키지 사용
library(AER)
model = lwage76 ~ ed76 + exp76 + smsa76
#모든 외생변수를 우변으로 지정 / 엄마학력과 기타 외생변수로 도구변수 회귀
inst = ~momed + exp76 + smsa76
tsls = ivreg(model, inst, data=Schooling) 
# tsls = ivreg(lwage76 ~ ed76 + exp76 + smsa76|momed + exp76 + smsa76, data=Schooling)
coeftest(tsls)
# ed76        0.1442329  0.0123234  11.704 < 2.2e-16 ***
coeftest(tsls, vcov=vcovHC(tsls, type = "HC0"))

# 4) 설명변수의 내생성 검정
Schooling$vhat = stage1$resid
aux = lm(update(model, .~.+vhat), data = Schooling)
coeftest(aux)
# vhat        -0.0620576  0.0123683 -5.0175  5.54e-07 ***
# P<1% => ed76는 내생적

coeftest(aux, vcov=vcovHC)

# 5) 도구변수의 외생성 검정(과다식별검정)
model = lwage76 ~ ed76 + exp76 + smsa76
# ed76이 내생변수 = momed + 타 외생변수로 회귀
inst = ~momed + exp76 + smsa76
tsls = ivreg(model, inst, data=Schooling)
#2sls의 잔차
Schooling$uhat = tsls$resid
aux = lm(update(inst, uhat~.), data=Schooling)
summary(aux)
# F-statistic: 3.373e-26 on 3 and 3006 DF,  p-value: 1
# F가 0에 근접: 과도식별 = 0

#아버지 학력 추가
inst2 = ~momed + daded + exp76 + smsa76
tsls = ivreg(model, inst2, data=Schooling)
#2sls의 잔차
Schooling$uhat = tsls$resid
aux = lm(update(inst2, uhat~.), data=Schooling)
summary(aux)
# F-statistic: 0.8158 on 4 and 3005 DF,  p-value: 0.5149
# F 값이 낮음 / 과다식별됨

stat = nrow(Schooling)*summary(aux)$r.squared
stat
qchisq(.95, 1)
1-pchisq(stat, 1) #[1] 0.07076136
# 귀무가설(모두 외생적이다) 5%는 기각되지 않지만 10%에서는 기각됨
