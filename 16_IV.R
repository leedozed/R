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
