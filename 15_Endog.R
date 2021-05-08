# stata DB 받아오기
library(foreign)
Wage2 = read.dta("https://econpapers.repec.org/scripts/redir.pf?u=http%3A%2F%2Ffmwww.bc.edu%2Fec-p%2Fdata%2Fwooldridge%2Fwage2.dta;h=repec:boc:bocins:wage2")
names(Wage2)

nrow(Wage2)

library(lmtest)
model = lwage~educ+exper+tenure+married+south+urban+black
ols0 = lm(model, data=Wage2)

# educ         0.0654307  0.0062504 10.4683 < 2.2e-16 ***
# exper        0.0140430  0.0031852  4.4089 1.161e-05 ***
# tenure       0.0117473  0.0024530  4.7890 1.950e-06 ***
# married      0.1994171  0.0390502  5.1067 3.979e-07 ***
# south       -0.0909036  0.0262485 -3.4632 0.0005582 ***
# urban        0.1839121  0.0269583  6.8221 1.618e-11 ***
# black       -0.1883499  0.0376666 -5.0004 6.839e-07 ***

coeftest(ols0)

ols1 = lm(update(model, .~.+IQ),data=Wage2)
coeftest(ols1)
# educ         0.05441062  0.00692849  7.8532 1.120e-14 ***
# exper        0.01414585  0.00316510  4.4693 8.818e-06 ***
# tenure       0.01139509  0.00243938  4.6713 3.436e-06 ***
# married      0.19976443  0.03880248  5.1482 3.212e-07 ***
# south       -0.08016946  0.02625292 -3.0537 0.0023247 **
# urban        0.18194631  0.02679287  6.7908 1.991e-11 ***
# black       -0.14312531  0.03949245 -3.6241 0.0003057 ***
# IQ           0.00355910  0.00099181  3.5885 0.0003500 ***