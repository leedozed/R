install.packages("car")
install.packages("sandwich")
install.packages("Ecdat")
install.packages("lmtest")

library(lmtest)

data(Housing, package = "Ecdat")
nrow(Housing)
names(Housing)

ols = lm(log(price)~log(lotsize)+bedrooms+bathrms, data = Housing)
coeftest(ols)
