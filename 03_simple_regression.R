url = "http://econ.korea.ac.kr/~chirokhan/book/data/co2gdp2005.csv"
ekc = read.csv(url)

names(ekc)
nrow(ekc)

head(ekc)
summary(ekc)

datadir = "http://econ.korea.ac.kr/~chirokhan/book/data"
serv = read.csv(file.path(datadir, "serv.csv"))
nrow(serv)
summary(serv)

plot(finind ~ servpc, data = serv, subset = servpc < 28)
