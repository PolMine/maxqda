library(polmineR)
use("polmineR")


setwd("~/Lab/github/maxqda/inst/extdata/html")

for (country in c("canada", "qatar", "argentina", "kuwait")){
  P <- partition("REUTERS", places = country, name = country)
  H <- html(P, cpos = TRUE, charoffset = TRUE)
  cat(as.character(H), file = sprintf("%s.html", country))
}
