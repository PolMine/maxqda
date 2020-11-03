library(polmineR)

secretary_general_2000_speech <- corpus("UNGA") %>%
  subset(date == "2000-04-03") %>%
  subset(speaker == "The Secretary-General") %>%
  html(cpos = TRUE, charoffset = TRUE) %>%
  as.character() %>%
  cat(file = "~/Lab/github/maxqda/inst/extdata/html/annan.html")


