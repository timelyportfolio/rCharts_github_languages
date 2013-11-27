#require(devtools)
#install_github("rgithub","cscheid")
library(github)

languages <- c("python","javascript","r")

#get our ctx to use with github api
ctx <- create.github.context("https://api.github.com/")

results <- lapply(languages,function(lang) {
  search.repositories(
    ctx,
    q=sprintf("language:%s",lang),
    "&sort=stars&order=desc"
  )$content
})