#require(devtools)
#install_github("rgithub","cscheid")
library(github)

languages <- c("python","javascript","ruby","r")

#get our ctx to use with github api
ctx <- create.github.context("https://api.github.com/")

results <- lapply(languages,function(lang) {
  result <- search.repositories(
    ctx,
    q=sprintf("language:%s",lang),
    "&sort=stars&order=desc"
  )
})

names(results) <- languages

info.df <- do.call(rbind,lapply(
  languages,
  function(lang){
    do.call(rbind,lapply(
      results[[lang]]$content$items,
      function(repo){
          return(data.frame(
            lang,
            repo$name,
            repo$stargazers_count,
            repo$forks,
            repo$created_at,
            repo$updated_at
          ))
      }
    ))
  }
))

require(lattice)
#simple stupid graph to get a first look
xyplot(log(repo.stargazers_count)~repo.name,groups=lang,data=info.df)