#require(devtools)
#install_github("rgithub","cscheid")
library(github)

languages <- c("python","java","javascript","ruby","r")

#get our ctx to use with github api
ctx <- create.github.context("https://api.github.com/")

results <- lapply(languages,function(lang) {
  result <- search.repositories(
    ctx,
    q=sprintf("language:%s",lang),
    "&sort=stars&order=desc&per_page=100"
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
xyplot(log(repo.stargazers_count,base=10)~repo.name,groups=lang,data=info.df)
densityplot(~log(repo.stargazers_count,base=10),groups=lang,data=info.df,plot.points="rug")
dotplot(lang~log(repo.stargazers_count,base=10),groups=lang,data=info.df)
stripplot(
  log(repo.stargazers_count,base=10)~lang,
  groups=lang,
  labels = info.df$repo.name,
  data=info.df,
  panel = function(x,y,labels,...){
    panel.superpose(
      x, y, ...,
      panel.groups = function(x,y,subscripts,col.symbol,...){
        panel.xyplot(x=x,y=y,subscripts=subscripts,col.symbol=col.symbol,...)
        panel.text(
          x=x[1],
          y=y[1],
          labels=labels[subscripts[1]],
          col=col.symbol
        )
      }
    )
  }
)

info.df$repo.stargazers_count_log <- log(info.df$repo.stargazers_count,base=10)
info.df <- ddply(info.df,.(lang),transform, rank=rank(-repo.stargazers_count))

require(rCharts)
d1 <- dPlot(
  x = c("lang","repo.name"),
  y = "repo.stargazers_count_log",
  groups = "lang",
  data = info.df,
  type = "bubble"
)
d1


n1 <- nPlot(
  x = "repo.name",
  y = "repo.stargazers_count",
  group = "lang",
  data = info.df[order(info.df$repo.stargazers_count),],
  type = "multiBarChart"  
)
n1

n2 <- nPlot(
  x = "rank",
  y = "repo.stargazers_count",
  group = "lang",
  data = info.df[order(info.df$repo.stargazers_count),],
  type = "scatterChart"  
)
n2