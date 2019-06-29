Elo.SSE = NULL
for(i in 1:length(Elo.Test)){
  Series.List = Elo.Test %>% extract2(i)
  for(j in 1:length(Series.List)){
    Map.List = Series.List %>% extract2(j)
    Elo.SSE[length(Elo.SSE) + 1] = (Map.List[[3]][1] - Map.List[[2]])^2
  }
}
sum(Elo.SSE)
