#### Packages ####
library(tidyverse)
library(magrittr)

#### Functions ####
Create_Player = function(Player){
  # Set initial values
  Name = deparse(substitute(Player))
  if(grepl('[', Name, fixed = T)){
    Name = Player
  } else {
    Name = Name
  }
  d = Sys.Date()
  HP_Elo = 1500
  SnD_Elo = 1500
  Control_Elo = 1500
  
  
  # Compile into lists
  hp.df = data.frame(Date = d, Elo = HP_Elo)
  snd.df = data.frame(Date = d, Elo = SnD_Elo)
  control.df = data.frame(Date = d, Elo = Control_Elo)
  rownames(hp.df)[1] = 'Initial' ; rownames(snd.df)[1] = 'Initial' ; rownames(control.df)[1] = 'Initial'
  elo.list = list(Hardpoint = hp.df, 'Search and Destroy' = snd.df, Control = control.df)
  
  
  # Return list
  return.list = list(Name = Name, Elo = elo.list)
  return(return.list)
}


Elo_Probability = function(score1, score2){
  # This function returns the probability a team will win based on both team's scores
  # It takes two teams scores and returns list of teams scores as well as the Probability Team1 wins
  Prob = 1 / (1 + 10 ^ ((score2 - score1) / 400))
  
  return.list = list(Ratings = c(score1, score2), Probability = Prob)
  return(return.list)
}


get_Elo = function(Player){
  # Gets Elo Rating for individual player and returns vector of the 3 game modes
  Name = deparse(substitute(Player))
  if(Name %in% names(Player.List)){
    Name = Name
  } else {
    Name = Player
  }
  
  Elo.df = Player.List %>% extract2(Name) %>% extract2('Elo') %>% map(2)
  HP.Elo = Elo.df %>% extract2(1)
  SND.Elo = Elo.df %>% extract2(2)
  Control.Elo = Elo.df %>% extract2(3)
  
  Elo.Vec = c(last(HP.Elo), last(SND.Elo), last(Control.Elo))
  
  return(Elo.Vec)
}


get_wElo = function(Player){
  # Gets wElo for each individual player and returns that single numeric
  # CHANGE PLAYER LIST IF PLAYER DATABASE HAS DIFFERENT NAME
  
  Name = deparse(substitute(Player))
  if(Name %in% names(Player.List)){
    Name = Name
  } else {
    Name = Player
  }
  
  Elo.Vec = get_Elo(Name)
  wElo = (Elo.Vec %*% c(2, 2, 1)) / 5
  wElo = as.numeric(wElo)
  
  return(wElo)
}


wElo_Standings = function(){
  Names.Vec = names(Player.List)
  Elo.df = data.frame(Player = Names.Vec)
  Elo.Vec = NULL
  for(i in Names.Vec){
    Elo.Vec[i] = get_wElo(i)
  }
  
  Elo.df = cbind.data.frame(Elo.df, wElo = Elo.Vec)
  rownames(Elo.df) = 1:nrow(Elo.df)
  Elo.df = Elo.df %>% arrange(desc(wElo))
  
  return(Elo.df)
}


get_TeamElo = function(players_names){
  elo.list = list()
  for(i in players_names){
    elo.list[[i]] = get_Elo(i)
  }
  hp.elo = map(elo.list, 1) %>% unlist()
  snd.elo = map(elo.list, 2)  %>% unlist()
  control.elo = map(elo.list, 3)  %>% unlist()
  all.elo = c(mean(hp.elo), mean(snd.elo), mean(control.elo))
  
  return(all.elo)
}

update_PlayerElo = function(Player, elo.change, opponent){
  Name = deparse(substitute(Player))
  if(Name %in% names(Player.List)){
    Name = Name
  } else {
    Name = Player
  }
  
  # set up data.frames
  num.games = length(elo.change)
  elo.index = c(1, 2, 3, 1, 2)[1:num.games]
  d = Sys.Date()
  
  
  for(i in 1:num.games){
    df = Player.List %>% extract2(Name) %>% extract2('Elo') %>% extract2(elo.index[i])
    prev.elo = get_Elo(Name)[elo.index[i]]
    new.elo = prev.elo + elo.change[i]
    
    date.col = c(df[, 'Date'], d)
    elo.col = c(df[,'Elo'], new.elo)
    df.rownames = c(rownames(df), paste(d, opponent, paste0('Map', i)))
    
    new.df = cbind.data.frame(Date = date.col, Elo = elo.col)
    rownames(new.df) = df.rownames
    
    Player.List[[Name]][['Elo']][[elo.index[i]]] <<- new.df
  }
}

Series = function(file_path){
  # only put something by wins
  series = read.csv(file_path, header = F)
  
  
  # get series data
  teams = series[c(1,9), 1] %>% as.character()
  players = series[c(3:7, 11:15), 1] %>% as.character()
  t1.players = players[1:5]
  t2.players = players[6:10]
  num.games = nrow(series) / 16
  results = (!(series[seq(1, nrow(series), 8), 2] == '')) %>% as.numeric()
  elo.index = c(1, 2, 3, 1, 2)[1:num.games]
  
  
  # Create new players that aren't in Player.List
  all.players = names(Player.List)
  '%nin%' = Negate('%in%') # not in
  for(i in 1:length(players)){
    if(players[i] %nin% all.players){
      Player.List[[players[i]]] <<- Create_Player(players[i])
    }
  }
  
  
  # Get team elo's
  team1.elo = get_TeamElo(t1.players)
  team2.elo = get_TeamElo(t2.players)
  
  
  # Elo simulation w/ k = 32
  # Make more flexible to be able to save results from match w/ given team elo's
  k = 32
  elo.change = list()
  for(i in 1:num.games){
    t1.prob = Elo_Probability(team1.elo[(elo.index)[i]], team2.elo[(elo.index)[i]])
    t2.prob = Elo_Probability(team2.elo[(elo.index)[i]], team1.elo[(elo.index)[i]])
    prob = c(t1.prob$Probability, t2.prob$Probability)
    winner = results[c((2*i)-1, 2*i)]
    elo.change[[i]] = k*(winner - prob)
  }
  
  
  # Change Player Elo (ADD IN COD RATING FUNCTIONALITY HERE)
  t1.change = map(elo.change, 1) %>% unlist()
  t2.change = map(elo.change, 2) %>% unlist()
  
  for(i in t1.players){
    update_PlayerElo(i, t1.change, teams[2])
  }
  for(j in t2.players){
    update_PlayerElo(j, t2.change, teams[1])
  }
  return(team1.elo)
}

Elo_Probability(c(10, 20, 30), c(30, 20, 10))
test = Series('C:/Users/franc/Downloads/test geng uyu 2-3  - Sheet1.csv')


#### Ideas
# Reupdate elo probability based on games played in the series
# Add date functionality so duplicate row names do not appear (opponent column?)
