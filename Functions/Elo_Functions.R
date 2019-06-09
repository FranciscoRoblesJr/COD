#### Packages ####
library(tidyverse)
library(magrittr)

#### Functions ####
### Create new Player ###
Create_Player = function(Player){
  # Set initial values
  Name = deparse(substitute(Player))
  if(grepl('[', Name, fixed = T)){
    Name = Player
  } else {
    Name = Name
  }
  d = as.Date('2019-02-03')
  HP_Elo = 1500
  SnD_Elo = 1500
  Control_Elo = 1500
  
  
  # Compile into lists
  hp.df = data.frame(Date = d, Place = '', Team = '', Opponent = '', Map = '', Result = '', Elo = HP_Elo, 
                     stringsAsFactors = F)
  snd.df = data.frame(Date = d, Place = '', Team = '', Opponent = '', Map = '', Result = '', Elo = SnD_Elo, 
                      stringsAsFactors = F)
  control.df = data.frame(Date = d, Place = '', Team = '', Opponent = '', Map = '', Result = '', Elo = Control_Elo, 
                          stringsAsFactors = F)
  elo.list = list(Hardpoint = hp.df, 'Search and Destroy' = snd.df, Control = control.df)
  
  
  # Return list
  return.list = list(Name = Name, Elo = elo.list)
  return(return.list)
}

### Elo Probability ###
Elo_Probability = function(score1, score2){
  # This function returns the probability a team will win based on both team's scores
  # It takes two teams scores and returns list of teams scores as well as the Probability Team1 wins
  Prob = 1 / (1 + 10 ^ ((score2 - score1) / 400))
  
  return.list = list(Ratings = c(score1, score2), Probability = Prob)
  return(return.list)
}

### Get Player Elo ###
get_Elo = function(Player){
  # Gets Elo Rating for individual player and returns vector of the 3 game modes
  Name = deparse(substitute(Player))
  if(Name %in% names(Player.List)){
    Name = Name
  } else {
    Name = Player
  }
  
  Elo.list = Player.List %>% extract2(Name) %>% extract2('Elo') %>% map('Elo')
  HP.Elo = Elo.list %>% extract2(1)
  SND.Elo = Elo.list %>% extract2(2)
  Control.Elo = Elo.list %>% extract2(3)
  
  Elo.Vec = c(last(HP.Elo), last(SND.Elo), last(Control.Elo))
  
  return(Elo.Vec)
}

### Get Team Elo ###
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

### Update Player Elo for Series ###
update_PlayerElo = function(teams, all_players, results, place, d){
  # set up data.frames
  num.games = length(results) / 2
  elo.index = c(1, 2, 3, 1, 2)[1:num.games]
  t1.players = all_players[1:5]
  t2.players = all_players[6:10]
  
  k = 32
  elo.list = list()
  for(i in 1:num.games){
    # Calculate Elo change for that game
    team1.elo = get_TeamElo(t1.players) %>% extract2(elo.index[i])
    team2.elo = get_TeamElo(t2.players) %>% extract2(elo.index[i])
    elo.list[[i]] = Elo_Probability(team1.elo, team2.elo)
    prob = c(elo.list[[i]]$Probability, 1 - elo.list[[i]]$Probability)
    winner = results[c((2*i)-1, 2*i)]
    elo.list[[i]][['Result']] = winner
    change = k*(winner - prob)
    
    # Change the player elo's
    # Add in COD Rating to new.elo line for both for loops
    for(j in 1:length(t1.players)){
      df = Player.List %>% extract2(t1.players[j]) %>% extract2('Elo') %>% extract2(elo.index[i])
      prev.elo = get_Elo(t1.players[j]) %>% extract2(elo.index[i])
      new.elo = prev.elo + change[1]
      
      if(winner[1] == 1){
        team1.win = 'W'
      } else {
        team1.win = 'L'
      }
      new.row = list(Date = d, Place = place, Team = teams[1], Opponent = teams[2], Map = paste0('Map', i), 
                     Result = team1.win, Elo = new.elo)
      df = rbind.data.frame(df, new.row)
      Player.List[[t1.players[j]]][['Elo']][[elo.index[i]]] <<- df
    }
    for(l in 1:length(t2.players)){
      df = Player.List %>% extract2(t2.players[l]) %>% extract2('Elo') %>% extract2(elo.index[i])
      prev.elo = get_Elo(t2.players[l]) %>% extract2(elo.index[i])
      new.elo = prev.elo + change[2]
      
      if(winner[2] == 1){
        team2.win = 'W'
      } else {
        team2.win = 'L'
      }
      new.row = list(Date = d, Place = place, Team = teams[2], Opponent = teams[1], Map = paste0('Map', i), 
                     Result = team2.win, Elo = new.elo)
      df = rbind.data.frame(df, new.row, stringsAsFactors = F)
      Player.List[[t2.players[l]]][['Elo']][[elo.index[i]]] <<- df
    }
  }
  return(elo.list)
}

Series = function(file_path, d = Sys.Date()){
  # only put something by wins
  series = read.csv(file_path, header = F)
  place = file_path %>% str_split('/') %>% map(2) %>% unlist()
  d = as.Date(d)
  
  
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
  
  Match.Elo.Results = update_PlayerElo(teams, players, results, place, d)
  
  # Create object that saves Match Elo Results and have it save directly in funciton
  # Wont need to return anything at that point
  return(Match.Elo.Results)
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

get_PlayerData = function(Player, beg.d = as.Date('2019-02-03'), d = Sys.Date()){
  Name = deparse(substitute(Player))
  if(Name %in% names(Player.List)){
    Name = Name
  } else {
    Name = Player
  }
  beg.d = as.Date(beg.d)
  d = as.Date(d)
  
  df.list = Player.List %>% extract2(Name) %>% extract2('Elo')
  modes = c(names(df.list), 'wElo')
  df.list = df.list %>% map(~extract(.x, c('Date', 'Elo')))
  dates = seq(beg.d, d, by="days")
  
  player.df = data.frame()
  for(i in 1:length(dates)){
    filt.df = map(df.list, ~filter(.x, Date <= dates[i])) %>% map(~extract(.x, 'Elo'))
    elo.day = map(filt.df, ~tail(.x, 1)) %>% unlist()
    welo.day = (elo.day %*% c(2, 2, 1)) / 5
    elo.day = c(elo.day, welo.day)
    day = rep(as.Date(dates[i]), 4)
    day.df = data.frame(Date = day, Mode = modes, Elo = elo.day)
    player.df = rbind.data.frame(player.df, day.df)
  }
  rownames(player.df) = NULL
  player.df$Mode = factor(player.df$Mode, levels = c('Hardpoint', 'Search and Destroy', 'Control', 'wElo'))
  
  
  return.list = list(Player = Name, Data = player.df)
  return(return.list)
}

Player_Graph = function(Player, beg.d = as.Date('2019-02-03'), d = Sys.Date()){
  Name = deparse(substitute(Player))
  if(Name %in% names(Player.List)){
    Name = Name
  } else {
    Name = Player
  }
  beg.d = as.Date(beg.d)
  d = as.Date(d)
  
  player.df = get_PlayerData(Name, beg.d, d) %>% extract2('Data')
  
  elo.graph = ggplot(player.df, aes(Date, Elo, color = Mode)) + geom_line(size = 2, alpha = 0.6) + 
    labs(title = paste(Name, 'Elo Rating from', beg.d, 'to', d), color = 'Mode')
  
  return(elo.graph)
}

Create_Team = function(Name, Players){
  Name = deparse(substitute(Name))
  Team.List[[Name]] <<- list(Team = Name, Players = Players)
}

Team_Graph = function(team, beg.d = as.Date('2019-02-03'), d = Sys.Date()){
  Team = deparse(substitute(team))
  beg.d = as.Date(beg.d)
  d = as.Date(d)
  players = Team.List %>% extract2(Team) %>% extract2('Players')
  dates = seq(beg.d, d, by="days")
  
  
  player.list = list()
  for(i in players){
    player.list[[i]] = get_PlayerData(i, beg.d, d) %>% extract2('Data')
  }
  
  modes = c('Hardpoint', 'Search and Destroy', 'Control', 'wElo')
  total.df = data.frame()
  for(i in 1:length(dates)){
    HP.day = player.list %>% map(~filter(.x, Date == dates[i] & Mode == 'Hardpoint')) %>% map(~extract(.x, 'Elo')) %>% unlist() %>% mean()
    SnD.day = player.list %>% map(~filter(.x, Date == dates[i] & Mode == 'Search and Destroy')) %>% map(~extract(.x, 'Elo')) %>% unlist() %>% mean()
    Control.day = player.list %>% map(~filter(.x, Date == dates[i] & Mode == 'Control')) %>% map(~extract(.x, 'Elo')) %>% unlist() %>% mean()
    wElo.day = player.list %>% map(~filter(.x, Date == dates[i] & Mode == 'wElo')) %>% map(~extract(.x, 'Elo')) %>% unlist() %>% mean()
    
    elo.day = c(HP.day, SnD.day, Control.day, wElo.day)
    day = rep(dates[i], 4)
    
    
    day.df = data.frame(Date = day, Mode = modes, Elo = elo.day)
    total.df = rbind.data.frame(total.df, day.df)
  }
  total.df$Mode = factor(total.df$Mode, levels = c('Hardpoint', 'Search and Destroy', 'Control', 'wElo'))
  
  elo.plot = ggplot(total.df, aes(Date, Elo, color = Mode)) + geom_line(size = 2, alpha = 0.6) +
    labs(title = paste(Team, 'Elo Rating from', beg.d, 'to', d), color = 'Mode')
  
  return(elo.plot)
}

