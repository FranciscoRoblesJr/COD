#### Packages ####
library(tidyverse)
library(magrittr)

#### Functions ####
Create_Player = function(Player){
  # Set initial values
  Name = deparse(substitute(Player))
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

get_Elo = function(Player){
  Name = deparse(substitute(Player))
  if(Name %in% names(Player.List)){
    Name = Name
  } else {
    Name = Player
  }
  
  Elo.df = Player.List %>% extract2(Name) %>% extract2('Elo')
  HP.df = Elo.df %>% extract2(1)
  SND.df = Elo.df %>% extract2(2)
  Control.df = Elo.df %>% extract2(3)
  
  Elo.Vec = c(HP.df[nrow(HP.df), 2], SND.df[nrow(SND.df), 2], Control.df[nrow(Control.df), 2])
  
  return(Elo.Vec)
}

Elo_Probability = function(score1, score2){
  # This function returns the probability a team will win based on both team's scores
  Prob = 1 / (1 + 10 ^ ((score2 - score1) / 400))
  
  return.list = list(Ratings = c(score1, score2), Probability = Prob)
  return(return.list)
}

get_wElo = function(Player){
  # Must have Player.List loaded
  # CHANGE PLAYER LIST IF PLAYER DATABASE HAS DIFFERENT NAME
  
  # Need if else for for-loop in wElo_Standings()
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

#### Notes/Testing ####
Elo_Probability(1400, 1000)
# Set players to 1500
# Have team Elo be average of players
# k = 32
# Scale amount by COD Rating (If i can)

test1 = Create_Player(Scump)
test2 = Create_Player(Crimsix)
test3 = Create_Player(Karma)
Player.List = list(Scump = test1, Crimsix = test2, Karma = test3)
# Use these two lines to reorder lists of players alphabetically
Player.List %>% map(1) %>% unlist() %>% order() -> index
Player.List[index]
# need to see if this works