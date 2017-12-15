Player<-function(d){
  d<-d[-c(8645,8646,8647,8648,8649,8650,8651),]
  d<-d[d$ypa <=14,]
  d_out <- plyr::ddply(d, .(year), function(x){
    players<-unique(d$qb)
    n<-length(players)
    
    completion_vector<-rep(NA,n)
    ypa_vector<-rep(NA,n)
    td_int_vector<-rep(NA,n)
    
    for (i in 1:n){
      cur_player<-players[i]
      d_filt<-d[d$qb==cur_player,] #now just have boomer for 1996
      cmp<-mean(d_filt$cmp)
      ypa<-mean(d_filt$ypa)
      completion_vector[i]<-cmp
      ypa_vector[i]<-ypa
       td<-sum(d_filt$td)
       int<-as.numeric(d_filt$int)
       df<-data.frame(int=int,stringsAsFactors = FALSE)
       df<-dplyr::filter(df,!is.na(df$int))
       int<-sum(df$int)
       td_int_ratio<-td/int
       td_int_vector[i]<-td_int_ratio
      
       #remove these players by filtering on is.na
    }  
    cur_year<-d$year[1]
    year_vector<-rep(cur_year,n)
    d_out<-data.frame(Players=players,
                      Completions=completion_vector,YPA=ypa_vector,
                      TD_INT=td_int_vector,
                      stringsAsFactors = FALSE)
    return(d_out)
  }) 
  

  return(d_out)
}



#do these two get functions for every year in the analysis so will have mult answers
get_highest_ypa.Player<-function(player,year){
  player_df<-player[player$year==year,]
  int<-which.max(player_df$YPA)
  return(player_df$Player[int])
}

get_highest_cmp.Player<-function(player,year){
  player_df<-player[player$year==year,]
  int<-which.max(player_df$Completions)
  return(player_df$Player[int])
}

#i'll do this for my fav players

get_player_stat.Player<-function(player,QB,year,stat){ 

  player_df<-player[player$year==year,]
  player_df<-player_df[player_df$Players==QB,]
  return(player_df[[stat]])
}


#but do this PCA for more relevant stats and figure out which object it goes with




