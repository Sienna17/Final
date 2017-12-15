Game_points<-function(d){ #bc want multiple data points to run a reg that's why im not avg
  ratings<-d$rate
  game_points<-d$game_points
  new_d<-data.frame(Rating=ratings,Home_Away=d$home_away,
                    Game_points=game_points,
                    stringsAsFactors = FALSE)
  new_d2<-dummy.data.frame(new_d,sep = ".")
  n<-nrow(d)
  percentage_completion_vector<-rep(NA,n) 
  
  for (i in 1:n){
    cur_completions<-d$cmp[i]
    cur_attempts<-d$att[i]
    cur_percentage_completion<-cur_completions/cur_attempts
    percentage_completion_vector[i]<-cur_percentage_completion
  }
  
  final_d<-data.frame(Ratings=new_d$Rating,
                      Game_points=new_d$Game_points,
                      Percentage_Completion=percentage_completion_vector,
                      Year=d$year, Home_Away=new_d2$Home_Away.away,
                      stringsAsFactors = FALSE) #1 if away and 0 if home
  return(final_d)
}



#do this for every year and find which year the relationship is most significant
#plot the one that is the most significant (in total 2 plots)
get_pc_gp_relationship.Game_points<-function(gp,year){
  gp_df<-gp[gp$Year==year,]
  reg<-lm(gp_df$Game_points ~gp_df$Percentage_Completion)
  coef<-reg$coefficients
  m<-coef[2]
  b<-coef[1]
  p_value<-summary(reg)$coefficients[2,4]
  return(p_value)
}

get_rating_gp_relationship.Game_points<-function(gp,year){
  gp_df<-gp[gp$Year==year,]
  reg<-lm(Game_points ~Ratings,data=gp_df)
  coef<-reg$coefficients
  m<-coef[2]
  b<-coef[1]
  p_value<-summary(reg)$coefficients[2,4]
  return(p_value)
}
