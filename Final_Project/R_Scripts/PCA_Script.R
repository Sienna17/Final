
do_PCA<-function(gp){
  df<-gp[,1:3]
  
  df<-dplyr::filter(df,!is.na(Ratings) & !is.na(Game_points) & !is.na(Percentage_Completion))
  
  
  p <- prcomp(df,scale.=TRUE)
  
  pc1<-p$x[,1] #grabbing first column of matrix
  #represents the x coordinate of each player in the 2 dim space that we projected all 520 dimensions onto
  pc2<-p$x[,2] #grabbing second column of matrix
  
  df<-data.frame(PC1=pc1,PC2=pc2,
                 stringsasFactors=FALSE)
  #x value is pc1, y value is pc2, color is by party
  
  p<-ggplot()
  p<-p+geom_point(mapping=aes(x=PC1,y=PC2),data=df)
  p<-p+geom_point(mapping=aes(x=PC1,y=PC2),data=df)
  return(p)
}
