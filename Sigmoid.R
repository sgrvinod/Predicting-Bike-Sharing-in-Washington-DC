#Set working directory
setwd("C:/Users/Sagar/OneDrive/Documents/Learning/Bikesharing")

#Create function to compute sigmoid
sigmoid<-function(z){
  sig=1/(1+exp(-z))
  return(sig)
  
}