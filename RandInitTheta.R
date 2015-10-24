#Set working directory
setwd("C:/Users/Sagar/OneDrive/Documents/Learning/Bikesharing")

#Create function to randomly initialize theta parameters
RandInitTheta<-function(insize,outsize){
  
  epsilon=sqrt(6)/(sqrt(insize)+sqrt(outsize))
  theta=runif(outsize*(insize+1),-epsilon,+epsilon)
  return(theta)
  
}