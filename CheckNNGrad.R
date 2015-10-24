#Set working directory
setwd("C:/Users/Sagar/OneDrive/Documents/Learning/Bikesharing")

#Create function to compute sigmoid
CheckNNGrad<-function(theta,insize,outsize,hidsize,X,y,lambda){
  
  #Initialise numgrad
  numgrad=rep(0,length(theta))
  
  #Calculate numgrad for each theta
  for (i in 1:length(theta)){
    epsilon=0.0001
    temppos=theta
    tempneg=theta
    temppos[i]=temppos[i]+epsilon
    tempneg[i]=tempneg[i]-epsilon
    source("ComputeCostAndGrad.R")
    Jpos=ComputeCostNN3(temppos,insize,outsize,hidsize,X,y,lambda)
    Jneg=ComputeCostNN3(tempneg,insize,outsize,hidsize,X,y,lambda)
    numgrad[i]=(Jpos-Jneg)/(2*epsilon)
    
  }
  return(numgrad)

}