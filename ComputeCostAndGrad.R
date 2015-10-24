#Set working directory
setwd("C:/Users/Sagar/OneDrive/Documents/Learning/Bikesharing")

#Create function to compute cost function for linear regression
ComputeCostLR<-function(theta,X,y,lambda){
  
  #Coerce parameters to matrix
  theta<-matrix(theta,ncol=length(theta))
  
  #Initialize J to zero
  J=0
  
  #Find number of observations
  m=nrow(X)
  
  #Find cost
  J=(1/(2*m))*(t(X%*%t(theta)-y)%*%(X%*%t(theta)-y))
  J=J+(lambda/(2*m))*((theta[1,2:ncol(theta), drop=FALSE])%*%t(theta[1,2:ncol(theta), drop=FALSE]))
  
  return(as.numeric(J))
}

#Create function to compute gradients for linear regression
ComputeGradLR<-function(theta,X,y,lambda){
  
  #Coerce parameters to matrix
  theta<-matrix(theta,ncol=length(theta))
  
  #Initialize grad to zero
  grad=matrix(0,nrow(theta),ncol(theta))
  
  #Find number of observations
  m=nrow(X)
  
  #Find gradients
  grad=(1/m)*(t(X%*%t(theta)-y)%*%X)
  grad[1,2:ncol(grad)]=grad[1,2:ncol(grad)]+(lambda/m)*theta[1,2:ncol(theta)]
  
  return(grad)
}

#Create function to compute cost for 3 layer neural network
ComputeCostNN3<-function(theta,insize,hidsize,outsize,X,y,lambda){
  
  #Roll theta into its matrices
  theta1<-matrix(theta[1:(hidsize*(insize+1))],ncol=insize+1)
  theta2<-matrix(theta[((hidsize*(insize+1))+1):length(theta)],ncol=hidsize+1)
  
  #Find number of observations
  m=nrow(X)

  #Initialize cost to zero
  J=0
  
  #Perform forward propagation
  a1wb=X
  a1=X[,2:ncol(X)]
  z2=a1wb%*%t(theta1)
  source("Sigmoid.R")
  a2=sigmoid(z2)
  a2wb=cbind(matrix(1,nrow(X),1),a2)
  z3=a2wb%*%t(theta2)
  a3=z3
  a3wb=cbind(matrix(1,nrow(X),1),a3)
  
  J=(1/(2*m))*(t(a3-y)%*%(a3-y))
  J=J+(lambda/2*m)*(sum(theta1[,2:ncol(theta1)]*theta1[,2:ncol(theta1)])+
                      sum(theta2[,2:ncol(theta2)]*theta2[,2:ncol(theta2)]))
  
  return(as.numeric(J))

}

#Create function to compute gradients for 3 layer neural network
ComputeGradNN3<-function(theta,insize,hidsize,outsize,X,y,lambda){
  
  #Roll theta into its matrices
  theta1<-matrix(theta[1:(hidsize*(insize+1))],ncol=insize+1)
  theta2<-matrix(theta[((hidsize*(insize+1))+1):length(theta)],ncol=hidsize+1)
  capdelta1<-matrix(0,nrow(theta1),ncol(theta1))
  capdelta2<-matrix(0,nrow(theta2),ncol(theta2))
  grad1<-matrix(0,nrow(theta1),ncol(theta1))
  grad2<-matrix(0,nrow(theta2),ncol(theta2))
  
  #Find number of observations
  m=nrow(X)

  #Implement forward propagation
  for (i in 1:m){
    a1wb=t(X[i,,drop=FALSE])
    z2=theta1%*%a1wb
    source("Sigmoid.R")
    a2=sigmoid(z2)
    a2wb=rbind(matrix(1,1,1),a2)
    z3=theta2%*%a2wb
    a3=z3
    
    delta3=a3-y[i,,drop=FALSE]
    delta2wb=(t(theta2)%*%delta3)*(a2wb*(1-a2wb))
    delta2=delta2wb[2:nrow(delta2wb),,drop=FALSE]
    
    capdelta1=capdelta1+delta2%*%t(a1wb)
    capdelta2=capdelta2+delta3%*%t(a2wb)
  }
  
  grad1=(1/m)*capdelta1
  grad2=(1/m)*capdelta2
  grad1[,2:ncol(grad1)]=grad1[,2:ncol(grad1)]+(lambda/m)*theta1[,2:ncol(theta1)]
  grad2[,2:ncol(grad2)]=grad2[,2:ncol(grad2)]+(lambda/m)*theta2[,2:ncol(theta2)]
  
  grad=c(as.vector(grad1),as.vector(grad2))
  return(grad)
}