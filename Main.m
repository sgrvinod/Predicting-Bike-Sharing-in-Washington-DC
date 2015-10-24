clear; close all; clc

%Load data to be trained
load('X.csv')
load('yreg.csv')
y=yreg;

%Make data suitable
%It already is preprocessed in R

%Split into training and test sets
train.indices=randperm(size(X, 1),ceil(0.5*size(X, 1)));
cv.indices=setdiff(1:size(X, 1),train.indices);
Xtrain=X(train.indices,:);
ytrain=y(train.indices,:);
Xcv=X(cv.indices,:);
ycv=y(cv.indices,:);

%Choose network size
input_layer_size=size(X, 2);  
hidden_layer_size=500;   
num_labels=1;   

%Initialize thetas
theta1=randInitializeWeights(input_layer_size,hidden_layer_size);
theta2=randInitializeWeights(hidden_layer_size,num_labels);

%Initialise lambda
lambda=0;

%Create function to return cost and gradients
initJ=0;
gradvec=zeros(numel(theta1)+numel(theta2),1);
thetavec=[theta1(:);theta2(:)];
[initJ gradvec]=nnCostFunction(thetavec,input_layer_size,hidden_layer_size,num_labels,Xtrain,ytrain,lambda);

%Calculate gradients using finite differences, use to verify gradients
%numgrad=checkNNGradients(thetavec,input_layer_size,hidden_layer_size,num_labels,Xtrain,ytrain,lambda);


%Set options for fmincg
options=optimset('MaxIter',5000);

%Set pointer to cost function to be optimized
costFunction=@(p) nnCostFunction(p, ...
                                   input_layer_size, ...
                                   hidden_layer_size, ...
                                   num_labels, X, y, lambda);

%Start stopwatch
tic               

%Optimize
[optimtheta, cost]=fmincg(costFunction, thetavec, options);

%Stop stopwatch
toc

%Obtain theta matrices back from optimtheta vector
optimtheta1=reshape(optimtheta(1:hidden_layer_size * (input_layer_size + 1)), ...
                 hidden_layer_size, (input_layer_size + 1));
optimtheta2=reshape(optimtheta((1 + (hidden_layer_size * (input_layer_size + 1))):end), ...
                 num_labels, (hidden_layer_size + 1));

%Predict
pcv=predict(optimtheta1,optimtheta2,Xcv);
ptrain=predict(optimtheta1,optimtheta2,Xtrain);

%Calculate standard errors
mcv=size(Xcv,1);
mtrain=size(Xtrain,1);
stderrcv=(1/mcv)*sum(abs(pcv-ycv))
stderrtrain=(1/mtrain)*sum(abs(ptrain-ytrain))

