function p=predict(Theta1, Theta2, X)
%Function to calculate NN hypothesis using forward prop.

%Create matrix to hold predictions
p=zeros(size(X, 1), 1);

%Implement forward propagation
a1=X;
a1wb=[ones(size(X, 1),1),X];
z2=a1wb*Theta1';
%Tanh activation for hidden layer
a2=tanh(z2);
a2wb=[ones(size(a2, 1),1),a2];
z3=a2wb*Theta2';
a3=z3;
htheta=a3;
p=htheta;

end
