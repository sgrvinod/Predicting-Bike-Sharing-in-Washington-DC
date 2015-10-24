function numgrad=checkNNGradients(theta,input_layer_size, ...
                                   hidden_layer_size, ...
                                   num_labels, ...
                                   X, y, lambda)
%Function to calculate gradients numerically using finite differences
                               
numgrad=zeros(size(theta));
for i=1:length(theta),
    temppos=theta;
    tempneg=theta;
    epsilon=0.0001;
    temppos(i)=temppos(i)+epsilon;
    tempneg(i)=tempneg(i)-epsilon;
    [Jpos gradvec]=nnCostFunction(temppos,input_layer_size,hidden_layer_size,num_labels,X,y,lambda);
    [Jneg gradvec]=nnCostFunction(tempneg,input_layer_size,hidden_layer_size,num_labels,X,y,lambda);
    numgrad(i)=(Jpos-Jneg)/(2*epsilon);
end



end

    
