function W = randInitializeWeights(L_in, L_out)
%Function to randomly initialize theta matrix

%Create mapping matrix
W=zeros(L_out, 1 + L_in);

%Initialize
epsilon=sqrt(6)/(sqrt(L_in+L_out));
W=(rand(L_out,1+L_in)*2*epsilon)-epsilon;

end
