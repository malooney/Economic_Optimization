
clc
clear;

i= 1;

P_x= linspace(2000, 4000, 3);

b= [10000];
lb=[0 0];
x0 = [0; 0];

ans= zeros(numel(P_x), 5);

options=optimset('Algorithm','interior-point');

v = @(x) -1* (-2* x(1)^2- x(2)^2+ x(1)* x(2)+ 8* x(1)+ 3* x(2) );

for i= 1:numel(P_x);
    
A= [P_x(i) 1000;];
[x,fval,exitflag,output,lambda] = fmincon(v, x0, A, b, [], [], lb, [], [], options);

ans(i,1:2)= x;
ans(i,3)= P_x(i);
ans(i,4)= -fval;
ans(i,5)= lambda.ineqlin;

i= i+1;
end

plot(ans(:,1), ans(:,3));
xlabel('demand for television advertising');
ylabel('price of television advertisement');

print('mideterm_Q1_plot', '-dpdf', '-bestfit') % export plot to file