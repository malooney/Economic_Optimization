
clc
clear;
P= 0:99;
A = [4 3 2; 3 2 2; 10 6 3];
x0 = [0; 0; 0];
lb=[0 0 0];
ans= [];
options=optimset('Algorithm','interior-point');
v = @(x)-(20* x(1)- 10* x(1)^2+ 30* x(2)- 9* x(2)^2+ 15* x(3)- 8* x(3)^2);

for i= 1:numel(P);
b= [1300 1000 P(i)];
[x, fval] = fmincon(v, x0, A, b, [], [], lb, [], [], options);
ans(i,1:3)= x;
ans(i,4)= P(i);
ans(i,5)= -fval;
end;

plot(ans(:,4), ans(:,5));
xlabel('Pollution');
ylabel('Profit');

