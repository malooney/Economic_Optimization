
clc
clear;

i= 1;       % price counter

P_x= linspace(2000, 4000, 30);

beq= [10000];
lb=[0 0];
x0 = [0; 0];

ans= zeros(numel(P_x), 5);

options=optimset('Algorithm','interior-point');

v = @(x) -1* (-2* x(1)^2- x(2)^2+ x(1)* x(2)+ 8* x(1)+ 3* x(2) );

figure('units','normalized','outerposition',[0 0 0.75 1]) % make plot window open to X-fraction of screen
figure(1); hold on % allow plot build-up
tp= 0;

for i= 1:numel(P_x);
    
Aeq= [P_x(i) 1000;];
[x,fval,exitflag,output,lambda] = fmincon(v, x0, [], [], Aeq, beq, lb, [], [], options);

ans(i,1:2)= x;
ans(i,3)= P_x(i);
ans(i,4)= -fval;
ans(i,5)= lambda.eqlin;

i= i+1;

end

subplot(2,2,1);
plot(ans(:,1), ans(:,3));
title('Plot 1');
xlabel('demand for television advertising (min.)');
ylabel('price of television advertisement');

subplot(2,2,2);
plot(ans(:,5),ans(:,4));
title('Plot 2');
xlabel('Lambda');
ylabel('Profit ($1000)');

subplot(2,2,3);
plot(ans(:,1),ans(:,4))
hold on
plot(ans(:,2),ans(:,4))
title('Plot 3');
xlabel('Qunatity of TV and Radio Adv. (min.)');
ylabel('Profit ($1000)');
legend({...
    'TV Adv.',...
    'Radio Adv.'},...
    'FontSize', 10, 'Location', 'Southeast','Box','on');

print('mideterm_Q1_plot', '-dpdf', '-bestfit') % export plot to file

