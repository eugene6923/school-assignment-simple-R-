data problem1;
do x=0.001 to 10 by 0.1;
z1=pdf('chisq',x,3);
z2=pdf('chisq',x,10);
z3=pdf('chisq',x,20);
output;
end;
run;
symbol i=spline;
proc gplot data=problem1;
plot z1*x z2*x z3*x/overlay;
run;

data problem2;
pi=3.14159265354;
rho1=0.8;
rho2=-0.4;
do x=-2 to 2 by 0.05;
do y=-2 to 2 by 0.05;
SSQ1=(x*x -2*rho1*x*y + y*y)/(1-rho1*rho1);
SSQ2=((x-3)*(x-3)-2*rho2*(x-3)*(y-3)+(y-3)*(y-3))/(1-rho2*rho2);
f1=1/(2*pi*sqrt(1-rho1*rho1))*exp(-0.5*SSQ1);
f2=1/(2*pi*sqrt(1-rho2*rho2))*exp(-0.5*SSQ2);
z=0.7*f1 + 0.3*f2;
output;
end;
end;
run;
proc plot data=problem2;
plot y*x=z/
HAXIS= -2.5 TO 2.5
VAXIS= -2.5 TO 2.5
HREF=0
VREF=0 
contour;
run;
