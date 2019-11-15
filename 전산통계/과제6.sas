1)
data ACM;
do drink = 'yes','no';
do smoke = 'yes', 'no';
do marihuana='yes','no';
input count @@;
output;
end;end;end;
cards;
911 538 44 456 3 43 2 279
run;

2)
proc freq data=ACM;
weight count;
tables drink*smoke /CHISQ;
run;

3)
data ACM_M1 ACM_M2;
set ACM;
if marihuana='yes' then output ACM_M1;
else if marihuana='no' then output ACM_M2;
run;

a)
proc freq data=ACM_M1;
weight count;
tables drink*smoke /CHISQ;
run;

b)
proc freq data=ACM_M2;
weight count;
tables drink*smoke /CHISQ;
run;
