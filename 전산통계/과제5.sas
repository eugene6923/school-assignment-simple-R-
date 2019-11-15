data data1;
input id gender $ income deposit;
cards;
1 M 30 10  
2 M 45 50  
3 F 60 15  
4 F 46 20 
5 M 10 8 
run;

data data2;
input id gender $ income deposit;
cards;
1 M 60 40
2 M . 20 
3 F 40 . 
4 F . 26 
5 M 15 9
run;

data inc_dep1;
merge data1 data2;
by id;
run;
proc print data=inc_dep1; run;

data inc_dep2;
update data1 data2;
by id;
run;
proc print data=inc_dep2; run;

data inc_dep_total;
merge data1 (rename=(income=income1) rename=(deposit=deposit1)) data2 (rename=(income=income2) rename=(deposit=deposit2));
income_total=income1+income2;
deposit_total=deposit1+deposit2;
run;
proc print data=inc_dep_total; run;
