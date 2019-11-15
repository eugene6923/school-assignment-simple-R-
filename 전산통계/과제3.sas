

LIBNAME myfolder 'c:\temp';
data myfolder.testscore;
infile 'c:\temp\testscore.txt' firstobs=2;
input report1 - report4 mid final;
run;
proc print data=myfolder.testscore; run;
data myfolder.testscore2;
set myfolder.testscore;
hw=report1+report2+report3+report4;
run;
proc print data=myfolder.testscore2; run;


data testscore2_1;
infile 'C:\Temp\myfolder.testscore2.sas';
hw_sum=hw*0.5;
mid2=mid*0.4;
final2=final*0.4;
totalscore=hw_sum+mid2+final2;
if totalscore>=90 then grade='A';
else if totalscore>=70 then grade='B';
else if totalscore>=50 then grade='C';
else if totalscore>=30 then grade='D';
else if totalscore<30 then grade='F';

proc sort data=testscore2_1 out=score_sort;
by descending totalscore;
run;
proc print data=score_sort;
run;

data testscore2_2 (drop= report1 report2 report3 report4 mid final hw);
set score_sort;
run;
proc print data=testscore2_2; run;
