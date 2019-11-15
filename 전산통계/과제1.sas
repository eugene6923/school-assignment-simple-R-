LIBNAME myfolder 'd:\temp';
data myfolder.testscore;
infile 'd:\temp\testscore.txt' firstobs=2;
input report1 - report4 mid final;
hw=report1+report2+report3+report4;
proc print data=myfolder.testscore; run;
