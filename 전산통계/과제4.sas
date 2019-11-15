1번 문제

1)

LIBNAME myfolder 'c:\temp';
data myfolder.score;
input id $ dept $ age test1 test2 test3 gender $;
cards;
001 stat 22 9 12 14 m
002 law 21 10 15 7 f
003 econ 23 10 17 11 f
004 math 27 16 17 8 m
005 engl 21 11 12 16 f
006 stat 22 18 18 18 f
007 econ 7 17 18 18 m
run;
proc print data=myfolder.score; run;

2)

data score2;
set myfolder.score;
minscore=min(test1,test2,test3);
average=sum(of test1-test3)/3;
if minscore>=10 then result='pass';
else result='fail';
run;
proc print data=score2; run;

3)

data female;
set score2;
if gender='f';
run;
proc print data=female; run;


2번 문제

1)

data mid;
input name $ sex $ pre mid;
cards;
김철수 m 30 10
강민호 m 45 50
이영희 f . 15
박지수 f . 20
최병호 f . 8
run;
proc print data=mid; run;

data final;
input name $ sex $ pre final;
cards;
이영희 f 32 10
김철수 m . 40
박지수 f 15 20
강민호 m . 15
run;
proc print data=final; run;

2)

data problem2_2;
set mid final;
run;
proc print; run;

3)

data problem2_3;
set mid;
set final;
run;
proc print; run;

4)

proc sort data=mid; by name; run;
proc sort data=final; by name; run;

data problem2_4;
set mid final;
by name;
run;
proc print; run;
