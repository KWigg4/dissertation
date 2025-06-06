**********************************************************************************************
AUTHOR   : KTW
PROJECT  : ISP Utilization
PURPOSE  : Macro, COST dv's
VERSION  : 2023-10-20
OUTPUT   : pdf & log file

https://stats.oarc.ucla.edu/sas/dae/negative-binomial-regression/
The param=ref option changes the coding of prog from effect coding, which is the default, to reference coding (which gives dummy coding)
The ref=first option changes the reference group to the first level of prog.  We have used two options
on the model statement.  The type3 option is used to get the multi-degree-of-freedom test of the categorical
variables listed on the class statement
***********************************************************************************************;
%macro hurdle(dat=,pvar=,cvar=,dv=,type=);

/*SECTION 01: INTRO / CONFIG/ DOCUMENTATION*/
OPTIONS SOURCE;
/*pageno=1 linesize=88 pagesize=60 */
%LET root  = %qsubstr(%sysget(SAS_EXECFILEPATH), 1,
             %length(%sysget(SAS_EXECFILEPATH))-%length(%sysget(SAS_EXECFILEname)));
%LET script  = %qsubstr(%sysget(SAS_EXECFILENAME), 1,
               %length(%sysget(SAS_EXECFILENAME))-4); * remove .sas to use in log, pdf;
%LET today = %SYSFUNC(today(), YYMMDD10.);
%LET log   = &root.run_&dv._type_&type._&today..log;

PROC PRINTTO LOG = "&log"; RUN;

/*SECTION 02: PROB MODEL*/
PROC GENMOD DATA  = &dat;
CLASS mcaid_id
       int(ref='0')         int_imp(ref='0')
       budget_grp_new(ref='MAGI Eligible Children')
       race(ref='non-Hispanic White/Caucasian')
       sex(ref='Female')
       rae_person_new(ref='3')
       age_cat(ref='ages 21-44')
       fqhc(ref ='0')
       bh_oth17(ref='0')    bh_oth18(ref='0')       bh_oth19(ref='0')
       bh_er17(ref='0')     bh_er18(ref='0')        bh_er19(ref='0')
       bh_hosp17(ref='0')   bh_hosp18(ref='0')      bh_hosp19(ref='0')
       adj_pd_total_17cat(ref='0')
       adj_pd_total_18cat(ref='0')
       adj_pd_total_19cat(ref='0')
      &pvar;
MODEL &pvar(event='1') = int int_imp time season1 season2 season3 budget_grp_new race sex rae_person_new age_cat fqhc
                         bh_oth17 bh_oth18 bh_oth19 bh_er17 bh_er18 bh_er19 bh_hosp17 bh_hosp18 bh_hosp19
                         adj_pd_total_17cat adj_pd_total_18cat adj_pd_total_19cat / DIST=binomial;
REPEATED SUBJECT = mcaid_id / type=&type ;
store out.&dv._pmodel_&type;
output out = predout pred = pred;
run;

/*SECTION 03: POSITIVE COST MODEL*/
TITLE 'Cost Model';
PROC GENMOD DATA  = &dat desc;
WHERE &cvar > 0;
CLASS mcaid_id
       int(ref='0')         int_imp(ref='0')
       budget_grp_new(ref='MAGI Eligible Children')
       race(ref='non-Hispanic White/Caucasian')
       sex(ref='Female')
       rae_person_new(ref='3')
       age_cat(ref='ages 21-44')
       fqhc(ref ='0')
       bh_oth17(ref='0')    bh_oth18(ref='0')       bh_oth19(ref='0')
       bh_er17(ref='0')     bh_er18(ref='0')        bh_er19(ref='0')
       bh_hosp17(ref='0')   bh_hosp18(ref='0')      bh_hosp19(ref='0')
       adj_pd_total_17cat(ref='0')
       adj_pd_total_18cat(ref='0')
       adj_pd_total_19cat(ref='0') ;
MODEL &cvar = int int_imp time season1 season2 season3 budget_grp_new race sex rae_person_new age_cat fqhc
              bh_oth17 bh_oth18 bh_oth19 bh_er17 bh_er18 bh_er19 bh_hosp17 bh_hosp18 bh_hosp19
              adj_pd_total_17cat adj_pd_total_18cat adj_pd_total_19cat / dist=gamma link=log ;
REPEATED SUBJECT = mcaid_id / type = &type;
store out.&dv._cmodel_&type;
RUN;
TITLE;

/*SECTION 04: AGGREGATING ACTUAL, PREDICTED OUTCOMES W/ GROUP OF INTEREST*/

* the predictions for cost will be made for each person twice, once exposed and once unexposed;
* OUT:[P_INTGROUP]      IN :[out=out.&dv._pmodel]
 prob of util------------------------------------------------ ;
proc plm restore=out.&dv._pmodel_&type ;
   score data=data.intgroup out=p_intgroup predicted=p_prob / ilink;
run;

* OUT:[CP_INTGROUP]     IN :[out=out.&dv._cmodel]
prob of cost -------------------------------------------------;
proc plm restore=out.&dv._cmodel_&type ;
   score data=p_intgroup out=cp_intgroup predicted=p_cost / ilink;
run;

* OUT:[out.&dv._mean] IN:[cp_intgroup]
Person average cost is calculated------------------------------;
data out.&dv._mean_&type ;
  set cp_intgroup;
  a_cost = p_prob*p_cost;* (1-p term = 0);
run;

* OUT:[out.&dv._avp] IN:[out.&dv._mean]
Group average cost is calculated and contrasted-----------------;
proc sql;
create table out.&dv._avp_&type as
  select mean(case when exposed=1 then a_cost else . end ) as cost_exposed,
         mean(case when exposed=0 then a_cost else . end ) as cost_unexposed,
  calculated cost_exposed - calculated cost_unexposed as cost_diff
  from out.&dv._mean_&type;
quit;

* put AVP results in log for reference/ comparing results;
data _NULL_;
SET  out.&dv._avp_&type;
put  cost_exposed= cost_unexposed= cost_diff=;
RUN;

proc rank data = predout out = predgroup groups = 10;
  var pred;
  ranks predgroup;
run;

proc means data = predgroup   noprint nway;
  var pred ind_&dv.;
  class predgroup;
  output out = out.&dv._meanout_&type mean = /autoname;
run;

data _NULL_;
SET  out.&dv._meanout_&type (DROP= _TYPE_);
/*SET out.visits_pc_meanout_exch (DROP= _TYPE_); */
put _all_;
RUN;

PROC PRINTTO; RUN;
%mend;



