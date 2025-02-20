data df;
    set pg1.proj3;
    
    if UrinaryBladder = 'NA' then UrinaryBladder_num = .; /* Replace 'NA' with missing */
     else if UrinaryBladder ne '' then UrinaryBladder_num = input(UrinaryBladder, 8.); /* Convert to numeric */
     else UrinaryBladder_num = .; /* Handle empty strings as missing */
    
    drop VAR1 UrinaryBladder;
    rename 
    	ScoreKidneys = Kidneys
    	UrinaryBladder_num = UrinaryBladder
    	'Superficial inflammation of bon'n = Bone
    	'Synovial Hyperplasia'n = SynovialHyperplasia
    	'Exudate within joint and/or ten'n = Exudate
    	'Overall inflammation (whole tis'n = OverallInflammation
    	OverallScore = OverallScore
        'Adapted overall score (includin'n = AdaptedOverallScore;
run;

%let outpath=/home/u64063880/EPG1V2/output;
ods noproctitle;
ods pdf file="&outpath/summary_statistics.pdf" style=journal;

/* 1. Desciptive Statistics */
proc means data=df n mean median std min max;
    var  Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate 
    	 OverallInflammation OverallScore AdaptedOverallScore;
    class Group; /* Grouping by experimental group */
run;

/* 2. Visualizations */
/* Box plot for all inflammatory scores by group */
proc sgplot data=df;
    vbox Kidneys / category=Group;
    title "Boxplot of Kidneys by Group";
run;
proc sgplot data=df;
    vbox UrinaryBladder / category=Group;
    title "Boxplot of UrinaryBladder by Group";
run;
proc sgplot data=df;
    vbox Bone / category=Group;
    title "Boxplot of Bone by Group";
run;
proc sgplot data=df;
    vbox SynovialHyperplasia / category=Group;
    title "Boxplot of SynovialHyperplasia by Group";
run;
proc sgplot data=df;
    vbox Exudate / category=Group;
    title "Boxplot of Exudate by Group";
run;
proc sgplot data=df;
    vbox OverallInflammation / category=Group;
    title "Boxplot of OverallInflammatio by Group";
run;
proc sgplot data=df;
    vbox OverallScore / category=Group;
    title "Boxplot of OverallScore by Group";
run;
proc sgplot data=df;
    vbox AdaptedOverallScore / category=Group;
    title "Boxplot of AdaptedOverallScore by Group";
run;

/* 3.Histograms with shapir-wilk normatily test */

%macro histograms_with_shapiro(var);
    /* Perform Shapiro-Wilk Test and get p-value */
    proc univariate data=df normaltest noprint;
        var &var;
        output out=shapiro_&var PROBN=pval;
    run;

    /* Capture the p-value from the output dataset */
    data _null_;
        set shapiro_&var;
        call symputx('pvalue', pval);
    run;

    /* Create Histogram and add footnote with Shapiro-Wilk p-value */
    proc sgplot data=df;
        histogram &var / group=Group;
        density &var;
        title "Histogram of &var";
        footnote j=c "Shapiro-Wilk p-value = &pvalue";
        styleattrs datacolors=(red green blue purple);
    run;

    /* Clean up temporary output dataset */
    proc datasets lib=work nolist;
        delete shapiro_&var;
    quit;
%mend;

/* Call the macro for each variable */
%histograms_with_shapiro(Kidneys);
%histograms_with_shapiro(UrinaryBladder);
%histograms_with_shapiro(Bone);
%histograms_with_shapiro(SynovialHyperplasia);
%histograms_with_shapiro(Exudate);
%histograms_with_shapiro(OverallInflammation);
%histograms_with_shapiro(OverallScore);
%histograms_with_shapiro(AdaptedOverallScore);


/* 4. T-test s */
/* Group A vs. Group B */
proc ttest data=df;
    class Group; 
    var Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate 
    	 OverallInflammation OverallScore AdaptedOverallScore;
    where Group in ('A', 'B'); 
    title "Independent t-test: Group A vs. Group B";
run;
/* Group C vs. Group D */
proc ttest data=df;
    class Group;
    var Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate 
    	 OverallInflammation OverallScore AdaptedOverallScore;
    where Group in ('C', 'D'); 
    title "Independent t-test: Group C vs. Group D";
run;

/* Group B vs. Group D */
proc ttest data=df;
    class Group;
    var Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate 
    	 OverallInflammation OverallScore AdaptedOverallScore;
    where Group in ('B', 'D'); 
    title "Independent t-test: Group B vs. Group D";
run;

/* Male vs. Female */
proc ttest data=df;
    class Sex;
    var Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate 
    	 OverallInflammation OverallScore AdaptedOverallScore;
    where Group in ('B', 'D'); 
    title "Independent t-test: sex";
run;

/* 5. Bootstrap */
%macro bootstrap_analysis(data=, group_var=, vars=, n_boot=10000, group1=, group2=);

/* Step 1: data subset */
data bootstrap_data;
    set &data;
    where &group_var in ("&group1", "&group2");
run;

/* Step 2: variable */
%let var_count = %sysfunc(countw(&vars));
%do i = 1 %to &var_count;
    %let var = %scan(&vars, &i);

/* Step 3: bootstrap sample */
proc surveyselect data=bootstrap_data out=boot_samples_&var seed=1234
                  method=urs samprate=1 outhits rep=&n_boot;
run;

/* Step 4: group mean in bootstrap sample */
/* only the code for Sex */
proc sort data=boot_samples_&var;
    by replicate Sex;
run;
/*                       */
proc means data=boot_samples_&var noprint;
    by replicate &group_var;
    var &var;
    output out=boot_means_&var mean=mean_value;
run;

/* Step 5: calculate group mean difference */
proc transpose data=boot_means_&var out=boot_diff_&var prefix=group_;
    by replicate;
    id &group_var;
    var mean_value;
run;

data boot_diff_&var;
    set boot_diff_&var;
    mean_diff = group_&group1 - group_&group2;
run;

/* Step 6: CI */
proc univariate data=boot_diff_&var noprint;
    var mean_diff;
    output out=ci_&var pctlpts=2.5 97.5 pctlpre=ci_;
run;

proc means data=p_value_&var noprint;
    var pos_diff;
    output out=pval_&var mean=bootstrap_p_value;
run;

/* Step 7: Print CI Result */
proc print data=ci_&var;
    title "Bootstrap Confidence Interval for &var";
run;
title;

%end;

%mend bootstrap_analysis;

/* Apply bootstrap macro */
%bootstrap_analysis(data=df, 
                    group_var=Group, 
                    vars=Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate OverallInflammation OverallScore AdaptedOverallScore, 
                    n_boot=10000, 
                    group1=A, 
                    group2=B);
%bootstrap_analysis(data=df, 
                    group_var=Group, 
                    vars=Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate OverallInflammation OverallScore AdaptedOverallScore, 
                    n_boot=10000, 
                    group1=C, 
                    group2=D);
%bootstrap_analysis(data=df, 
                    group_var=Group, 
                    vars=Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate OverallInflammation OverallScore AdaptedOverallScore, 
                    n_boot=10000, 
                    group1=B, 
                    group2=D);             
%bootstrap_analysis(data=df, 
                    group_var=Sex, 
                    vars=Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate OverallInflammation OverallScore AdaptedOverallScore, 
                    n_boot=10000, 
                    group1=Male, 
                    group2=Female);

/* Print the t-test & bootstrap result */                    
proc import datafile="/home/u64063880/EPG1V2/data/ttest_result1.csv"
    out=pg1.ttest_result1
    dbms=csv replace;
    guessingrows=max;
run;
proc print data=pg1.ttest_result1;
run;

proc import datafile="/home/u64063880/EPG1V2/data/ttest_result2.csv"
    out=pg1.ttest_result2
    dbms=csv replace;
    guessingrows=max;
run;
proc print data=pg1.ttest_result2;
run;

proc import datafile="/home/u64063880/EPG1V2/data/ttest_result3.csv"
    out=pg1.ttest_result3
    dbms=csv replace;
    guessingrows=max;
run;
proc print data=pg1.ttest_result3;
run;

proc import datafile="/home/u64063880/EPG1V2/data/ttest_result4.csv"
    out=pg1.ttest_result4
    dbms=csv replace;
    guessingrows=max;
run;
proc print data=pg1.ttest_result4;
run;


/* 6. MANOVA */

/* A vs. B */
proc glm data=df;
    class Group;
    where Group in ('A', 'B'); 
    model Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate
          OverallInflammation OverallScore AdaptedOverallScore = Group;
    manova h=Group / printe printh;
    title "MANOVA for A vs. B";
run;

/* C vs. D */
proc glm data=df;
    class Group;
    where Group in ('C', 'D');
    model Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate
          OverallInflammation OverallScore AdaptedOverallScore = Group;
    manova h=Group / printe printh;
    title "MANOVA for C vs. D";
run;

/* B vs. D */
proc glm data=df;
    class Group;
    where Group in ('B', 'D');
    model Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate
          OverallInflammation OverallScore AdaptedOverallScore = Group;
    manova h=Group / printe printh;
    title "MANOVA for B vs. D";
run;

/* Male vs. Female */
proc glm data=df;
    class Sex; 
    where Sex in ('Male', 'Female'); 
    model Kidneys UrinaryBladder Bone SynovialHyperplasia Exudate
          OverallInflammation OverallScore AdaptedOverallScore = Sex; 
    manova h=Sex / printe printh; 
    title "MANOVA for Male vs. Female";
run;
ods pdf close;
ods proctitle;
