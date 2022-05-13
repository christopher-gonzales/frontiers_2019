/*TITLE:        descriptives_and_analyses_rlpl_t1t2_c2
  INVESTIGATOR: Megan McClelland (Lead)
  SAMPLE:       RLPL Grant (IES Goal 2) - Cohort 2: t1, t2, 
  PURPOSE:      To run basic descriptives and analyses of the RLPL Paper
  Last Edited:  5/12/22
  Edited By:    Christopher R Gonzales
*/

********************************************************************************
*       Descriptives file for Goal 2 Paper January 2019                        *
********************************************************************************


****extra stata modules****

*note: only uncomment if needed to be installed on the current workstation

/*
*fre - frequency tables
ssc install fre

*pcorrmatt - pairwise correlations controlling for multiple variables
ssc install pcorrmat
*/

****Data file location****

cd "..\data"
clear
use alldata_t1t2_c2.dta


********************************************************************************
*       (0) Update Data File                                                   *
********************************************************************************

***Update Missing Birthdates in Posted Data File****

replace bday = date("27sep2013","DMY") if id ==96
replace bday = date("16may2013","DMY") if id==167
replace bday = date("2sep2013","DMY") if id==160
replace bday = date("18oct2013","DMY") if id==224
replace bday = date("26sep2013","DMY") if id==226
replace bday = date("20sep2013","DMY") if id==155

**recalculate age variables
replace agemos_1 = (avdate_1 - bday)/30.5 if agemos_1==.
replace ageyrs_1 = (avdate_1 - bday)/365.25 if ageyrs_1==.

replace agemos_2 = (avdate_2 - bday)/30.5 if agemos_2==.
replace ageyrs_2 = (avdate_2 - bday)/30.5 if ageyrs_2==.

********************************************************************************
*       (1) Summary of Time Points/Measures available                          *
********************************************************************************

*Sample size at each time point* 

/*
*note "missing_" variables are a dummy variable tracking whether participants 
completed any tasks during the time point* 
*/ 

fre missing_*

**recode missing_* to change . (system missing) to 1 (missing code)
recode missing_* (.=1)

*total sample size with data at both time points
tab2 missing_*

*Attrition Analysis*
*create dummy code for missing at any time point (=0; 1 = missing either time)
gen attrition=0 if missing_1==0 & missing_2==0
recode attrition (.=1)

fre attrition

pwcorr attrition agemos_* gender htkrts_* dngtsm_* dcbrsm_* wjwmw_* wjlww_* ///
penssm_* cmasm_*, obs sig

**drop cases if missing on both timepoints
drop if attrition==1

***Count of each measure at each timepoint***

**EF Measures**

misstable sum htkrts_* dngtsm_* dcbrsm_* wjwmw_*

**Academic Measures**

misstable sum wjlww_* penssm_* cmasm_*

********************************************************************************
*     (2) Descriptives of individual Measures - Table 1 & Table 2              *
********************************************************************************

***Recode Cond to collapse over App Groups***
recode cond (3=1) (4=2), gen(cond2)
label def cond2 0 "BAU" 1 "SR" 2 "SR+"
label val cond2 cond2 

***Recode Cond to Any treatment vs no treatment***
recode cond (2=1) (3=1) (4=1), gen(treatment)
label def treatment 0 "BAU" 1 "Any Treatment"
label val treatment treatment

***generate raw difference scores***
gen htkrts_diff = htkrts_2-htkrts_1
gen dngtsm_diff = dngtsm_2 - dngtsm_1
gen dcbrsm_diff = dcbrsm_2 - dcbrsm_1
gen wjwmw_diff = wjwmw_2 - wjwmw_1
gen wjlww_diff = wjlww_2 - wjlww_1
gen penssm_diff = penssm_2 - penssm_1
gen cmasm_diff = cmasm_2 - cmasm_1

sum htkrts_diff - cmasm_diff

***generate residual scores***
regress htkrts_2 htkrts_1
predict htkrts_res, resid

regress dngtsm_2 dngtsm_1
predict dngtsm_res, resid

regress dcbrsm_2 dcbrsm_1
predict dcbrsm_res, resid

regress wjwmw_2 wjwmw_1
predict wjwmw_res, resid

regress wjlww_2 wjlww_1
predict wjlww_res, resid

regress penssm_2 penssm_1
predict penssm_res, resid

regress cmasm_2 cmasm_1
predict cmasm_res, resid

sum htkrts_res - cmasm_res

***Table 1 - Bivariate Correlations of Measures***

*full table

pwcorr agemos_1 gender cpsan_1 htkrts_1 htkrts_2 htkrts_diff dngtsm_1 dngtsm_2 ///
dngtsm_diff dcbrsm_1 dcbrsm_2 dcbrsm_diff wjlww_1 wjlww_2 wjlww_diff ///
penssm_1 penssm_2 penssm_diff cmasm_1 cmasm_2 cmasm_diff 

pwcorr agemos_1 gender cspan_1 htkrts_1 htkrts_2 htkrts_diff dngtsm_1 dngtsm_2 ///
dngtsm_diff dcbrsm_1 dcbrsm_2 dcbrsm_diff wjlww_1 wjlww_2 wjlww_diff ///
penssm_1 penssm_2 penssm_diff cmasm_1 cmasm_2 cmasm_diff,

pwcorr agemos_1 gender cspan_1 htkrts_1 htkrts_2 htkrts_diff dngtsm_1 dngtsm_2 ///
dngtsm_diff dcbrsm_1 dcbrsm_2 dcbrsm_diff wjlww_1 wjlww_2 wjlww_diff ///
penssm_1 penssm_2 penssm_diff cmasm_1 cmasm_2 cmasm_diff, sig


cpcorr agemos_1 gender htkrts_1 htkrts_2 htkrts_diff dngtsm_1 dngtsm_2 ///
dngtsm_diff dcbrsm_1 dcbrsm_2 dcbrsm_diff wjlww_1 wjlww_2 wjlww_diff ///
penssm_1 penssm_2 penssm_diff cmasm_1 cmasm_2 cmasm_diff, f(%4.2f) sig

*no sig or obs to copy over for formatting
pwcorr agemos_1 gender htkrts_1 htkrts_2 htkrts_res dngtsm_1 dngtsm_2 ///
dngtsm_res dcbrsm_1 dcbrsm_2 dcbrsm_res wjwmw_1 wjwmw_2 wjwmw_res wjlww_1 ///
wjlww_2 wjlww_res penssm_1 penssm_2 penssm_res, 

*final table 
pwcorr agemos_1 gender cspan_1 htkrts_1 htkrts_2 htkrts_diff dngtsm_1 dngtsm_2 ///
dngtsm_diff wjlww_1 wjlww_2 wjlww_diff penssm_1 penssm_2 penssm_diff sr sr_plus, 

*final table 
pwcorr agemos_1 gender cspan_1 htkrts_1 htkrts_2 htkrts_diff dngtsm_1 dngtsm_2 ///
dngtsm_diff wjlww_1 wjlww_2 wjlww_diff penssm_1 penssm_2 penssm_diff sr sr_plus, obs sig

***Table 2 descriptives******

*descriptives bau
sum agemos_1 gender cspan_1 htkrts_1 htkrts_2 htkrts_diff dngtsm_1 dngtsm_2 ///
dngtsm_diff dcbrsm_1 dcbrsm_2 dcbrsm_diff wjlww_1 ///
wjlww_2 wjlww_diff penssm_1 penssm_2 penssm_diff ///
if cond2==0

*descriptives SR
sum agemos_1 gender cspan_1 htkrts_1 htkrts_2 htkrts_diff dngtsm_1 dngtsm_2 ///
dngtsm_diff dcbrsm_1 dcbrsm_2 dcbrsm_diff wjlww_1 ///
wjlww_2 wjlww_diff penssm_1 penssm_2 penssm_diff ///
if cond2==1

*descriptives SR+
sum agemos_1 gender cspan_1 htkrts_1 htkrts_2 htkrts_diff dngtsm_1 dngtsm_2 ///
dngtsm_diff dcbrsm_1 dcbrsm_2 dcbrsm_diff wjlww_1 ///
wjlww_2 wjlww_diff penssm_1 penssm_2 penssm_diff ///
if cond2==2

*descriptives Any Treatment
sum agemos_1 gender cspan_1 htkrts_1 htkrts_2 htkrts_diff dngtsm_1 dngtsm_2 ///
dngtsm_diff dcbrsm_1 dcbrsm_2 dcbrsm_diff wjlww_1 ///
wjlww_2 wjlww_diff penssm_1 penssm_2 penssm_diff if treatment==1

**f-tests and power calcualtions

*demographics
ttest agemos_1, by(treatment)
esize twosample agemos_1, by(treatment) all

tab gender cond2, chi2
tab cspan_1 treatment, chi2

**Anytreatment vs BAU***

ttest htkrts_1, by(treatment)
esize twosample htkrts_1, by(treatment) all

ttest htkrts_2, by(treatment)
esize twosample htkrts_2, by(treatment) all

ttest htkrts_diff, by(treatment)
esize twosample htkrts_diff, by(treatment) all

ttest dngtsm_1, by(treatment)
esize twosample dngtsm_1, by(treatment) all

ttest dngtsm_2, by(treatment)
esize twosample dngtsm_2, by(treatment) all

ttest dngtsm_diff, by(treatment)
esize twosample dngtsm_diff, by(treatment) all

ttest wjlww_1, by(treatment)
esize twosample wjlww_1, by(treatment) all

ttest wjlww_2, by(treatment)
esize twosample wjlww_2, by(treatment) all

ttest wjlww_diff, by(treatment)
esize twosample wjlww_diff, by(treatment) all

ttest penssm_1, by(treatment)
esize twosample penssm_1, by(treatment) all

ttest penssm_2, by(treatment)
esize twosample penssm_2, by(treatment) all

ttest penssm_diff, by(treatment)
esize twosample penssm_diff, by(treatment) all


********************************************************************************
*     (3) Regression Analyses                                                  *
********************************************************************************

****Recodes and Centering****

*recode cspan to flip 0 value
recode cspan_1 (0=1) (1=0), gen(native)
label def native 0 "ELL" 1 "non-ELL"
label val native native

*mean center agemos_1
sum agemos_1

gen agecent = agemos_1-52
label var agecent "agemos_1 centered at 52 months"
 

*mean center htkrts_1

sum htkrts_1

gen htkrcent_1 = htkrts_1 - 22.83
label var htkrcent_1 "htkrts_1 centered at mean"


 
***test imbalance of cspan variable across treatment groups***
*BAU vs SR vs SR+
tab native cond2, chi2

*BAU vs Any treatment
tab native treatment, chi2

*Low SR code

recode htkr_c_1 (2=1), gen(dropback_1)
label def dropback 0 "No dropback" 1 "Any dropback"
label val dropback dropback_1

*SR and SR+ effect size codes
recode cond2 (1=0) (2=1), gen(sr_plus)
recode cond2 (2=0) (1=1), gen(sr)
recode cond2 (1=0) (2=0) (0=1), gen(bau)

***************HTKS***************

***BAU vs Any Treatment***

*Main effects only*
regress htkrts_diff htkrts_1 c.agecent i.cspan_1 i.treatment, beta
margins i.treatment, contrast(nowald effects)
esize twosample htkrts_diff, by(treatment)


***BAU vs SR vs SR+***

*Main effects only
regress htkrts_diff htkrts_1 c.agemos_1 i.cspan_1 i.cond2, beta
margins h.cond2, contrast(nowald effects)

esize twosample htkrts_diff, by(SR)
esize twosample htkrts_diff, by(SR_plus)


***************Pens***************

***BAU vs Any Treatment***

*Main effects only*
regress penssm_diff penssm_1 c.agecent i.cspan_1 i.treatment, beta
margins i.treatment, contrast(nowald effects)
esize twosample penssm_diff, by(treatment)


***BAU vs SR vs SR+***

*Main effects only
regress penssm_diff penssm_1 c.agemos_1 i.cspan_1 i.cond2, beta
margins h.cond2, contrast(nowald effects)

esize twosample penssm_diff, by(SR)
esize twosample penssm_diff, by(SR_plus)
 
***************LetterWord***************

***BAU vs Any Treatment***

*Main effects only*
regress wjlww_diff wjlww_1 c.agecent i.cspan_1 i.treatment, beta
margins i.treatment, contrast(nowald effects)
esize twosample wjlww_diff, by(treatment)


***BAU vs SR vs SR+***

*Main effects only
regress wjlww_diff wjlww_1 c.agemos_1 i.cspan_1 i.cond2, beta
margins h.cond2, contrast(nowald effects)




****************Predicting Time 2 from Time 1******************

******BAU vs SR vs SR+****

****HTKS****
regress htkrts_2 c.htkrts_1 c.agemos_1 i.cspan_1 i.cond2, beta


****PENS****
regress penssm_2 c.penssm_1 c.agemos_1 i.cspan_1 i.cond2, beta


****LetterWord****
regress wjlww_2 c.wjlww_1 c.agemos_1 i.cspan_1 i.cond2, beta

******BAU VS Any Treatment****
****HTKS****
regress htkrts_2 c.htkrts_1 c.agecent i.cspan_1 i.treatment, beta


****PENS****
regress penssm_2 c.penssm_1 c.agemos_1 i.cspan_1 i.treatment, beta


****LetterWord****
regress wjlww_2 c.wjlww_1 c.agemos_1 i.cspan_1 i.treatment, beta


**********LOW SR Kids*********
****HTKS****



regress htkrts_2 c.htkrcent_1 i.cspan_1 c.agecent i.cond2##dropback, beta
contrast i.cond2##i.dropback
margins i.cond2##i.dropback, contrast(nowald effects)
marginsplot, noci


regress dngtsm_2 c.dngtcent_1##cond2 i.cspan_1 c.agecent, beta

regress wjlww_2 c.wjlwwcent_1##cond2 i.cspan_1 c.agecent, beta

regress htkrts_diff c.htkrts_1 i.cspan_1 c.agemos_1 i.cond2 if dropback==1, beta


regress penssm_diff c.penssm_1 i.cspan_1 c.agemos_1 i.cond2 if dropback==1, beta



*******Table 3****

sem (htkrts_1 agemos_1 cspan_1  treatment -> htkrts_2), method(mlmv) vce(cluster cluster_1)  
sem (penssm_1 agemos_1 cspan_1 treatment -> penssm_2), method(mlmv) vce(cluster cluster_1) 
sem (wjlww_1 agemos_1 cspan_1 treatment -> wjlww_2), method(mlmv) vce(cluster cluster_1) 


sem (htkrts_1 agemos_1 cspan_1 gender parented sr sr_plus -> htkrts_2), method(mlmv) vce(cluster cluster_1)  
sem (penssm_1 agemos_1 cspan_1 sr sr_plus -> penssm_2), method(mlmv) vce(cluster cluster_1)  
sem (wjlww_1 agemos_1 cspan_1 sr sr_plus -> wjlww_2), method(mlmv) vce(cluster cluster_1) 

gsem (EF_1 -> htkrts_1 dcbrsm_1 dngtsm_1) (EF_2 -> htkrts_2 dcbrsm_2 dngtsm_2) (EF_1 agemos_1 cspan_1 sr sr_plus -> EF_2), vce(cluster cluster_1) 


***Interaction term)

gen srdbint = sr*dropback_1
gen sr_plusdbin = sr_plus*dropback_1
gen treatint = treatment*dropback_1

sem (htkrts_1 agemos_1 cspan_1 dropback_1 treatment treatint-> htkrts_2), method(mlmv) vce(cluster cluster_1) standardized


********************************************************************************
*                    UpDated Analyses G2 Revision 6-28-19                      *
********************************************************************************

*Adding Gender and ParentEd back into regression models*

*merge in demographic data
merge 1:1 id using pdem_rlpl.dta

*create average of parent1 and parent2 years of education
egen parented = rowmean(dem11 dem16), missingc

recode htkrts_* penssm_* wjlww_* (.a .r .n .m=.)

*SR and SR+ effect size codes
recode cond2 (1=0) (2=1), gen(sr_plus)
recode cond2 (2=0) (1=1), gen(sr)
recode cond2 (1=0) (2=0) (0=1), gen(bau)

****Htks

*Bau VS Anytreatment

sem (htkrts_1 agemos_1 cspan_1 gender parented treatment -> htkrts_2), ///
method(mlmv) vce(cluster cluster_1) standardized

*Bau vs SR vs SR+

sem (htkrts_1 agemos_1 cspan_1 gender parented sr sr_plus -> htkrts_2), ///
method(mlmv) vce(cluster cluster_1)

*SR vs SR+

sem (htkrts_1 agemos_1 cspan_1 gender parented bau sr_plus -> htkrts_2), ///
method(mlmv) vce(cluster cluster_1)

****PENS

*Bau VS Anytreatment

sem (penssm_1 agemos_1 cspan_1 gender parented treatment -> penssm_2), ///
method(mlmv) vce(cluster cluster_1) standardized

*Bau vs SR vs SR+

sem (penssm_1 agemos_1 cspan_1 gender parented sr sr_plus -> penssm_2), ///
method(mlmv) vce(cluster cluster_1) 

*SR vs SR+

sem (penssm_1 agemos_1 cspan_1 gender parented bau sr_plus -> penssm_2), ///
method(mlmv) vce(cluster cluster_1)

****LetterWord

*Bau VS Anytreatment

sem (wjlww_1 agemos_1 cspan_1 gender parented treatment -> wjlww_2), ///
method(mlmv) vce(cluster cluster_1) 

*Bau vs SR vs SR+

sem (wjlww_1 agemos_1 cspan_1 gender parented sr sr_plus -> wjlww_2), ///
method(mlmv) vce(cluster cluster_1) standardized

***dngt

*Bau VS Anytreatment

sem (dngtsm_1 agemos_1 cspan_1 gender parented treatment -> dngtsm_2), ///
method(mlmv) vce(cluster cluster_1) 

*Bau vs SR vs SR+

sem (dngtsm_1 agemos_1 cspan_1 gender parented sr sr_plus -> dngtsm_2), ///
method(mlmv) vce(cluster cluster_1) standardized


****LOW SR interaction 

***Centering

*mean center agemos_1

gen agecent = agemos_1-52
label var agecent "agemos_1 centered at 52 months"
 

*mean center htkrts_1

sum htkrts_1

gen htkrcent_1 = htkrts_1 - 22.83
label var htkrcent_1 "htkrts_1 centered at mean"

*mean center parent ed

sum parented

gen parentedcent = parented-11.26
label var parentedcent "parent ed centered at mean"


*subgroup analysis
sem (htkrts_diff <- htkrcent_1 cspan_1 agecent gender parented sr sr_plus), method(mlmv) vce(cluster cluster_1)


***Calculating ICC's***

mixed htkrts_1 || cluster_1:
estat icc

mixed htkrts_2 || cluster_1:
estat icc

mixed htkrts_2 htkrts_1 || cluster_1:
estat icc

mixed penssm_1 || cluster_1:
estat icc

mixed penssm_2 || cluster_1:
estat icc

mixed penssm_2 penssm_1 agemos_1 cspan_1|| cluster_1:
estat icc

mixed wjlww_1 || cluster_1:
estat icc

mixed wjlww_2 || cluster_1:
estat icc

mixed wjlww_2 wjlww_1 || cluster_1:
estat icc



sem (htkrts_2<-), method(mlmv) 




