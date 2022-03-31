*************************************************************
***Socioeconomic inequalities in blood
***pressure: co-ordinated analysis of 147,775
***participants from repeated birth cohort and
***cross-sectional datasets, 1989 to 2016
***BMC Medicine (2020) 18:338
**************************************************************

use "N:\Temp\HSE1994_2016_BP.dta", clear
renvars, lower
svyset [pweight=wt_nurse],psu(point1)
drop if age<25
keep if inrange(topqual5,1,4)
*N=129,118

*Add mmHg to med users.
generate omsysval10=-2
replace omsysval10 = omsysval if (bpmedd==0)
replace omsysval10 = omsysval+10 if (bpmedd==1)
generate omdiaval5=-2
replace omdiaval5 = omdiaval if (bpmedd==0)
replace omdiaval5 = omdiaval+5 if (bpmedd==1)

*% below/above mmHg thresholds
gen a2 = omsysval10>=140
gen a3 = omdiaval5>=90
svy:mean a2 a3
di 1-(.2862072)         
di 1-(.2142006)
*0.71/0.29.
*0.79/0.21

*Hypertension (survey-defined).
generate htn = (highbp4==2|highbp4==3|highbp4==4)

*Medication.
generate BPmed = (highbp4==2|highbp4==3)

*Controlled.
generate BPcontrol = (highbp4==2)

generate degree = topqual5==1

******************************************
*******Table 1: 1994;2003;2016 ***********
******************************************

tab1 hseyear  /* N (unwtd) */
svy: mean omsysval,over(hseyear)
estat sd
svy: mean omdiaval,over(hseyear)
estat sd
svy: mean htn,over(hseyear)
svy: mean BPmed,over(hseyear)
svy: mean degree,over(hseyear)


******************************
**ridit scores (yr-specific)
******************************
wridit topqual5 if !missing(topqual5) [pweight=wt_nurse], gene(ed5_ridit) by(hseyear)

*************************************************.
* Fig 1.
* SII. By year. SBP+10: sex- and age-adjusted.
**************************************************

svy: regress omsysval10 c.ed5_ridit i.sex i.ag16g10                /* Pooled */

foreach num of numlist 1/5 7/10 12/23 {
generate subpop=0
replace subpop=1 if hseyear==`num'
svy,subpop(subpop): regress omsysval10 c.ed5_ridit i.sex i.ag16g10
drop subpop
}

*************************************************.
* Fig S2.
* SII. By year. DBP+5: sex- and age-adjusted.
*************************************************.

svy: regress omdiaval5 c.ed5_ridit i.sex i.ag16g10

foreach num of numlist 1/5 7/10 12/23 {
generate subpop=0
replace subpop=1 if hseyear==`num'
svy,subpop(subpop): regress omdiaval5 c.ed5_ridit i.sex i.ag16g10
drop subpop
}

*************************************************.
* Fig S2.
* SII. By year. HTN: sex- and age-adjusted.
*************************************************.

svy: regress htn c.ed5_ridit i.sex i.ag16g10

foreach num of numlist 1/5 7/10 12/23 {
generate subpop=0
replace subpop=1 if hseyear==`num'
svy,subpop(subpop): regress htn c.ed5_ridit i.sex i.ag16g10
drop subpop
}

*************************************************.
* Fig S3.
* SII. By year. use of BP Tx: sex-, BP- and age-adjusted.
*************************************************.

svy: regress BPmed c.ed5_ridit omsysval10 i.sex i.ag16g10          

foreach num of numlist 1/5 7/10 12/23 {
generate subpop=0
replace subpop=1 if hseyear==`num'
svy,subpop(subpop): regress BPmed c.ed5_ridit omsysval10 i.sex i.ag16g10  
drop subpop
}

*************************************************.
* Fig S4.
* SII. By year. raw SBP: sex- and age-adjusted.
*************************************************.
svy: regress omsysval c.ed5_ridit i.sex i.ag16g10

foreach num of numlist 1/5 7/10 12/23 {
generate subpop=0
replace subpop=1 if hseyear==`num'
svy,subpop(subpop): regress omsysval c.ed5_ridit i.sex i.ag16g10
drop subpop
}

*************************************************.
* Fig S5.
* SII. By year. SBP: sex-, bmi- and age-adjusted.
*************************************************.

mvdecode bmival2, mv(-1)
wridit topqual5 if !missing(topqual5) & !missing(bmival2) [pweight=wt_nurse], gen(ed5_ridit2) by(hseyear)

gen a = bmival2!=.

svy,subpop(a): regress omsysval10 c.ed5_ridit2 i.sex i.ag16g10                /* Pooled */
svy,subpop(a): regress omsysval10 c.ed5_ridit2 c.bmival2 i.sex i.ag16g10      /* Pooled */

foreach num of numlist 1/5 7/10 12/23 {
generate subpop=0
replace subpop=1 if hseyear==`num' & bmival2!=.
svy,subpop(subpop): regress omsysval10 c.ed5_ridit2 i.sex i.ag16g10
svy,subpop(subpop): regress omsysval10 c.ed5_ridit2 c.bmival2 i.sex i.ag16g10
drop subpop
}

******************************************************
* Fig S6.
* SII. By year. SBP: age-adjusted: stratified by gender.
********************************************************

wridit topqual5 if !missing(topqual5) & sex==1 [pweight=wt_nurse], gene(ed5_riditm) by(hseyear)
wridit topqual5 if !missing(topqual5) & sex==2 [pweight=wt_nurse], gene(ed5_riditf) by(hseyear)

gen male = sex==1
svy,subpop(male): regress omsysval10 c.ed5_riditm i.ag16g10

foreach num of numlist 1/5 7/10 12/23 {
generate subpop=0
replace subpop=1 if hseyear==`num' & sex==1
svy,subpop(subpop): regress omsysval10 c.ed5_riditm i.ag16g10
drop subpop
}

gen female = sex==2
svy,subpop(female): regress omsysval10 c.ed5_riditf i.ag16g10

foreach num of numlist 1/5 7/10 12/23 {
generate subpop=0
replace subpop=1 if hseyear==`num' & sex==2
svy,subpop(subpop): regress omsysval10 c.ed5_riditf i.ag16g10
drop subpop
}

*******************************************************************
* Fig S7.
* SII. By year. SBP: age- and gender-adjusted: stratified age-group
********************************************************************

wridit topqual5 if !missing(topqual5) & inrange(ag16g10,2,4) [pweight=wt_nurse], gen(ed5_riditage1) by(hseyear)

gen age1 = inrange(ag16g10,2,4)
svy,subpop(age1): regress omsysval10 c.ed5_riditage1 i.sex i.ag16g10

foreach num of numlist 1/5 7/10 12/23 {
generate subpop=0
replace subpop=1 if hseyear==`num' & inrange(ag16g10,2,4)
svy,subpop(subpop): regress omsysval10 c.ed5_riditage1 i.sex i.ag16g10
drop subpop
}

wridit topqual5 if !missing(topqual5) & inrange(ag16g10,5,7) [pweight=wt_nurse], gen(ed5_riditage2) by(hseyear)

gen age2 = inrange(ag16g10,5,7)
svy,subpop(age2): regress omsysval10 c.ed5_riditage2 i.sex i.ag16g10

foreach num of numlist 1/5 7/10 12/23 {
generate subpop=0
replace subpop=1 if hseyear==`num' & inrange(ag16g10,5,7)
svy,subpop(subpop): regress omsysval10 c.ed5_riditage2 i.sex i.ag16g10
drop subpop
}


*******************************
*** Fig 3 Quantile regression
*** SBP and DBP.
********************************

*SBP: threshold at 0.71.
*0.5Q is at 4.
*0.75Q is at 5.
*thus: 0.71 = 4 + (21/25) = 84% (so 84% of the distance between 4 and 5).

*DBP: threshold at 0.79.
*0.75Q is at 5.
*0.90Q is at 6.
*thus: 0.79 = 5 + (4/15) = 5.27 (so 27% of the distance between 5 and 6).

*SBP
local quantiles 5 10 25 50 75 90 95     
local models ""                            
local xlabel ""                             
local j=1                                   

foreach q of numlist `quantiles' {

    qreg omsysval10  ed5_ridit i.sex i.ag16g10 [pw=wt_nurse], quantile(`q') 
    estimates store me_tu`q'
    local models `"`models' me_tu`q' || "'
    local xlabel `"`xlabel' `j++' "Q{sub:`q'}""'
}

di "`models'
di `"`xlabel'"'

coefplot `models' /// 
, vertical bycoefs  ///
xlab(none) xlabel(`xlabel', add) ///
ytitle("SBP difference, mmHg") name(QregSBP, replace) ylabel(-2(1)12, angle(horiz)) drop(_cons 2.sex 2.ag16g10 3.ag16g10 4.ag16g10 5.ag16g10 6.ag16g10 7.ag16g10) ///
ciopts(recast(rcap))  graphregion(color(white) ) yline(0, lcolor(gs8) lpattern(dash)) title("") xline(4.84, lcolor(gs8) lpattern(shortdash))  ///
text(9 5  "Hypertension threshold (140mmHg)",size(medsmall))


*DBP.
local quantiles 5 10 25 50 75 90 95 
local models ""                             
local xlabel ""                             
local j=1                                   

foreach q of numlist `quantiles' {

    qreg omdiaval5  ed5_ridit i.sex i.ag16g10 [pw=wt_nurse], quantile(`q') 
    estimates store me_tu`q'
    local models `"`models' me_tu`q' || "'
    local xlabel `"`xlabel' `j++' "Q{sub:`q'}""'
}

di "`models'
di `"`xlabel'"'

coefplot `models' /// 
, vertical bycoefs  ///
xlab(none) xlabel(`xlabel', add) ///
ytitle("DBP difference, mmHg") name(QregDBP, replace) ylabel(-2(1)12, angle(horiz)) drop(_cons 2.sex 2.ag16g10 3.ag16g10 4.ag16g10 5.ag16g10 6.ag16g10 7.ag16g10) ///
ciopts(recast(rcap))  graphregion(color(white) ) yline(0, lcolor(gs8) lpattern(dash)) title("") xline(5.27, lcolor(gs8) lpattern(shortdash)) ////
text(9 5  "Hypertension threshold (90mmHg)",size(medsmall))







