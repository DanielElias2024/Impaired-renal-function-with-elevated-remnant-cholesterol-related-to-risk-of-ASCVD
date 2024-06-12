# Impaired-renal-function-with-elevated-remnant-cholesterol-related-to-risk-of-ASCVD

use "cgps", clear 
count  //109,574

**Fig1**

gen r_eGFR2009 = round(eGFR2009*10)/10
gen rev_eGFR09 = r_eGFR2009*-1 
replace rev_eGFR09=. if rev_eGFR09<-140 


*All 140-10)
* Cubic spline
mkspline stub_eGFR_3_new  = rev_eGFR09 , cubic nknots(3) di
mkspline stub_eGFR_4_new  = rev_eGFR09 , cubic nknots(4)
mkspline stub_eGFR_5_new  = rev_eGFR09 , cubic nknots(5)
mkspline stub_eGFR_6_new  = rev_eGFR09 , cubic nknots(6)
mkspline stub_eGFR_7_new = rev_eGFR09 , cubic nknots(7)

* Upper panel *
glm ldl stub_eGFR_7_new* age i.sex
predict yhat2
predict SE_yhat2, stdp
gen ldlup=yhat2+(1.96*SE_yhat2)
gen ldllow=yhat2-(1.96*SE_yhat2)

glm remnants stub_eGFR_7_new* age i.sex
predict yhatrem2
predict SE_yhatrem, stdp
gen remup=yhatrem2+(1.96*SE_yhatrem)
gen remlow=yhatrem2-(1.96*SE_yhatrem)

glm chol stub_eGFR_7_new* age i.sex
predict yhatchol2
predict SE_yhatchol, stdp
gen cholup=yhatchol2+(1.96*SE_yhatchol)
gen chollow=yhatchol2-(1.96*SE_yhatchol)

glm hdl stub_eGFR_7_new* age i.sex
predict yhathdl2
predict SE_yhathdl, stdp
gen hdlup=yhathdl2+(1.96*SE_yhathdl)
gen hdllow=yhathdl2-(1.96*SE_yhathdl)

twoway (fpfit yhat2 rev_eGFR09 if eGFR2009<140, lcol(red) acol(gs10)) || (fpfit ldlup rev_eGFR09 if eGFR2009<140, lcol(red) lp(shortdash)) ||  (fpfit ldllow rev_eGFR09 if eGFR2009<140, lcol(red) lp(shortdash)) ///
(fpfit yhatrem2 rev_eGFR09 if eGFR2009<140, lcol(orange) )  || (fpfit remup rev_eGFR09 if eGFR2009<140, lcol(orange) lp(shortdash))   || (fpfit remlow rev_eGFR09 if eGFR2009<140, lcol(orange) lp(shortdash))  ///
(fpfit yhatchol2 rev_eGFR09 if eGFR2009<140, lcol(black) )  ||(fpfit cholup rev_eGFR09 if eGFR2009<140, lcol(black) lp(shortdash))  || (fpfit chollow rev_eGFR09 if eGFR2009<140, lcol(black) lp(shortdash)) ///
(fpfit yhathdl2 rev_eGFR09 if eGFR2009<140, lcol(black) xline(-15 -30 -45 -60 -90 -140) acol(gs10))  || (fpfit hdlup rev_eGFR09 if eGFR2009<140, lcol(black) lp(shortdash))  ///
(fpfit hdllow rev_eGFR09 if eGFR2009<140, lcol(black)  xline(-15 -30 -45 -60 -90 -140) acol(gs10) lp(shortdash)  /// 
 xlabel(-140(10) -10) xtit(eGFR2009) ylab(0 (1) 10 ,angle(0) nogrid) ytit(mmol/L) title("Lipids in individuals with renal impairment") ///
legend(order(2 "LDL-C" 4 "Remnants-C" 6 "Total cholesterol" 8 "HDL-C" )) name(one60_2009_Nostat, replace))

* Lower panel 
glm nonhdl stub_eGFR_7_new* age i.sex
predict yhatnonhdl2
predict SE_yhatnonhdl, stdp
gen nonhdlup=yhatnonhdl2+(1.96*SE_yhatnonhdl)
gen nonhdllow=yhatnonhdl2-(1.96*SE_yhatnonhdl)


glm trig stub_eGFR_7_new* age i.sex
predict yhattg2
predict SE_yhattg, stdp
gen tgup=yhattg2+(1.96*SE_yhattg)
gen tglow=yhattg2-(1.96*SE_yhattg)

glm apobmg stub_eGFR_7_new* age i.sex
predict yhatApoB2
predict SE_yhatApoB, stdp
gen ApoBup=yhatApoB2+(1.96*SE_yhatApoB)
gen ApoBlow=yhatApoB2-(1.96*SE_yhatApoB)

**One axis**

twoway (fpfit yhatApoB2 rev_eGFR09 if rev_eGFR09>-140, lcol(orange) acol(gs10)) || (fpfit ApoBup rev_eGFR09 if rev_eGFR09>-140, lcol(orange) lp(shortdash)) || || (fpfit ApoBlow rev_eGFR09 if rev_eGFR09>-140, lcol(orange) lp(shortdash)) ///
(fpfit yhattg2 rev_eGFR09 if rev_eGFR09>-140, lcol(blue) acol(gs10)) ||(fpfit tgup rev_eGFR09 if rev_eGFR09>-140, lcol(blue) lp(shortdash)) ||(fpfit tglow rev_eGFR09 if rev_eGFR09>-140, lcol(blue) lp(shortdash)) ///
(fpfit yhatnonhdl2 rev_eGFR09 if rev_eGFR09>-140, lcol(orange ) acol(gs10)) ||(fpfit nonhdlup rev_eGFR09 if rev_eGFR09>-140, lcol(orange ) lp(shortdash)) || (fpfit nonhdllow rev_eGFR09 if rev_eGFR09>-140, lcol(orange ) lp(shortdash)  ///
xline(-140 -90 -60 -45 -30 -15, lcol(gs10) lp(shortdash)) xlabel(-140(40)-10) xtit(eGFR2009) ///
ylab(0 (1) 10 ,angle(0) nogrid) ytit(mmol/L) title("Lipids in individuals with all categories eGFR") legend(order(2 "ApoB" 4 "Triglycerides" 6 "Non-HDL-C" )) name(one90_2009, replace))

**Two axis (last version 04-dic-2023)**
twoway (fpfit yhattg2 rev_eGFR09 if rev_eGFR09>-140, lcol(blue) acol(gs10)) || (fpfit tgup rev_eGFR09 if rev_eGFR09>-140, lcol(blue) lp(shortdash)) ||(fpfit tglow rev_eGFR09 if rev_eGFR09>-140, lcol(blue) lp(shortdash)) ///
(fpfit yhatnonhdl2 rev_eGFR09 if rev_eGFR09>-140, lcol(orange ) acol(gs10)) || (fpfit nonhdlup rev_eGFR09 if rev_eGFR09>-140, lcol(orange ) lp(shortdash)) || (fpfit nonhdllow rev_eGFR09 if rev_eGFR09>-140, lcol(orange ) lp(shortdash)) || ///
(fpfit yhatApoB2 rev_eGFR09 if rev_eGFR09>-140, lcol(orange) acol(gs10) yaxis(2) ytitle("mg/dL", axis(2)) ylabel(0(20)160, angle(0) axis(2) tlw(medium)))  || (fpfit ApoBup rev_eGFR09 if rev_eGFR09>-140, lcol(orange) lp(shortdash) yaxis(2)) ///
|| (fpfit ApoBlow rev_eGFR09 if rev_eGFR09>-140, lcol(orange) lp(shortdash) yaxis(2) xline(-140 -90 -60 -45 -30 -15, lcol(gs10) lp(shortdash)) xlabel(-140(40)-10) xtit(eGFR2009) ///
ylab(0 (1) 10 ,angle(0) nogrid) ytit(mmol/L) title("Lipids in individuals with all categories eGFR") legend(order(2 "Triglycerides" 4 "Non-HDL-C" 6 "ApoB" )) name(one90_2009, replace))


**Fig 2**

*drop if  Hyperchol_medication==0
*drop if  Hyperchol_medication==1
gen eGFR_4cat09=.
replace eGFR_4cat09=0 if eGFR2009>=90 & eGFR2009!=. 
replace eGFR_4cat09=1 if eGFR2009>=60 & eGFR2009<90
replace eGFR_4cat09=2 if eGFR2009>=45 & eGFR2009<60
replace eGFR_4cat09=3 if eGFR2009>=30 & eGFR2009<45
replace eGFR_4cat09=4 if eGFR2009<30

label values eGFR_4cat09 eGFR_4cat09_label
tab eGFR_4cat09, m

**All individuals***
preserve

regress ldl age i.sex 
predict r, residuals
hist r
sum(ldl)
local mean = r(mean)
gen ldladj = `mean' + r

drop r

regress remnants age i.sex 
predict r, residuals
hist r
sum(remnants)
local mean = r(mean)
gen remnantadj = `mean' + r

drop r

tabstat eGFR2009, stat(min max) by(eGFR_4cat09)

collapse (mean) ldladj remnantadj, by(eGFR_4cat09)
graph bar remnantadj ldladj, over(eGFR_4cat09) stack blabel(bar, color(white) position(inside) format(%4.2f))
graph bar remnantadj ldladj, over(eGFR_4cat09) stack bar(1, color(orange)) bar(2, color(red)) blabel(bar, color(white) position(inside) format(%4.2f)) ylabel(,angle(0)) ytitle("Non-HDL-C (mmol/L)") legend(order(1 "Remnants-C" 2 "LDL-C")) 

graph bar remnantadj ldladj, over(eGFR_4cat09) stack bar(1, color(orange)) bar(2, color(red)) ylabel(,angle(0) nogrid) ytitle("Non-HDL-C (mmol/L)") legend(order(1 "Remnants-C" 2 "LDL-C")) title("Sex and age adjusted means, All individuals")

restore


***Fig 3***

gen birthyear=year(birthday)
*drop if  Hyperchol_medication==0 
*drop if  Hyperchol_medication==1 

**AMI**
preserve
keep if CKD09_cat==2
stset amidato, fail(ami==1) origin(birthday) entry(usdate) scale(365.25) 
stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear i.eGFR_cat09
**prev_CKD bmi5 dm_prev lpa_corr apob 
stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear i.eGFR_cat09
**prev_CKD bmi5 dm_prev lpa_corr apob 
restore

**Ischemic stroke**
preserve
keep if CKD09_cat==2
stset isdato_uval, fail(is_uval==1) origin(birthday) entry(usdate) scale(365.25) 
stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear i.eGFR_cat09
**prev_CKD bmi5 dm_prev lpa_corr apob 
stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear i.eGFR_cat09
**prev_CKD bmi5 dm_prev lpa_corr apob 
restore

**ASCVD**
preserve
keep if CKD09_cat==2
stset ASCVD_date, fail(ASCVD==1) origin(birthday) entry(usdate) scale(365.25) 
stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear i.eGFR_cat09
**prev_CKD bmi5 dm_prev lpa_corr apob 
stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear i.eGFR_cat09
**prev_CKD bmi5 dm_prev lpa_corr apob 
restore

**Fig 4**

*drop if  Hyperchol_medication==0 
*drop if  Hyperchol_medication==1

***==== In all individuals====****
*MI*
preserve
stset amidato, fail(ami==1) origin(birthday) entry(usdate) scale(365.25) 
stcox b1.CKD09_cat sex i.status_smoking icum_smoking birthyear c.isystolic c.idiastolic 
restore
*Stroke*
preserve
stset isdato_uval, fail(is_uval==1) origin(birthday) entry(usdate) scale(365.25) 
stcox b1.CKD09_cat sex i.status_smoking icum_smoking birthyear c.isystolic c.idiastolic 
restore
*ASCVD*
preserve
stset ASCVD_date, fail(ASCVD==1) origin(birthday) entry(usdate) scale(365.25) 
stcox b1.CKD09_cat sex i.status_smoking icum_smoking birthyear c.isystolic c.idiastolic 
restore


**Fig 5**
 
//Insert the entire line in commando line in STATA from net to replace
net install med4way, from(https://raw.githubusercontent.com/anddis/med4way/master/) replace

 ** Exposure: eGFR<60 versus eGFR>60
 gen eGFR60=.
 replace eGFR60=0 if eGFR2009>=60 & eGFR2009!=.
 replace eGFR60=1 if eGFR2009<60 
 bysort eGFR60: tabstat eGFR2009, stat(min max)

 *gen birthyear=year(birthday)
 replace remnants=0.0001 if remnants==0  
 gen logrem=ln(remnants) 
 
 tabstat logrem, stat(p50)  
 
stset ASCVD_date, fail(ASCVD) ent(usdate) ori(birthday) id(obushnr)
med4way eGFR60 logrem age sex icum_smoking status_smoking birthyear, a0(0) a1(1) m(-.4780356) yreg(cox) mreg(linear") full

nlcom (_b[ereri_pie]+_b[ereri_intmed])/(_b[ereri_cde]+_b[ereri_intref]+_b[ereri_intmed]+_b[ereri_pie]), noheader


*************************************************************************
**		All individuals + remnant cholesterol as mediator ASCVD
*************************************************************************
preserve
*keep if Hyperchol_medication==1 
*keep if Hyperchol_medication==1 

stset ASCVD_date, fail(ASCVD) ent(usdate) ori(birthday) id(obushnr) 
stcox eGFR60 logrem sex icum_smoking status_smoking birthyear
estat phtest, detail

tabstat logrem, stat(p50)  //-.3566749
stset ASCVD_date, fail(ASCVD) ent(usdate) ori(birthday) id(obushnr)
med4way eGFR60 logrem age sex icum_smoking status_smoking birthyear, a0(0) a1(1) m(-.4780356) yreg(cox) mreg(linear) full

nlcom (_b[ereri_pie]+_b[ereri_intmed])/(_b[ereri_cde]+_b[ereri_intref]+_b[ereri_intmed]+_b[ereri_pie]), noheader  //
restore



**Convert mmol/mcmol/ to mg/dL 
gen cholmg=chol*38.67
gen ldlmg=ldl*38.67
gen hdlmg=hdl*38.67
gen remnantsmg=remnants*38.67
gen nonhdlmg=nonhdl*38.67
gen apobmg=apob*100
gen trigmg=trig*88.57
gen glucosemg=glucose*18
gen creatinimg=creatini*0.01131



**Test for interaction**
***Myocardial Infarction***
keep if CKD09_cat==2
*drop if  Hyperchol_medication==0 
*drop if  Hyperchol_medication==1


**Remnants-c interaction with sex

stset amidato, fail(ami==1) origin(birthday) entry(usdate) scale(365.25)
stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear
estimate store A
gen interact=sex*remnants
stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear c.interact
estimate store B
lrtest A B
capture drop interact

**** LDL-C interaction sex
stset amidato, fail(ami==1) origin(birthday) entry(usdate) scale(365.25)
stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear
estimate store C
gen interact=sex*ldl
stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear c.interact
estimate store D
lrtest C D
capture drop interact

**Test for interaction**
***Ischemic Stroke***

*drop if  Hyperchol_medication==0 
*drop if  Hyperchol_medication==1

**Remnants-c interaction with sex
stset isdato_uval, fail(is_uval==1) origin(birthday) entry(usdate) scale(365.25)

stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear
estimate store A

gen interact=sex*remnants
stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear c.interact
estimate store B

lrtest A B
capture drop interact

**** LDL-C interaction sex
stset isdato_uval, fail(is_uval==1) origin(birthday) entry(usdate) scale(365.25)

stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear
estimate store C

gen interact=sex*ldl
stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear c.interact
estimate store D

lrtest C D

capture drop interact

stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear

stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear

**Test for interaction**

***ASCVD***

*drop if  Hyperchol_medication==0 
*drop if  Hyperchol_medication==1

**Remnants-c interaction with sex
stset ASCVD_date, fail(ASCVD==1) origin(birthday) entry(usdate) scale(365.25)

stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear
estimate store A

gen interact=sex*remnants
stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear c.interact
estimate store B

lrtest A B
capture drop interact

**** LDL-C interaction sex
stset ASCVD_date, fail(ASCVD==1) origin(birthday) entry(usdate) scale(365.25)

stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear
estimate store C

gen interact=sex*ldl
stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear c.interact
estimate store D

lrtest C D

capture drop interact




**Whole population: 

**ASCVD
preserve
stset ASCVD_date, fail(ASCVD==1) origin(birthday) entry(usdate) scale(365.25) id(obushnr)
stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear c.eGFR2009
estimate store ModelA
stcox sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear c.eGFR2009##c.remnants
estimate store ModelB
lrtest ModelA ModelB
restore

**Interaction remnant cholesterol and statins**

**AMI
preserve
keep if eGFR2009<60
stset amidato, fail(ami==1) origin(birthday) entry(usdate) scale(365.25) id(obushnr)
stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear Hyperchol_medication
estimate store ModelA
stcox sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear i.Hyperchol_medication##c.remnants
estimate store ModelB
lrtest ModelA ModelB
restore

**Ischemic stroke
preserve
keep if eGFR2009<60
stset icvddato_uval, fail(icvd_uval==1) origin(birthday) entry(usdate) scale(365.25) id(obushnr)
stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear Hyperchol_medication
estimate store ModelA
stcox sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear i.Hyperchol_medication##c.remnants
estimate store ModelB
lrtest ModelA ModelB
restore


**ASCVD
preserve
keep if eGFR2009<60
stset ASCVD_date, fail(ASCVD==1) origin(birthday) entry(usdate) scale(365.25) id(obushnr)
stcox remnants sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear Hyperchol_medication
estimate store ModelA
stcox sex ldl isystolic idiastolic i.status_smoking icum_smoking birthyear i.Hyperchol_medication##c.remnants
estimate store ModelB
lrtest ModelA ModelB
restore


**Interaction LDL cholesterol and statins**

**AMI
preserve
keep if eGFR2009<60
stset amidato, fail(ami==1) origin(birthday) entry(usdate) scale(365.25) id(obushnr)
stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear Hyperchol_medication
estimate store ModelA
stcox sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear i.Hyperchol_medication##c.ldl
estimate store ModelB
lrtest ModelA ModelB
restore

**Ischemic stroke
preserve
keep if eGFR2009<60
stset icvddato_uval, fail(icvd_uval==1) origin(birthday) entry(usdate) scale(365.25) id(obushnr)
stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear Hyperchol_medication
estimate store ModelA
stcox sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear i.Hyperchol_medication##c.ldl
estimate store ModelB
lrtest ModelA ModelB
restore


**ASCVD
preserve
keep if eGFR2009<60
stset ASCVD_date, fail(ASCVD==1) origin(birthday) entry(usdate) scale(365.25) id(obushnr)
stcox ldl sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear Hyperchol_medication
estimate store ModelA
stcox sex remnants isystolic idiastolic i.status_smoking icum_smoking birthyear i.Hyperchol_medication##c.ldl
estimate store ModelB
lrtest ModelA ModelB
restore
