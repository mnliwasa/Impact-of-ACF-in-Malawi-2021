cd "C:\Users\Marriott Nliwasa\Documents\My R projects\TB-Trends"
insheet using hivdata2.csv

**Sex
gen sex2=.
replace sex2=2 if sex == "females"
replace sex2=1 if sex== "males"
tab sex2

lab val sex2 sex2
lab def sex2 1 "male" 2 "female"
tab sex2


**age
gen age2=.
replace age2 = 1 if age =="16-19"
replace age2 = 2 if age =="20-29"
replace age2 = 3 if age =="30-39"
replace age2 = 4 if age =="40-49"
replace age2 = 5 if age =="50+"
tab age2

lab val age2 age2
lab def age2 1 "16-19" 2 "20-29" 3 "30-39" 4 "40-49" 5 "50+"
tab age2

**acf
gen acf2=.
replace acf2 = 2 if acf=="Non-ACF"
replace acf2=1 if acf=="ACF"
tab acf2

lab val acf2 acf2
lab def acf2 1 "ACF" 2 "Non-ACF"
tab acf2

**hiv

gen hiv2=.
replace hiv2 = 2 if hiv=="a) HIV-negative"
replace hiv2=1 if hiv=="b) HIV-positive"
tab hiv2

lab val hiv2 hiv2
lab def hiv2 1 "b) HIV-positive" 2 "a) HIV-negative"
tab hiv2, m

**ACFperiod
gen acfperiod2=.
replace acfperiod2 = 1 if acfperiod =="Non-ACF.apre-ACF"
replace acfperiod2 = 2 if acfperiod =="Non-ACF.bACF"
replace acfperiod2 = 3 if acfperiod =="Non-ACF.cpost-ACF"
replace acfperiod2 = 4 if acfperiod =="ACF.apre-ACF"
replace acfperiod2 = 5 if acfperiod =="ACF.bACF"
replace acfperiod2 = 6 if acfperiod =="ACF.cpost-ACF"
tab acfperiod2

lab val acfperiod2 acfperiod2
lab def acfperiod2 1 "Non-ACFxapre-ACF" 2 "Non-ACFxbACF" 3 "Non-ACFxcpost-ACF" 4 "ACFxapre-ACF" 5 "ACFxbACF" 6 "ACF.cpost-ACF"
tab acfperiod2

keep if n !=.

drop sex age hiv acf acfperiod

gen hm=.
replace hm=1 if hiv2!=.
replace hm=0 if hiv2==.
tab hm 

tab sex2 hm, row chi2
tab hm age2, col chi2
tab hm acfperiod2, col chi2

recode hiv2 2=0

mi set mlong
mi register imputed hiv2
mi misstable summ, all
set seed 29390

mi impute logit hiv2 = acfperiod2 age2 sex2, rseed(29390) add(10)


gen populationnew=(population)*hiv_prev if hiv2==1
replace populationnew=(population)*(1-hiv_prev) if hiv2==2

*univariate HIV
poisson smrxpert ib2.hiv2 , exposure(populationnew) irr



*univariate HIV
mi estimate : poisson n i.hiv2 , exposure(populationnew) irr
mi estimate, irr
est store A

*multivariate hiv
mi estimate : poisson n i.hiv2 i.acfperiod2 ib2.sex2 i.age2, exposure(populationnew) irr
mi estimate, irr
est store A

mi estimate : poisson n i.hiv2##ib2.sex2 i.acfperiod2  i.age2, exposure(populationnew) irr
mi estimate, irr
est store B

lrtest A B, force


*complete case comparison
mi estimate : poisson n i.hiv2 i.acf2 ib2.sex2 i.age2, exposure(populationnew) irr
mi estimate, irr
est store A


*predicted**overall

global ylist n
global xlist i.age2 i.sex2
poisson $ylist $xlist, exposure(populationnew)irr
margins, dydx (*) atmeans
predict yhat, n
summarize yhat
summarize $ylist

collapse (sum) n (sum) populationnew (sum) yhat , by (sex2 age2 nperiod acf2)
gen obs_cnr=n/populationnew*100000
statsby mean=r(mean) lower=r(lb) upper=r(ub), clear by (sex2 age2 n obs_cnr yhat acf2 period populationnew): ci means n, poisson exposure(populationnew)
gen upper2=upper*100000
gen lower2=lower*100000
drop mean upper lower 

gen pred_cnr=yhat/populationnew*100000
statsby mean=r(mean) lower=r(lb) upper=r(ub), clear by (sex2 age2 n obs_cnr pred_cnr yhat acf2 period upper2 lower2): ci means yhat, poisson exposure(populationnew)
gen upper3=upper*100000
gen lower3=lower*100000
drop mean upper lower 	



*predicted**age and sex

collapse (sum) n (sum) populationnew (sum) yhat , by (sex2 age2 nperiod acf2)
gen obs_cnr=n/populationnew*100000
statsby mean=r(mean) lower=r(lb) upper=r(ub), clear by (sex2 age2 n obs_cnr yhat acf2 period populationnew): ci means n, poisson exposure(populationnew)
gen upper2=upper*100000
gen lower2=lower*100000
drop mean upper lower 

gen pred_cnr=yhat/populationnew*100000
statsby mean=r(mean) lower=r(lb) upper=r(ub), clear by (sex2 age2 n obs_cnr pred_cnr yhat acf2 period upper2 lower2): ci means yhat, poisson exposure(populationnew)
gen upper3=upper*100000
gen lower3=lower*100000
drop mean upper lower 	

*overall
collapse (sum) smr_clinic (sum) quarterpop (sum) yhat , by (period area)
gen obs_cnr=smr_clinic/quarterpop*100000
statsby mean=r(mean) lower=r(lb) upper=r(ub), clear by (smr_clinic obs_cnr yhat area period quarterpop): ci means smr_clinic, poisson exposure(quarterpop)
gen upper2=upper*100000
gen lower2=lower*100000
drop mean upper lower 

gen pred_cnr=yhat/quarterpop*100000
statsby mean=r(mean) lower=r(lb) upper=r(ub), clear by (smr_clinic obs_cnr pred_cnr yhat area period upper2 lower2): ci means yhat, poisson exposure(quarterpop)
gen upper3=upper*100000
gen lower3=lower*100000
drop mean upper lower 




