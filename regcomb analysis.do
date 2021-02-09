cd "C:\Users\Marriott Nliwasa\Documents\My R projects"

insheet using hiv_regcomb.csv


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

gen populationnew=(population)*hiv_prev if hiv2==1
replace populationnew=(population)*(1-hiv_prev) if hiv2==2


*overall cnr
drop if hiv2==.
collapse (sum) smrxpert (sum) populationnew
list				 
gen cnr= smrxpert/(round(populationnew)/4)*100000
gen popii=(round(populationnew)/4)

statsby mean=r(mean) lower=r(lb) upper=r(ub), clear by (cnr): ci means smrxpert, poisson exposure(popii)
gen upper2=upper*100000
gen lower2=lower*100000
drop mean upper lower 
order cnr lower2 upper2					 
list

*sex
drop if hiv2==.
collapse (sum) smrxpert (sum) populationnew, by(sex2)	
list				 
gen cnr= smrxpert/(round(populationnew)/4)*100000
gen popii=(round(populationnew)/4)

statsby mean=r(mean) lower=r(lb) upper=r(ub), clear by (cnr sex2): ci means smrxpert, poisson exposure(popii)
gen upper2=upper*100000
gen lower2=lower*100000
drop mean upper lower 
order cnr lower2 upper2					 
list


*age
drop if hiv2==.
collapse (sum) smrxpert (sum) populationnew, by(age2)	
list				 
gen cnr= smrxpert/(round(populationnew)/4)*100000
gen popii=(round(populationnew)/4)

statsby mean=r(mean) lower=r(lb) upper=r(ub), clear by (cnr age2): ci means smrxpert, poisson exposure(popii)
gen upper2=upper*100000
gen lower2=lower*100000
drop mean upper lower 
order cnr lower2 upper2					 
list


*hiv2
drop if hiv2==.
collapse (sum) smrxpert (sum) populationnew, by(hiv2)	
list				 
gen cnr= smrxpert/(round(populationnew)/4)*100000
gen popii=(round(populationnew)/4)

statsby mean=r(mean) lower=r(lb) upper=r(ub), clear by (cnr hiv2): ci means smrxpert, poisson exposure(popii)
gen upper2=upper*100000
gen lower2=lower*100000
drop mean upper lower 
order cnr lower2 upper2					 
list


*area
drop if hiv2==.
collapse (sum) smrxpert (sum) populationnew, by(acf2)	
list				 
gen cnr= smrxpert/(round(populationnew)/4)*100000
gen popii=(round(populationnew)/4)

statsby mean=r(mean) lower=r(lb) upper=r(ub), clear by (cnr acf2): ci means smrxpert, poisson exposure(popii)
gen upper2=upper*100000
gen lower2=lower*100000
drop mean upper lower 
order cnr lower2 upper2					 
list


*regression analyses
drop if hiv2==.
poisson smrxpert ib2.sex2, exposure(populationnew) irr
poisson smrxpert i.age2, exposure(populationnew) irr
poisson smrxpert ib2.hiv2, exposure(populationnew) irr

poisson smrxpert ib2.hiv2 i.acf2 ib2.sex2 i.age2 i.year, exposure(populationnew) irr



*interaction
poisson smrxpert ib2.hiv2##ib2.sex2 i.acf2  i.age2 i.year, exposure(populationnew) irr




