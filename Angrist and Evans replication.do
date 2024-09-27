**************The effect of a third child on parents' labour supply in the U.S - replication of Table 2 in Angrist and Evans (1998) but uses 2021 1% American Community Sample (ACS) IPUMS data instead of data from 1980 and 1990. 
set more off
clear all 
capture log close

cd "OneDrive - University of Bristol/Dissertation/Dissertation do-file"

use "C:\Users\yk21598\OneDrive - University of Bristol\usa_00006.dta", clear


***MOTHERS DATA SET UP

*Generate dummy for mothers 
gen mother = 0
forvalues i = 1/20 {
gen dum = (momloc==`i') 
egen max_dum = max(dum), by(serial) 
replace mother = 1 if pernum==`i' & max_dum==1
drop dum max_dum
}

save "C:\Users\yk21598\OneDrive - University of Bristol\dmother_created.dta", replace

use "C:\Users\yk21598\OneDrive - University of Bristol\dmother_created.dta", clear

*Recasting all relevant variables to long
recast long mother serial pernum momloc momrule sploc

*Number of mothers per family
sort  serial
by  serial: egen mom_count=sum(mother)
sum mom_count
edit  serial mom_count mother momloc momloc pernum momrule if mom_count>1

*Making a unique person identifier
egen personid = concat (serial pernum)
destring personid, replace 
recast long personid 

*Children
gen child=0
replace child=1 if mother==0 & sploc==0 & momloc !=0
recast long child

*Fathers
gen father=0
replace father=1 if mother==0 & sploc!=0 & nchild>=1
recast long father

*Mothers fam ID 
sort serial pernum 
gen famid=0 
recast long famid
replace famid=personid if mother==1 

*Childrens fam ID 
egen cfam_id= concat (serial momloc) if child ==1 
destring cfam_id, replace
recast long cfam_id
replace famid = cfam_id if child==1 & sploc==0 & mother==0

*Fathers famID
egen ffam_id= concat (serial sploc) if father ==1
destring ffam_id, replace
recast long ffam_id 
replace famid = ffam_id if sploc>0 & father==1

*Check results for families with more than 1 mother
edit serial mother father child momloc sploc pernum personid famid if mom_count>1

*Drop famID for families without any mothers and/or children
drop if famid==0

save "C:\Users\yk21598\OneDrive - University of Bristol\famid.dta", replace

use "C:\Users\yk21598\OneDrive - University of Bristol\famid.dta", clear

*************************************************************************************
*Create dataset for children

keep if child==1

save "C:\Users\yk21598\OneDrive - University of Bristol\children.dta", replace

*************************************************************************************
*Create dataset for fathers

use "C:\Users\yk21598\OneDrive - University of Bristol\famid.dta", clear
keep if father==1

save "C:\Users\yk21598\OneDrive - University of Bristol\father.dta", replace

*************************************************************************************
*Mothers

use "C:\Users\yk21598\OneDrive - University of Bristol\famid.dta", clear

*drop husbands
keep if mother == 1 | child == 1

*Generating number of children per subfamily 
bys famid: egen nchildren=sum(child)

*Identifying subfamilies where oldest child is less than 18 years old
gsort famid -mother -age
bys famid: gen ene_ch2 = _n - 1 if mother == 0
bys famid: egen child_maxage=max(age) if child == 1
gen oldest=1 if age==child_maxage
gen oldest_less_18=1 if oldest==1 & age<18 & age!=.
bys famid: egen fam_oldest_less_18=max(oldest_less_18)

*Identifying subfamilies where 2nd child is less than 1 year old
gen d_under1=1 if age==0
bys famid: egen n_under1=sum(d_under1)

sort serial famid age
gen second_less_1=1 if ene_ch==2 & age==0
bys famid: egen fam_second_less_1=max(second_less_1)

*Idenfifying 2nd oldest child in subfamily
gen child_second_maxage=child_maxage-1 if child_maxage!=.
gen second_oldest=1 if ene_ch2==child_second_maxage

*Keep eligible mothers
keep if mother==1 & age>=21 & age<=35 & nchildren>=2  & fam_second_less_1!=1 &  fam_oldest_less_18==1 
sort famid

*Identifying subfamilies in which oldest or 2nd oldest child have birth quarter "not allocated"
gen oldest_birthqtr_allocated=1 if oldest==1 & birthqtr==1
bys famid: egen fam_oldest_birthqtr_allocated=max(oldest_birthqtr_allocated)
gen second_oldest_birthqtr_allocated=1 if second_oldest==1 & birthqtr==1
bys famid: egen fam_secondo_birthqtr_allocated=max(second_oldest_birthqtr_allocated)

*Keep eligible mothers
keep if fam_oldest_birthqtr_allocated!=1 & fam_secondo_birthqtr_allocated!=1

save "C:\Users\yk21598\OneDrive - University of Bristol\eligible_mothers.dta", replace

*************************************************************************************
*Reshape children
use "C:\Users\yk21598\OneDrive - University of Bristol\children.dta", clear

*Generating a number per children in each subfamily, ordered from oldest to youngest
gsort famid -age
bys famid: gen number=_n

*Reshape
keep serial famid momloc number age birthqtr sex qage qsex
reshape wide serial momloc age birthqtr sex qage qsex, i(famid) j(number)
sort famid

save "C:\Users\yk21598\OneDrive - University of Bristol\children_reshaped.dta", replace

*************************************************************************************
*Merge children and mothers
use "C:\Users\yk21598\OneDrive - University of Bristol\eligible_mothers.dta", clear

merge 1:1 famid using  "C:\Users\yk21598\OneDrive - University of Bristol\children_reshaped.dta"
keep if _merge==3

gen dummy_q = 1 if (qage1 == 1 | qage2 == 1 | qsex1 == 2 | qsex2 == 2)

drop if dummy_q == 1

save "C:\Users\yk21598\OneDrive - University of Bristol\eligible_motherschildren.dta", replace


**************TABLE 4: DESCRIPTIVE STATISTICS FORMATION**************

use "C:\Users\yk21598\OneDrive - University of Bristol\eligible_motherschildren.dta", clear

*****************************ALL WOMEN***************************

*More than two children
gen more_than_2 = 1 if nchild > 2
replace more_than_2=0 if nchild<=2

*First born is a boy
gen first_boy=0
replace first_boy=1 if sex1 == 1

*Second born is a boy
gen second_boy=0
replace second_boy=1 if sex2 == 1 

*Two boys
gen two_boys=0
replace two_boys=1 if sex1 == 1 & sex2 == 1

*Two girls
gen two_girls=0
replace two_girls=1 if sex1 == 2 & sex2 == 2

*Same sex
gen same_sex=0
replace same_sex=1 if sex1 == sex2

*Generating quarter of birth for child2
gen ageqtr2=0
replace ageqtr2 = (age2*4) + 1 if birthqtr2 == 1
replace ageqtr2 = (age2*4) + 2 if birthqtr2 == 2
replace ageqtr2 = (age2*4) + 3 if birthqtr2 == 3
replace ageqtr2 = (age2*4) + 4 if birthqtr2 == 4

*Age at first birth
gen age_first_birth = age - age1
sum age_first_birth
drop if age_first_birth<15

*Worked for pay
gen worked_for_pay = 0
replace worked_for_pay = 1 if wkswork1 > 0 & incwage > 0

dtable nchildren more_than_2 first_boy second_boy two_boys two_girls same_sex age age_first_birth worked_for_pay wkswork1 uhrswork inctot ftotinc, title("Table 4: Descriptive Statistics, Women Aged 21-35 With 2 or More Children, 2021 IPUMS") export(table3.docx, replace)

save "C:\Users\yk21598\OneDrive - University of Bristol\eligible_motherschildren_allwomen.dta", replace


*************************MARRIED WOMEN***************************

use "C:\Users\yk21598\OneDrive - University of Bristol\eligible_motherschildren_allwomen.dta", clear

*Generate age in quarters
gen ageqtrmother=0
replace ageqtrmother = (age*4) + 1 if birthqtr == 1
replace ageqtrmother = (age*4) + 2 if birthqtr == 2
replace ageqtrmother = (age*4) + 3 if birthqtr == 3
replace ageqtrmother = (age*4) + 4 if birthqtr == 4

*Married
gen married_mother = 0
replace married_mother = 1 if (marst == 1 | marst == 2) 

*Age at first birth
sum age_first_birth if married_mother == 1
drop if age<15

*Non-wife income
gen non_wife_inc=ftotinc - incwage

save "C:\Users\yk21598\OneDrive - University of Bristol\eligible_mothers&children_withsum.dta", replace

use "C:\Users\yk21598\OneDrive - University of Bristol\eligible_mothers&children_withsum.dta", clear

dtable married_mother nchildren more_than_2 first_boy second_boy two_boys two_girls same_sex age age_first_birth worked_for_pay wkswork1 uhrswork inctot ftotinc non_wife_inc if married_mother==1, export(table4.docx, replace)

*******************HUSBANDS OF MARRIED WOMEN***************************
*Reshape fathers
use "C:\Users\yk21598\OneDrive - University of Bristol\father.dta", clear

*Rename variables 
rename age husband_age
rename birthqtr husband_birthqtr
rename classwkr husband_classwkr
rename wkswork husband_wkswork
rename uhrswork husband_uhrswork
rename incwage husband_incwage
rename ftotinc husband_ftotinc
rename marst husband_marst

*Removing any famid with 2 fathers
gsort famid -husband_age
bys famid: gen number=_n
drop if number == 2

*Identify fathers
gen dad = 0
replace dad = 1 if father == 1

*Reshape
keep serial famid father sploc husband_marst husband_age husband_birthqtr husband_classwkr husband_wkswork husband_uhrswork husband_incwage husband_ftotinc dad
reshape wide serial sploc husband_marst husband_age husband_birthqtr husband_classwkr husband_wkswork husband_uhrswork husband_incwage husband_ftotinc dad, i(famid) j(father)
sort famid

save "C:\Users\yk21598\OneDrive - University of Bristol\fathers_reshaped.dta", replace

*Merge Fathers
use "C:\Users\yk21598\OneDrive - University of Bristol\eligible_mothers&children_withsum.dta", clear

drop _merge
merge 1:1 famid using "C:\Users\yk21598\OneDrive - University of Bristol\fathers_reshaped.dta"
drop if _merge==2

save "C:\Users\yk21598\OneDrive - University of Bristol\families.dta", replace

use "C:\Users\yk21598\OneDrive - University of Bristol\families.dta", clear

***Father summary statistics 

*Age at first birth
gen husband_age_first_birth = husband_age1 - age1
sum husband_age_first_birth
drop if husband_age_first_birth<15

*Worked for pay
gen husband_worked_for_pay = 0
replace husband_worked_for_pay = 1 if husband_wkswork1 > 0 & husband_incwage1 > 0

dtable husband_age1 husband_age_first_birth husband_worked_for_pay husband_wkswork1 husband_uhrswork husband_incwage1 husband_ftotinc if married_mother == 1, export(table5.docx, replace) note("Note: Standard errors in parentheses. Data taken from the 2021 IPUMS. The married women sample contains women who were married at the time of the census.")

graph combine table3.docx, table4.docx and table5.docx, col(3) name(descriptive_table4, replace)

*************************************************************************
*Create race dummies
gen black = 0
replace black = 1 if race==3

gen other_race = 0
replace other_race = 1 if race==4 | race==5 | race==6 | race==7 | race==8 | race==9 | race==10 | race==11 | race== 12 | race==13

gen hispanic = 0
replace hispanic = 1 if race == 2

gen white = 0
replace white = 1 if race == 1

save "C:\Users\yk21598\OneDrive - University of Bristol\families2.dta", replace

***********TABLE 5: REGRESSION FORMATION**************

use "C:\Users\yk21598\OneDrive - University of Bristol\families2.dta", clear
ssc install reghdfe
ssc install ftools
ssc install ivreg2
ssc install ranktest
ssc install estout

*******************************ALL WOMEN*************************************

***OLS
reg worked_for_pay more_than_2 age age_first_birth first_boy second_boy black hispanic other_race
estimates store C1R1

reg wkswork1 more_than_2 age age_first_birth first_boy second_boy black hispanic other_race
estimates store C1R2

reg uhrswork more_than_2 age age_first_birth first_boy second_boy black hispanic other_race
estimates store C1R3

reg incwage more_than_2 age age_first_birth first_boy second_boy black hispanic other_race
estimates store C1R4

gen lnftotinc = ln(ftotinc)
reg lnftotinc more_than_2 age age_first_birth first_boy second_boy black hispanic other_race
estimates store C1R5

save "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults1.rtf", replace

esttab C1R1 C1R2 C1R3 C1R4 C1R5, title("Table 5: OLS and 2SLS Estimates of Labour Supply Models, Women Aged 21-35 With 2 or More Children") subtitle("Panel (a): All Women") mtitle("Worked for Pay" "Weeks Worked" "Hours/Week Worked" "Income Wage" "ln(Family Income)") drop(age age_first_birth first_boy second_boy black hispanic other_race), using "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults1.rtf", rtf replace se(%9.3f) b(%9.3f) star(* .10 ** .05 *** .01)

***2SLS (SAME SEX)
ivreg2 worked_for_pay (more_than_2 = same_sex) age age_first_birth first_boy second_boy black hispanic other_race, ffirst robust
estimates store C2R1

ivreg2 wkswork1 (more_than_2 = same_sex) age age_first_birth first_boy second_boy black hispanic other_race, ffirst robust
estimates store C2R2

ivreg2 uhrswork (more_than_2 = same_sex) age age_first_birth first_boy second_boy black hispanic other_race, ffirst robust
estimates store C2R3

ivreg2 incwage (more_than_2 = same_sex) age age_first_birth first_boy second_boy black hispanic other_race, ffirst robust
estimates store C2R4

ivreg2 lnftotinc (more_than_2 = same_sex) age age_first_birth first_boy second_boy black hispanic other_race, ffirst robust
estimates store C2R5

save "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults2.rtf", replace

esttab C2R1 C2R2 C2R3 C2R4 C2R5, drop(age age_first_birth first_boy second_boy black hispanic other_race), using "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults2.rtf", rtf replace append se(%9.3f) b(%9.3f) star(* .10 ** .05 *** .01)

***2SLS (TWO BOYS, TWO GIRLS)
gen two_boysgirls = 0
replace two_boysgirls = 1 if two_boys == 1 | two_girls == 1
ivreg2 worked_for_pay (more_than_2 = two_boysgirls) age age_first_birth first_boy black hispanic other_race, ffirst robust
estimates store C3R1

ivreg2 wkswork1 (more_than_2 = two_boysgirls) age age_first_birth first_boy black hispanic other_race, ffirst robust
estimates store C3R2

ivreg2 uhrswork (more_than_2 = two_boysgirls) age age_first_birth first_boy black hispanic other_race, ffirst robust
estimates store C3R3

ivreg2 incwage (more_than_2 = two_boysgirls) age age_first_birth first_boy black hispanic other_race, ffirst robust
estimates store C3R4

ivreg2 lnftotinc (more_than_2 = two_boysgirls) age age_first_birth first_boy black hispanic other_race, ffirst robust
estimates store C3R5

save "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults3.rtf", replace

esttab C3R1 C3R2 C3R3 C3R4 C3R5, drop(age age_first_birth first_boy black hispanic other_race), using "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults3.rtf", rtf replace append se(%9.3f) b(%9.3f) star(* .10 ** .05 *** .01)


****************************MARRIED WOMEN*******************************

***OLS
reg worked_for_pay more_than_2 age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1
estimates store C4R1

reg wkswork1 more_than_2 age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1
estimates store C4R2

reg uhrswork more_than_2 age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1
estimates store C4R3

reg incwage more_than_2 age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1
estimates store C4R4

reg lnftotinc more_than_2 age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1
estimates store C4R5

gen non_wife_income = ftotinc - incwage
gen ln_non_wife_income = ln(non_wife_income)
reg ln_non_wife_income more_than_2 age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1
estimates store C4R6

save "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults4.rtf", replace

esttab C4R1 C4R2 C4R3 C4R4 C4R5 C4R6, title("Panel (b): Married Women") drop(age age_first_birth first_boy second_boy black hispanic other_race), using "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults4.rtf", rtf replace append se(%9.3f) b(%9.3f) star(* .10 ** .05 *** .01)

***2SLS (SAME SEX)
ivreg2 worked_for_pay (more_than_2 = same_sex) age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1, ffirst robust 
estimates store C5R1

ivreg2 wkswork1 (more_than_2 = same_sex) age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1, ffirst robust 
estimates store C5R2

ivreg2 uhrswork (more_than_2 = same_sex) age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1, ffirst robust 
estimates store C5R3

ivreg2 incwage (more_than_2 = same_sex) age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1, ffirst robust 
estimates store C5R4

ivreg2 lnftotinc (more_than_2 = same_sex) age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1, ffirst robust 
estimates store C5R5 

ivreg2 ln_non_wife_income (more_than_2 = same_sex) age age_first_birth first_boy second_boy black hispanic other_race if married_mother==1, ffirst robust 
estimates store C5R6 

save "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults5.rtf", replace

esttab C5R1 C5R2 C5R3 C5R4 C5R5 C5R6, drop(age age_first_birth first_boy second_boy black hispanic other_race), using "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults5.rtf", rtf replace append se(%9.3f) b(%9.3f) star(* .10 ** .05 *** .01)

***2SLS (TWO BOYS, TWO GIRLS)
ivreg2 worked_for_pay (more_than_2 = two_boysgirls) age age_first_birth first_boy black hispanic other_race if married_mother==1, ffirst robust
estimates store C6R1

ivreg2 wkswork1 (more_than_2 = two_boysgirls) age age_first_birth first_boy black hispanic other_race if married_mother==1, ffirst robust
estimates store C6R2

ivreg2 uhrswork (more_than_2 = two_boysgirls) age age_first_birth first_boy black hispanic other_race if married_mother==1, ffirst robust
estimates store C6R3

ivreg2 incwage (more_than_2 = two_boysgirls) age age_first_birth first_boy black hispanic other_race if married_mother==1, ffirst robust
estimates store C6R4

ivreg2 lnftotinc (more_than_2 = two_boysgirls) age age_first_birth first_boy black hispanic other_race if married_mother==1, ffirst robust
estimates store C6R5

ivreg2 ln_non_wife_income (more_than_2 = two_boysgirls) age age_first_birth first_boy black hispanic other_race if married_mother==1, ffirst robust
estimates store C6R6 

save "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults6.rtf", replace

esttab C6R1 C6R2 C6R3 C6R4 C6R5 C6R6, drop(age age_first_birth first_boy black hispanic other_race), using "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults6.rtf", rtf replace append se(%9.3f) b(%9.3f) star(* .10 ** .05 *** .01)

************************HUSBANDS OF MARRIED WOMEN**************************************

***OLS
reg husband_worked_for_pay more_than_2 husband_age1 husband_age_first_birth first_boy second_boy black hispanic other_race if dad1==1
estimates store C7R1

reg husband_wkswork1 more_than_2 husband_age1 husband_age_first_birth first_boy second_boy black hispanic other_race if dad1==1
estimates store C7R2

reg husband_uhrswork1 more_than_2 husband_age1 husband_age_first_birth first_boy second_boy black hispanic other_race if dad1==1
estimates store C7R3

reg husband_incwage1 more_than_2 husband_age1 husband_age_first_birth first_boy second_boy black hispanic other_race if dad1==1
estimates store C7R4

save "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults7.rtf", replace

esttab C7R1 C7R2 C7R3 C7R4, title("Panel (C): Husbands of Married Women") drop(husband_age1 husband_age_first_birth first_boy second_boy black hispanic other_race), using "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults7.rtf", rtf replace append se(%9.3f) b(%9.3f) star(* .10 ** .05 *** .01)

***2SLS (SAME SEX)
ivreg2 husband_worked_for_pay (more_than_2 = same_sex) husband_age1 husband_age_first_birth first_boy second_boy black hispanic other_race if dad1==1, ffirst robust 
estimates store C8R1

ivreg2 husband_wkswork1 (more_than_2 = same_sex) husband_age1 husband_age_first_birth first_boy second_boy black hispanic other_race if dad1==1, ffirst robust 
estimates store C8R2

ivreg2 husband_uhrswork1 (more_than_2 = same_sex) husband_age1 husband_age_first_birth first_boy second_boy black hispanic other_race if dad1==1, ffirst robust 
estimates store C8R3

ivreg2 husband_incwage1 (more_than_2 = same_sex) husband_age1 husband_age_first_birth first_boy second_boy black hispanic other_race if dad1==1, ffirst robust 
estimates store C8R4

save "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults8.rtf", replace

esttab C8R1 C8R2 C8R3 C8R4, drop(husband_age1 husband_age_first_birth first_boy second_boy black hispanic other_race), using "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults8.rtf", rtf replace append se(%9.3f) b(%9.3f) star(* .10 ** .05 *** .01)

***2SLS (TWO BOYS, TWO GIRLS)
gen two_boysgirls = 0
replace two_boysgirls = 1 if two_boys == 1 | two_girls == 1

ivreg2 husband_worked_for_pay (more_than_2 = two_boysgirls) husband_age1 husband_age_first_birth first_boy black hispanic other_race if dad1==1, ffirst robust
estimates store C9R1

ivreg2 husband_wkswork1 (more_than_2 = two_boysgirls) husband_age1 husband_age_first_birth first_boy black hispanic other_race if dad1==1, ffirst robust
estimates store C9R2 

ivreg2 husband_uhrswork1 (more_than_2 = two_boysgirls) husband_age1 husband_age_first_birth first_boy black hispanic other_race if dad1==1, ffirst robust
estimates store C9R3

ivreg2 husband_incwage1 (more_than_2 = two_boysgirls) husband_age1 husband_age_first_birth first_boy black hispanic other_race if dad1==1, ffirst robust
estimates store C9R4 

save "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults9.rtf", replace

esttab C9R1 C9R2 C9R3 C9R4, drop(husband_age1 husband_age_first_birth first_boy black hispanic other_race), using "C:\Users\yk21598\OneDrive - University of Bristol\dissaemresults9.rtf", rtf replace se(%9.3f) b(%9.3f) star(* .10 ** .05 *** .01) nonotes addnotes("Note: Standard errors in parentheses. *p < 0.1, **p < 0.05, ***p < 0.01. Other covariates in the models are Age, Age at first birth, plus indicators for Boy 1st, Boy 2nd, Black, Hispanic and Other race. The variable Boy 2nd is excluded from Two Boys, Two Girls estimation.")

log close
