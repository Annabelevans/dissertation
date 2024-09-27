*************Differences in time spent on housework and childcare between East and West Germany - replication of Table D1 (Appendix) in Lippmann et al (2020) using 22 waves of data from the German Socio-Economic Panel between 1991-2012. 
set more off
clear all 
capture log close

cd "OneDrive - University of Bristol/Dissertation/Dissertation do-file"

use "C:\Users\yk21598\OneDrive - University of Bristol\gsoep.dta", clear

*cleaning dataset and replacing negative values 
keep if whweek >= 0 & incjob1_mg > 0 & incjob1_mn > 0
drop if missing(hwork) | missing(ccare) | edu4 == -1 

bysort wave cpf_hid (female): keep if _N == 2 & female[1] != female[2] 

bysort pid: drop if _N == 1 
drop if pid == 2088902 

*creating easthh dummy 
bysort wave cpf_hid: drop if loc89[1] != loc89[2] 
bysort wave cpf_hid: gen easthh = (loc89[1] == 1 & loc89[2] == 1) 

bysort wave cpf_hid: egen max_inc_mg = max(incjob1_mg)
bysort wave cpf_hid: egen max_inc_mn = max(incjob1_mn)

*generating wifeearnsmore dummy
gen wifeearnsmore = (female == 1 & incjob1_mg == max_inc_mg)
replace wifeearnsmore = 1 if female == 0 & incjob1_mg != max_inc_mg 

bysort wave cpf_hid: egen total_incjob1_mg = total(incjob1_mg)
bysort wave cpf_hid: egen household_inc = total(incjob1_mn)

*Generating average income variable for each individual across all waves of observations
egen avgincome = mean(incjob1_mn), by (pid)

*Splitting the average income variable into genders
gen avgincomefemale = avgincome if female==1
gen avgincomemale = avgincome if female==0 

*Generating total avg incomes for each gender under the same household ID 
bysort cpf_hid: egen total_female_income = total(avgincomefemale)
bysort cpf_hid: egen total_male_income = total(avgincomemale)

gen relative_inc = incjob1_mn / household_inc if female == 1 
bysort wave cpf_hid (female): replace relative_inc = relative_inc[2] if missing(relative_inc) 

*Generating age and housework dummies 
gen age_w = age if female==1 
gen age_m = age if female==0 
gen age_sq_m = age_m^2
gen age_sq_w = age_w^2

gen hwork_female = hwork if female == 1
gen hwork_male = hwork if female == 0
replace hwork_female = 0 if missing(hwork_female)
replace hwork_male = 0 if missing(hwork_male)

*Generating a variable which combines housework and childcare
gen hwork_inc_ccare = hwork + ccare
gen hwork_inc_ccare_female = hwork + ccare if female == 1
gen hwork_inc_ccare_male = hwork + ccare if female == 0

save "C:\Users\yk21598\OneDrive - University of Bristol\gsoep2.dta", replace

****************TABLE 1: DESCRIPTIVE STATS****************************
use "C:\Users\yk21598\OneDrive - University of Bristol\gsoep2.dta", clear

*generating a kids dummy and dropping kids
gen kids=1 if kidsn_hh17>=1
replace kids=0 if kidsn_hh17==0
drop if kids==0

*generating descriptive statistics table for east germans 
dtable relative_inc wifeearnsmore hwork_female hwork_male hwork_inc_ccare_female hwork_inc_ccare_male total_female_income total_male_income age_w age_m if easthh==1, title("Table 1: Descriptive Statistics of East and West Samples, Married Couples Aged 18-65 with Children") note("Note: The data is taken from the German Socio-Economic Panel, using all the waves from 1991 until 2012. Eastern (Western) couples are those in which both spouses lived in GDR (FRG) before 1989. Income measures are based on 2010 euros. ") export(table1.docx, replace)

*generating descrptive statistics table for west germans 
dtable relative_inc wifeearnsmore hwork_female hwork_male hwork_inc_ccare_female hwork_inc_ccare_male total_female_income total_male_income age_w age_m if east==0, export(table2.docx, replace)

*combining tables 
graph combine table1.docx and table2.docx, col(2) name(descriptive_table1, replace)


*************TABLE 2: REGRESSION*********************************
use "C:\Users\yk21598\OneDrive - University of Bristol\gsoep2.dta", clear

*generating kids dummy and keeping kids
gen kids=1 if kidsn_hh17>=1
replace kids=0 if kidsn_hh17==0

*generating additional variables for partners age, education and income
bysort wave cpf_hid (female): gen p_age = cond(female == 0, age[2], age[1])
bysort wave cpf_hid (female): gen p_edu4 = cond(female == 0, edu4[2], edu4[1])
bysort wave cpf_hid (female): gen p_income = cond(female == 0, incjob1_mg[2], incjob1_mg[1])

gen log_inc = log(incjob1_mg) 
gen log_p_inc = log(p_income) 
gen log_household_inc = log(household_inc) 

ssc install estout 
ssc install ftools 
ssc install reghdfe

*creating controls
global control_v "relative_inc log_household_inc log_inc log_p_inc age p_age c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 i.state i.wavey"

global control_v2 "relative_inc c.relative_inc##c.easthh log_household_inc log_inc log_p_inc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 i.state i.wavey"

xtset pid wavey

*(1) WifeEarnsMore - West Germany
reg hwork_inc_ccare wifeearnsmore $control_v if female == 1 & easthh == 0, cluster(pid)
estimates store WW

*(2) WifeEarnsMore - East Germany 
reg hwork_inc_ccare wifeearnsmore $control_v if female == 1 & easthh == 1, cluster(pid)
estimates store WE

*(3) WifeEarnsMore, WifeEarnsMorexEast, East - All
reg hwork_inc_ccare wifeearnsmore c.wifeearnsmore#c.easthh $control_v2 if female == 1, cluster(pid)
estimates store WA 

*(4) WifeEarnsMore - West Germany (FE)
xtreg hwork_inc_ccare wifeearnsmore $control_v if female == 1 & east == 0, fe cluster(pid)
estimates store WW2

*(5) WifeEarnsMore - East Germany (FE)
xtreg hwork_inc_ccare wifeearnsmore $control_v if female == 1 & east == 1, fe cluster(pid)
estimates store WE2

*(6) WifeEarnsMore, WifeEarnsMorexEast - All (FE)
xtreg hwork_inc_ccare wifeearnsmore c.wifeearnsmore#c.easthh $control_v2 if female == 1, fe cluster(pid) 
estimates store WA2

*creating regression table
esttab WW WE WA WW2 WE2 WA2,  title("Table 2: Housework Including Childcare and Relative Income") mtitles("West" "East" "All" "West" "East" "All") coeflabels(wifeearnsmore "WifeEarnsMore" c.wifeearnsmore#c.easthh "WifeEarnsMorexEast" easthh "East") keep(wifeearnsmore c.wifeearnsmore#c.easthh easthh), using "C:\Users\yk21598\OneDrive - University of Bristol\minortable5.rtf", rtf replace se(%9.3f) b(%9.3f) star(* .10 ** .05 *** .01) note("Standard errors in parentheses. *p < 0.10, **p < 0.05, *** p < 0.01. The data is taken from the German Socio-Economic Panel using all the waves from 1991 to 2012. The sample only contains dual-earner couples. East is a dummy equal to 1 when both spouses lived in the former GDR before 1989. Controls include relative income between spouses, log of household income, log of woman's income and log of man's income, respondent and partners' age and age squared, respondent and partner's education level, a dummy controlling for the presence of children, year fixed-effects, land fixed-effects and individual fixed-effects (columns 4, 5 and 6).")


****************TABLE 3: ROBUSTNESS SINGLE-EARNER**************************
use "C:\Users\yk21598\OneDrive - University of Bristol\gsoep.dta", clear

*keep if income equals zero
keep if whweek >= 0 & incjob1_mg >= 0 & incjob1_mn >= 0
drop if missing(hwork) | missing(ccare) | edu4 == -1 

*complete same steps as previous regression
bysort wave cpf_hid (female): keep if _N == 2 & female[1] != female[2] 

bysort pid: drop if _N == 1 
drop if pid == 2088902 

bysort wave cpf_hid: drop if loc89[1] != loc89[2] 
bysort wave cpf_hid: gen easthh = (loc89[1] == 1 & loc89[2] == 1) 

bysort wave cpf_hid: egen max_inc_mg = max(incjob1_mg)
bysort wave cpf_hid: egen max_inc_mn = max(incjob1_mn)

gen wifeearnsmore = (female == 1 & incjob1_mg == max_inc_mg)
replace wifeearnsmore = 1 if female == 0 & incjob1_mg != max_inc_mg 

bysort wave cpf_hid: egen total_incjob1_mg = total(incjob1_mg)
bysort wave cpf_hid: egen household_inc = total(incjob1_mn)

egen avgincome = mean(incjob1_mn), by (pid)

gen avgincomefemale = avgincome if female==1
gen avgincomemale = avgincome if female==0 

bysort cpf_hid: egen total_female_income = total(avgincomefemale)
bysort cpf_hid: egen total_male_income = total(avgincomemale)

gen relative_inc = incjob1_mn / household_inc if female == 1 
bysort wave cpf_hid (female): replace relative_inc = relative_inc[2] if missing(relative_inc) 

gen age_w = age if female==1 
gen age_m = age if female==0 
gen age_sq_m = age_m^2
gen age_sq_w = age_w^2

gen hwork_female = hwork if female == 1
gen hwork_male = hwork if female == 0
replace hwork_female = 0 if missing(hwork_female)
replace hwork_male = 0 if missing(hwork_male)

gen hwork_inc_ccare = hwork + ccare
gen hwork_inc_ccare_female = hwork + ccare if female == 1
gen hwork_inc_ccare_male = hwork + ccare if female == 0

gen kids=1 if kidsn_hh17>=1
replace kids=0 if kidsn_hh17==0

bysort wave cpf_hid (female): gen p_age = cond(female == 0, age[2], age[1])
bysort wave cpf_hid (female): gen p_edu4 = cond(female == 0, edu4[2], edu4[1])
bysort wave cpf_hid (female): gen p_income = cond(female == 0, incjob1_mg[2], incjob1_mg[1])

gen log_inc = log(incjob1_mg) 
gen log_p_inc = log(p_income) 
gen log_household_inc = log(household_inc) 

*generate dummies for individuals who do not work
gen notwork1 = 1 if incjob1_mn == 0
replace notwork1 = 0 if missing(notwork1)

*split into men and women
gen wom_notwork1 = notwork1 if female == 1
gen man_notwork1 = notwork1 if female == 0
replace wom_notwork1 = 0 if missing(wom_notwork1)
replace man_notwork1 = 0 if missing(man_notwork1)

*learn about dummies
tab wom_notwork1 easthh // only 135 women earn 0, 108 in West Germany and 27 in East Germany
tab man_notwork1 easthh // only 100 men earn 0, 52 in West Germany and 48 in East Germany

save "C:\Users\yk21598\OneDrive - University of Bristol\gsoep4.dta", replace

use "C:\Users\yk21598\OneDrive - University of Bristol\gsoep4.dta", clear 

global control_v "relative_inc log_household_inc log_inc log_p_inc age p_age c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 i.state i.wavey"

global control_v2 "relative_inc c.relative_inc##c.easthh log_household_inc log_inc log_p_inc c.age##c.age c.p_age##c.p_age kids i.edu4 i.p_edu4 i.state i.wavey"

xtset pid wavey

*column 1
reg hwork_inc_ccare wifeearnsmore $control_v if female == 1 & easthh == 0, cluster(pid)
estimates store WWSE

*column 2
reg hwork_inc_ccare wifeearnsmore $control_v if female == 1 & easthh == 1, cluster(pid)
estimates store WESE

*column 3
reg hwork_inc_ccare wifeearnsmore c.wifeearnsmore#c.easthh $control_v2 if female == 1, cluster(pid)
estimates store WASE

*column 4
xtreg hwork_inc_ccare wifeearnsmore $control_v if female == 1 & east == 0, fe cluster(pid)
estimates store WW2SE

*column 5
xtreg hwork_inc_ccare wifeearnsmore $control_v if female == 1 & east == 1, fe cluster(pid)
estimates store WE2SE

*column 6
xtreg hwork_inc_ccare wifeearnsmore c.wifeearnsmore#c.easthh $control_v2 if female == 1, fe cluster(pid) 
estimates store WA2SE

*creating regression table
esttab WWSE WESE WASE WW2SE WE2SE WA2SE,  title("Table 3: Housework Including Childcare and Relative Income - Single and Dual Earner Couples") mtitles("West" "East" "All" "West" "East" "All") coeflabels(wifeearnsmore "WifeEarnsMore" c.wifeearnsmore#c.easthh "WifeEarnsMorexEast" easthh "East") keep(wifeearnsmore c.wifeearnsmore#c.easthh easthh), using "C:\Users\yk21598\OneDrive - University of Bristol\minortable3.rtf", rtf replace se(%9.3f) b(%9.3f) star(* .10 ** .05 *** .01) note("Standard errors in parentheses. *p < 0.10, **p < 0.05, *** p < 0.01. The data is taken from the German Socio-Economic Panel using all the waves from 1991 to 2012. The sample contains single-earner and dual-earner couples. East is a dummy equal to 1 when both spouses lived in the former GDR before 1989. Controls include relative income between spouses, log of household income, log of woman's income and log of man's income, respondent and partners' age and age squared, respondent and partner's education level, a dummy controlling for the presence of children, year fixed-effects, land fixed-effects and individual fixed-effects (columns 4, 5 and 6).")

log close
