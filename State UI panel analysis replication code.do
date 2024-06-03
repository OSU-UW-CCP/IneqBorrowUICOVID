///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///// Replication code for "Inequality in High-Cost Borrowing and Unemployment Insurance Generosity
/////				   in US States during the COVID-19 Pandemic"
/////	Last updated: 01/17/2024 
/////  

* Load proprietary credit panel dataset from Experian (not available for public use; see supplemental data for publicy-available replication data)
use "finaldata_for_uianalysis_postcovid_b.dta", clear

set scheme cleanplots

* create groups of controls		
gl state gspcapita pct_gsp_ch unemploy_rate govdem anyeitc refundablestateeitc1yes pct_snap maxben4_snap stayathome nonessential utilitiesm anystate_q slrest_v4
gl zip unemploy_zip i.race mhi_zip 
gl ind_postcovid i.income_qrtile_score_201912 i.scoregroup2 i.anyrehbal_201912
gl state_ea gspcapita pct_gsp_ch unemploy_rate govdem anyeitc refundablestateeitc1yes pct_snap maxben4_snap_eb stayathome nonessential utilitiesm anystate_q slrest_v4

***********************************************************************************************************************************************************
*** Descriptive statistics for main analytic sample
*** Table 1	

* Find the analytic sample 
reg hsn_hasclartrade c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)
keep if e(sample)==1

* Create dummies as needed
tab income_qrtile_score_201912, gen(incomegroup)
tab race, gen(racialcomp)
tab scoregroup2, gen(cscoregroup)
tab archive, gen(periodobs)

* Step 2. Gen summary statistics for full sample and by outcome status (0,1)
gl descriptives hsn_hasclarinq hsn_hasclartrade hsn_newpiftrade hsn_newrehtrade ///
		state_uigen_pua gspcapita pct_gsp_ch unemploy_rate govdem anyeitc refundablestateeitc1yes pct_snap maxben4_snap ///
		stayathome nonessential utilitiesm anystate_q ///
		incomegroup1 incomegroup2 incomegroup3 incomegroup4 racialcomp1 racialcomp2 racialcomp3 racialcomp4 ///
		cscoregroup1 cscoregroup2 cscoregroup3 cscoregroup4 hsn_hasccbalance ///
		unemploy_zip mhi_zip slrest_v4 ///
		periodobs1 periodobs2 periodobs3 periodobs4 periodobs5 periodobs6 periodobs7
						 
eststo full: estpost sum $descriptives
						 
eststo yesclarinq: estpost sum $descriptives if hsn_hasclarinq==1 
eststo noclarinq: estpost sum $descriptives if hsn_hasclarinq==0

eststo yesclartrade: estpost sum $descriptives if hsn_hasclartrade==1
eststo noclartrade: estpost sum $descriptives if hsn_hasclartrade==0

eststo yespiftrade: estpost sum $descriptives if hsn_newpiftrade==1
eststo nopiftrade: estpost sum $descriptives if hsn_newpiftrade==0

eststo yesrehtrade: estpost sum $descriptives if hsn_newrehtrade==1
eststo norehtrade: estpost sum $descriptives if hsn_newrehtrade==0

* Step 3. T-tests 
eststo test1: estpost ttest $descriptives if insamp==1, by(hsn_hasclarinq) unequal
eststo test2: estpost ttest $descriptives if insamp==1, by(hsn_hasclartrade) unequal
eststo test3: estpost ttest $descriptives if insamp==1, by(hsn_newpiftrade) unequal
eststo test4: estpost ttest $descriptives if insamp==1, by(hsn_newrehtrade) unequal

* Step 4. Estab
esttab full yesclarinq noclarinq test1 yesclartrade noclartrade test2 yespiftrade nopiftrade test3 yesrehtrade norehtrade test4 ///
	using "SumStats_byoutcomes_revision_`c(current_date)'.csv", ///
	replace cells ("mean(pattern(1 1 1 0 1 1 0 1 1 0 1 1 0) fmt(2)) sd(pattern(1 1 1 0 1 1 0 1 1 0 1 1 0) fmt(2)) b(star pattern(0 0 0 1 0 0 1 0 0 1 0 0 1) fmt(0))")  ///
	label title (Descriptives) nonumbers mtitles ("Full" "Y:ClarInq" "N:ClarInq" "Test" "Y:ClarTrd" "N:ClarTrd" "Test" "Y:PifTrd" "N:PifTrd" "Test" "Y:RehTrd" "N:RehTrd" "Test") 



******************************************************************************************************************************************************************
*** Collapse and look at state-level bivariate associations
*** 2020-2021, use the pua-adjusted max UI benefit generosity measure (in $1,000s)	

* Step 1. Collapse to the state level
collapse hsn_hasclartrade hsn_newpiftrade hsn_newrehtrade state_uigen_pua, by(stab)


* Step 2. Calculate correlation coefficients at state-level
corr state_uigen_pua hsn_hasclartrade // ctrades
corr state_uigen_pua hsn_newpiftrade // new pif trades
corr state_uigen_pua hsn_newrehtrade // new reh trades

* Step 3. Generate panel graphs w/ scatterplots of credit outcomes with lfits 
scatter hsn_hasclartrade state_uigen_pua, msymbol(none) mlabel(stab) || (lfit hsn_hasclartrade state_uigen_pua),  leg(order(1 "" 2 "Pearson's r = -0.531") position(2) ring(0)) note("") xtitle("") title("New AFS loan") 

gr save "scatterclartrade_uigen_pua_`c(current_date)'.gph", replace
 
scatter hsn_newpiftrade state_uigen_pua, msymbol(none) mlabel(stab) || (lfit hsn_newpiftrade state_uigen_pua), leg(order(1 "" 2 "Pearson's r = -0.477") position(2) ring(0)) note("") xtitle("") title("New personal finance loan")

gr save "scatterpiftrade_uigen_pua_`c(current_date)'.gph", replace

scatter hsn_newrehtrade state_uigen_pua, msymbol(none) mlabel(stab) || (lfit hsn_newrehtrade state_uigen_pua), leg(order(1 "" 2 "Pearson's r = -0.022") position(2) ring(0)) note("") xtitle("") title("New credit card")

gr save "scatterrehtrade_uigen_pua_`c(current_date)'.gph", replace
	
	
	
************************************************************************************************************************************************************
* vizualize the PUA uigen max results with income interactions
* Figure 2

// clarity trades (any)			
reg hsn_hasclartrade c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Pr(New AFS loan)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_hasclartrade_uigen_pua.gph", replace
gr export "hsn_hasclartrade_uigen_pua.png", replace


// new pif trades
reg hsn_newpiftrade c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Pr(New personal loan)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	
gr save "hsn_newpiftrade_uigen_pua.gph", replace	
gr export "hsn_newpiftrade_uigen_pua.png", replace


// new reh trades
reg hsn_newrehtrade c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + PUA (in $1,000s)") ytitle("Pr(New credit card)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_newrehtrade_uigen_pua.gph", replace
gr export "hsn_newrehtrade_uigen_pua.png", replace



*****************************************************************************************************************************************************
** Temporal variation in state max ben UI generosity, 2020-2021
**

preserve

use "statedata.dta", replace

* period
gen period=4 if archive==201803
	replace period=5 if archive==201806
	replace period=6 if archive==201809
	replace period=7 if archive==201812
	replace period=8 if archive==201903
	replace period=9 if archive==201906
	replace period=10 if archive==201909
	replace period=11 if archive==201912
	replace period=12 if archive==202003
	replace period=13 if archive==202006
	replace period=14 if archive==202009
	replace period=15 if archive==202012
	replace period=16 if archive==202103
	replace period=17 if archive==202106
	replace period=18 if archive==202109
	replace period=19 if archive==202112
	

collapse state_uigen_pua state_uigenmin_pua state_rr3 if period>=4, over(statefips period)

xtset statefips period

* Figure 4 (included in main text)
xtline state_uigen_pua, overlay legend(off) ytitle("Max benefit + PUA (in $1,000s)") xtitle("") xline(12, lp(solid) lw(thick) lc(red)) ///
						xlab(4 "Q1:2018" 5 "Q2:2018" 6 "Q3:2018" 7 "Q4:2018" 8 "Q1:2019" ///
						9 "Q2:2019" 10 "Q3:2019" 11 "Q4:2019" 12 "Q1:2020" 13 "Q2:2020" 14 "Q3:2020" 15 "Q4:2020" 16 "Q1:2021" ///
						17 "Q2:2021" 18 "Q3:2021" 19 "Q4:2021", angle(90))

gr save "maxben_pua_state_trends_2015_2021.gph", replace
gr export "maxben_pua_state_trends_2015_2021.png", replace

* Supplemental Table 6
xtline state_uigenmin_pua, overlay legend(off) ytitle("Min benefit + PUA (in $1,000s)") xtitle("") xline(12, lp(solid) lw(thick) lc(red)) ///
						xlab(4 "Q1:2018" 5 "Q2:2018" 6 "Q3:2018" 7 "Q4:2018" 8 "Q1:2019" ///
						9 "Q2:2019" 10 "Q3:2019" 11 "Q4:2019" 12 "Q1:2020" 13 "Q2:2020" 14 "Q3:2020" 15 "Q4:2020" 16 "Q1:2021" ///
						17 "Q2:2021" 18 "Q3:2021" 19 "Q4:2021", angle(90))

gr save "minben_pua_state_trends_2015_2021.gph", replace
gr export "minben_pua_state_trends_2015_2021.png", replace

* Supplemental Table 7
xtline state_rr3, overlay legend(off) ytitle("Pua-adjusted replacement ratio (in %-points)") xtitle("") xline(12, lp(solid) lw(thick) lc(red)) ///
						xlab(4 "Q1:2018" 5 "Q2:2018" 6 "Q3:2018" 7 "Q4:2018" 8 "Q1:2019" ///
						9 "Q2:2019" 10 "Q3:2019" 11 "Q4:2019" 12 "Q1:2020" 13 "Q2:2020" 14 "Q3:2020" 15 "Q4:2020" 16 "Q1:2021" ///
						17 "Q2:2021" 18 "Q3:2021" 19 "Q4:2021", angle(90))

gr save "rr3_pua_state_trends_2015_2021.gph", replace
gr export "rr3_pua_state_trends_2015_2021.png", replace


restore



******************************************************************************************************************************************************
*** Tab number of consumer-observations in each state
*** Supplemental Table 1

tab state



*******************************************************************************************************************************************************
*** Supplemental Table 2. Full Model Results
***

* fix units s.t. decimals are moved up
* put everything in 10s of units
* so, uigen is in $10,000
* rr2 is in 10s of percentage-points
foreach var in state_uigen_pua state_uigenmin_pua state_rr3 {
	     	
	 replace  `var'=`var'/10   
	     
		}  

foreach outvar in hsn_hasclartrade hsn_newpiftrade hsn_newrehtrade {

* Heterogeneity: Dollar amount - w/ income interactions - state + year FEs
reg `outvar' c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)
outreg2 using "reg_postcovid_`outvar'_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append

* Heterogeneity: Min benefit - w/ income interactions - state + year FEs
reg `outvar' c.state_uigenmin_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)
outreg2 using "reg_postcovid_`outvar'_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append 

* Heterogeneity: Replacement ratio - w/ income interactions - state + year FEs
reg `outvar' c.state_rr3##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)
outreg2 using "reg_postcovid_`outvar'_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append 

			}		
				
* put everything back into original units for graphing
foreach var in state_uigen_pua state_iur_uigen_pua state_uigenmin_pua state_iur_uigenmin_pua ///
	     state_rr3 state_iur_rr3 state_uigen state_iur_uigen state_uigenmin state_iur_uigen ///
	     state_rr2 state_iur_rr2 {
	     	
	 replace  `var'=`var'*10   
	     
		}  



*****************************************************************************************************************************************************
** Use pua-adjusted state replacement ratio
** Supplemental Figure 1

replace state_rr3=state_rr3*100 // set so in percentage-points

// clarity trades (any)			
reg hsn_hasclartrade c.state_rr3##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_rr3=(30(10)150) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("PUA-adjusted replacement ratio (in %-points)") ytitle("Pr(New AFS loan)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_hasclartrade_rr_pua.gph", replace
gr export "hsn_hasclartrade_rr_pua.png", replace


// new pif trades
reg hsn_newpiftrade c.state_rr3##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_rr3=(30(10)150) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("PUA-adjusted replacement ratio (in %-points)") ytitle("Pr(New personal loan)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_newpiftrade_rr_pua.gph", replace	     
gr export "hsn_newpiftrade_rr_pua.png", replace


// new reh trades
reg hsn_newrehtrade c.state_rr3##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_rr3=(30(10)150) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("PUA-adjusted replacement ratio (in %-points)") ytitle("Pr(New credit card)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_newrehtrade_rr_pua.gph", replace	     
gr export "hsn_newrehtrade_rr_pua.png", replace



************************************************************************************************************************************************
** Use pua-adjusted state minimum UI benefit generosity
** Supplemental Figure 2

// clarity trades (any)			
reg hsn_hasclartrade c.state_uigenmin_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigenmin_pua=(.12(2)20.12) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Min benefit + FPUC (in $1,000s)") ytitle("Pr(New AFS loan)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_hasclartrade_uigenmin_pua.gph", replace	     
gr export "hsn_hasclartrade_uigenmin_pua.png", replace


// new pif trades
reg hsn_newpiftrade c.state_uigenmin_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigenmin_pua=(.12(2)20.12) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Min benefit + FPUC (in $1,000s)") ytitle("Pr(New personal loan)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_newpiftrade_uigenmin_pua.gph", replace	     
gr export "hsn_newpiftrade_uigenmin_pua.png", replace


// new reh trades
reg hsn_newrehtrade c.state_uigenmin_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigenmin_pua=(.12(2)20.12) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Min benefit + FPUC (in $1,000s)") ytitle("Pr(New credit card)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_newrehtrade_uigenmin_pua.gph", replace	     
gr export "hsn_newrehtrade_uigenmin_pua.png", replace



**********************************************************************************************************************************************************
** State max benefit + pua predicting new credit inquiries
** Supplemental Figure 3

// clarity inquiries (any)			
reg hsn_hasclarinq c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Pr(New AFS loan)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_hasclarinq_uigen_pua.gph", replace	     
gr export "hsn_hasclarinq_uigen_pua.png", replace


// new personal inquiries
reg hsn_newiqfinq c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Pr(New personal loan)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_newiqfinq_uigen_pua.gph", replace	     
gr export "hsn_newiqfinq_uigen_pua.png", replace


// new credit card inquiries
reg hsn_newiqbinq c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Pr(New credit card)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_newiqbinq_uigen_pua.gph", replace		     
gr export "hsn_newiqbinq_uigen_pua.png", replace	



*****************************************************************************************************************************************************************
** State max benefit + PUA predicting log balances
** Supplemental Figure 4

* First step, log outcomes to account for skew
foreach var in hsn_reh5030 hsn_split_fip5020 hsn_clarbalance {

    gen  ln_`var'=log(1+`var')
        
        }
	
// log clarity balances			
reg ln_hsn_clarbalance c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Log total AFS balance") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_logclarbal_uigen_pua.gph", replace	     
gr export "hsn_logclarbal_uigen_pua.png", replace


// log personal loan balances
reg ln_hsn_split_fip5020 c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Log total personal balance") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_logpifbal_uigen_pua.gph", replace	     
gr export "hsn_logpifbal_uigen_pua.png", replace


// log credit card balances
reg ln_hsn_reh5030 c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Log total credit card balance") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_logrehbal_uigen_pua.gph", replace			     
gr export "hsn_logrehbal_uigen_pua.png", replace		



********************************************************************************************************************************************************
** State max benefit plus PUA predicting having a high credit card utilization ratio
** Supplemental Figure 5

// utilization ratio >=70%		
reg hsn_cc_util_70plus c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Pr(Utilization >= 70%)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_ccutil70plus_uigen_pua.gph", replace		     
gr export "hsn_ccutil70plus_uigen_pua.png", replace	

// utilization ratio >=80%		
reg hsn_cc_util_80plus c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Pr(Utilization >= 80%)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_ccutil80plus_uigen_pua.gph", replace		     
gr export "hsn_ccutil80plus_uigen_pua.png", replace	

// utilization ratio >=90%		
reg hsn_cc_util_90plus c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Pr(Utilization >= 90%)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_ccutil90plus_uigen_pua.gph", replace	     
gr export "hsn_ccutil90plus_uigen_pua.png", replace
		

***********************************************************************************************************************************************************************************
*** Adjust for Federal Emergency Allotment Benefit Supplements to the SNAP Program (heterogeneous termination to the EA benefits by state in 2021) 
***  Add control for time-varying SNAP benefit that includes $95 monthly benefit top off
*** Supplemental Figure 6


// clarity trades (any)			
reg hsn_hasclartrade c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state_ea, vce(cluster consumersequence)


margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Pr(New AFS loan)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_hasclartrade_uigen_pua_adjmaxsnapea.gph", replace
gr export "hsn_hasclartrade_uigen_pua_adjmaxsnapea.png", replace


// new pif trades
reg hsn_newpiftrade c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state_ea, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + FPUC (in $1,000s)") ytitle("Pr(New personal loan)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	
gr save "hsn_newpiftrade_uigen_pua_adjustmaxsnapea.gph", replace	
gr export "hsn_newpiftrade_uigen_pua_adjustmaxsnapea.png", replace


// new reh trades
reg hsn_newrehtrade c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state_ea, vce(cluster consumersequence)

margins, at(state_uigen_pua=(4(4)48) income_qrtile_score_201912=(0 1 2 3)) atmeans
marginsplot, title("") xtitle("Max benefit + PUA (in $1,000s)") ytitle("Pr(New credit card)") ///
	     legend(order(5 "p0-p25" 6 "p25-p50" 7 "p50-p75" 8 "p75-p100"))
	     
gr save "hsn_newrehtrade_uigen_pua_adjustmaxsnapea.gph", replace
gr export "hsn_newrehtrade_uigen_pua_adjustmaxsnapea.png", replace
	
	
			
***********************************************************************************************************************************************************************************
*** Include a lagged measure of new credit borrowing in Q4 2019
*** Supplemental Table 3

preserve
* first--grab lagged borrowing indicators from larger dataset covering pre-covid times
use consumersequence archive hsn_hasclartrade hsn_newpiftrade hsn_newrehtrade using "uianalysis_postcovid_panel_data_b.dta", clear

keep if archive==201912

gen hsn_hasclartrade_201912=hsn_hasclartrade
gen hsn_newpiftrade_201912=hsn_newpiftrade
gen hsn_newrehtrade_201912=hsn_newrehtrade

keep consumersequence hsn_hasclartrade_201912 hsn_newpiftrade_201912 hsn_newrehtrade_201912

save "uianalysis_postcovid_panel_data_borrowing201912.dta", replace
restore

merge m:1 consumersequence using "uianalysis_postcovid_panel_data_borrowing201912.dta"
drop if _merge==2


* Table results............................................................................................................................................
* Has clarity trade
reg hsn_hasclartrade c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state hsn_hasclartrade_201912, vce(cluster consumersequence)

outreg2 using "reg_laggedborrowing_`outvar'_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label replace

* Has new pif trade
reg hsn_newpiftrade c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state hsn_newpiftrade_201912, vce(cluster consumersequence)

outreg2 using "reg_laggedborrowing_`outvar'_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append

* Has new reh trade
reg hsn_newrehtrade c.state_uigen_pua##i.income_qrtile_score_201912 i.archive i.stateid $ind_postcovid $zip $state hsn_newrehtrade_201912, vce(cluster consumersequence)	

outreg2 using "reg_laggedborrowing_`outvar'_`c(current_date)'.xls", ///
excel auto(3) dec(3) sdec(3) 2aster alpha (.001, .01, .05, .1) symbol (***, **, *, +) label append
	

