
****************************************************************************************************
* Annual TAXSIM Calculator for PSID Data: 1993
* Authors: Margaret McKeehan and Nick Frazier
* 
* Uses Family Public Data for 1993
* Must specify file name and location below
****************************************************************************************************

****************************************************************************************************
* Notes and Assumptions
****************************************************************************************************
*This code uses the year before the date of the interview as the tax year to be consistent with
*     the PSID's practice of having respondents report their income and other financial values 
*     for the prior year. Certain demographic variables, like location and marital status, indicate
*     the respondent's status at the time of interview and so are assumed to be the same as the
*      previous year. Users for whom this assumption is important are warned that the survey becomes 
*     biennial after 1997 which complicates reconciliation.
*
* This code was written to get best estimate possible for any year, not for continuity across years
* This code does not incorporate or rely on PSID accuracy codes
* Note: state taxes may affect federal taxes through the state and local tax deduction
* All non-wage, non-pension income is treated as "other property income" - dividends are not
*     separated out.
****************************************************************************************************

****************************************************************************************************
* Section 1: File Preamble
****************************************************************************************************
*** Basic File Setup
capture clear all
set more off

*** Set Run Parameters
local CALC_STATE_TAX = 1
local INCLUDE_PROP_TAX = 0

*** Store File Locations
local datacd "Input Data Location"

*** Load Data
local dataname ="`datacd'" + "\" + "Input Data Name.dta"
use "`dataname'"

****************************************************************************************************
* Section 2: Rename and Clean Variables
****************************************************************************************************

*** Rename 1993 Variables
rename V22406 age1_1993, replace
rename V22408 age2_1993, replace
rename V23159 childcare1_1993, replace
rename V21602 family_ID1_1993, replace
rename V21959 tax_inc1_1993, replace
rename V22027 gssi2_1993, replace
rename V22301 gssi3_1993, replace
rename V22412 mstat1_1993, replace
rename V23336 mstat2_1993, replace
rename V22409 num_dep2_1993, replace
rename V23179 num_dep3_1993, replace
rename V22043 pension_inc3_1993, replace
rename V22317 pension_inc4_1993, replace
rename V22303 pension_inc6_1993, replace
rename V22304 pension_inc6_TU_1993, replace
rename V22061 pension_inc8_1993, replace
rename V22062 pension_inc8_TU_1993, replace
rename V21617 prop_tax1_1993, replace
rename V21622 rent1_1993, replace
rename V21620 rent2_1993, replace
rename V21621 rent2_TU_1993, replace
rename V21603 state1_1993, replace
rename V22366 transfers1_1993, replace
rename V22108 unemp_inc2_1993, replace
rename V22204 unemp_inc3_1993, replace
rename V22094 unemp_inc4_1993, replace
rename V22095 unemp_inc4_TU_1993, replace
rename V22190 unemp_inc6_1993, replace
rename V22191 unemp_inc6_TU_1993, replace
rename V23323 wages1_1993, replace
rename V23324 wages2_1993, replace
rename V21807 wages3_1993, replace
rename V21806 wages4_1993, replace

*** Clean 1993 Variables
replace age1_1993 = . if age1_1993 == 99
replace age2_1993 = . if age2_1993 == 99
replace num_dep3_1993 = 0 if num_dep3_1993 == 9
replace mstat2_1993 = . if mstat2_1993 == 9
replace mstat1_1993 = . if mstat1_1993 == 9
replace state1_1993 = . if state1_1993 == 99
replace childcare1_1993 = 0 if childcare1_1993 == 99999
replace pension_inc6_1993 = . if pension_inc6_1993 == 99999
replace pension_inc8_1993 = . if pension_inc8_1993 == 99999
replace pension_inc6_TU_1993 = . if pension_inc6_TU_1993 == 9
replace pension_inc6_TU_1993 = . if pension_inc6_TU_1993 == 7
replace pension_inc8_TU_1993 = . if pension_inc8_TU_1993 == 9
replace pension_inc8_TU_1993 = . if pension_inc8_TU_1993 == 7
replace unemp_inc4_1993 = 0 if unemp_inc4_1993 == 99999
replace unemp_inc4_TU_1993 = . if unemp_inc4_TU_1993 == 9
replace unemp_inc4_TU_1993 = . if unemp_inc4_TU_1993 == 7
replace unemp_inc6_1993 = 0 if unemp_inc6_1993 == 99999
replace unemp_inc6_TU_1993 = . if unemp_inc6_TU_1993 == 9
replace unemp_inc6_TU_1993 = . if unemp_inc6_TU_1993 == 7
replace rent2_TU_1993 = . if inlist(rent2_TU_1993, 1, 2, 3, 4, 6, 7, 9)

*** Drop Variables Not Used by TAXSIM
capture drop V*
capture drop ER*

****************************************************************************************************
* Section 3: Annualize Data
****************************************************************************************************
* Note: No Data Annualization Required Before 1983

*** Replace Time Unit Indicators with Multiplier Values
replace pension_inc6_TU_1993 = . if pension_inc6_TU == 1
replace pension_inc6_TU_1993 = . if pension_inc6_TU == 2
replace pension_inc6_TU_1993 = . if pension_inc6_TU == 3
replace pension_inc6_TU_1993 = 26 if pension_inc6_TU == 4
replace pension_inc6_TU_1993 = 12 if pension_inc6_TU == 5
replace pension_inc6_TU_1993 = 1 if pension_inc6_TU == 6
replace pension_inc8_TU_1993 = . if pension_inc8_TU == 1
replace pension_inc8_TU_1993 = . if pension_inc8_TU == 2
replace pension_inc8_TU_1993 = . if pension_inc8_TU == 3
replace pension_inc8_TU_1993 = 26 if pension_inc8_TU == 4
replace pension_inc8_TU_1993 = 12 if pension_inc8_TU == 5
replace pension_inc8_TU_1993 = 1 if pension_inc8_TU == 6
replace rent2_TU_1993 = 12 if rent2_TU == 5
replace unemp_inc4_TU_1993 = . if unemp_inc4_TU == 1
replace unemp_inc4_TU_1993 = . if unemp_inc4_TU == 2
replace unemp_inc4_TU_1993 = . if unemp_inc4_TU == 3
replace unemp_inc4_TU_1993 = 26 if unemp_inc4_TU == 4
replace unemp_inc4_TU_1993 = 12 if unemp_inc4_TU == 5
replace unemp_inc4_TU_1993 = 1 if unemp_inc4_TU == 6
replace unemp_inc6_TU_1993 = . if unemp_inc6_TU == 1
replace unemp_inc6_TU_1993 = . if unemp_inc6_TU == 2
replace unemp_inc6_TU_1993 = . if unemp_inc6_TU == 3
replace unemp_inc6_TU_1993 = 26 if unemp_inc6_TU == 4
replace unemp_inc6_TU_1993 = 12 if unemp_inc6_TU == 5
replace unemp_inc6_TU_1993 = 1 if unemp_inc6_TU == 6

*** Annualize Values
* Variable childcare1_TU not included in data set
replace pension_inc6_1993 = pension_inc6_1993
replace pension_inc8_1993 = pension_inc8_1993
replace rent2_1993 = rent2_1993
replace unemp_inc4_1993 = unemp_inc4_1993
replace unemp_inc6_1993 = unemp_inc6_1993

****************************************************************************************************
* Section 4: Calculate TAXSIM Inputs
****************************************************************************************************

*** Generate a TAXSIM Identifier
gen taxsimid = _n
label var taxsimid "TAXSIM Identifier"

*** Set Tax Year
* Year of observation used for data cleaning, prior year used for tax calculation
local obs_year = 1993
gen year = 1992

*** Set a State Identifier
gen state = .
* Recode state for TAXSIM
replace state = 1 if state1_1993 == 1   // AL
replace state = 2 if state1_1993 == 50   // AK
replace state = 3 if state1_1993 == 2   // AZ
replace state = 4 if state1_1993 == 3   // AR
replace state = 5 if state1_1993 == 4   // CA
replace state = 6 if state1_1993 == 5   // CO
replace state = 7 if state1_1993 == 6   // CT
replace state = 8 if state1_1993 == 7   // DE
replace state = 9 if state1_1993 == 8   // DC
replace state = 10 if state1_1993 == 9   // FL
replace state = 11 if state1_1993 == 10   // GA
replace state = 12 if state1_1993 == 51   // HI
replace state = 13 if state1_1993 == 11   // ID
replace state = 14 if state1_1993 == 12   // IL
replace state = 15 if state1_1993 == 13   // IN
replace state = 16 if state1_1993 == 14   // IA
replace state = 17 if state1_1993 == 15   // KS
replace state = 18 if state1_1993 == 16   // KY
replace state = 19 if state1_1993 == 17   // LA
replace state = 20 if state1_1993 == 18   // ME
replace state = 21 if state1_1993 == 19   // MD
replace state = 22 if state1_1993 == 20   // MA
replace state = 23 if state1_1993 == 21   // MI
replace state = 24 if state1_1993 == 22   // MN
replace state = 25 if state1_1993 == 23   // MS
replace state = 26 if state1_1993 == 24   // MO
replace state = 27 if state1_1993 == 25   // MT
replace state = 28 if state1_1993 == 26   // NE
replace state = 29 if state1_1993 == 27   // NV
replace state = 30 if state1_1993 == 28   // NH
replace state = 31 if state1_1993 == 29   // NJ
replace state = 32 if state1_1993 == 30   // NM
replace state = 33 if state1_1993 == 31   // NY
replace state = 34 if state1_1993 == 32   // NC
replace state = 35 if state1_1993 == 33   // ND
replace state = 36 if state1_1993 == 34   // OH
replace state = 37 if state1_1993 == 35   // OK
replace state = 38 if state1_1993 == 36   // OR
replace state = 39 if state1_1993 == 37   // PA
replace state = 40 if state1_1993 == 38   // RI
replace state = 41 if state1_1993 == 39   // SC
replace state = 42 if state1_1993 == 40   // SD
replace state = 43 if state1_1993 == 41   // TN
replace state = 44 if state1_1993 == 42   // TX
replace state = 45 if state1_1993 == 43   // UT
replace state = 46 if state1_1993 == 44   // VT
replace state = 47 if state1_1993 == 45   // VA
replace state = 48 if state1_1993 == 46   // WA
replace state = 49 if state1_1993 == 47   // WV
replace state = 50 if state1_1993 == 48   // WI
replace state = 51 if state1_1993 == 49   // WY

*** Remove State Tax Calc if Set for Removal
if `CALC_STATE_TAX' == 0 {
     replace state = 0
}
* Remove state tax calculation for years before 1977
replace state = 0 if `obs_year' < 1977
if `obs_year' < 1977 {
     di as error "no state taxes prior to 1977"
}

*** Set the Head's Marital Status
* Uses reported marital status if available (1977 data +)
* Uses cohabitation status if reported marital status is not available (1968-1976 data)
gen mstat_temp = mstat1_1993
* Recode marital status for TAXSIM 
gen mstat = .
replace mstat = 2 if mstat_temp == 1
replace mstat = 1 if mstat_temp == 2
replace mstat = 1 if mstat_temp == 3
replace mstat = 1 if mstat_temp == 4
replace mstat = 2 if mstat_temp == 5
replace mstat = 2 if mstat_temp == 8
drop mstat_temp

*** Set Number of Dependent Children
gen depchild = num_dep2_1993

*** Set the Number of Dependent Exemptions
* If available, uses total # exemptions - (head + wife) exemptions (1970-1989)
* Otherwise uses number of children under 18 + dependents outside of the home (if available)
gen depx = 0
replace depx = num_dep2_1993
* Replace if the number of individuals reported elsewhere is greater
replace depx = depchild + mstat if depx < depchild + mstat
replace depx = 0 if depx < 0 // Does not permit negative exemption numbers
replace depx = depx + num_dep3_1993

*** Set the Age Indicator for the Filing Individual / Couple
* Follows rules specified at http://users.nber.org/~taxsim/taxsim9/
gen agex = 0
replace agex = agex + 1 if age1_1993 > 65 & age1_1993 ~= .
replace agex = agex + 1 if age2_1993 > 65 & age2_1993 ~= .

*** Set the Head's Wage and Salary Income
gen pwages = wages1_1993

*** Set the Spouse's Wage and Salary Income
* Uses total labor income if available (1993 and earlier)
* Sums non-business and business labor income variables across categories (1993+)
gen swages = wages2_1993
* Business labor income not added

*** Set Pension Income
* Pension income for 1968-1969 surveys not calculated because only bracketed data available
gen pensions = 0
replace pensions = pension_inc8_1993
* No additional head pension data required
* Calculate spouse's pension income (1985 + )
gen spouse_pensions = 0
replace spouse_pensions = spouse_pensions + pension_inc4_1993
* No additional spouse pension data required
* Sum pensions across the head and spouse
replace pensions = pensions + spouse_pensions
drop spouse_pensions

*** Set Other Property Income
gen otherprop = 0
replace otherprop = tax_inc1_1993 - pensions - pwages - swages if tax_inc1_1993 > pensions + pwages + swages

*** Set Social Security Benefits
* Uses combined head and wife social security income if available (1970-1985)
* Else sums head and wife social security income if available (1986-1993, 2005+)
* Otherwise uses combined family income
gen gssi = 0
replace gssi = gssi2_1993
replace gssi = gssi + gssi3_1993

*** Set Unemployment Income
* No unemployment income available prior to the 1970 survey
* Uses only head's unemployment income 1970-1984 (only head's available)
* Sums head and spouse unemployment income 1984+
gen ui = 0
replace ui = unemp_inc2_1993
replace ui = ui + unemp_inc3_1993

*** Set Government Transfer Income
* Measured (when available) as total transfer income less unemployment income
gen transfers = 0
replace transfers = transfers1_1993 - ui

*** Set Rent Paid
gen rentpaid = 0
replace rentpaid = rent1_1993

*** Set Estimated Property Tax Payments
gen proptax = 0
replace proptax = prop_tax1_1993
if `INCLUDE_PROP_TAX' == 0 {
     replace proptax = 0
}

*** Set Childcare Expenses
gen childcare = 0
replace childcare = childcare1_1993

*** Additional notes
* Mortgage interest, short-term capital gains, and long-term capital gains not calculated

*** Run TAXSIM
taxsim9, replace full
replace year = year + 1
