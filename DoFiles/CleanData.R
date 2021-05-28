# CleanData

years <- c('1999','2001','2003','2005', '2007', '2009', '2011','2013')
shortYears <-  c('99','01','03','05', '07', '09', '11','13')

#Load the PSID data necessary to compute active savings, combine with the wealth supplement, rename variables.

# load the individual cross-year file
load( "Data/updated/IND2015ER.rda" )

# limit the file to only the variables needed
# need years 2001, 2003, 2005, 2007, 2009, 2011
x.KeepVars <-
  c('ER30001',    # 1968 interview number
    'ER30002',    # 1968 person number
    'ER31997',    # primary sampling unit variable 
    'ER31996',    # stratification variable
    'ER33501',    # interview number, 1999
    'ER33601',    # interview number, 2001
    'ER33701',    # interview number, 2003
    'ER33801',    # interview number, 2005
    'ER33901',    # interview number, 2007
    'ER34001',    # interview number, 2009
    'ER34101',    # interview number, 2011
    'ER34201',    # interview number, 2013
    'ER33502',    # household head, 1999
    'ER33602',    # household head, 2001 
    'ER33702',    # household head, 2003 
    'ER33802',    # household head, 2005
    'ER33902',    # household head, 2007
    'ER34002',    # household head, 2009
    'ER34102',    # household head, 2011
    'ER34202',    # household head, 2013
    'ER33503',    # household relationship status, 1999
    'ER33603',    # household relationship status, 2001
    'ER33703',    # household relationship status, 2003
    'ER33803',  	# household relationship status, 2005
    'ER33903',  	# household relationship status, 2007
    'ER34003',  	# household relationship status, 2009
    'ER34103',  	# household relationship status, 2011
    'ER34203'     # household relationship status, 2013
  )

# create a "skinny"	data.frame object that only contains the
# columns you need for this analysis,
# specified in the KeepVars charactER vector
w <- x[ , x.KeepVars ]

# remove the original data.frame object then free up memory
rm( x ) ; gc()



# 1999  - NOTE * denotes since 1994 (not in the last 2 years, as with other years)

f99.KeepVars <-
  c('ER13002',   # family interview number
    'ER15077',   # wtr buy/sell more stocks   
    'ER15078',   # balance in/out stocks
    'ER15036',   # added to private annuities *  
    'ER15040',   # cashed from private annuities *
    'ER15052',   # added to other real estate *
    'ER15057',   # removed from other real estate *
    'ER15066',   # invested farm/business *
    'ER15071',   # removed from farm/business *
    'ER13077',   # moved?
    'ER13047',   # mortgage debt
    'ER16515A5',  # housing	
    'ER16515A1',  # food
    'ER16515B6',  # transportation
    'ER16515C9',  # education
    'ER16515D1',  # childcare
    'ER16515D2',   # healthcare
    'ER13210',     # self employed?
    'ER16462'    #income
  )
    

w99.KeepVars <-
  c('S401',  # family interview number
    'S405',  # checking/saving   
    'S415',  # bond value
    'S413',  # vehicle value
    'S407',  # other debt
    'S420'   # home equity
  )

# 2001

f01.KeepVars <-
  c('ER17002',   # family interview number
    'ER20394',   # longitudinal weight
    'ER19279',   # added to stocks
    'ER19285',   # removed from stocks
    'ER19232',   # added to private annuities   
    'ER19237',   # cashed from private annuities 
    'ER19248',   # added to other real estate 
    'ER19253',   # removed from other real estate 
    'ER19262',   # invested farm/business 
    'ER19267',   # removed from farm/business 
    'ER17088',   # moved?
    'ER19203',   # net value of stocks
    'ER17052',   # mortgage debt
    'ER17043',   # own/rent?
    'ER19202',   # owns stocks?
    'ER20457',   # education 
    'ER19989',   # race
    'ER19172',   # kids outide family unit
    'ER17004',   # state 
    'ER20453',   # extra family income 
    'ER20456',   # income 
    'ER17016',   # kids
    'ER17012',   # family size
    'ER17216',   # employed?
    'ER17013',    # age of head
    'ER20456A1',   # food 
    'ER20456A5',   # housing 
    'ER20456D1',    # childcare
    'ER19179',      # dependents outside FU 
    'ER20456C9',     # education expenditure
    'ER20456D2',      # healthcare expenditure
    'ER20456B6',     # transport expenditure 
    'ER17221',        # self employed? 
    'ER17024',     # marital status 
    'ER17015',     # spouse age
    'ER19173'     # no outside FU
  )
w01.KeepVars <-
  c(
    'ER17002', # family intERview numbER
    'S505',    # value of checking/saving
    'S515',    # value of bonds 
    'S507',    # value of other debt
    'S513',    # value of vehicles 
    'S517',    # total wealth
    'S520'    # home equity
  )

# 2003

f03.KeepVars <-
  c(
    'ER21002',   # family intERview numbER
    'ER24179',   # longitudinal weight
    'ER22568',   # net value of stocks
    'ER22596',   # value of checking/saving 
    'ER21051',   # mortgage debt
    'ER21042',   # own/rent?
    'ER22567',   # owns stocks?
    'ER22674',   # added to stocks 
    'ER22680',   # removed from stocks    
    'ER22627',   # added to private annuities   
    'ER22632',   # cashed from private annuities 
    'ER22643',   # added to other real estate 
    'ER22648',   # removed from other real estate  
    'ER22657',   # invested farm/business  
    'ER22662',   # removed from farm/business  
    'ER21117',   # moved?
    'ER22668',   # bought more/sold more stocks? 
    'ER24148',   # education 
    'ER23426',   # race
    'ER22537',   # kids outide family unit
    'ER21003',   # state 
    'ER24102',   # extra family income 
    'ER24099',   # income 
    'ER21020',   # kids
    'ER21016',   # family size
    'ER21123',   # employed?
    'ER21017',    # age 
    'ER24138A1',   # food 
    'ER24138A5',   # housing 
    'ER24138D1',    # childcare 
    'ER22544',     # dependents outside FU
    'ER24138C9',    # education expenditure 
    'ER24138D2',     # healthcare expenditure 
    'ER24138B6',     # transport expenditure 
    'ER21147',       # self-employed? 
    'ER21023',    # marital status 
    'ER21019',    # spouse age
    'ER22538'     # no outside FU 
  )

w03.KeepVars <-
  c(
    'ER21002', # family intERview numbER
    'S615',    # value of bonds 
    'S607',    # value of other debt
    'S613',    # value of vehicles 
    'S620',    # home equity
    'S617',    # total wealth
    'S609',     # oth real estate
    'S619'     # private annuity
  )

# 2005 
f05.KeepVars <-
  c(
    'ER25002',   # family intERview numbER
    'ER28078',   # longitudinal weight
    'ER26549',   # net value of stocks
    'ER26655',   # added to stocks
    'ER26661',   # removed from stocks    
    'ER26608',   # added to private annuities
    'ER26613',   # cashed from private annuities
    'ER26624',   # added to other real estate
    'ER26629',   # removed from other real estate 
    'ER26638',   # invested farm/business  
    'ER26643',   # removed from farm/business 
    'ER26577',   # value of checking/saving
    'ER25042',   # mortgage debt 
    'ER25098',   # moved? 
    'ER25028',   # own/rent?
    'ER26548',   # owns stocks?
    'ER26649',   # bought more/sold more stocks?
    'ER28047',   # education 
    'ER27393',   # race
    'ER26518',   # kids outide family unit
    'ER25003',   # state
    'ER28009',   # extra family income
    'ER28037',   # income 
    'ER25020',   # kids 
    'ER25016',   # family size
    'ER25104',   # employed 
    'ER25017',    # age of head 
    'ER28037A1',   # food
    'ER28037A5',    # housing 
    'ER28037D2',     # childcare 
    'ER26525',       # dependents outside FU  
    'ER28037D1',      # education expenditure 
    'ER28037D3',      # healthcare expenditure 
    'ER28037B7',       # transport expenditure 
    'ER25823',        # vacations 
    'ER25828',       # recreation 
    'ER28037E1',      # clothing	
    'ER25129',        # self-employed? 
    'ER25023',   # marital status
    'ER25019',    # spouse age 
    'ER26519'    # no outside FU 
  )

w05.KeepVars <-
  c(
    'ER25002', # family intERview numbER
    'S715',    # value of bonds  
    'S707',    # value of other debt 
    'S713',    # value of vehicles 
    'S720',    # home equity
    'S717',    # total wealth
    'S709',     # oth real estate
    'S719'     # private annuity 
  )

# 2007 - to year
f07.KeepVars <-
  c(
    'ER36002',   # family intERview numbER
    'ER41069',   # longitudinal weight
    'ER37567',   # net value of stocks  
    'ER37673',   # added to stocks
    'ER37679',   # removed from stocks
    'ER37626',   # added to private annuities  
    'ER37631',   #  cashed from private annuities 
    'ER37642',   # added to other real estate 
    'ER37647',   # removed from other real estate
    'ER37656',   # invested farm/business 
    'ER37661',   # removed from farm/business 
    'ER37595',   # value of checking/saving 
    'ER36042',   # mortgage debt
    'ER36103',   # moved? 
    'ER36028',   # own/rent?
    'ER37566',   # own stocks? 
    'ER37667',   # bought more/sold more stocks?
    'ER41037',   # education 
    'ER40565',   # race
    'ER37536',   # kids outide family unit  
    'ER36003',   # state 
    'ER40999',   # extra family income
    'ER41027',   # income
    'ER36020',   # kids
    'ER36016',   # family size
    'ER36109',   # employed? 
    'ER36017',    # age of head 
    'ER41027A1',   # food 
    'ER41027A5',   # housing
    'ER41027D2',    # childcare 
    'ER37543',       # dependents outside FU 
    'ER41027D1',      # education expenditure
    'ER41027D3',      # healthcare expenditure 
    'ER41027B7',      # transport expenditure 
    'ER36841',        # vacations and trips 
    'ER36846',        # recreation 
    'ER41027E1',     # clothing
    'ER36134',       # self-employed? 
    'ER36023',      # marital status
    'ER36019',      # spouse age 
    'ER37537'     # no outside FU 

    )

w07.KeepVars <-
  c(
    'ER36002', # family intERview numbER
    'S815',    # value of bonds 
    'S807',    # value of other debt
    'S813',    # value of vehicles 
    'S820',    # home equity 
    'S817',    # total wealth
    'S809',     # oth real estate
    'S819'     # private annuity 
  )


# 2009 - from year
f09.KeepVars <-
  c(
    'ER42002',   # family intERview numbER 
    'ER47012',   # longitudinal weight
    'ER43558',   # net value of stocks  
    'ER43664',   # added to stocks
    'ER43670',   # removed from stocks
    'ER43617',   # added to private annuities   
    'ER43622',   # cashed from private annuities 
    'ER43633',   # added to other real estate 
    'ER43638',   # removed from other real estate  
    'ER43647',   # invested farm/business  
    'ER43652',   # removed from farm/business  
    'ER43586',   # value of checking/saving 
    'ER46960',   # value of bonds 
    'ER46946',   # value of other debt 
    'ER46956',   # value of vehicles 
    'ER42043',   # mortgage debt 
    'ER42132',   # moved?
    'ER46966',   # home equity 
    'ER42029',   # own/rent 2009
    'ER46970',   # total wealth
    'ER43557',   # owns stocks?
    'ER43658',   # bought more/sold more stocks? 
    'ER46981',   # education 
    'ER46543',   # race 
    'ER43527',   # kids outide family unit 
    'ER42003',   # state
    'ER46907',   # extra family income
    'ER46935',   # income 
    'ER42020',   # kids 
    'ER42016',   # family size
    'ER42140',   # employed?
    'ER42017',    # age of head
    'ER46971A1',   # food 
    'ER46971A5',   # housing 
    'ER46971D2',   # childcare 
    'ER43534',      # dependents outside FU 
    'ER46971D1',    # education expenditure 
    'ER46971D3',     # healthcare expenditure 
    'ER46971B7',     # transport expenditure 
    'ER42832',        # vacations 
    'ER42837',        # recreation 
    'ER46971E1',   # clothing
    'ER42169',      # self-employed?
    'ER42023',    # marital status
    'ER42019',    # spouse age 
    'ER43528',     # no outside FU 
    'ER46950',     # oth real estate 
    'ER46964'     # private annuity
  )

# 2011 - to year
f11.KeepVars <-
  c(
    'ER47302',   # family intERview numbER
    'ER52436',   # longitudinal weight
    'ER48883',   # net value of stocks   
    'ER49009',   # added to stocks
    'ER49015',   # removed from stocks
    'ER48905',   # added to private annuities
    'ER48967',   # removed from private annuities
    'ER48978',   # added to other real estate
    'ER48983',   # removed from other real estate 
    'ER48992',   # invested farm/business  
    'ER48997',   # removed from farm/business  
    'ER48911',   # value of checking/saving 
    'ER52364',   # value of bonds 
    'ER52372',   # other debt 1	(credit card)
    'ER52376',   #  other debt 2 (student loan) 
    'ER52380',   #  other debt 3 (medical debt)
    'ER52384',   #  other debt 4	(legal debt)
    'ER52388',   #  other debt 5	(family loan debt)
    'ER52360',   # value of vehicles 
    'ER47348',   # mortgage debt 
    'ER47440',   # moved?
    'ER52390',   # home equity
    'ER47329',   # own/rent? 
    'ER52394',   # total wealth
    'ER48882',   # owns stocks?
    'ER49003',   # bought more/sold more stocks? 
    'ER52405',   # education
    'ER51904',   # race
    'ER48852',   # kids outide family unit 
    'ER47303',   # state
    'ER52315',   # extra family income 
    'ER52343',   # income
    'ER47320',   # kids
    'ER47316',   # family size
    'ER47448',    # employed?
    'ER47317',    # age of head
    'ER52395A1',   # food 
    'ER52395A5',   # housing
    'ER52395D2',    # childcare 
    'ER48859',       # dependents outside FU 
    'ER52395D1',     # education expenditure 
    'ER52395D3',     # healthcare expenditure
    'ER52395B7',     # transport expenditure
    'ER48154',       # vacations 
    'ER48159',        # recreation
    'ER52395E1',    # clothing
    'ER47482',       # self-employed? 
    'ER47323',      # marital status 
    'ER47319',     # spouse age 
    'ER48853',     # no outside FU
    'ER52354',     # oth real estate 
    'ER52368'     # private annuity 
    )
# 2013 - to year
f13.KeepVars <-
  c(
    'ER53002',   # family intERview numbER
    'ER58257',   # longitudinal weight
    'ER54634',   # net value of stocks   
    'ER54764',   # added to stocks
    'ER54770',   # removed from stocks
    'ER54655',   # added to private annuities
    'ER54729',   # removed from private annuities
    'ER54740',   # added to other real estate
    'ER54745',   # removed from other real estate 
    'ER54754',   # invested farm/business  
    'ER54759',   # removed from farm/business  
    'ER54661',   # value of checking/saving 
    'ER58177',   # value of bonds 
    'ER58185',   # other debt 1	(credit card)
    'ER58189',   #  other debt 2 (student loan) 
    'ER58193',   #  other debt 3 (medical debt)
    'ER58197',   #  other debt 4	(legal debt)
    'ER58201',   #  other debt 5	(family loan debt)
    'ER58173',   # value of vehicles 
    'ER53048',   # mortgage debt 
    'ER53140',   # moved?
    'ER58207',   # home equity
    'ER53029',   # own/rent? 
    'ER58211',   # total wealth
    'ER54633',   # owns stocks?
    'ER58223',   # education
    'ER57659',   # race
    'ER54595',   # kids outide family unit 
    'ER53003',   # state
    'ER58124',   # extra family income 
    'ER58152',   # income
    'ER53020',   # kids
    'ER53016',   # family size
    'ER53148',    # employed?
    'ER53017',    # age of head
    'ER58212A1',   # food 
    'ER58212A5',   # housing
    'ER58212D2',    # childcare 
    'ER54602',       # dependents outside FU 
    'ER58212D1',     # education expenditure 
    'ER58212D3',     # healthcare expenditure
    'ER58212B7',     # transport expenditure
    'ER53848',       # vacations 
    'ER53853',        # recreation
    'ER58212E1',   # clothing
    'ER53182',     # self-employed? 
    'ER53023',   # marital status
    'ER53019',    # spouse age 
    'ER54596',    # no outside FU 
    'ER54612',    # oth real estate
    'ER58181'    # private annuity
    )

# extract data 
for(i in years)
{
  #from family level files
  load(paste("Data/updated/FAM",i,"ER.rda",sep=""))
  assign(paste("f",substr(i, 3, 4), sep=""), x[ , eval(as.name(paste("f",substr(i, 3, 4),".KeepVars",sep=""))) ])
  #eval(call("rm",paste("fam",i,sep="")))
  if(eval(i)<2009) {
    #from extra wealth supplement files (not included in main release from '84-'07)
    assign(paste("wealth",i, sep=""), read.csv(paste("Data/",i,"WealthSupplement.csv", sep="")))
    assign(paste("w",substr(i,3,4),sep=""), eval(as.name(paste("wealth",i,sep="")))[ , eval(as.name(paste("w",substr(i, 3, 4),".KeepVars",sep=""))) ])
    eval(call("rm",paste("wealth",i,sep=""))); gc()  
  } 
}
rm(i)
rm(f99.KeepVars,
   f01.KeepVars,
   f03.KeepVars,
   f05.KeepVars, f07.KeepVars,
   f09.KeepVars, f11.KeepVars, f13.KeepVars,
   w99.KeepVars,
   w01.KeepVars,
   w03.KeepVars,
   w05.KeepVars,
   w07.KeepVars, x.KeepVars, x) ; gc()

# merge the family and wealth supplement files
# using the interview number field available in both tables 

f99 <- merge(f99, w99, by.x = 'ER13002', by.y = 'S401')
f01 <- merge( f01 , w01 , by.x = 'ER17002', by.y = 'ER17002')
f03 <- merge( f03 , w03 , by.x = 'ER21002', by.y = 'ER21002')
f05 <- merge( f05 , w05 , by.x = 'ER25002', by.y = 'ER25002')
f07 <- merge( f07 , w07 , by.x = 'ER36002', by.y = 'ER36002')

# get rid of supplemental data, we don't need them anymore
rm(w99,w01,w03, w05, w07) ; gc()

f99 <- plyr::rename(f99, c('ER13002' = 'intNum99',
                           'ER15077' = 'buySellStocks99', 
                           'ER15078' = 'balanceStocks99',   
                           'ER15036' = 'addedPrivateAnnuity99', 
                           'ER15040' = 'removedPrivateAnnuity99',
                           'ER15052' = 'addedOthRealEstate99',
                           'ER15057' = 'removedOthRealEstate099',
                           'ER15066' = 'investedFarmBusiness99',
                           'ER15071' = 'removedFarmBusiness99',
                           'ER13077' = 'wtrMoved99',
                           'ER13047' = 'mortgageDebt99',
                           'ER16515A5' = 'housing99',
                           'ER16515A1' = 'food99',
                           'ER16515B6' = 'transportExp99',
                           'ER16515C9' = 'educExp99',
                           'ER16515D1' = 'childcare99',
                           'ER16515D2' = 'healthExp99',
                           'ER13210' = 'selfEmp99',
                           'ER16462' = 'income99',
                           'S405' = 'checkingValue99',
                           'S415' = 'bondValue99',
                           'S413' = 'vehicleValue99',
                           'S407' = 'othDebtValue99',
                           'S420' = 'homeEquity99'))

f01 <- plyr::rename(f01, c('ER17002' = 'intNum01',
                           'ER20394' = 'longWeight01',
                           'ER19203' = 'stockValue01',
                           'S505' = 'checkingValue01',
                           'ER19279' = 'addedStocks01',  
                           'ER19285' = 'removedStocks01', 
                           'ER19232' = 'addedPrivateAnnuity01',   
                           'ER19237' = 'removedPrivateAnnuity01',  
                           'ER19248' = 'addedOthRealEstate01',  
                           'ER19253' = 'removedOthRealEstate01', 
                           'ER19262' = 'addedFarmBusiness01',  
                           'ER19267' = 'removedFarmBusiness01', 
                           'ER17088' = 'wtrMoved01',
                           'ER17052' = 'mortgageDebt01',
                           'ER17043' = 'ownRent01',
                           'S515' = 'bondValue01',
                           'S507' = 'othDebtValue01',
                           'S513' = 'vehicleValue01', 
                           'S520' = 'homeEquity01',
                           'ER20457' = 'educ01',
                           'ER19989' = 'race01',
                           'ER17004' = 'state01',
                           'ER20453' = 'exIncome01', 
                           'ER20456' = 'income01',
                           'ER17016' = 'kids01',
                           'ER17012' = 'famSize01',
                           'ER17216' = 'employed01',
                           'ER19172' = 'kidsOut01',
                           'S517' = 'totalWealth01',
                           'ER17013' = 'age01',
                           'ER20456A1' = 'food01',
                           'ER20456A5' = 'housing01',
                           'ER20456D1' = 'childcare01',
                           'ER19179' = 'othDepend01',
                           'ER20456C9' = 'educExp01',
                           'ER20456D2' = 'healthExp01',
                           'ER20456B6' = 'transportExp01',
                           'ER17221' = 'selfEmp01',
                           'ER17024' = 'marSta01',
                           'ER17015' = 'ageSpouse01',
                           'ER19173' = 'numDepend01' 
))

f03 <- plyr::rename(f03, c('ER21002' = 'intNum03',
                           'ER24179' = 'longWeight03',
                           'ER22568' = 'stockValue03',  
                           'ER22674' = 'addedStocks03',
                           'ER22680' = 'removedStocks03',
                           'ER22627' = 'addedPrivateAnnuity03',   
                           'ER22632' = 'removedPrivateAnnuity03',  
                           'ER22643' = 'addedOthRealEstate03',  
                           'ER22648' = 'removedOthRealEstate03',   
                           'ER22657' = 'addedFarmBusiness03',     
                           'ER22662' = 'removedFarmBusiness03',  
                           'ER22596' = 'checkingValue03',
                           'ER21051' = 'mortgageDebt03', 
                           'ER21117' = 'wtrMoved03', 
                           'ER21042' = 'ownRent03',  
                           'S615' = 'bondValue03', 
                           'S607' = 'othDebtValue03',
                           'S613' = 'vehicleValue03',
                           'S620' = 'homeEquity03',
                           'S617' = 'totalWealth03',
                           'ER24148' = 'educ03',
                           'ER23426' = 'race03',
                           'ER21003' = 'state03',
                           'ER24102' = 'exIncome03',
                           'ER24099' = 'income03',
                           'ER21020' = 'kids03',
                           'ER21016' = 'famSize03',
                           'ER21123' = 'employed03',
                           'ER22537' = 'kidsOut03',
                           'ER21017' = 'age03',
                           'ER24138A1' = 'food03',
                           'ER24138A5' = 'housing03',
                           'ER24138D1' = 'childcare03',
                           'ER22544' = 'othDepend03',
                           'ER24138C9' = 'educExp03',
                           'ER24138D2' = 'healthExp03',
                           'ER24138B6' = 'transportExp03',
                           'ER22668' = 'boughtSoldStocks03',
                           'ER21147' = 'selfEmp03',
                           'ER21023' = 'marSta03',
                           'ER21019' = 'ageSpouse03',
                           'ER22538' = 'numDepend03',
                           'S609' = 'othRealEstate03',
                           'S619' = 'privateAnnuity03' 
))


f05 <- plyr::rename(f05, c('ER25002' = 'intNum05',
                     'ER28078' = 'longWeight05',
                     'ER26549' = 'stockValue05',  
                     'ER26655' = 'addedStocks05',
                     'ER26661' = 'removedStocks05',
                     'ER26608' = 'addedPrivateAnnuity05',   
                     'ER26613' = 'removedPrivateAnnuity05',  
                     'ER26624' = 'addedOthRealEstate05',  
                     'ER26629' = 'removedOthRealEstate05',   
                     'ER26638' = 'addedFarmBusiness05',     
                     'ER26643' = 'removedFarmBusiness05',  
                     'ER26577' = 'checkingValue05',
                     'ER25042' = 'mortgageDebt05', 
                     'ER25098' = 'wtrMoved05', 
                     'ER25028' = 'ownRent05',  
                     'S715' = 'bondValue05', 
                     'S707' = 'othDebtValue05',
                     'S713' = 'vehicleValue05',
                     'S720' = 'homeEquity05',
                     'S717' = 'totalWealth05',
                     'ER28047' = 'educ05',
                     'ER27393' = 'race05',
                     'ER25003' = 'state05',
                     'ER28009' = 'exIncome05',
                     'ER28037' = 'income05',
                     'ER25020' = 'kids05',
                     'ER25016' = 'famSize05',
                     'ER25104' = 'employed05',
                     'ER26518' = 'kidsOut05',
                     'ER25017' = 'age05',
                     'ER28037A1' = 'food05',
                     'ER28037A5' = 'housing05',
                     'ER28037D2' = 'childcare05',
                     'ER26525' = 'othDepend05',
                     'ER28037D1' = 'educExp05',
                     'ER28037D3' = 'healthExp05',
                     'ER28037B7' = 'transportExp05',
                     'ER25823' = 'vacations05',
                     'ER25828' = 'recreation05',
                     'ER26649' = 'boughtSoldStocks05',
                     'ER28037E1' = 'clothing05',
                     'ER25129' = 'selfEmp05',
                     'ER25023' = 'marSta05',
                     'ER25019' = 'ageSpouse05',
                     'ER26519' = 'numDepend05',
                     'S709' = 'othRealEstate05',
                     'S719' = 'privateAnnuity05' 
))

f07 <- plyr::rename(f07, c('ER36002' = 'intNum07',
                     'ER41069' = 'longWeight07',
                     'ER37567' = 'stockValue07',    
                     'ER37673' = 'addedStocks07',
                     'ER37679' = 'removedStocks07',
                     'ER37626' = 'addedPrivateAnnuity07',
                     'ER37631' = 'removedPrivateAnnuity07',
                     'ER37642' = 'addedOthRealEstate07', 
                     'ER37647' = 'removedOthRealEstate07',
                     'ER37656' = 'addedFarmBusiness07', 
                     'ER37661' =  'removedFarmBusiness07',
                     'ER37595' = 'checkingValue07', 
                     'ER36042' = 'mortgageDebt07',  
                     'ER36103' = 'wtrMoved07',
                     'ER36028' = 'ownRent07',
                     'S815' = 'bondValue07',
                     'S807' = 'othDebtValue07',
                     'S813' = 'vehicleValue07',
                     'S820' = 'homeEquity07', 
                     'S817' = 'totalWealth07',
                     'ER41037' = 'educ07',
                     'ER40565' = 'race07',
                     'ER36003' = 'state07',
                     'ER40999' = 'exIncome07',
                     'ER41027' = 'income07',
                     'ER36020' = 'kids07',
                     'ER36016' = 'famSize07',
                     'ER36109' = 'employed07',
                     'ER37536' = 'kidsOut07',
                     'ER36017' = 'age07',
                     'ER41027A1' = 'food07',
                     'ER41027A5' = 'housing07',
                     'ER41027D2' = 'childcare07',
                     'ER37543' = 'othDepend07',
                     'ER41027D1' = 'educExp07',
                     'ER41027D3' = 'healthExp07',
                     'ER41027B7' = 'transportExp07',
                     'ER36841' = 'vacations07',
                     'ER36846' = 'recreation07',
                     'ER37667' = 'boughtSoldStocks07',
                     'ER36134' = 'selfEmp07',
                     'ER41027E1' = 'clothing07',
                     'ER36023' = 'marSta07',
                     'ER36019' = 'ageSpouse07',
                     'ER37537' = 'numDepend07',
                     'S809' = 'othRealEstate07',
                     'S819' = 'privateAnnuity07' 
))
f09 <- plyr::rename(f09, c(    'ER42002' = 'intNum09',
                         'ER47012' = 'longWeight09',
                         'ER43558' = 'stockValue09',  
                         'ER43664' = 'addedStocks09',
                         'ER43670' = 'removedStocks09',
                         'ER43617' = 'addedPrivateAnnuity09',  
                         'ER43622' = 'removedPrivateAnnuity09', 
                         'ER43633' = 'addedOthRealEstate09',
                         'ER43638' = 'removedOthRealEstate09', 
                         'ER43647' = 'addedFarmBusiness09', 
                         'ER43652' = 'removedFarmBusiness09', 
                         'ER43586' = 'checkingValue09',
                         'ER46960' = 'bondValue09',
                         'ER46946' = 'othDebtValue09',
                         'ER46956' = 'vehicleValue09',
                         'ER42043' = 'mortgageDebt09', 
                         'ER42132' = 'wtrMoved09',
                         'ER46966' = 'homeEquity09',
                         'ER42029' = 'ownRent09',
                         'ER46970' = 'totalWealth09',
                         'ER46981' = 'educ09',
                         'ER46543' = 'race09',
                         'ER42003' = 'state09',
                         'ER46907' = 'exIncome09',
                         'ER46935' = 'income09',
                         'ER42020' = 'kids09',
                         'ER42016' = 'famSize09',
                         'ER42140' = 'employed09',
                         'ER43527' = 'kidsOut09',
                         'ER42017' = 'age09',
                         'ER46971A1' = 'food09',
                         'ER46971A5' = 'housing09',
                         'ER46971D2' = 'childcare09',
                         'ER43534' = 'othDepend09',
                         'ER46971D1' = 'educExp09',
                         'ER46971D3' = 'healthExp09',
                         'ER46971B7' = 'transportExp09',
                         'ER42832' = 'vacations09',
                         'ER42837' = 'recreation09',
                         'ER43658' = 'boughtSoldStocks09',
                         'ER42169' = 'selfEmp09',
                         'ER46971E1' = 'clothing09',
                         'ER42023' = 'marSta09',
                         'ER42019' = 'ageSpouse09',
                         'ER43528' = 'numDepend09',
                         'ER46950' = 'othRealEstate09',
                         'ER46964' = 'privateAnnuity09' 
))
f11 <- plyr::rename(f11, c('ER47302' = 'intNum11',
                         'ER52436' = 'longWeight11',
                         'ER48883' = 'stockValue11',   
                         'ER49009' = 'addedStocks11',
                         'ER49015' = 'removedStocks11',
                         'ER48905' = 'addedPrivateAnnuity11',
                         'ER48967' = 'removedPrivateAnnuity11',   
                         'ER48978' = 'addedOthRealEstate11',
                         'ER48983' = 'removedOthRealEstate11',
                         'ER48992' = 'addedFarmBusiness11',  
                         'ER48997' = 'removedFarmBusiness11',  
                         'ER48911' = 'checkingValue11', 
                         'ER52364' = 'bondValue11',
                         'ER52372' = 'othDebt11_1',	
                         'ER52376' = 'othDebt11_2',
                         'ER52380' = 'othDebt11_3',
                         'ER52384' = 'othDebt11_4',
                         'ER52388' = 'othDebt11_5',
                         'ER52360' = 'vehicleValue11', 
                         'ER47348' = 'mortgageDebt11',
                         'ER47440' = 'wtrMoved11',
                         'ER52390' = 'homeEquity11',
                         'ER47329' = 'ownRent11',
                         'ER52394' = 'totalWealth11',
                         'ER52405' = 'educ11',
                         'ER51904' = 'race11',
                         'ER47303' = 'state11',
                         'ER52315' = 'exIncome11',
                         'ER52343' = 'income11',
                         'ER47320' = 'kids11',
                         'ER47316' = 'famSize11',
                         'ER47448' = 'employed11',
                         'ER48852' = 'kidsOut11',
                         'ER47317' = 'age11',
                         'ER52395A1' = 'food11',
                         'ER52395A5' = 'housing11',
                         'ER52395D2' = 'childcare11',
                         'ER48859' = 'othDepend11',
                         'ER52395D1' = 'educExp11',
                         'ER52395D3' = 'healthExp11',
                         'ER52395B7' = 'transportExp11',
                         'ER48154' = 'vacations11',
                         'ER48159' = 'recreation11',
                         'ER49003' = 'boughtSoldStocks11',
                         'ER47482' = 'selfEmp11',
                         'ER52395E1' = 'clothing11',
                         'ER47323' = 'marSta11',
                         'ER47319' = 'ageSpouse11',
                         'ER48853' = 'numDepend11',
                         'ER52354' = 'othRealEstate11',
                         'ER52368' = 'privateAnnuity11' 
                        
))
f13 <- plyr::rename(f13, c('ER53002' = 'intNum13',
                         'ER58257' = 'longWeight13',
                         'ER54634' = 'stockValue13',   
                         'ER54764' = 'addedStocks13', 
                         'ER54770' = 'removedStocks13',
                         'ER54655' = 'addedPrivateAnnuity13',
                         'ER54729' = 'removedPrivateAnnuity13',   
                         'ER54740' = 'addedOthRealEstate13',
                         'ER54745' = 'removedOthRealEstate13',
                         'ER54754' = 'addedFarmBusiness13',  
                         'ER54759' = 'removedFarmBusiness13',  
                         'ER54661' = 'checkingValue13', 
                         'ER58177' = 'bondValue13',
                         'ER58185' = 'othDebt13_1',	
                         'ER58189' = 'othDebt13_2',
                         'ER58193' = 'othDebt13_3',
                         'ER58197' = 'othDebt13_4',
                         'ER58201' = 'othDebt13_5',
                         'ER58173' = 'vehicleValue13', 
                         'ER53048' = 'mortgageDebt13',
                         'ER53140' = 'wtrMoved13',
                         'ER58207' = 'homeEquity13',
                         'ER53029' = 'ownRent13',
                         'ER58211' = 'totalWealth13',
                         'ER58223' = 'educ13',
                         'ER57659' = 'race13',
                         'ER53003' = 'state13',
                         'ER58124' = 'exIncome13',
                         'ER58152' = 'income13',
                         'ER53020' = 'kids13',
                         'ER53016' = 'famSize13',
                         'ER53148' = 'employed13',
                         'ER54595' = 'kidsOut13',
                         'ER53017' = 'age13',
                         'ER58212A1' = 'food13',
                         'ER58212A5' = 'housing13',
                         'ER58212D2' = 'childcare13',
                         'ER54602' = 'othDepend13',
                         'ER58212D1' = 'educExp13',
                         'ER58212D3' = 'healthExp13',
                         'ER58212B7' = 'transportExp13',
                         'ER53848' = 'vacations13',
                         'ER53853' = 'recreation13',
                         'ER53182' = 'selfEmp13',
                         'ER58212E1' = 'clothing13',
                         'ER53023' = 'marSta13',
                         'ER53019' = 'ageSpouse13',
                         'ER54596' = 'numDepend13',
                         'ER54612' = 'othRealEstate13',
                         'ER58181' = 'privateAnnuity13'
))
   

# rename all the individual variables something useful
w <- plyr::rename(w, c("ER30001" = "1968IntNum",
                 'ER30002' = "1968PersonNum",
                 'ER31997' = "primarySamplingUnit",
                 'ER31996' = "stratification",
                 'ER33501' = 'intNum99',
                 'ER33601' = "intNum01",
                 'ER33701' = "intNum03",
                 'ER33801' = "intNum05",
                 'ER33901' = "intNum07",
                 'ER34001' = "intNum09",
                 'ER34101' = "intNum11",
                 'ER34201' = "intNum13",
                 'ER33502' = 'sequenceNum99',
                 'ER33602' = 'sequenceNum01',
                 'ER33702' = 'sequenceNum03',
                 'ER33802' = "sequenceNum05",
                 'ER33902' = "sequenceNum07",
                 'ER34002' = "sequenceNum09",
                 'ER34102' = "sequenceNum11",
                 'ER34202' = "sequenceNum13",
                 'ER33503' = 'hhRelStatus99',
                 'ER33603' = 'hhRelStatus01',
                 'ER33703' = 'hhRelStatus03',
                 'ER33803' = "hhRelStatus05",
                 'ER33903' = "hhRelStatus07",
                 'ER34003' = "hhRelStatus09",
                 'ER34103' = "hhRelStatus11",
                 'ER34203' = "hhRelStatus13"
))


# create a unique ID variable (according to PSID instructions)
w$uniqueID <- (w$'1968IntNum'*1000) + w$'1968PersonNum'




# MERGE individual and family level data
intNum <- c()
for(i in shortYears){
  intNum <- c(intNum, paste('intNum',i,sep=''))
}
j<-1
for(i in shortYears){
  assign(paste0("f",i),merge(eval(as.name(paste("f",i,sep=''))),w,
                             by=intNum[j], all=FALSE)) 
  j <-j+1
}
rm(intNum, i, j)
rm(w)


# keep only household heads
# identified by having relationship to head of 'head' and sequence number in range 1-20
for (i in  shortYears){
  assign(paste0("f",i),
         subset(eval(as.name(paste0("f",i))),
                eval(as.name(paste0("sequenceNum",i)))<=20 &
                  eval(as.name(paste0("sequenceNum",i)))>0 &
                  eval(as.name(paste0("hhRelStatus",i)))==10))
}  
rm(i)



# deal with NA/DK data

# intNum
for(i in shortYears){
  # only at at the family level - w intNum is 0 if NA, so don't get rid
  assign(paste0("f",i), 
         subset(eval(as.name(paste0("f",i))),
                eval(as.name(paste0("intNum",i)))!=0))
}
rm(i)
# deflate data  - sorry future me, past me is too tired to do this nicely :)
cpiData <- read.csv("Data/CPI_base_2015.csv", 
                    header = TRUE)


# stockValue
f01$stockValue01 <- ifelse(f01$stockValue01>999999996 ,0,f01$stockValue01)
f03$stockValue03 <- ifelse(f03$stockValue03> 999999996 | f03$stockValue03< -99999998 ,0,f03$stockValue03)
f05$stockValue05 <- ifelse(f05$stockValue05> 999999996 | f05$stockValue05< -99999998 ,0,f05$stockValue05)
f07$stockValue07 <- ifelse(f07$stockValue07<0 | f07$stockValue07>999999996,0,f07$stockValue07) # no negative
f09$stockValue09 <- ifelse(f09$stockValue09< -99999998 | f09$stockValue09>999999996 ,0,f09$stockValue09)
f11$stockValue11 <- ifelse(f11$stockValue11< -99999998| f11$stockValue11>999999996,0,f11$stockValue11)
f13$stockValue13 <- ifelse(f13$stockValue13< -99999998 | f13$stockValue13>999999996,0,f13$stockValue13)


# stocks added
f01$addedStocks01 <- ifelse(f01$addedStocks01> 999999996,0,f01$addedStocks01)
f03$addedStocks03 <- ifelse(f03$addedStocks03> 999999996,0,f03$addedStocks03)
f05$addedStocks05 <- ifelse(f05$addedStocks05> 999999996,0,f05$addedStocks05)
f07$addedStocks07 <- ifelse(f07$addedStocks07> 999999996,0,f07$addedStocks07)
f09$addedStocks09 <- ifelse(f09$addedStocks09> 999999996,0,f09$addedStocks09)
f11$addedStocks11 <- ifelse(f11$addedStocks11> 999999996,0,f11$addedStocks11)
f13$addedStocks13 <- ifelse(f13$addedStocks13> 999999996 ,0,f13$addedStocks13)



# stocks removed 
f01$removedStocks01 <- ifelse(f01$removedStocks01> 999999996 | f01$removedStocks01< -99999998 ,0,f01$removedStocks01)
f03$removedStocks03 <- ifelse(f03$removedStocks03> 999999996 | f03$removedStocks03< -99999998 ,0,f03$removedStocks03)
f05$removedStocks05 <- ifelse(f05$removedStocks05> 999999996 | f05$removedStocks05< -99999998 ,0,f05$removedStocks05)
f07$removedStocks07 <- ifelse(f07$removedStocks07> 999999996 | f07$removedStocks07<0 ,0,f07$removedStocks07) # no negative values, but negative wild card
f09$removedStocks09 <- ifelse(f09$removedStocks09> 999999996 | f09$removedStocks09< -99999998 ,0,f09$removedStocks09)
f11$removedStocks11 <- ifelse(f11$removedStocks11> 999999996 | f11$removedStocks11< -99999998 ,0,f11$removedStocks11)
f13$removedStocks13 <- ifelse(f13$removedStocks13> 999999996 | f13$removedStocks13< -99999998 ,0,f13$removedStocks13)


# private annuities - added
f01$addedPrivateAnnuity01<- ifelse(f01$addedPrivateAnnuity01> 999999996,0,f01$addedPrivateAnnuity01)
f03$addedPrivateAnnuity03<- ifelse(f03$addedPrivateAnnuity03> 999999996,0,f03$addedPrivateAnnuity03)
f05$addedPrivateAnnuity05<- ifelse(f05$addedPrivateAnnuity05> 999999996,0,f05$addedPrivateAnnuity05)
f07$addedPrivateAnnuity07<- ifelse(f07$addedPrivateAnnuity07> 999999996,0,f07$addedPrivateAnnuity07)
f09$addedPrivateAnnuity09<- ifelse(f09$addedPrivateAnnuity09> 999999996,0,f09$addedPrivateAnnuity09)
f11$addedPrivateAnnuity11<- ifelse(f11$addedPrivateAnnuity11> 999999996,0,f11$addedPrivateAnnuity11)
f13$addedPrivateAnnuity13<- ifelse(f13$addedPrivateAnnuity13> 999999996,0,f13$addedPrivateAnnuity13)

# private annuities - removed
f01$removedPrivateAnnuity01<- ifelse(f01$removedPrivateAnnuity01> 999999996,0,f01$removedPrivateAnnuity01)
f03$removedPrivateAnnuity03<- ifelse(f03$removedPrivateAnnuity03> 999999996,0,f03$removedPrivateAnnuity03)
f05$removedPrivateAnnuity05<- ifelse(f05$removedPrivateAnnuity05> 999999996,0,f05$removedPrivateAnnuity05)
f07$removedPrivateAnnuity07<- ifelse(f07$removedPrivateAnnuity07> 999999996,0,f07$removedPrivateAnnuity07)
f09$removedPrivateAnnuity09<- ifelse(f09$removedPrivateAnnuity09> 999999996,0,f09$removedPrivateAnnuity09)
f11$removedPrivateAnnuity11<- ifelse(f11$removedPrivateAnnuity11> 999999996,0,f11$removedPrivateAnnuity11)
f13$removedPrivateAnnuity13<- ifelse(f13$removedPrivateAnnuity13> 999999996,0,f13$removedPrivateAnnuity13)

# other real estate - added 
f01$addedOthRealEstate01<- ifelse(f01$addedOthRealEstate01> 999999996,0,f01$addedOthRealEstate01)
f03$addedOthRealEstate03<- ifelse(f03$addedOthRealEstate03> 999999996,0,f03$addedOthRealEstate03)
f05$addedOthRealEstate05<- ifelse(f05$addedOthRealEstate05> 999999996,0,f05$addedOthRealEstate05)
f07$addedOthRealEstate07<- ifelse(f07$addedOthRealEstate07> 999999996,0,f07$addedOthRealEstate07)
f09$addedOthRealEstate09<- ifelse(f09$addedOthRealEstate09> 999999996,0,f09$addedOthRealEstate09)
f11$addedOthRealEstate11<- ifelse(f11$addedOthRealEstate11> 999999996,0,f11$addedOthRealEstate11)
f13$addedOthRealEstate13<- ifelse(f13$addedOthRealEstate13> 999999996,0,f13$addedOthRealEstate13)

# other real estate - removed
f01$removedOthRealEstate01<- ifelse(f01$removedOthRealEstate01< -99999998 | f01$removedOthRealEstate01>999999996,0,f01$removedOthRealEstate01)
f03$removedOthRealEstate03<- ifelse(f03$removedOthRealEstate03< -99999998 | f03$removedOthRealEstate03>999999996,0,f03$removedOthRealEstate03)
f05$removedOthRealEstate05<- ifelse(f05$removedOthRealEstate05< -99999998 | f05$removedOthRealEstate05>999999996,0,f05$removedOthRealEstate05)
f07$removedOthRealEstate07<- ifelse(f07$removedOthRealEstate07< -99999998 | f07$removedOthRealEstate07>999999996,0,f07$removedOthRealEstate07)
f09$removedOthRealEstate09<- ifelse(f09$removedOthRealEstate09< -99999998 | f09$removedOthRealEstate09>999999996,0,f09$removedOthRealEstate09)
f11$removedOthRealEstate11<- ifelse(f11$removedOthRealEstate11< -99999998 | f11$removedOthRealEstate11>999999996,0,f11$removedOthRealEstate11)
f13$removedOthRealEstate13<- ifelse(f13$removedOthRealEstate13< -99999998 | f13$removedOthRealEstate13>999999996,0,f13$removedOthRealEstate13)

# farm/business - added 
f01$addedFarmBusiness01<- ifelse(f01$addedFarmBusiness01> 999999996,0,f01$addedFarmBusiness01)
f03$addedFarmBusiness03<- ifelse(f03$addedFarmBusiness03> 999999996,0,f03$addedFarmBusiness03)
f05$addedFarmBusiness05<- ifelse(f05$addedFarmBusiness05> 999999996,0,f05$addedFarmBusiness05)
f07$addedFarmBusiness07<- ifelse(f07$addedFarmBusiness07> 999999996,0,f07$addedFarmBusiness07)
f09$addedFarmBusiness09<- ifelse(f09$addedFarmBusiness09> 999999996,0,f09$addedFarmBusiness09)
f11$addedFarmBusiness11<- ifelse(f11$addedFarmBusiness11> 999999996,0,f11$addedFarmBusiness11)
f13$addedFarmBusiness13<- ifelse(f13$addedFarmBusiness13> 999999996,0,f13$addedFarmBusiness13)

# farm/business - removed 
f01$removedFarmBusiness01<- ifelse(f01$removedFarmBusiness01> 999999996,0,f01$removedFarmBusiness01)
f03$removedFarmBusiness03<- ifelse(f03$removedFarmBusiness03> 999999996,0,f03$removedFarmBusiness03)
f05$removedFarmBusiness05<- ifelse(f05$removedFarmBusiness05> 999999996 |f05$removedFarmBusiness05< -99999998,0,f05$removedFarmBusiness05)
f07$removedFarmBusiness07<- ifelse(f07$removedFarmBusiness07> 999999996,0,f07$removedFarmBusiness07)
f09$removedFarmBusiness09<- ifelse(f09$removedFarmBusiness09> 999999996,0,f09$removedFarmBusiness09)
f11$removedFarmBusiness11<- ifelse(f11$removedFarmBusiness11> 999999996,0,f11$removedFarmBusiness11)
f13$removedFarmBusiness13<- ifelse(f13$removedFarmBusiness13> 999999996,0,f13$removedFarmBusiness13)

# checking/saving 
f99$checkingValue99<- ifelse(f99$checkingValue99> 999999996,0,f99$checkingValue99)
f01$checkingValue01<- ifelse(f01$checkingValue01> 999999996,0,f01$checkingValue01)
f03$checkingValue03<- ifelse(f03$checkingValue03> 999999996,0,f03$checkingValue03)
f05$checkingValue05<- ifelse(f05$checkingValue05> 999999996 | f05$checkingValue05< 0,0,f05$checkingValue05) # wild code
f07$checkingValue07<- ifelse(f07$checkingValue07> 999999996,0,f07$checkingValue07)
f09$checkingValue09<- ifelse(f09$checkingValue09> 999999996 | f09$checkingValue09< -99999996,0,f09$checkingValue09) 
f11$checkingValue11<- ifelse(f11$checkingValue11> 999999996 | f11$checkingValue11< -99999996,0,f11$checkingValue11) 
f13$checkingValue13<- ifelse(f13$checkingValue13> 999999996,0,f13$checkingValue13) 

# bonds
f99$bondValue99<- ifelse(f99$bondValue99> 999999998 | f99$bondValue99< -99999998,0,f99$bondValue99)
f01$bondValue01<- ifelse(f01$bondValue01> 999999998 | f01$bondValue01< -99999998,0,f01$bondValue01)
f03$bondValue03<- ifelse(f03$bondValue03> 999999998 | f03$bondValue03< -99999998,0,f03$bondValue03)
f05$bondValue05<- ifelse(f05$bondValue05> 999999998 | f05$bondValue05< -99999998,0,f05$bondValue05)
f07$bondValue07<- ifelse(f07$bondValue07> 999999996 | f07$bondValue07< -99999998,0,f07$bondValue07)
f09$bondValue09<- ifelse(f09$bondValue09> 999999996 | f09$bondValue09< -99999998,0,f09$bondValue09)
f11$bondValue11<- ifelse(f11$bondValue11> 999999997 | f11$bondValue11< -99999997,0,f11$bondValue11)
f13$bondValue13<- ifelse(f13$bondValue13> 999999997 | f13$bondValue13< -99999997,0,f13$bondValue13)

# non-collateralised debt
f99$othDebtValue99<- ifelse(f99$othDebtValue99> 999999998 ,0,f99$othDebtValue99)
f01$othDebtValue01<- ifelse(f01$othDebtValue01> 999999998 ,0,f01$othDebtValue01)
f03$othDebtValue03<- ifelse(f03$othDebtValue03> 999999998 ,0,f03$othDebtValue03)
f05$othDebtValue05<- ifelse(f05$othDebtValue05> 999999998 ,0,f05$othDebtValue05)
f07$othDebtValue07<- ifelse(f07$othDebtValue07> 999999996 ,0,f07$othDebtValue07)
f09$othDebtValue09<- ifelse(f09$othDebtValue09> 999999996 ,0,f09$othDebtValue09)
f11$othDebtValue11 <- (f11$othDebt11_1 + f11$othDebt11_2 + f11$othDebt11_3 +f11$othDebt11_4 + f11$othDebt11_5)
f13$othDebtValue13 <- (f13$othDebt13_1 + f13$othDebt13_2 + f13$othDebt13_3 +f13$othDebt13_4 + f13$othDebt13_5)

# vehicles
f99$vehicleValue99<- ifelse(f99$vehicleValue99> 999999998 | f99$vehicleValue99< -99999998,0,f99$vehicleValue99)
f01$vehicleValue01<- ifelse(f01$vehicleValue01> 999999998 | f01$vehicleValue01< -99999998,0,f01$vehicleValue01)
f03$vehicleValue03<- ifelse(f03$vehicleValue03> 999999998 | f03$vehicleValue03< -99999998,0,f03$vehicleValue03)
f05$vehicleValue05<- ifelse(f05$vehicleValue05> 999999998 | f05$vehicleValue05< -99999998,0,f05$vehicleValue05)
f07$vehicleValue07<- ifelse(f07$vehicleValue07> 999999996 | f07$vehicleValue07< -99999998,0,f07$vehicleValue07)
f09$vehicleValue09<- ifelse(f09$vehicleValue09> 999999996 | f09$vehicleValue09< -99999998,0,f09$vehicleValue09)
f11$vehicleValue11<- ifelse(f11$vehicleValue11> 999999997 | f11$vehicleValue11< -99999997,0,f11$vehicleValue11)
f13$vehicleValue13<- ifelse(f13$vehicleValue13> 999999997 | f13$vehicleValue13< -99999997,0,f13$vehicleValue13)

# mortgage debt
f99$mortgageDebt99<- ifelse(f99$mortgageDebt99> 9999996, 0,f99$mortgageDebt99)
f01$mortgageDebt01<- ifelse(f01$mortgageDebt01> 9999996, 0,f01$mortgageDebt01)
f03$mortgageDebt03<- ifelse(f03$mortgageDebt03> 9999996, 0,f03$mortgageDebt03)
f05$mortgageDebt05<- ifelse(f05$mortgageDebt05> 9999996, 0,f05$mortgageDebt05)
f07$mortgageDebt07<- ifelse(f07$mortgageDebt07> 9999996, 0,f07$mortgageDebt07)
f09$mortgageDebt09<- ifelse(f09$mortgageDebt09> 9999996, 0,f09$mortgageDebt09)
f11$mortgageDebt11<- ifelse(f11$mortgageDebt11> 9999996, 0,f11$mortgageDebt11)
f13$mortgageDebt13<- ifelse(f13$mortgageDebt13> 9999996, 0,f13$mortgageDebt13)

# wtr moved
f01$wtrMoved01<- ifelse(f01$wtrMoved01== 1, 1,ifelse(f01$wtrMoved01== 5, 0,NA))
f03$wtrMoved03<- ifelse(f03$wtrMoved03== 1, 1,ifelse(f03$wtrMoved03== 5, 0,NA))
f05$wtrMoved05<- ifelse(f05$wtrMoved05== 1, 1,ifelse(f05$wtrMoved05== 5, 0,NA))
f07$wtrMoved07<- ifelse(f07$wtrMoved07== 1, 1,ifelse(f07$wtrMoved07== 5, 0,NA))
f09$wtrMoved09<- ifelse(f09$wtrMoved09== 1, 1,ifelse(f09$wtrMoved09== 5, 0,NA))
f11$wtrMoved11<- ifelse(f11$wtrMoved11== 1, 1,ifelse(f11$wtrMoved11== 5, 0,NA))
f13$wtrMoved13<- ifelse(f13$wtrMoved13== 1, 1,ifelse(f13$wtrMoved13== 5, 0,NA))

# own/rent?

f01$ownRent01<- ifelse(f01$ownRent01==5|f01$ownRent01==8,0,1)
f03$ownRent03<- ifelse(f03$ownRent03==5|f03$ownRent03==8,0,1)
f05$ownRent05<- ifelse(f05$ownRent05==5|f05$ownRent05==8,0,1)
f07$ownRent07<- ifelse(f07$ownRent07==5|f07$ownRent07==8,1,ifelse(f07$ownRent07==9,NA,0)) # wild code
f09$ownRent09<- ifelse(f09$ownRent09==5|f09$ownRent09==8,0,1)
f11$ownRent11<- ifelse(f11$ownRent11==5|f11$ownRent11==8,0,1)
f13$ownRent13<- ifelse(f13$ownRent13==5|f13$ownRent13==8,0,1)

# home equity
f99$homeEquity99<- ifelse(f99$homeEquity99> 999999996 | f99$homeEquity99<= - 99999998, 0,f99$homeEquity99)
f01$homeEquity01<- ifelse(f01$homeEquity01> 999999996 | f01$homeEquity01<= - 99999998, 0,f01$homeEquity01)
f03$homeEquity03<- ifelse(f03$homeEquity03> 999999996 | f03$homeEquity03<= - 99999998, 0,f03$homeEquity03)
f05$homeEquity05<- ifelse(f05$homeEquity05> 999999996 | f05$homeEquity05<= - 99999998, 0,f05$homeEquity05)
f07$homeEquity07<- ifelse(f07$homeEquity07> 999999996 | f07$homeEquity07<= - 99999998, 0,f07$homeEquity07)
f09$homeEquity09<- ifelse(f09$homeEquity09> 999999996 | f09$homeEquity09<= - 99999998, 0,f09$homeEquity09)
f11$homeEquity11<- ifelse(f11$homeEquity11> 999999997 | f11$homeEquity11<= - 99999998, 0,f11$homeEquity11)
f13$homeEquity13<- ifelse(f13$homeEquity13> 999999997 | f13$homeEquity13<= - 99999997, 0,f13$homeEquity13)

# total wealth

f01$totalWealth01<- ifelse(f01$totalWealth01 < -99999998 | f01$totalWealth01> 999999998,NA,f01$totalWealth01)
f03$totalWealth03<- ifelse(f03$totalWealth03 < -99999998 | f03$totalWealth03> 999999998,NA,f03$totalWealth03)
f05$totalWealth05<- ifelse(f05$totalWealth05 < -99999998 | f05$totalWealth05> 999999998,NA,f05$totalWealth05)
f07$totalWealth07<- ifelse(f07$totalWealth07 < -99999998 | f07$totalWealth07> 999999996,NA,f07$totalWealth07)
f09$totalWealth09<- ifelse(f09$totalWealth09 < -99999998 | f09$totalWealth09> 999999996,NA,f09$totalWealth09)
f11$totalWealth11<- ifelse(f11$totalWealth11 < -99999997 | f11$totalWealth11> 999999997,NA,f11$totalWealth11)
f13$totalWealth13<- ifelse(f13$totalWealth13 < -99999997 | f13$totalWealth13> 999999997,NA,f13$totalWealth13)


 # money income
f99$income99 <- ifelse(f99$income99 > 9999998 |f99$income99 < -999998, NA, f99$income99)
f01$income01 <- ifelse(f01$income01 > 9999998 |f01$income01 < -999998, NA, f01$income01)
f03$income03 <- ifelse(f03$income03 > 9999998 |f03$income03 < -999998, NA, f03$income03)
f05$income05 <- ifelse(f05$income05 > 9999998 |f05$income05 < -999998, NA, f05$income05)
f07$income07 <- ifelse(f07$income07 > 9999998 |f07$income07 < -999998, NA, f07$income07)
f09$income09 <- ifelse(f09$income09 > 9999998 |f09$income09 < -999998, NA, f09$income09)
f11$income11 <- ifelse(f11$income11 > 9999997 |f11$income11 < -999997, NA, f11$income11)
f13$income13 <- ifelse(f13$income13 > 9999997 |f13$income13 < -999997, NA, f13$income13)

# education
f01$educ01 <- ifelse(f01$educ01>=0 & f01$educ01<=11,1,ifelse(f01$educ01==12,2,ifelse(f01$educ01>=13 & f01$educ01<=17,3,NA)))
f03$educ03 <- ifelse(f03$educ03>=0 & f03$educ03<=11,1,ifelse(f03$educ03==12,2,ifelse(f03$educ03>=13 & f03$educ03<=17,3,NA)))
f05$educ05 <- ifelse(f05$educ05>=0 & f05$educ05<=11,1,ifelse(f05$educ05==12,2,ifelse(f05$educ05>=13 & f05$educ05<=17,3,NA)))
f07$educ07 <- ifelse(f07$educ07>=0 & f07$educ07<=11,1,ifelse(f07$educ07==12,2,ifelse(f07$educ07>=13 & f07$educ07<=17,3,NA)))
f09$educ09 <- ifelse(f09$educ09>=0 & f09$educ09<=11,1,ifelse(f09$educ09==12,2,ifelse(f09$educ09>=13 & f09$educ09<=17,3,NA)))
f11$educ11 <- ifelse(f11$educ11>=0 & f11$educ11<=11,1,ifelse(f11$educ11==12,2,ifelse(f11$educ11>=13 & f11$educ11<=17,3,NA)))
f13$educ13 <- ifelse(f13$educ13>=0 & f13$educ13<=11,1,ifelse(f13$educ13==12,2,ifelse(f13$educ13>=13 & f13$educ13<=17,3,NA)))

# race
f01$race01 <- ifelse(f01$race01==1,1,ifelse(f01$race01==2,2,ifelse(f01$race01>=3 & f01$race01<8,3,NA)))
f03$race03 <- ifelse(f03$race03==1,1,ifelse(f03$race03==2,2,ifelse(f03$race03>=3 & f03$race03<9,3,NA)))
f05$race05 <- ifelse(f05$race05==1,1,ifelse(f05$race05==2,2,ifelse(f05$race05>=3 & f05$race05<9 & f05$race05!=0,3,NA)))
f07$race07 <- ifelse(f07$race07==1,1,ifelse(f07$race07==2,2,ifelse(f07$race07>=3 & f07$race07<9 & f07$race07!=0,3,NA)))
f09$race09 <- ifelse(f09$race09==1,1,ifelse(f09$race09==2,2,ifelse(f09$race09>=3 & f09$race09<9,3,NA)))
f11$race11 <- ifelse(f11$race11==1,1,ifelse(f11$race11==2,2,ifelse(f11$race11>=3 & f11$race11<8,3,NA)))
f13$race13 <- ifelse(f13$race13==1,1,ifelse(f13$race13==2,2,ifelse(f13$race13>=3 & f13$race13<9,3,NA)))

# f03$other03 <- ifelse(f03$race03>=3 & f03$race03<9, 1,0)
# f03$white03 <- ifelse(f03$race03==1,1,0)
# f05$white05 <- ifelse(f05$race05==1,1,0)
# f07$white07 <- ifelse(f07$race07==1,1,0)
# f09$white09 <- ifelse(f09$race09==1,1,0)
# f11$white11 <- ifelse(f11$race11==1,1,0)
# f13$white13 <- ifelse(f13$race13==1,1,0)
# 
# f01$black01 <- ifelse(f01$race01==2, 1,0)
# f03$black03 <- ifelse(f03$race03==2, 1,0)
# f05$black05 <- ifelse(f05$race05==2, 1,0)
# f07$black07 <- ifelse(f07$race07==2, 1,0)
# f09$black09 <- ifelse(f09$race09==2, 1,0)
# f11$black11 <- ifelse(f11$race11==2, 1,0)
# f13$black13 <- ifelse(f13$race13==2, 1,0)
# 
# f01$other01 <- ifelse(f01$race01>=3 & f01$race01<8, 1,0)
# f03$other03 <- ifelse(f03$race03>=3 & f03$race03<9, 1,0)
# f05$other05 <- ifelse(f05$race05>=3 & f05$race05<9 & f05$race05!=0, 1,0)
# f07$other07 <- ifelse(f07$race07>=3 & f07$race07<9 & f07$race07!=0, 1,0)
# f09$other09 <- ifelse(f09$race09>=3 & f09$race09<9, 1, 0)
# f11$other11 <- ifelse(f11$race11>=3 & f11$race11<8, 1,0)
# f13$other13 <- ifelse(f13$race13>=3 & f13$race13<9, 1,0)

# f01$white01 <- ifelse(f01$race01==1, 1, ifelse(f01$race01==8|f01$race01==9,NA,0))
# f03$white03 <- ifelse(f03$race03==1, 1, ifelse(f03$race03==9,NA,0))
# f05$white05 <- ifelse(f05$race05==1, 1, ifelse(f05$race05==0|f05$race05==9,NA,0))
# f07$white07 <- ifelse(f07$race07==1, 1, ifelse(f07$race07==0|f07$race07==9,NA,0))
# f09$white09 <- ifelse(f09$race09==1, 1, ifelse(f09$race09==9,NA,0))
# f11$white11 <- ifelse(f11$race11==1, 1, ifelse(f11$race11==8|f11$race11==9,NA,0))
# f13$white13 <- ifelse(f13$race13==1, 1, ifelse(f13$race13==9,NA,0))
# 
# f01$black01 <- ifelse(f01$race01==2, 1, ifelse(f01$race01==8|f01$race01==9,NA,0))
# f03$black03 <- ifelse(f03$race03==2, 1, ifelse(f03$race03==9,NA,0))
# f05$black05 <- ifelse(f05$race05==2, 1, ifelse(f05$race05==0|f05$race05==9,NA,0))
# f07$black07 <- ifelse(f07$race07==2, 1, ifelse(f07$race07==0|f07$race07==9,NA,0))
# f09$black09 <- ifelse(f09$race09==2, 1, ifelse(f09$race09==9,NA,0))
# f11$black11 <- ifelse(f11$race11==2, 1, ifelse(f11$race11==8|f11$race11==9,NA,0))
# f13$black13 <- ifelse(f13$race13==2, 1, ifelse(f13$race13==9,NA,0))
# 
# f01$other01 <- ifelse(f01$race01>=3 & f01$race01<8, 1, ifelse(f01$race01==8|f01$race01==9,NA,0))
# f03$other03 <- ifelse(f03$race03>=3 & f03$race03<9, 1, ifelse(f03$race03==9,NA,0))
# f05$other05 <- ifelse(f05$race05>=3 & f05$race05<9 & f05$race05!=0, 1, ifelse(f05$race05==0|f05$race05==9,NA,0))
# f07$other07 <- ifelse(f07$race07>=3 & f07$race07<9 & f07$race07!=0, 1,ifelse(f07$race07==0|f07$race07==9,NA,0))
# f09$other09 <- ifelse(f09$race09>=3 & f09$race09<9, 1, ifelse(f09$race09==9,NA,0))
# f11$other11 <- ifelse(f11$race11>=3 & f11$race11<8, 1, ifelse(f11$race11==8|f11$race11==9,NA,0))
# f13$other13 <- ifelse(f13$race13>=3 & f13$race13<9, 1, ifelse(f13$race13==9,NA,0))

# kids outside family unit

f01$kidsOut01 <- ifelse(f01$kidsOut01==1,1,ifelse(f01$kidsOut01==5,0,NA))
f03$kidsOut03 <- ifelse(f03$kidsOut03==1,1,ifelse(f03$kidsOut03==5,0,NA))
f05$kidsOut05 <- ifelse(f05$kidsOut05==1,1,ifelse(f05$kidsOut05==5,0,NA))
f07$kidsOut07 <- ifelse(f07$kidsOut07==1,1,ifelse(f07$kidsOut07==5,0,NA))
f09$kidsOut09 <- ifelse(f09$kidsOut09==1,1,ifelse(f09$kidsOut09==5,0,NA))
f11$kidsOut11 <- ifelse(f11$kidsOut11==1,1,ifelse(f11$kidsOut11==5,0,NA))
f13$kidsOut13 <- ifelse(f13$kidsOut13==1,1,ifelse(f13$kidsOut13==5,0,NA))

# region
f01$region01 <- ifelse(f01$state01==6|f01$state01==18|f01$state01==20|f01$state01==28|f01$state01==29| f01$state01==31|f01$state01==37|f01$state01==38|f01$state01==44,1,
                       ifelse(f01$state01==12|f01$state01==13|f01$state01==14|f01$state01==15|f01$state01==21|f01$state01==22|f01$state01==24|f01$state01==26|f01$state01==33|f01$state01==34|f01$state01==40|f01$state01==48,2,
                              ifelse(f01$state01==1|f01$state01==3|f01$state01==7|f01$state01==8|f01$state01==9|f01$state01==10|f01$state01==16|f01$state01==17|f01$state01==19|f01$state01==23|f01$state01==32|f01$state01==35|f01$state01==39|f01$state01==41|f01$state01==42|f01$state01==45|f01$state01==47,3,
                                     ifelse(f01$state01==2|f01$state01==4|f01$state01==5|f01$state01==11|f01$state01==25|f01$state01==27|f01$state01==30|f01$state01==36|f01$state01==43|f01$state01==46|f01$state01==49|f01$state01==50|f01$state01==51,4,NA))))
f03$region03 <- ifelse(f03$state03==6|f03$state03==18|f03$state03==20|f03$state03==28|f03$state03==29| f03$state03==31|f03$state03==37|f03$state03==38|f03$state03==44,1,
                       ifelse(f03$state03==12|f03$state03==13|f03$state03==14|f03$state03==15|f03$state03==21|f03$state03==22|f03$state03==24|f03$state03==26|f03$state03==33|f03$state03==34|f03$state03==40|f03$state03==48,2,
                              ifelse(f03$state03==1|f03$state03==3|f03$state03==7|f03$state03==8|f03$state03==9|f03$state03==10|f03$state03==16|f03$state03==17|f03$state03==19|f03$state03==23|f03$state03==32|f03$state03==35|f03$state03==39|f03$state03==41|f03$state03==42|f03$state03==45|f03$state03==47,3,
                                     ifelse(f03$state03==2|f03$state03==4|f03$state03==5|f03$state03==11|f03$state03==25|f03$state03==27|f03$state03==30|f03$state03==36|f03$state03==43|f03$state03==46|f03$state03==49|f03$state03==50|f03$state03==51,4,NA))))
f05$region05 <- ifelse(f05$state05==6|f05$state05==18|f05$state05==20|f05$state05==28|f05$state05==29| f05$state05==31|f05$state05==37|f05$state05==38|f05$state05==44,1,
                       ifelse(f05$state05==12|f05$state05==13|f05$state05==14|f05$state05==15|f05$state05==21|f05$state05==22|f05$state05==24|f05$state05==26|f05$state05==33|f05$state05==34|f05$state05==40|f05$state05==48,2,
                              ifelse(f05$state05==1|f05$state05==3|f05$state05==7|f05$state05==8|f05$state05==9|f05$state05==10|f05$state05==16|f05$state05==17|f05$state05==19|f05$state05==23|f05$state05==32|f05$state05==35|f05$state05==39|f05$state05==41|f05$state05==42|f05$state05==45|f05$state05==47,3,
                                ifelse(f05$state05==2|f05$state05==4|f05$state05==5|f05$state05==11|f05$state05==25|f05$state05==27|f05$state05==30|f05$state05==36|f05$state05==43|f05$state05==46|f05$state05==49|f05$state05==50|f05$state05==51,4,NA))))
f07$region07 <- ifelse(f07$state07==6|f07$state07==18|f07$state07==20|f07$state07==28|f07$state07==29| f07$state07==31|f07$state07==37|f07$state07==38|f07$state07==44,1,
                       ifelse(f07$state07==12|f07$state07==13|f07$state07==14|f07$state07==15|f07$state07==21|f07$state07==22|f07$state07==24|f07$state07==26|f07$state07==33|f07$state07==34|f07$state07==40|f07$state07==48,2,
                              ifelse(f07$state07==1|f07$state07==3|f07$state07==7|f07$state07==8|f07$state07==9|f07$state07==10|f07$state07==16|f07$state07==17|f07$state07==19|f07$state07==23|f07$state07==32|f07$state07==35|f07$state07==39|f07$state07==41|f07$state07==42|f07$state07==45|f07$state07==47,3,
                                     ifelse(f07$state07==2|f07$state07==4|f07$state07==5|f07$state07==11|f07$state07==25|f07$state07==27|f07$state07==30|f07$state07==36|f07$state07==43|f07$state07==46|f07$state07==49|f07$state07==50|f07$state07==51,4,NA))))
f09$region09 <- ifelse(f09$state09==6|f09$state09==18|f09$state09==20|f09$state09==28|f09$state09==29| f09$state09==31|f09$state09==37|f09$state09==38|f09$state09==44,1,
                       ifelse(f09$state09==12|f09$state09==13|f09$state09==14|f09$state09==15|f09$state09==21|f09$state09==22|f09$state09==24|f09$state09==26|f09$state09==33|f09$state09==34|f09$state09==40|f09$state09==48,2,
                              ifelse(f09$state09==1|f09$state09==3|f09$state09==7|f09$state09==8|f09$state09==9|f09$state09==10|f09$state09==16|f09$state09==17|f09$state09==19|f09$state09==23|f09$state09==32|f09$state09==35|f09$state09==39|f09$state09==41|f09$state09==42|f09$state09==45|f09$state09==47,3,
                                ifelse(f09$state09==2|f09$state09==4|f09$state09==5|f09$state09==11|f09$state09==25|f09$state09==27|f09$state09==30|f09$state09==36|f09$state09==43|f09$state09==46|f09$state09==49|f09$state09==50|f09$state09==51,4,NA))))
f11$region11 <- ifelse(f11$state11==6|f11$state11==18|f11$state11==20|f11$state11==28|f11$state11==29| f11$state11==31|f11$state11==37|f11$state11==38|f11$state11==44,1,
                       ifelse(f11$state11==12|f11$state11==13|f11$state11==14|f11$state11==15|f11$state11==21|f11$state11==22|f11$state11==24|f11$state11==26|f11$state11==33|f11$state11==34|f11$state11==40|f11$state11==48,2,
                              ifelse(f11$state11==1|f11$state11==3|f11$state11==7|f11$state11==8|f11$state11==9|f11$state11==10|f11$state11==16|f11$state11==17|f11$state11==19|f11$state11==23|f11$state11==32|f11$state11==35|f11$state11==39|f11$state11==41|f11$state11==42|f11$state11==45|f11$state11==47,3,
                                  ifelse(f11$state11==2|f11$state11==4|f11$state11==5|f11$state11==11|f11$state11==25|f11$state11==27|f11$state11==30|f11$state11==36|f11$state11==43|f11$state11==46|f11$state11==49|f11$state11==50|f11$state11==51,4,NA))))
f13$region13 <- ifelse(f13$state13==6|f13$state13==18|f13$state13==20|f13$state13==28|f13$state13==29| f13$state13==31|f13$state13==37|f13$state13==38|f13$state13==44,1,
                       ifelse(f13$state13==12|f13$state13==13|f13$state13==14|f13$state13==15|f13$state13==21|f13$state13==22|f13$state13==24|f13$state13==26|f13$state13==33|f13$state13==34|f13$state13==40|f13$state13==48,2,
                              ifelse(f13$state13==1|f13$state13==3|f13$state13==7|f13$state13==8|f13$state13==9|f13$state13==10|f13$state13==16|f13$state13==17|f13$state13==19|f13$state13==23|f13$state13==32|f13$state13==35|f13$state13==39|f13$state13==41|f13$state13==42|f13$state13==45|f13$state13==47,3,
                                  ifelse(f13$state13==2|f13$state13==4|f13$state13==5|f13$state13==13|f13$state13==25|f13$state13==27|f13$state13==30|f13$state13==36|f13$state13==43|f13$state13==46|f13$state13==49|f13$state13==50|f13$state13==51,4,NA))))

# extra income
f01$exIncome01 <- ifelse(f01$exIncome01==0,0,ifelse(f01$exIncome01<9999999 & f01$exIncome01> -999999,1,NA))
f03$exIncome03 <- ifelse(f03$exIncome03==0,0,ifelse(f03$exIncome03<9999999, 1,NA))
f05$exIncome05 <-  ifelse(f05$exIncome05==0,0,ifelse(f05$exIncome05<9999999 & f05$exIncome05> -999999,1,NA))
f07$exIncome07 <- ifelse(f07$exIncome07==0,0,ifelse(f07$exIncome07<9999999 & f07$exIncome07> -999999,1,NA))
f09$exIncome09 <- ifelse(f09$exIncome09==0,0,ifelse(f09$exIncome09<9999999 & f09$exIncome09> -999999,1,NA))
f11$exIncome11 <- ifelse(f11$exIncome11==0,0,ifelse(f11$exIncome11<9999998 & f11$exIncome11> -999998, 1,NA))
f13$exIncome13 <- ifelse(f13$exIncome13==0,0,ifelse(f13$exIncome13<9999998 & f13$exIncome13> -999998, 1,NA))

#labour market
f01$employed01 <- ifelse(f01$employed01==1,1,ifelse(f01$employed01==4,2,ifelse(f01$employed01==3,3,ifelse(f01$employed01==2|f01$employed01==5|f01$employed01==6,4,NA))))
f03$employed03 <- ifelse(f03$employed03==1,1,ifelse(f03$employed03==4,2,ifelse(f03$employed03==3,3,ifelse(f03$employed03==2|f03$employed03==5|f03$employed03==6,4,NA))))
f05$employed05 <- ifelse(f05$employed05==1,1,ifelse(f05$employed05==4,2,ifelse(f05$employed05==3,3,ifelse(f05$employed05==2|f05$employed05==5|f05$employed05==6,4,NA))))
f07$employed07 <- ifelse(f07$employed07==1,1,ifelse(f07$employed07==4,2,ifelse(f07$employed07==3,3,ifelse(f07$employed07==2|f07$employed07==5|f07$employed07==6,4,NA))))
f09$employed09 <- ifelse(f09$employed09==1,1,ifelse(f09$employed09==4,2,ifelse(f09$employed09==3,3,ifelse(f09$employed09==2|f09$employed09==5|f09$employed09==6,4,NA))))
f11$employed11 <- ifelse(f11$employed11==1,1,ifelse(f11$employed11==4,2,ifelse(f11$employed11==3,3,ifelse(f11$employed11==2|f11$employed11==5|f11$employed11==6,4,NA))))
f13$employed13 <- ifelse(f13$employed13==1,1,ifelse(f13$employed13==4,2,ifelse(f13$employed13==3,3,ifelse(f13$employed13==2|f13$employed13==5|f13$employed13==6,4,NA))))


# # retired
# 
# f01$retired01 <- ifelse(f01$employed01==4,1,ifelse(f01$employed01==99| f01$employed01==98,NA,0))
# f03$retired03 <- ifelse(f03$employed03==4,1,ifelse(f03$employed03==0| f03$employed03==22| f03$employed03==99,NA,0))
# f05$retired05 <- ifelse(f05$employed05==4,1,ifelse(f05$employed05==99| f05$employed05==0,NA,0))
# f07$retired07 <- ifelse(f07$employed07==4,1,ifelse(f07$employed07==9| f07$employed07==0,NA,0))
# f09$retired09 <- ifelse(f09$employed09==4,1,ifelse(f09$employed09==8| f09$employed09==99,NA,0))
# f11$retired11 <- ifelse(f11$employed11==4,1,ifelse(f11$employed11==99,NA,0))
# f13$retired13 <- ifelse(f13$employed13==4,1,ifelse(f13$employed13==99,NA,0))
# 
# 
# 
# # unemployed
# 
# f01$unemployed01 <- ifelse(f01$employed01==3,1,ifelse(f01$employed01==99| f01$employed01==98,NA,0))
# f03$unemployed03 <- ifelse(f03$employed03==3,1,ifelse(f03$employed03==0| f03$employed03==22| f03$employed03==99,NA,0))
# f05$unemployed05 <- ifelse(f05$employed05==3,1,ifelse(f05$employed05==99| f05$employed05==0,NA,0))
# f07$unemployed07 <- ifelse(f07$employed07==3,1,ifelse(f07$employed07==9| f07$employed07==0,NA,0))
# f09$unemployed09 <- ifelse(f09$employed09==3,1,ifelse(f09$employed09==8| f09$employed09==99,NA,0))
# f11$unemployed11 <- ifelse(f11$employed11==3,1,ifelse(f11$employed11==99,NA,0))
# f13$unemployed13 <- ifelse(f13$employed13==3,1,ifelse(f13$employed13==99,NA,0))
# 
# 
# 
# # employed
# 
# f01$employed01 <- ifelse(f01$employed01==1,1,ifelse(f01$employed01==99| f01$employed01==98,NA,0))
# f03$employed03 <- ifelse(f03$employed03==1,1,ifelse(f03$employed03==0| f03$employed03==22| f03$employed03==99,NA,0))
# f05$employed05 <- ifelse(f05$employed05==1,1,ifelse(f05$employed05==99| f05$employed05==0,NA,0))
# f07$employed07 <- ifelse(f07$employed07==1,1,ifelse(f07$employed07==9| f07$employed07==0,NA,0))
# f09$employed09 <- ifelse(f09$employed09==1,1,ifelse(f09$employed09==8| f09$employed09==99,NA,0))
# f11$employed11 <- ifelse(f11$employed11==1,1,ifelse(f11$employed11==99,NA,0))
# f13$employed13 <- ifelse(f13$employed13==1,1,ifelse(f13$employed13==99,NA,0))


# Variables to keep - KV
# We include food at home and food away from
# home, utilities, gasoline, car maintenance, public transportation, child care, health
# expenditures, and education
# food expenditure

#for(j in c("food","transportExp","childCare"))
#for(i in shortYears){
#  assign(paste0("f",i),
#         subset(eval(as.name(paste0("f",i))),
                

# consumption
f03$housing03 <- ifelse(f03$housing03<0 | f03$housing03>999997,NA,f03$housing03)
f05$housing05 <- ifelse(f05$housing05<0 | f05$housing05>999997,NA,f05$housing05)
f07$housing07 <- ifelse(f07$housing07<0 | f07$housing07>999997,NA,f07$housing07)
f09$housing09 <- ifelse(f09$housing09<0 | f09$housing09>999997,NA,f09$housing09)
f11$housing11 <- ifelse(f11$housing11<0 | f11$housing11>999997,NA,f11$housing11)
f13$housing13 <- ifelse(f13$housing13<0 | f13$housing13>999997,NA,f13$housing13)

f03$transportExp03 <- ifelse(f03$transportExp03<0 | f03$transportExp03>999997,NA,f03$transportExp03)
f05$transportExp05 <- ifelse(f05$transportExp05<0 | f05$transportExp05>999997,NA,f05$transportExp05)
f07$transportExp07 <- ifelse(f07$transportExp07<0 | f07$transportExp07>999997,NA,f07$transportExp07)
f09$transportExp09 <- ifelse(f09$transportExp09<0 | f09$transportExp09>999997,NA,f09$transportExp09)
f11$transportExp11 <- ifelse(f11$transportExp11<0 | f11$transportExp11>999997,NA,f11$transportExp11)
f13$transportExp13 <- ifelse(f13$transportExp13<0 | f13$transportExp13>999997,NA,f13$transportExp13)

f03$childcare03 <- ifelse(f03$childcare03<0 | f03$childcare03>999997,NA,f03$childcare03)
f05$childcare05 <- ifelse(f05$childcare05<0 | f05$childcare05>999997,NA,f05$childcare05)
f07$childcare07 <- ifelse(f07$childcare07<0 | f07$childcare07>999997,NA,f07$childcare07)
f09$childcare09 <- ifelse(f09$childcare09<0 | f09$childcare09>999997,NA,f09$childcare09)
f11$childcare11 <- ifelse(f11$childcare11<0 | f11$childcare11>999997,NA,f11$childcare11)
f13$childcare13 <- ifelse(f13$childcare13<0 | f13$childcare13>999997,NA,f13$childcare13)

f03$healthExp03 <- ifelse(f03$healthExp03<0 | f03$healthExp03>999997,NA,f03$healthExp03)
f05$healthExp05 <- ifelse(f05$healthExp05<0 | f05$healthExp05>999997,NA,f05$healthExp05)
f07$healthExp07 <- ifelse(f07$healthExp07<0 | f07$healthExp07>999997,NA,f07$healthExp07)
f09$healthExp09 <- ifelse(f09$healthExp09<0 | f09$healthExp09>999997,NA,f09$healthExp09)
f11$healthExp11 <- ifelse(f11$healthExp11<0 | f11$healthExp11>999997,NA,f11$healthExp11)
f13$healthExp13 <- ifelse(f13$healthExp13<0 | f13$healthExp13>999997,NA,f13$healthExp13)

f05$vacations05 <- ifelse(f05$vacations05<0 | f05$vacations05>999997,NA,f05$vacations05)
f07$vacations07 <- ifelse(f07$vacations07<0 | f07$vacations07>999997,NA,f07$vacations07)
f09$vacations09 <- ifelse(f09$vacations09<0 | f09$vacations09>999997,NA,f09$vacations09)
f11$vacations11 <- ifelse(f11$vacations11<0 | f11$vacations11>999997,NA,f11$vacations11)
f13$vacations13 <- ifelse(f13$vacations13<0 | f13$vacations13>999997,NA,f13$vacations13)

f05$recreation05 <- ifelse(f05$recreation05<0 | f05$recreation05>999997,NA,f05$recreation05)
f07$recreation07 <- ifelse(f07$recreation07<0 | f07$recreation07>999997,NA,f07$recreation07)
f09$recreation09 <- ifelse(f09$recreation09<0 | f09$recreation09>999997,NA,f09$recreation09)
f11$recreation11 <- ifelse(f11$recreation11<0 | f11$recreation11>999997,NA,f11$recreation11)
f13$recreation13 <- ifelse(f13$recreation13<0 | f13$recreation13>999997,NA,f13$recreation13)

f05$clothing05 <- ifelse(f05$clothing05<0 | f05$clothing05>999997,NA,f05$clothing05)
f07$clothing07 <- ifelse(f07$clothing07<0 | f07$clothing07>999997,NA,f07$clothing07)
f09$clothing09 <- ifelse(f09$clothing09<0 | f09$clothing09>999997,NA,f09$clothing09)
f11$clothing11 <- ifelse(f11$clothing11<0 | f11$clothing11>999997,NA,f11$clothing11)
f13$clothing13 <- ifelse(f13$clothing13<0 | f13$clothing13>999997,NA,f13$clothing13)

f99$consumption99 <- (f99$food99 + f99$transportExp99 +  f99$childcare99 + f99$healthExp99 + f99$educExp99 + f99$housing99)
f01$consumption01 <- (f01$food01 + f01$transportExp01 +  f01$childcare01 + f01$healthExp01 + f01$educExp01 + f01$housing01)
f03$consumption03 <- (f03$food03  + f03$transportExp03 +  f03$childcare03 + f03$healthExp03 + f03$educExp03 + f03$housing03)
f05$consumption05 <- (f05$food05  + f05$transportExp05 +  f05$childcare05 + f05$healthExp05 + f05$educExp05 + f05$housing05)
f07$consumption07 <- (f07$food07 + f07$transportExp07 +  f07$childcare07 + f07$healthExp07 + f07$educExp07 + f07$housing07)
f09$consumption09 <- (f09$food09 + f09$transportExp09 +  f09$childcare09 + f09$healthExp09 + f09$educExp09 + f09$housing09)
f11$consumption11 <- (f11$food11 + f11$transportExp11 +  f11$childcare11 + f11$healthExp11 + f11$educExp11 + f11$housing11)
f13$consumption13 <- (f13$food13 + f13$transportExp13 +  f13$childcare13 + f13$healthExp13 + f13$educExp13 + f13$housing13)

f05$consumption_plus05 <- (f05$food05 + f05$transportExp05 +  f05$childcare05 + f05$healthExp05 + f05$educExp05 + f05$vacations05 + f05$recreation05 + f05$clothing05 + f05$housing05)
f07$consumption_plus07 <- (f07$food07 + f07$transportExp07 +  f07$childcare07 + f07$healthExp07 + f07$educExp07 + f07$vacations07 + f07$recreation07 + f07$clothing07 + f07$housing07)
f09$consumption_plus09 <- (f09$food09 + f09$transportExp09 +  f09$childcare09 + f09$healthExp09 + f09$educExp09 + f09$vacations09 + f09$recreation09 + f09$clothing09 + f09$housing09)
f11$consumption_plus11 <- (f11$food11 + f11$transportExp11 +  f11$childcare11 + f11$healthExp11 + f11$educExp11 + f11$vacations11 + f11$recreation11 + f11$clothing11 + f11$housing11)
f13$consumption_plus13 <- (f13$food13 + f13$transportExp13 +  f13$childcare13 + f13$healthExp13 + f13$educExp13 + f13$vacations13 + f13$recreation13 + f13$clothing13 + f13$housing13)



# liquid assets


f03$netLiquidAssets03 <- (f03$checkingValue03 + f03$stockValue03) - f03$othDebtValue03
f05$netLiquidAssets05 <- (f05$checkingValue05 + f05$stockValue05) - f05$othDebtValue05
f07$netLiquidAssets07 <- (f07$checkingValue07 + f07$stockValue07) - f07$othDebtValue07
f09$netLiquidAssets09 <- (f09$checkingValue09 + f09$stockValue09) - f09$othDebtValue09
f11$netLiquidAssets11 <- (f11$checkingValue11 + f11$stockValue11) - f11$othDebtValue11
f13$netLiquidAssets13 <- (f13$checkingValue13 + f13$stockValue13) - f13$othDebtValue13

# illiquid assets

f03$othRealEstate03 <- ifelse(f03$othRealEstate03>999999998 | f03$othRealEstate03< -99999998,NA,f03$othRealEstate03)
f05$othRealEstate05 <- ifelse(f05$othRealEstate05>999999998 | f05$othRealEstate05< -99999998,NA,f05$othRealEstate05)
f07$othRealEstate07 <- ifelse(f07$othRealEstate07>999999996 | f07$othRealEstate07< -99999998,NA,f07$othRealEstate07)
f09$othRealEstate09 <- ifelse(f09$othRealEstate09>999999996 | f09$othRealEstate09< -99999998,NA,f09$othRealEstate09)
f11$othRealEstate11 <- ifelse(f11$othRealEstate11>999999997 | f11$othRealEstate11< -99999997,NA,f11$othRealEstate11)
f13$othRealEstate13 <- ifelse(f13$othRealEstate13>999999996,NA,f13$othRealEstate13)

f03$privateAnnuity03 <- ifelse(f03$privateAnnuity03>999999998,NA,f03$privateAnnuity03)
f05$privateAnnuity05 <- ifelse(f05$privateAnnuity05>999999998,NA,f05$privateAnnuity05)
f07$privateAnnuity07 <- ifelse(f07$privateAnnuity07>999999996,NA,f07$privateAnnuity07)
f09$privateAnnuity09 <- ifelse(f09$privateAnnuity09>999999996,NA,f09$privateAnnuity09)
f11$privateAnnuity11 <- ifelse(f11$privateAnnuity11>999999997,NA,f11$privateAnnuity11)
f13$privateAnnuity13 <- ifelse(f13$privateAnnuity13>999999997,NA,f13$privateAnnuity13)

f03$netIlliquidAssets03 <- (f03$homeEquity03 + f03$othRealEstate03 + f03$bondValue03 + f03$privateAnnuity03) 
f05$netIlliquidAssets05 <- (f05$homeEquity05 + f05$othRealEstate05 + f05$bondValue05 + f05$privateAnnuity05)
f07$netIlliquidAssets07 <- (f07$homeEquity07 + f07$othRealEstate07 + f07$bondValue07 + f07$privateAnnuity07)
f09$netIlliquidAssets09 <- (f09$homeEquity09 + f09$othRealEstate09 + f09$bondValue09 + f09$privateAnnuity09)
f11$netIlliquidAssets11 <- (f11$homeEquity11 + f11$othRealEstate11 + f11$bondValue11 + f11$privateAnnuity11) 
f13$netIlliquidAssets13 <- (f13$homeEquity13 + f13$othRealEstate13 + f13$bondValue13 + f13$privateAnnuity13)

f03$creditLimit03 = ifelse(f03$income03/12<0,0,f03$income03/12<0)
f05$creditLimit05 = ifelse(f05$income05/12<0,0,f05$income05/12<0)
f07$creditLimit07 = ifelse(f07$income07/12<0,0,f07$income07/12<0)
f09$creditLimit09 = ifelse(f09$income09/12<0,0,f09$income09/12<0)
f11$creditLimit11 = ifelse(f11$income11/12<0,0,f11$income11/12<0)
f13$creditLimit13 = ifelse(f13$income13/12<0,0,f13$income13/12<0)

f03$poorHTM03 <- ifelse(f03$netIlliquidAssets03<=0 & f03$netLiquidAssets03<=0 & f03$netLiquidAssets03<=((f03$income03/2) - f03$creditLimit03),1,ifelse(f03$netIlliquidAssets03>0 & f03$netLiquidAssets03<=0 & f03$netLiquidAssets03<=((f03$income03/2) - f03$creditLimit03),2,0))
f05$poorHTM05 <- ifelse(f05$netIlliquidAssets05<=0 & f05$netLiquidAssets05<=0 & f05$netLiquidAssets05<=((f05$income05/2) - f05$creditLimit05),1,ifelse(f05$netIlliquidAssets05>0 & f05$netLiquidAssets05<=0 & f05$netLiquidAssets05<=((f05$income05/2) - f05$creditLimit05),2,0))
f07$poorHTM07 <- ifelse(f07$netIlliquidAssets07<=0 & f07$netLiquidAssets07<=0 & f07$netLiquidAssets07<=((f07$income07/2) - f07$creditLimit07),1,ifelse(f07$netIlliquidAssets07>0 & f07$netLiquidAssets07<=0 & f07$netLiquidAssets07<=((f07$income07/2) - f07$creditLimit07),2,0))
f09$poorHTM09 <- ifelse(f09$netIlliquidAssets09<=0 & f09$netLiquidAssets09<=0 & f09$netLiquidAssets09<=((f09$income09/2) - f09$creditLimit09),1,ifelse(f09$netIlliquidAssets09>0 & f09$netLiquidAssets09<=0 & f09$netLiquidAssets09<=((f09$income09/2) - f09$creditLimit09),2,0))
f11$poorHTM11 <- ifelse(f11$netIlliquidAssets11<=0 & f11$netLiquidAssets11<=0 & f11$netLiquidAssets11<=((f11$income11/2) - f11$creditLimit11),1,ifelse(f11$netIlliquidAssets11>0 & f11$netLiquidAssets11<=0 & f11$netLiquidAssets11<=((f11$income11/2) - f11$creditLimit11),2,0))
f13$poorHTM13 <- ifelse(f13$netIlliquidAssets13<=0 & f13$netLiquidAssets13<=0 & f13$netLiquidAssets13<=((f13$income13/2) - f13$creditLimit13),1,ifelse(f13$netIlliquidAssets13>0 & f13$netLiquidAssets13<=0 & f13$netLiquidAssets13<=((f13$income13/2) - f13$creditLimit13),2,0))

# f03$richHTM03 <- ifelse(f03$netIlliquidAssets03>0 & f03$netLiquidAssets03<=0 & f03$netLiquidAssets03<=((f03$income03/2) - f03$creditLimit03),2,0)
# f05$richHTM05 <- ifelse(f05$netIlliquidAssets05>0 & f05$netLiquidAssets05<=0 & f05$netLiquidAssets05<=((f05$income05/2) - f05$creditLimit05),2,0)
# f07$richHTM07 <- ifelse(f07$netIlliquidAssets07>0 & f07$netLiquidAssets07<=0 & f07$netLiquidAssets07<=((f07$income07/2) - f07$creditLimit07),2,0)
# f09$richHTM09 <- ifelse(f09$netIlliquidAssets09>0 & f09$netLiquidAssets09<=0 & f09$netLiquidAssets09<=((f09$income09/2) - f09$creditLimit09),2,0)
# f11$richHTM11 <- ifelse(f11$netIlliquidAssets11>0 & f11$netLiquidAssets11<=0 & f11$netLiquidAssets11<=((f11$income11/2) - f11$creditLimit11),2,0)
# f13$richHTM13 <- ifelse(f13$netIlliquidAssets13>0 & f13$netLiquidAssets13<=0 & f13$netLiquidAssets13<=((f13$income13/2) - f13$creditLimit13),2,0)

# self-employed

f99$selfEmp99 <- ifelse(f99$selfEmp99==1,0,1)
f01$selfEmp01 <- ifelse(f01$selfEmp01==1,0,1)
f03$selfEmp03 <- ifelse(f03$selfEmp03==1,0,1)
f05$selfEmp05 <- ifelse(f05$selfEmp05==1,0,1)
f07$selfEmp07 <- ifelse(f07$selfEmp07==1,0,1)
f09$selfEmp09 <- ifelse(f09$selfEmp09==1,0,1)
f11$selfEmp11 <- ifelse(f11$selfEmp11==1,0,1)
f13$selfEmp13 <- ifelse(f13$selfEmp13==1,0,1)

# state

# f01$state01 <- ifelse(f01$state01>0 & f01$state01<51, f01$state01, NA)
# f03$state03 <- ifelse(f03$state03>0 & f03$state03<51, f03$state03, NA)
# f05$state05 <- ifelse(f05$state05>0 & f05$state05<51, f05$state05, NA)
# f07$state07 <- ifelse(f07$state07>0 & f07$state07<51, f07$state07, NA)
# f09$state09 <- ifelse(f09$state09>0 & f09$state09<51, f09$state09, NA)
# f11$state11 <- ifelse(f11$state11>0 & f11$state11<51, f11$state11, NA)
# f13$state13 <- ifelse(f13$state13>0 & f13$state13<51, f13$state13, NA)

# age

f01 <- subset(f01, f01$age01<999)
f03 <- subset(f03, f03$age03<999)
f05 <- subset(f05, f05$age05<999)
f07 <- subset(f07, f07$age07<999)
f09 <- subset(f09, f09$age09<999)
f11 <- subset(f11, f11$age11<999)
f13 <- subset(f13, f13$age13<999)

# ageSpouse

f01 <- subset(f01, f01$ageSpouse01<999)
f03 <- subset(f03, f03$ageSpouse03<999)
f05 <- subset(f05, f05$ageSpouse05<999)
f07 <- subset(f07, f07$ageSpouse07<999)
f09 <- subset(f09, f09$ageSpouse09<999)
f11 <- subset(f11, f11$ageSpouse11<999)
f13 <- subset(f13, f13$ageSpouse13<999)

# numDependents

f01 <- subset(f01, f01$numDepend01<98)
f03 <- subset(f03, f03$numDepend03<98)
f05 <- subset(f05, f05$numDepend05<98)
f07 <- subset(f07, f07$numDepend07<98)
f09 <- subset(f09, f09$numDepend09<98)
f11 <- subset(f11, f11$numDepend11<98)
f13 <- subset(f13, f13$numDepend13<98)

# interest rates (average annual discount rate from IMF, use the year before)

interestRates <- read.csv("Data/interestRates.csv", 
                    header = TRUE)


f03$interestRate03 <- subset(interestRates$INTDSRUSM193N,interestRates$DATE=="2002")
f05$interestRate05 <- subset(interestRates$INTDSRUSM193N,interestRates$DATE=="2004")
f07$interestRate07 <- subset(interestRates$INTDSRUSM193N,interestRates$DATE=="2006")
f09$interestRate09 <- subset(interestRates$INTDSRUSM193N,interestRates$DATE=="2008")
f11$interestRate11 <- subset(interestRates$INTDSRUSM193N,interestRates$DATE=="2010")
f13$interestRate13 <- subset(interestRates$INTDSRUSM193N,interestRates$DATE=="2012")


rm(cpiData, interestRates) #shortYears,years)


