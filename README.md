
gen k= 0.33
gen censored_score = deprivation_score
replace censored_score = 0 if deprivation_score < k
gen poor = (deprivation_score >= k)
replace poor = 1 if deprivation_score >= k
tab KEBELE
describe MARKETPRICE LAGGPRICE
gen gross_value_sorghum_sold = QUANSOLD * MARKETPRICE
gen gross_value_sorghum_produced = QUANSORGM * MARKETPRICE
gen hci_sorghum = ( gross_value_sorghum_sold / gross_value_sorghum_produced ) * 100
ssc install  movestay
ssc install doseresponse
help gpscore
tab MARTHH,gen(maritalstatus)
movestay mpi SEXHH AGEHH MARTHH EDLHH SIZEHH ADE FARMEXP TOTLAND MARKETPRICE LAGGPRICE MBLOWNER TLU OFFNONFARM CRACCESS TRAINSORGPRO COOP EXCONTACT ,select( hci_sorghum = SORLAND )
gen commercial = ( sorghum_hci > .481283)
label define commercial 0 "Non-commercial" 1 "Commercial"
label values commercial commercial
tabulate commercial
gen deprivation_score = QMP1*0.167+ QMP2*0.167+ QMP3*0.167+ QMP4*0.167+ QMP5*0.056+ QMP6*0.056+ QMP7*0.056+ QMP8*0.056+ QMP9*0.056+ QMP10*0.056
gen multidimensionally_poor = (deprivation_score >= poverty_cutoff) * 1
sum deprivation_score [iw = 4]
sum deprivation_score [iw = 4]
gen A = r(mean) * 100
gen MPI = H * A
gen d1 = QMP1+ QMP2
gen d2 = QMP3 + QMP4
gen d3 = QMP5 + QMP6+ QMP7+ QMP8+ QMP9+ QMP10
mpi d1 ( QMP1 QMP2 ) d2 ( QMP3 QMP4 ) d3( QMP5 QMP6 QMP7 QMP8 QMP9 QMP10 ) , cutoff(0.333)
movestay mpi SEXHH AGEHH MARTHH EDLHH SIZEHH ADE FARMEXP TOTLAND  LAGGPRICE MBLOWNER TLU OFFNONFARM CRACCESS TRAINSORGPRO COOP EXCONTACT DISINPUT DISMARKT DISROAD MARKTINFO ,select( commercial = SORLAND )
gen Education_dp = ( QMP1 + QMP2 )*1/6
gen Health_dp = ( QMP3 + QMP4 )*1/6
gen Livingstandard_dp = (QMP5 + QMP6+ QMP7+ QMP8+ QMP9+ QMP10 )*1/18
gen mpi = sum(censored_deprivation_score) / _N
gen mpi = sum( censored_deprivation_score ) / _N


*****final endogenous regression 
movestay DS SEXHH lnAGEHH MARTHH EDLHH SIZEHH  lnTOTLAND  lnQUANSORGM  LAGGPRICE MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP pp ,select( commercial=  MARKTINFO)
reg DS SEXHH lnAGEHH MARTHH EDLHH SIZEHH  lnTOTLAND  lnQUANSORGM  LAGGPRICE MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP pp   MARKTINFO
probit commercial SEXHH lnAGEHH MARTHH EDLHH SIZEHH  lnTOTLAND  lnQUANSORGM  LAGGPRICE MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP pp   MARKTINFO
reg DS SEXHH lnAGEHH MARTHH EDLHH SIZEHH  lnTOTLAND  lnQUANSORGM  LAGGPRICE MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP pp   MARKTINFO
movestay mpi_povery SEXHH lnAGEHH MARTHH EDLHH SIZEHH  lnTOTLAND  lnQUANSORGM  LAGGPRICE MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP pp ,select( commercial=  MARKTINFO)
probit commercial SEXHH lnAGEHH MARTHH EDLHH SIZEHH  lnTOTLAND  lnQUANSORGM  LAGGPRICE MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP pp   MARKTINFO
reg mpi_povery SEXHH lnAGEHH MARTHH EDLHH SIZEHH  lnTOTLAND  lnQUANSORGM  LAGGPRICE MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP pp   MARKTINFO
****testing continous variable mean difference
ttest EXCONTACT ,by( commercial )
*** ESR prediction
mspredict C11, yc1_1
mspredict C12, yc1_2
mspredict C21, yc2_1
mspredict C22, yc2_2
****handling missing observation
replace C11=0 if missing( C11)
replace C21=0 if missing( C21)
replace C12=0 if missing( C12)
replace C22=0 if missing( C22)
*****ttest for predicted outcome
ttest C11 == C21
ttest C12 == C22
ttest C11 == C12
ttest C21 == C22
*****hetrogenity effect
generate ATT= C11==C21
generate ATU = C12==C22
ttest ATT == ATU
**** additional 
reg mpi_povery SEXHH lnAGEHH MARTHH EDLHH SIZEHH  lnTOTLAND    MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP   DSAO
probit commercial SEXHH lnAGEHH MARTHH EDLHH SIZEHH  lnTOTLAND    MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP   DSAO
movestay mpi_povery SEXHH lnAGEHH MARTHH EDLHH SIZEHH  lnTOTLAND    MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP ,select( commercial=  DSAO

*****final additional 
asdoc movestay mpi_povery SEXHH lnAGEHH MARTHH  SIZEHH  lnTOTLAND    MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP pp ,select( commercial=  DSAO
asdoc reg mpi_povery SEXHH lnAGEHH MARTHH  SIZEHH  lnTOTLAND    MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP pp  DSAO
asdoc probit commercial SEXHH lnAGEHH MARTHH  SIZEHH  lnTOTLAND    MBLOWNER TLU OFFNONFARM EXCONTACT CRACCESS COOP pp  DSAO
