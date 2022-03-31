* Encoding: UTF-8.
********************************************************************************************.
*SEP inequalities in BP.
*Lancet 2014.
*The Dinamap to Omron translations for adults aged 16 years and older were derived as follows: 
*predicted Omron=8·90(SE 2·94)+0·91(SE 0·02)*Dinamap for systolic blood pressure; and 
*predicted Omron=19·78(SE 1·86)+0·73(SE 0·03)*Dinamap for diastolic blood pressure.
*****************************************************************************************

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse94ai2nd.sav"
/keep age sex area newsyst newdiast respbp topqual sclass medcnjb med1 to med16
med1a
med2a
med3a
med4a
med5a
med6a
med7a
med8a
med9a
med10a
med11a
med12a
med13a
med14a
med15a
med16a bmi nuroutc visyr.

compute bmival2=bmi.
EXECUTE.

rename variables (medcnjb = medcnjd).
exe.

compute psu99=area.
EXECUTE.

* need to recode this to be consistent with other years.

sel if age>=16.
sel if respbp=1.
sel if newsyst>0.

compute topqual5=-2.
if topqual=1 topqual5=1.
if any(topqual,2,3) topqual5=2.
if any(topqual,4,5) topqual5=3.
if topqual=8 topqual5=4.
if any(topqual,6,7,9) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
fre topqual5.

* BPrespc (whether BP readings are valid).

* Dinamap to Omron.

DO IF respbp=1 & age>=16.
compute omsysval=(newsyst*0.91)+8.90.
compute omdiaval=(newdiast*0.73)+19.78.
end if.
exe.

compute wt_nurse=1.
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="1994".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.

compute HSEyear=1.
exe.

* BP medication?

*fre medcnjb med1.

compute bpmedd=0.
if any(med1,1,2,3,4,7) &  (med1a=1) bpmedd=1.
if any(med2,1,2,3,4,7) &  (med2a=1) bpmedd=1.
if any(med3,1,2,3,4,7) &  (med3a=1) bpmedd=1.
if any(med4,1,2,3,4,7) &  (med4a=1) bpmedd=1.
if any(med5,1,2,3,4,7) &  (med5a=1) bpmedd=1.
if any(med6,1,2,3,4,7) &  (med6a=1) bpmedd=1.
if any(med7,1,2,3,4,7) &  (med7a=1) bpmedd=1.
if any(med8,1,2,3,4,7) &  (med8a=1) bpmedd=1.
if any(med9,1,2,3,4,7) &  (med9a=1) bpmedd=1.
if any(med10,1,2,3,4,7) &  (med10a=1) bpmedd=1.
if any(med11,1,2,3,4,7) &  (med11a=1) bpmedd=1.
if any(med12,1,2,3,4,7) &  (med12a=1) bpmedd=1.
if any(med13,1,2,3,4,7) &  (med13a=1) bpmedd=1.
if any(med14,1,2,3,4,7) &  (med14a=1) bpmedd=1.
if any(med15,1,2,3,4,7) &  (med15a=1) bpmedd=1.
fre bpmedd.

* Hypertension.

compute HighBP4=-2.
IF (BPmedd=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmedd=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmedd=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmedd=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
fre highBP4 bpmedd.

select if highBP4>=0.
exe.

compute ag16g10=0.
if range(age,16,24) ag16g10=1.
if range(age,25,34) ag16g10=2.
if range(age,35,44) ag16g10=3.
if range(age,45,54) ag16g10=4.
if range(age,55,64) ag16g10=5.
if range(age,65,74) ag16g10=6.
if range(age,75,144) ag16g10=7.
EXECUTE.

save outfile = "N:\Temp\Temp1.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval topqual5 highBP4 bpmedd ag16g10 bmival2.

* 1995.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse95ai2nd.sav"
/keep age sex area newsyst newdiast bprespb2
qual 
qual
quala1
quala2
quala3
quala4
quala5
quala6
quala7
quala8
quala9
quala10 sclass
medbi01 to medbi15
medbai01
medbai02
medbai03
medbai04
medbai05
medbai06
medbai07
medbai08
medbai09
medbai10
medbai11
medbai12
medbai13
medbai14
medbai15 medcnjc nuroutc bmi.

compute bmival2=bmi.
exe.

rename variables (medcnjc = medcnjd).
exe.

rename variables (medbai01 medbai02 medbai03 medbai04 medbai05 = ytake012 ytake022 ytake032 ytake042 ytake052).
rename variables (medbai06 medbai07 medbai08 medbai09 medbai10 = ytake062 ytake072 ytake082 ytake092 ytake102).
rename variables (medbai11 medbai12 medbai13 medbai14 medbai15 = ytake112 ytake122 ytake132 ytake142 ytake152).
exe.

compute psu99=area.
EXECUTE.

sel if age>=16.
exe.
COMPUTE topqual3=0.
IF (qual<0 | (qual=1 & quala1<0)) topqual3=quala1.
IF (qual<0 | (qual=1 & quala1<0)) topqual2=quala1.
VECTOR veduc=quala1 TO quala10.
LOOP #I=1 TO 10.
IF (ANY(veduc(#I),1,23,24)) topqual3=1.
IF (ANY(veduc(#I),2,3,4,6) & topqual3<>1) topqual3=2.
IF (ANY(veduc(#I),5,7,9,10,11,25) & ~RANGE(topqual3,1,2)) topqual3=3.
IF (ANY(veduc(#I),8,12,13,15,17,20,22,26) & ~RANGE(topqual3,1,3)) topqual3=4.
IF (ANY(veduc(#I),14,16,18,21,27,28) & ~RANGE(topqual3,1,4)) topqual3=5.
IF (RANGE(veduc(#I),29,36) & ~RANGE(topqual3,1,5)) topqual3=6.
IF ((veduc(#I)=19 | qual=2) & ~RANGE(topqual3,1,6)) topqual3=7.
END LOOP.
VARIABLE LABEL topqual3 "(D) Highest Educational Qualification".
VALUE LABELS topqual3
1 'NVQ4/NVQ5/Degree or equiv'
2 'Higher ed below degree'
3 'NVQ3/GCE A Level equiv'
4 'NVQ2/GCE O Level equiv'
5 'NVQ1/CSE other grade equiv'
6 'Foreign/other'
7 'No qualification'.
EXECUTE.

compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
fre topqual5.

sel if bprespb2=1.
exe.
sel if newsyst>0.
exe.

* Dinamap to Omron.

DO IF bprespb2=1 & age>=16.
compute omsysval=(newsyst*0.91)+8.90.
compute omdiaval=(newdiast*0.73)+19.78.
end if.
exe.

compute wt_nurse=1.
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="1995".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.

compute HSEyear=2.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152  (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.

* create new variable.

compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.

compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.


compute ag16g10=0.
if range(age,16,24) ag16g10=1.
if range(age,25,34) ag16g10=2.
if range(age,35,44) ag16g10=3.
if range(age,45,54) ag16g10=4.
if range(age,55,64) ag16g10=5.
if range(age,65,74) ag16g10=6.
if range(age,75,144) ag16g10=7.
EXECUTE.

select if highBP4>=0.
EXECUTE.
save outfile = "N:\Temp\Temp2.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval topqual3 topqual5 highBP4 bpmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).

* 1996.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse96bind.sav"
/keep age sex area newsyst newdiast bprespb2 qual 
qual
quala1
quala2
quala3
quala4
quala5
quala6
quala7
quala8
quala9
quala10 sclass
medbi01 to medbi15
medbai01
medbai02
medbai03
medbai04
medbai05
medbai06
medbai07
medbai08
medbai09
medbai10
medbai11
medbai12
medbai13
medbai14
medbai15 medcnjc bmi nuroutc.

compute bmival2=bmi.
exe.
rename variables ( medcnjc = medcnjd).
exe.

rename variables (medbai01 medbai02 medbai03 medbai04 medbai05 = ytake012 ytake022 ytake032 ytake042 ytake052).
rename variables (medbai06 medbai07 medbai08 medbai09 medbai10 = ytake062 ytake072 ytake082 ytake092 ytake102).
rename variables (medbai11 medbai12 medbai13 medbai14 medbai15 = ytake112 ytake122 ytake132 ytake142 ytake152).
exe.

compute psu99=area.
EXECUTE.

COMPUTE topqual3=0.
IF (qual<0 | (qual=1 & quala1<0)) topqual3=quala1.
IF (qual<0 | (qual=1 & quala1<0)) topqual2=quala1.
VECTOR veduc=quala1 TO quala10.
LOOP #I=1 TO 10.
IF (ANY(veduc(#I),1,23,24)) topqual3=1.
IF (ANY(veduc(#I),2,3,4,6) & topqual3<>1) topqual3=2.
IF (ANY(veduc(#I),5,7,9,10,11,25) & ~RANGE(topqual3,1,2)) topqual3=3.
IF (ANY(veduc(#I),8,12,13,15,17,20,22,26) & ~RANGE(topqual3,1,3)) topqual3=4.
IF (ANY(veduc(#I),14,16,18,21,27,28) & ~RANGE(topqual3,1,4)) topqual3=5.
IF (RANGE(veduc(#I),29,36) & ~RANGE(topqual3,1,5)) topqual3=6.
IF ((veduc(#I)=19 | qual=2) & ~RANGE(topqual3,1,6)) topqual3=7.
END LOOP.
VARIABLE LABEL topqual3 "(D) Highest Educational Qualification".
VALUE LABELS topqual3
1 'NVQ4/NVQ5/Degree or equiv'
2 'Higher ed below degree'
3 'NVQ3/GCE A Level equiv'
4 'NVQ2/GCE O Level equiv'
5 'NVQ1/CSE other grade equiv'
6 'Foreign/other'
7 'No qualification'.
exe.

sel if age>=16.
exe.

compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.

sel if bprespb2=1.
exe.
sel if newsyst>0.
exe.

* Dinamap to Omron.

DO IF bprespb2=1 & age>=16.
compute omsysval=(newsyst*0.91)+8.90.
compute omdiaval=(newdiast*0.73)+19.78.
end if.
exe.

compute wt_nurse=1.
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="1996".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.

compute HSEyear=3.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152  (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.

* create new variable.

compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.

compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
EXECUTE.

compute ag16g10=0.
if range(age,16,24) ag16g10=1.
if range(age,25,34) ag16g10=2.
if range(age,35,44) ag16g10=3.
if range(age,45,54) ag16g10=4.
if range(age,55,64) ag16g10=5.
if range(age,65,74) ag16g10=6.
if range(age,75,144) ag16g10=7.
EXECUTE.

save outfile = "N:\Temp\Temp3.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval topqual3 topqual5 highBP4 bpmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).

* 1997.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse97ai.sav"
/keep age sex area newsyst newdiast bprespb2 topqual3 sclass medbi01 to medbi15 medcnjd
medbia01
medbia02
medbia03
medbia04
medbia05
medbia06
medbia07
medbia08
medbia09
medbia10
medbia11
medbia12
medbia13
medbia14
medbia15 ag16g10 bmival nuroutc.

compute bmival2=bmival.
exe.

rename variables (medbia01 medbia02 medbia03 medbia04 medbia05 = ytake012 ytake022 ytake032 ytake042 ytake052).
rename variables (medbia06 medbia07 medbia08 medbia09 medbia10 = ytake062 ytake072 ytake082 ytake092 ytake102).
rename variables (medbia11 medbia12 medbia13 medbia14 medbia15 = ytake112 ytake122 ytake132 ytake142 ytake152).
exe.

compute psu99=area.
EXECUTE.

sel if age>=16.
exe.

compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.

sel if bprespb2=1.
exe.
sel if newsyst>0.
exe.

* Dinamap to Omron.

DO IF bprespb2=1 & age>=16.
compute omsysval=(newsyst*0.91)+8.90.
compute omdiaval=(newdiast*0.73)+19.78.
end if.
EXECUTE.

compute wt_nurse=1.
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="1997".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.

compute HSEyear=4.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152  (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.

* create new variable.

compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.

compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
EXECUTE.

save outfile = "N:\Temp\Temp4.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval topqual5 BPmeds highBP4 ag16g10 bmival2
/rename (bpmeds = bpmedd).


* 1998.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse98ai.sav"
/keep age sex area newsyst newdiast bprespc topqual3 sclass bpmedd medbi01 to medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd ag16g10 bmival nuroutc.

compute bmival2=bmival.
exe.

rename variables (bpmedd = bpmedd5).
exe.
compute psu99=area.
EXECUTE.

* BPrespc (whether BP readings are valid).

sel if bprespc=1 & age>=16.
exe.

compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.

* Dinamap to Omron.

DO IF bprespc=1 & age>=16.
compute omsysval=(newsyst*0.91)+8.90.
compute omdiaval=(newdiast*0.73)+19.78.
end if.
EXECUTE.

compute wt_nurse=1.
exe.
FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="1998".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.

compute HSEyear=5.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi22.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi22.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.

* create new variable.

compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.

compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
exe.

save outfile = "N:\Temp\Temp5.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval topqual3 topqual5 highBP4 bpmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).


* 2000.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse00ai.sav"
/keep age sex area newsyst newdiast bprespc samptype topqual3 sclass newsyst newdiast
bpmedd medbi01 to medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd ag16g10 bmival nuroutc.
compute bmival2=bmival.
exe.
rename variables (bpmedd = bpmedd5).
exe.

* Dinamap to Omron.

DO IF bprespc=1 & age>=16.
compute omsysval=(newsyst*0.91)+8.90.
compute omdiaval=(newdiast*0.73)+19.78.
end if.
EXECUTE.

compute psu99=area.
EXECUTE.
sel if samptype=2 & age>=16.
exe.

sel if bprespc=1.
exe.

* Dinamap to Omron.

sel IF bprespc=1 & age>=16.
exe.

compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.

compute wt_nurse=1.
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2000".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.
compute HSEyear=7.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi22.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi22.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.
* create new variable.

compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.

compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
exe.
save outfile = "N:\Temp\Temp6.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval topqual3 topqual5 highBP4 bpmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).


* 2001.
DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse01ai.sav"
/keep newsyst age ag16g10 sex area bprespc topqual3 sclass newsyst newdiast
bpmedd medbi01 to medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd ag16g10 bmival nuroutc.
compute bmival2=bmival.
exe.

rename variables (bpmedd = bpmedd5).
exe.

* Dinamap to Omron.

DO IF bprespc=1 & age>=16.
compute omsysval=(newsyst*0.91)+8.90.
compute omdiaval=(newdiast*0.73)+19.78.
end if.
EXECUTE.

compute psu99=area.
EXECUTE.

sel if bprespc=1 & age>=16.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.
compute wt_nurse=1.
exe.
FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2001".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.
compute HSEyear=8.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi22.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.

* create new variable.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.

compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.
select if highBP4>=0.
EXECUTE.
save outfile = "N:\Temp\Temp7.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval topqual3 topqual5 HighBP4 bpmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).


* 2002.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse02ai.sav"
/keep newsyst age ag16g10 sex area bprespc samptype topqual3 sclass
newsyst newdiast
bpmedd medbi01 to medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd ag16g10 bmival nuroutc.
compute bmival2=bmival.
exe.
rename variables (bpmedd = bpmedd5).
exe.

* Dinamap to Omron.

DO IF bprespc=1 & age>=16.
compute omsysval=(newsyst*0.91)+8.90.
compute omdiaval=(newdiast*0.73)+19.78.
end if.
EXECUTE.
compute psu99=area.
EXECUTE.
sel if bprespc=1 & samptype=2 & age>=16.
exe.

compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.

compute wt_nurse=1.
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2002".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.
compute HSEyear=9.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi22.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.

* create new variable.

compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.
compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.
select if highBP4>=0.
exe.
save outfile = "N:\Temp\Temp8.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval topqual3 topqual5 highBP4 bpmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).

* Blood pressure (2003). (n=9122 wtd; N=9183 unwtd).

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse03ai.sav"
/KEEP area cluster sex age nurse_wt
hyper1om hyper2om
hy140om
hibp1om
hibp2om
hbp140om
hyper1di
hyper2di
hy140di
hibp1di
hibp2di hbp140di omsysval omdiaval topqual3 sclass bpmedd 
medbi01 to medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd ag16g10 bmival nuroutc.

compute bmival2=bmival.
exe.
rename variables (bpmedd = bpmedd5).
EXECUTE.
compute psu99=area.
EXECUTE.
select if age>=16.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.
sel if hy140om>=0.
exe.
rename var(nurse_wt=wt_nurse).
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2003".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.

compute HSEyear=10.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi22.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.

* create new variable.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: prescribed high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.
compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
exe.

save outfile = "N:\Temp\Temp9.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval hy140om topqual3 topqual5 highBP4 BPmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).

* Blood pressure (2005). (n=4308).

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse05ai.sav"
/KEEP area cluster sex age wt_nurse
hyper1om
hyper2om
hy140om
hibp1om
hibp2om
hbp140om
hyper1di
hyper2di
hy140di
hibp1di
hibp2di
hbp140di samptype omsysval omdiaval topqual3 sclass
bpmedd 
medbi01 to medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd ag16g10 bmival nuroutc.

compute bmival2=bmival.
exe.

rename variables (bpmedd = bpmedd5).
EXECUTE.
compute psu99=area.
EXECUTE.
select if samptype=1.
exe.
select if age>=16.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.
sel if hy140om>=0.
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2005".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.
compute HSEyear=12.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi22.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.
* create new variable.

compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.

compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
EXECUTE.

save outfile = "N:\Temp\Temp10.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval hy140om topqual3 topqual5 highBP4 BPmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).

* 2006.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse06ai.sav"
/KEEP psu sex age wt_nurse
hyper1om
hyper2om
hy140om
hibp1om
hibp2om
hbp140om
hyper1di
hyper2di
hy140di
hibp1di
hibp2di
hbp140di omsysval omdiaval topqual3 sclass
bpmedd 
medbi01 to medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd ag16g10 bmival nuroutc.
compute bmival2=bmival.
exe.
rename variables (bpmedd = bpmedd5).
EXECUTE.
compute psu99=psu.
EXECUTE.

select if age>=16.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.
sel if hy140om>=0.
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2006".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.
compute HSEyear=13.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi22.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.

* create new variable.

compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.

compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
EXECUTE.

save outfile = "N:\Temp\Temp11.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval hy140om topqual3 topqual5 highBP4 BPmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).

* 2007.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse07ai.sav"
/KEEP area sex age wt_nurse
hyper1om
hyper2om
hy140om
hibp1om
hibp2om
hbp140om
hyper1di
hyper2di
hy140di
hibp1di
hibp2di
hbp140di omsysval omdiaval topqual3 sclass
bpmedd 
medbi01 to medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd ag16g10 bmival nuroutc.
compute bmival2 = bmival.
exe.
rename variables (bpmedd = bpmedd5).
EXECUTE.
compute psu99=area.
EXECUTE.
select if age>=16.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.

sel if hy140om>=0.
exe.
FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2007".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.
compute HSEyear=14.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi22.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.

* create new variable.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.

compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
exe.

save outfile = "N:\Temp\Temp12.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval hy140om topqual3 topqual5 highBP4 BPmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).


* 2008.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse08ai.sav"
/KEEP psu sex age wt_nurse
hyper1om
hyper2om
hy140om
hibp1om
hibp2om
hbp140om
hyper1di
hyper2di
hy140di
hibp1di
hibp2di
hbp140di omsysval omdiaval topqual3 sclass
bpmedd 
medbi01 to medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd ag16g10 bmival nuroutc.
compute bmival2=bmival.
exe.
rename variables (bpmedd = bpmedd5).
EXECUTE.
compute psu99=psu.
EXECUTE.
select if age>=16.
exe.

compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.
sel if hy140om>=0.
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2008".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.
compute HSEyear=15.
EXECUTE.

DO REPEAT xxdrug=diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd.
COMPUTE xxdrug=0.
RECODE medbi01(-9 thru -1=COPY) INTO xxdrug.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi22.
IF xxcode=0 diur=-9.
IF xxcode=0 beta =-9.
IF xxcode=0 aceinh =-9.
IF xxcode=0 calciumb =-9.
IF xxcode=0 iron =-9.
IF xxcode=0 lipid =-9.
IF xxcode=0 obpdrug =-9.
IF xxcode=0 bpmedc=-9.
IF xxcode=0 bpmedd=-9.
END REPEAT.
DO REPEAT xxcode=medbi01 to medbi15.
IF RANGE(xxcode,20201,20208) diur=1.
IF xxcode=20400 beta=1.
IF xxcode=20505 aceinh=1.
IF xxcode=20602 calciumb=1.
IF ANY(xxcode,20501,20502,20503,20504,20506) obpdrug=1.
IF xxcode=21200 lipid=1.
IF xxcode=90101 iron=1.
END REPEAT.
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) bpmedc=1.
COUNT #bpdrug=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 (1).
IF ANY(1,diur,beta,aceinh,calciumb,obpdrug) & #bpdrug>0 bpmedd=1.
VARIABLE LABELS diur "(D) Diuretics (Blood pressure)".
VARIABLE LABELS beta "(D) Beta blockers (Blood pressure/Fibrinogen)".
VARIABLE LABELS aceinh "(D) Ace inhibitors (Blood pressure)".
VARIABLE LABELS calciumb "(D) Calcium blockers (Blood pressure)".
VARIABLE LABELS obpdrug "(D) Other drugs affecting BP" .
VARIABLE LABELS lipid "(D) Lipid lowering (Cholesterol/Fibrinogen)" .
VARIABLE LABELS iron "(D) Iron deficiency (Haemoglobin/Ferritin)" .
VARIABLE LABELS bpmedc "(D) Whether taking drugs affecting blood pressure".
VARIABLE LABELS bpmedd "(D) Whether taking drugs prescribed for blood pressure".
VALUE LABELS diur beta aceinh calciumb obpdrug lipid iron bpmedc bpmedd
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.

* create new variable.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.
compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.
select if highBP4>=0.
EXECUTE.
save outfile = "N:\Temp\Temp13.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval hy140om topqual3 topqual5 highBP4 BPmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).


* 2009.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse09ai.sav"
/KEEP psu sex age wt_nurse
hyper1om
hyper2om
hy140om
hibp1om
hibp2om
hbp140om
hyper1di
hyper2di
hy140di
hibp1di
hibp2di
hbp140di omsysval omdiaval topqual3 sclass
medbi01 to medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd bpmedd ag16g10 bmival nuroutc.

compute bmival2=bmival.
exe.
compute psu99=psu.
EXECUTE.

select if age>=16.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.
sel if hy140om>=0.
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2009".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.
compute HSEyear=16.
EXECUTE.

* create new variable.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.
compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
exe.
save outfile = "N:\Temp\Temp14.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval hy140om topqual3 topqual5 highBP4 BPmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).


* 2010.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse10ai.sav"
/KEEP psu sex age wt_nurse
hyper1om
hyper2om
hy140om
hibp1om
hibp2om
hbp140om
hyper1di
hyper2di
hy140di
hibp1di
hibp2di
hbp140di omsysval omdiaval topqual3
medbi01 to medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd bpmedd ag16g10 bmival nuroutc.

compute bmival2=bmival.
exe.
compute psu99=psu.
EXECUTE.

select if age>=16.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.

sel if hy140om>=0.
exe.

FORMAT psu99 (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu99,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2010".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.

compute HSEyear=17.
EXECUTE.

* create new variable.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.
compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
exe.

save outfile = "N:\Temp\Temp15.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval hy140om topqual3 topqual5 highBP4 BPmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).

* 2011.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse2011ai.sav"
/KEEP psu sex age wt_nurse omsysval omdiaval hy140om2 topqual3
medbi01 to medbi15
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd bpmedd ag16g10 bmival nuroutc.

compute bmival2=bmival.
exe.

STRING point1 (A8).
COMPUTE point1 = STRING(psu, F7.0).
EXECUTE.

select if age>=16.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.
sel if hy140om2>=0.
exe.
compute HSEyear=18.
EXECUTE.

* create new variable.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd=0) BPmeds=0.
if (medcnjd=1) & (BPmedd=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.
compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

missing values hy140om2 bpmedd ().
exe.
select if highBP4>=0.
exe.
save outfile = "N:\Temp\Temp16.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval hy140om2 topqual3 topqual5 highBP4 BPmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).

* 2012.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse2012ai.sav"
/KEEP psu sex age wt_nurse omsysval omdiaval hy140om2 topqual3 sclass
medbi01 to medbi15
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd bpmedd2 ag16g10 bmival2 nuroutc.

compute psu99=psu.
EXECUTE.

* recode numeric to string.
STRING point1 (A8).
COMPUTE point1 = STRING(psu99, F7.0).
EXECUTE.

select if age>=16.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.

sel if hy140om2>=0.
exe.
compute HSEyear=19.
EXECUTE.

* create new variable.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd2=0) BPmeds=0.
if (medcnjd=1) & (BPmedd2=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.
compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.
select if highBP4>=0.
exe.

save outfile = "N:\Temp\Temp17.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval hy140om2 topqual3 topqual5 highBP4 BPmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).

* 2013.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse2013ai.sav"
/KEEP psu sex age wt_nurse omsysval omdiaval hy140om2 topqual3 sclass
medbi01 to medbi15
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd bpmedd2 ag16g10 bmival2 nuroutc.

compute psu99=psu.
EXECUTE.

STRING point1 (A8).
COMPUTE point1 = STRING(psu99, F7.0).
EXECUTE.
select if age>=16.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.
sel if hy140om2>=0.
exe.
compute HSEyear=20.
EXECUTE.

* create new variable.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd2=0) BPmeds=0.
if (medcnjd=1) & (BPmedd2=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.

compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.
select if highBP4>=0.
exe.
save outfile = "N:\Temp\Temp18.sav"
/keep point1 HSEyear sex age wt_nurse omsysval omdiaval hy140om2 topqual3 topqual5 highBP4 BPmeds ag16g10 bmival2
/rename (bpmeds = bpmedd).

* 2014.
dataset close all.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\HSE2014ai.sav"
/KEEP psu sex ag16g10 wt_nurse omsysval omdiaval hy140om2 topqual3 sclass
medbi01 to medbi15
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd bpmedd2 ag16g10 age90 bmival2 nuroutc.

compute age=age90.
exe.
compute psu99=psu.
EXECUTE.

STRING point1 (A8).
COMPUTE point1 = STRING(psu99, F7.0).
EXECUTE.
select if ag16g10>=1.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.
sel if hy140om2>=0.
exe.
compute HSEyear=21.
EXECUTE.

* create new variable.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd2=0) BPmeds=0.
if (medcnjd=1) & (BPmedd2=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.
compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.
select if highBP4>=0.
exe.
save outfile = "N:\Temp\Temp19.sav"
/keep point1 HSEyear sex ag16g10 wt_nurse omsysval omdiaval hy140om2 topqual3 topqual5 highBP4 BPmeds ag16g10 age bmival2
/rename (bpmeds = bpmedd).

* 2015.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\HSE2015ai.sav"
/KEEP psu sex ag16g10 wt_nurse omsysval omdiaval hy140om2 topqual3 sclass medcnjd bpmedd2
hyper1om2
hyper2om2
hy140om2
hibp1om2
hibp2om2
hbp140om2 ag16g10 age16g5 bmival2 nuroutc.
compute age=0.
exe.
if age16g5=1 age=16.5.
if age16g5=2 age=18.5.
if age16g5=3 age=22.
if age16g5=4 age=27.
if age16g5=5 age=32.
if age16g5=6 age=37.
if age16g5=7 age=42.
if age16g5=8 age=47.
if age16g5=9 age=52.
if age16g5=10 age=57.
if age16g5=11 age=62.
if age16g5=12 age=67.
if age16g5=13 age=72.
if age16g5=14 age=77.
if age16g5=15 age=82.
if age16g5=16 age=87.
if age16g5=17 age=90.
EXECUTE.
compute psu99=psu.
EXECUTE.

STRING point1 (A8).
COMPUTE point1 = STRING(psu99, F7.0).
EXECUTE.
select if ag16g10>=1.
exe.

compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.
sel if hy140om2>=0.
exe.
compute HSEyear=22.
EXECUTE.

* create new variable.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd2=0) BPmeds=0.
if (medcnjd=1) & (BPmedd2=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.
compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
exe.
save outfile = "N:\Temp\Temp20.sav"
/keep point1 HSEyear sex ag16g10 wt_nurse omsysval omdiaval hy140om2 topqual3 topqual5 highBP4 BPmeds ag16g10 age16g5 age bmival2
/rename (bpmeds = bpmedd).


* 2016.

DATASET CLOSE ALL.
GET FILE="N:\Blood Pressure\SEP and BP\Datasets\hse2016_eul.sav"
/KEEP psu sex ag16g10 wt_nurse omsysval omdiaval hy140om2 topqual3 sclass bpmedd2 medcnjd hy140om2 PregNowB
PregNTJ
NCPregJ age16g5 bmival2 nuroutc.
compute age=0.
exe.
if age16g5=1 age=16.5.
if age16g5=2 age=18.5.
if age16g5=3 age=22.
if age16g5=4 age=27.
if age16g5=5 age=32.
if age16g5=6 age=37.
if age16g5=7 age=42.
if age16g5=8 age=47.
if age16g5=9 age=52.
if age16g5=10 age=57.
if age16g5=11 age=62.
if age16g5=12 age=67.
if age16g5=13 age=72.
if age16g5=14 age=77.
if age16g5=15 age=82.
if age16g5=16 age=87.
if age16g5=17 age=90.
EXECUTE.
select if ag16g10>=1.
exe.
compute psu99=psu.
EXECUTE.

STRING point1 (A8).
COMPUTE point1 = STRING(psu99, F7.0).
EXECUTE.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
exe.
sel if hy140om2>=0.
exe.
compute HSEyear=23.
EXECUTE.
compute BPmeds=-2.
if (medcnjd=2) BPmeds=0.
if (medcnjd=1) & (BPmedd2=0) BPmeds=0.
if (medcnjd=1) & (BPmedd2=1) BPmeds=1.
value labels BPmeds 0 "No" 1 "Yes: Prescribed for high BP".
EXECUTE.
missing values BPmeds (-2).
exe.

* Hypertension.
compute HighBP4=-2.
IF (BPmeds=0) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=1.
IF (BPmeds=1) & RANGE(omsysval,0,139.999) & RANGE(omdiaval,0,89.999) HighBP4=2.
IF (BPmeds=1) & (omsysval>=140 | omdiaval>=90) HighBP4=3.
IF (BPmeds=0) & (omsysval>=140 | omdiaval>=90) HighBP4=4.
VARIABLE LABELS HighBP4 "(D) Hypertensive categories: all prescribed drugs for BP (Omron readings)" .
VALUE LABELS HighBP4
1 'Normotensive untreated'
2 'Normotensive treated'
3 'Hypertensive treated'
4 'Hypertensive untreated'.
exe.

select if highBP4>=0.
exe.
save outfile = "N:\Temp\Temp21.sav"
/keep point1 HSEyear sex ag16g10 wt_nurse omsysval omdiaval hy140om2 topqual3 topqual5 highBP4 BPmeds age16g5 age bmival2
/rename (bpmeds = bpmedd).


dataset close all.
get file = "N:\Temp\Temp1.sav".
add files/file=*/file= "N:\Temp\Temp2.sav".
add files/file=*/file= "N:\Temp\Temp3.sav".
add files/file=*/file= "N:\Temp\Temp4.sav".
add files/file=*/file= "N:\Temp\Temp5.sav".
add files/file=*/file= "N:\Temp\Temp6.sav".
add files/file=*/file= "N:\Temp\Temp7.sav".
add files/file=*/file= "N:\Temp\Temp8.sav".
add files/file=*/file= "N:\Temp\Temp9.sav".
add files/file=*/file= "N:\Temp\Temp10.sav".
add files/file=*/file= "N:\Temp\Temp11.sav".
add files/file=*/file= "N:\Temp\Temp12.sav".
add files/file=*/file= "N:\Temp\Temp13.sav".
add files/file=*/file= "N:\Temp\Temp14.sav".
add files/file=*/file= "N:\Temp\Temp15.sav".
add files/file=*/file= "N:\Temp\Temp16.sav".
add files/file=*/file= "N:\Temp\Temp17.sav".
add files/file=*/file= "N:\Temp\Temp18.sav".
add files/file=*/file= "N:\Temp\Temp19.sav".
add files/file=*/file= "N:\Temp\Temp20.sav".
add files/file=*/file= "N:\Temp\Temp21.sav".
exe.
SAVE TRANSLATE OUTFILE='N:\Temp\HSE1994_2016_BP.dta'
  /TYPE=STATA
  /VERSION=8
  /EDITION=SE
  /MAP
  /REPLACE.




















