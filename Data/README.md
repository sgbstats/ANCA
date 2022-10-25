|Variable|Label|Type|Factor|Format|Derivation|Units|
|---|---|---|---|---|---|---|
|SNO|Study Number|Char||AAANNN|Centre Code + row number from 001 or original number as %3d<br />ANG: Angers <br />BAL: Baltimore (Johns Hopkins)<br />EDI: Edinburgh<br />GLA: Glasgow (Scotish Registry) <br />IRE: Irish Registry<br />IMP: Imperial<br />MAN: Manchester<br />MEX: Mexico City<br />PRA: Prague<br />PRE: Preston<br />SAL: Salford<br />ULU: Uludag||
|CENTRE|Centre|Char|||Centre Code||
|COHORT|Cohort|Char|TRUE||DEV or VAL (method TBC)||
|OLDCODE|Old Code|Char|||Source.`Code`||
|DX|Diagnosis|Char|TRUE||From DXN, <br />0=MPA<br />1=GPA<br />2=EGPA<br />3=ANCA negative<br />not  Otherwise=Other||
|DXN|Diagnosis N|Num|||Source.`Diagnosis`||
|AB|Antibody|Char|||From ABN,<br />0=Myeloperoxidase<br />1=Proteinase 3<br />2=ANCA negative<br />3=Double Positive<br />not  Otherwise=Other||
|ABN|Antibody N|Num|||Source.AB||
|SEX|Sex|Char|TRUE|MF|From SEXN<br />1=M<br />2=F||
|SEXN|Sex N|Num|||Source.`Gender`+1||
|AGEDX|Age at Diagnosis|Num|||PRES-DOB<br />or Source.`Age at diagnosis`<br />PRES and DOB have been removed for confidentiality|Years|
|HD|Initial need for RRT|Logical|||as.logical(source.`initial HD`)||
|HDC|Initial need for RRT C|Char||YN|From HD,<br />T=Y<br />F=N||
|CREATININE|Creatinine|Num|||Source.`Creatinine`|umol/l|
|EGFR|eGFR|Num|||Source.`eGFR`|ml/min/1.73m2|
|GLOM|Glomeruli on Biopsy|Num|||Source.`Glomeruli`||
|NORM|Normal Glomeruli|Num|||Source.`Normal`||
|PERCN|Percentage Normal Glomeruli|Num||.2d|100*GLOM/NORM||
|CRESC|Crescents|Num|||Source.`Crescents`||
|PERCC|Percentage Crescents|Num|||100*CRESC/GLOM||
|IFTA|IF/TA|Char|TRUE||From IFTAN,<br />0=None<br />1=Mild<br />2=Mild to Moderate <br />3=Moderate<br />4=Moderate to Severe<br />5=Severe||
|IFTAN|IFTA N|Num|||Source.`IFTA`||
|IFTAI|IFTA Imputed|Char|TRUE||IFTA if !is.na(IFTA)<br />None if IFTA=0 <br />Mild if IFTA<25<br />Mild - Moderate if IFTA<50<br />Moderate if IFTA<75<br />Moderate - Severe if IFTA<90<br />Severe if IFTA<=100<br />Missing otherwise||
|PERCIFTA|Percentage IFTA|Num|||Source.`IFTA (%)`||
|PERCIFTAI|Percentage IFTA Imputed|Num|||PERCIFTA if !is.na(PERCIFTA)<br />0 if IFTAN=0<br />16 if IFTAN=1<br />38 if IFTAN=2<br />62 if IFTAN=3<br />75 if IFTAN=4<br />90 if IFTAN=5||
|G|G Category|Char|TRUE||G0: EGFR>15<br />G1: EGFR<=15||
|GS|G Score|Num|||0: EGFR>15<br />3: EGFR<=15||
|N|N Category|Char|TRUE||N0: PERNC>25<br />N1: 10<PERCN<=25<br />N2: PERCN<=10||
|NS|N Score|Num|||0: PERNC>25<br />4: 10<PERCN<=25<br />6: PERCN<=10||
|TA|TA Category|Char|TRUE||T0: IFTAI in None, Mild to Moderate<br />T1: PERCIFTA in Moderate, Moderate to Severe, Severe||
|TAS|TA Score|Num|||0: IFTAI in None, Mild to Moderate<br />2: PERCIFTA in Moderate, Moderate to Severe, Severe||
|TASA|TA Senstitivity Analysis|Char|TRUE||T0: IFTAI in None, Mild to Moderate, Moderate<br />T1: PERCIFTA in Moderate to Severe, Severe||
|RRS|Renal Risk Score|Num|||GS+NS+TAS||
|RRSSA|Renal Risk Score Sensitivity Analysis|Num|||GS+NS+case_when(TASA=="T0"~0, TASA=="T1"~2)||
|RRSGR|Renal Risk Score Group|Char|TRUE||Low: RRS=0<br />Moderate: 2<=RRS<=7<br />High: RRS>=8||
|TTELSVISIT|Time to Event Last Visit|Num|||LSVISIT-PRES (Dates deleted for confidentiality)|Years|
|DTH|Death|Logical|||as.logical(Source.Mortality)||
|DTHC|Death C|Char||YN|From DTH,<br />T=Y<br />F=N||
|TTEDTH|Time to Event Death|Num|||Source.`Time of Death`|Years|
|TTEDTHCNSR|Time to Event Death Censored|Num|||If DTH: TTEDTH, Else TTELSVISIT|Years|
|ESKD|End Stage Kidney Disease|Logical|||as.logical(source.`ESKD`)||
|ESKDC|End Stage Kidney Disease C|Char||YN|From ESKD,<br />T=Y<br />F=N||
|TTEESKD|Time to Event ESKD|Num|||Source.`Time of ESKD`|Years|
|TTEESKDCNSR|Time to Event ESKD Censored|Num|||If ESKD, TTEESKD, else TTELSVISIT|Years|
|REC|Recovery|Logical|||as.logical(source.`Recovery`)||
|RECC|Recovery C|Char||YN|From REC,<br />T=Y<br />F=N||
|TTEREC|Time to Event Recovery|Num|||Source.`Time to Recovery`|Weeks|
|TTERECCNSR|Time to Event Recovery Censored|Num|||If REC, TTEREC,  else, TTELSVISIT|Weeks|
