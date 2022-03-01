* Encoding: UTF-8.
*SPSS syntax for AILD 2021 Dyads. 
*Note: The syntax below requires that dyads.sav is the active dataset.

MIXED reldis WITH male female time7c wrkstrscw wrkstrscb
  /FIXED=female male male*time7c female*time7c male*wrkstrscw female*wrkstrscw male*wrkstrscb female*wrkstrscb  | NOINT SSTYPE(3)
  /METHOD=ML
  /PRINT=G SOLUTION R TESTCOV
  /RANDOM=female male male*wrkstrscw female*wrkstrscw | SUBJECT(coupleid) COVTYPE(UN)
  /REPEATED=time | SUBJECT(coupleid*gender) COVTYPE(ar1)
  /TEST =  'm v f intercept' male 1 female -1
  /TEST =  'm v f  within slopes' female*wrkstrscw 1 male*wrkstrscw -1 
  /TEST =  'm v f  between slopes' female*wrkstrscb 1 male*wrkstrscb -1
  /TEST =  'm v f  time slopes' female*time7c 1 male*time7c -1.

*Comment: On /REPEATED, if there is no evidence of autocorrelation, COVETYPE(ID) can be substituted for COVTYPE(ar1).

