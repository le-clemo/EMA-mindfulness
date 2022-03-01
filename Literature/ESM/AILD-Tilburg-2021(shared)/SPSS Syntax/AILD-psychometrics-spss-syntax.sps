* Encoding: UTF-8.
*************************2021 AILD Workshop****************************
*COMMENT: Example from Bolger & Laurenceau book Chapter 7.
*COMMENT: Item1, Item2, Item3, and Item4 refer to “interested,” “determined,” “enthusiastic,”
and “inspired,” respectively. Ratings of each item were made on a scale from 1 (not at all) to 5 (extremely), 
and missing values are represented by –999. Data must be in the structure depicted in Figure 7.2 (see psychometrics.sav).

*COMMENT: Estimate Variance Components directly using ANOVA model.

VARCOMP Y BY Person Time Item 
  /RANDOM=Person Time Item
  /METHOD= MINQUE(1)
  /DESIGN=Person Time Item Item*Person Item*Time 
                    Person*Time.

*COMMENT: Estimate variance components for Person, Person*Item and 
          Person*Time using Mixed Model.

MIXED Y BY Time Item
  /FIXED=Time Item Time*Item | SSTYPE(3)
  /METHOD=REML
  /RANDOM=INTERCEPT Time Item | SUBJECT(Person).

*COMMENT: To go from long to wide for Mplus omega calculations.

SORT CASES BY person time .
CASESTOVARS
/ID = person time
/INDEX = item
/GROUPBY = INDEX .




