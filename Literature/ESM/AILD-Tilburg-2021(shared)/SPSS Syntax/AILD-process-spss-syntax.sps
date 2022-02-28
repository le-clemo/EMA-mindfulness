* Encoding: UTF-8.

*****************2021 AILD Workshop*********************
**********************Process example*****************
*COMMENT: Panels of intimacy over time by id.
*COMMENT: Upper panel relqual=0 (low relqual); Lower panel relqual=1 (high relqual)

SORT CASES  BY relqual.
SPLIT FILE SEPARATE BY relqual.
formats time intimacy (f4.2) relqual (f1.0).
GGRAPH 
  /GRAPHDATASET NAME="GraphDataset" VARIABLES= intimacy time id
  /GRAPHSPEC SOURCE=INLINE INLINETEMPLATE=["<setWrapPanels/>"].
BEGIN GPL
SOURCE: s=userSource( id( "GraphDataset" ) )
DATA: intimacy=col( source(s), name( "intimacy" ) )
DATA: time=col( source(s), name( "time" ) )
DATA: id=col( source(s), name( "id" ), unit.category() )
DATA: relqual=col(source(s), name("relqual"), unit.category() )
GUIDE: text.title( label( "Panel Plot by Relationship Quality" ) )
GUIDE: axis( dim( 1 ), label( "time" ) )
GUIDE: axis( dim( 2 ), label( "intimacy" ) )
GUIDE: axis( dim( 3 ), label( "id" ), opposite() )
GUIDE: legend( aesthetic( aesthetic.shape.interior ), null() )
SCALE: linear( dim( 1 ), min( 0 ), max( 27 ) )
SCALE: linear( dim( 2 ), min( 0 ), max( 10 ) )
ELEMENT: path( position( time * intimacy * id )), size(size."0.7" ))
END GPL.
SPLIT FILE OFF.

***********************************************************
*COMMENT: Panels of conflict over time by id.
*COMMENT: Upper panel relqual=0 (low relqual); lower panel relqual=1 (high relqual)

SORT CASES  BY relqual.
SPLIT FILE SEPARATE BY relqual.
formats time conflict (f4.2) relqual (f1.0).
GGRAPH 
  /GRAPHDATASET NAME="GraphDataset" VARIABLES= conflict time id
  /GRAPHSPEC SOURCE=INLINE INLINETEMPLATE=["<setWrapPanels/>"].
BEGIN GPL
SOURCE: s=userSource( id( "GraphDataset" ) )
DATA: conflict=col( source(s), name( "conflict" ) )
DATA: time=col( source(s), name( "time" ) )
DATA: id=col( source(s), name( "id" ), unit.category() )
DATA: relqual=col(source(s), name("relqual"), unit.category() )
GUIDE: text.title( label( "Panel Plot by Relationship Quality" ) )
GUIDE: axis( dim( 1 ), label( "time" ) )
GUIDE: axis( dim( 2 ), label( "conflict" ) )
GUIDE: axis( dim( 3 ), label( "id" ), opposite() )
GUIDE: legend( aesthetic( aesthetic.shape.interior ), null() )
SCALE: linear( dim( 1 ), min( 0 ), max( 27 ) )
SCALE: linear( dim( 2 ), min( 0 ), max( 1 ) )
ELEMENT: path( position( time * conflict * id ) )), size(size."0.8" ))
END GPL.
SPLIT FILE OFF.

********************************************
*COMMENT: MIXED syntax for process example.

MIXED intimacy WITH  relqual confbc confw time7c 
  /FIXED=relqual  confbc confbc*relqual confw confw*relqual time7c | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT confw | SUBJECT(id) COVTYPE(UN)
  /REPEATED=time7c | SUBJECT(id) COVTYPE(AR1)
  /SAVE = FIXPRED(FIXED) PRED(BLUP) SEFIXP(SEFIXED) SEPRED (SEPRED).

*******************************************
*COMMENT: Spaghetti plots based on multilevel modeling estimates (BLUPS).
SORT CASES BY relqual(A) confw(A).

GGRAPH
  /GRAPHDATASET NAME="GraphDataset" VARIABLES= BLUP conflict id relqual
  /GRAPHSPEC SOURCE=INLINE  INLINETEMPLATE=["<setWrapPanels/>"].
BEGIN GPL
SOURCE: s=userSource( id( "GraphDataset" ) )
DATA: BLUP=col( source(s), name( "BLUP" ) )
DATA: conflict=col( source(s), name( "conflict" ) )
DATA: id=col( source(s), name( "id" ), unit.category() )
DATA: relqual=col( source(s), name( "relqual" ), unit.category() )
GUIDE: text.title( label( "Spaghetti Plot " ) )
GUIDE: axis( dim( 1 ), label( "conflict" ) )
GUIDE: axis( dim( 2 ), label( "BLUP" ) )
GUIDE: axis( dim( 3 ), label( "relqual" ), opposite() )
GUIDE: legend( aesthetic( aesthetic.shape.interior ), null() )
SCALE: linear( dim( 1 ), min( 0 ), max( 1 ) )
SCALE: linear( dim( 2 ), min( 0 ), max( 10 ) )
ELEMENT: line( position( summary.mode( conflict * BLUP * relqual ) ), shape.interior( id ))
ELEMENT: line( position( smooth.linear( conflict * BLUP * relqual  ) ), color(relqual) )
END GPL.


*************************************************
Striped Down Process Model
*Note: If you want to run this one, then SPSS wants you to delete the existing FIXED, BLUP, etc. columns

MIXED intimacy WITH relqual confbc confw 
  /FIXED=relqual confbc confbc*relqual confw confw*relqual | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT confw | SUBJECT(id) COVTYPE(UN)
  /SAVE = FIXPRED(FIXED) PRED(BLUP) SEFIXP(SEFIXED) SEPRED (SEPRED).


************************************************
Panel Plotting of BLUPS with 95% Upper Limit and Lower Limit Confidence Bounds

compute blup_ll = blup-(1.96*SEPRED).
compute blup_ul = blup+(1.96*SEPRED).
execute.

SORT CASES  BY relqual.
SPLIT FILE SEPARATE BY relqual.
formats blup blup_ll blup_ul intimacy conflict (f4.2) relqual (f1.0).
GGRAPH
    /GRAPHDATASET NAME="GraphDataset" VARIABLES= blup blup_ll blup_ul intimacy conflict  id
    /GRAPHSPEC SOURCE=INLINE INLINETEMPLATE=["<setWrapPanels/>"].
BEGIN GPL
    SOURCE: s=userSource( id( "GraphDataset" ) )
    DATA: conflict=col( source(s), name( "conflict" ) )
    DATA: blup=col( source(s), name( "blup" ) )
    DATA: id=col( source(s), name( "id" ), unit.category() )
    DATA: blup_ll=col( source(s), name( "blup_ll" ) )
    DATA: blup_ul=col( source(s), name( "blup_ul" ) )
    DATA: relqual=col(source(s), name("relqual"), unit.category() )
    DATA: intimacy=col( source(s), name( "intimacy" ) )
    GUIDE: text.title( label( "Panel Plot by Relationship Quality" ) )
    GUIDE: axis( dim( 1 ), label( "conflict" ) )
    GUIDE: axis( dim( 2 ), label( "blup" ) )
    GUIDE: axis( dim( 3 ), label( "blup_ll" ))
    GUIDE: axis( dim( 4 ), label( "blup_ul" ))
    GUIDE: axis( dim( 5 ), label( "id" ), opposite() )
    GUIDE: axis( dim( 6 ), label( "intimacy" ))
    GUIDE: legend( aesthetic( aesthetic.shape.interior ), null() )
    SCALE: linear( dim( 1 ), min( 0 ), max( 1 ) )
    SCALE: linear( dim( 2 ), min( 0 ), max( 10 ) )
    SCALE: linear( dim( 3 ), min( 0 ), max( 10 ) )
    SCALE: linear( dim( 4 ), min( 0 ), max( 10 ) )
    SCALE: linear( dim( 6 ), min( 0 ), max( 10 ) )
    ELEMENT: line( position(smooth.linear( conflict * blup_ll * id ) ), color("95% CI"), transparency(transparency."0.1"), size(size."1.9"), shape(shape.dash) )
    ELEMENT: line( position(smooth.linear( conflict * blup_ul * id ) ), color("95% CI"), transparency(transparency."0.1"), size(size."1.9"), shape(shape.dash) )
    ELEMENT: line( position(smooth.linear( conflict * blup * id ) ), color("Conflict Slope"))
    ELEMENT: point( position( conflict * intimacy * id ), color("data points"))
END GPL.
SPLIT FILE OFF.

