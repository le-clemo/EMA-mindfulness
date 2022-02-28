* Encoding: UTF-8.
***********************************2021 AILD Workshop************************************
*COMMENT: Panels by id.
*COMMENT: Note that individual lines NOT based on fitted multilevel model.

formats time intimacy (f4.2).
GGRAPH
  /GRAPHDATASET NAME="GraphDataset" VARIABLES= intimacy time id 
  /GRAPHSPEC SOURCE=INLINE INLINETEMPLATE=["<setWrapPanels/>"].
BEGIN GPL
SOURCE: s=userSource( id( "GraphDataset" ) )
DATA: intimacy=col( source(s), name( "intimacy" ) )
DATA: time=col( source(s), name( "time" ) )
DATA: id=col( source(s), name( "id" ), unit.category() )
GUIDE: text.title( label( "Panel Plot of Intimacy Tx Dataset" ) )
GUIDE: axis( dim( 1 ), label( "time" ) )
GUIDE: axis( dim( 2 ), label( "intimacy" ) )
GUIDE: axis( dim( 3 ), label( "id" ), opposite() )
SCALE: linear( dim( 1 ), min( 0 ), max( 15 ) )
SCALE: linear( dim( 2 ), min( 0 ), max( 10 ) )
ELEMENT: point( position( time * intimacy * id ) )
ELEMENT: line( position(smooth.linear( time * intimacy * id ) ))
END GPL.

***************************************
*COMMENT: Panels by id (with both Tx in different colors).
*COMMENT: Note that individual lines *not* based on fitted multilevel model.

formats time intimacy (f4.2) treatment (f1.0).
GGRAPH 
  /GRAPHDATASET NAME="GraphDataset" VARIABLES= intimacy time id treatment
  /GRAPHSPEC SOURCE=INLINE INLINETEMPLATE=["<setWrapPanels/>"].
BEGIN GPL
SOURCE: s=userSource( id( "GraphDataset" ) )
DATA: intimacy=col( source(s), name( "intimacy" ) )
DATA: time=col( source(s), name( "time" ) )
DATA: treatment=col(source(s), name("treatment"), unit.category() )
DATA: id=col( source(s), name( "id" ), unit.category() )
GUIDE: text.title( label( "Panel Plot by Treatment" ) )
GUIDE: axis( dim( 1 ), label( "time" ) )
GUIDE: axis( dim( 2 ), label( "intimacy" ) )
GUIDE: axis( dim( 3 ), label( "id" ), opposite() )
GUIDE: legend( aesthetic( aesthetic.shape.interior ), null() )
SCALE: linear( dim( 1 ), min( 0 ), max( 15 ) )
SCALE: linear( dim( 2 ), min( 0 ), max( 10 ) )
ELEMENT: point( position( time * intimacy * id ), shape.interior(treatment), color(treatment) )
ELEMENT: line( position(smooth.linear( time * intimacy  * id) ), shape.interior(treatment), color(treatment))
END GPL.

***************************************
*COMMENT: Spaghetti plots (male and female side-by-side panels) with solid fixed effects lines.
*COMMENT: Note that individual lines *NOT *based on fitted multilevel model.

GGRAPH
  /GRAPHDATASET NAME="GraphDataset" VARIABLES= intimacy time id treatment
  /GRAPHSPEC SOURCE=INLINE  INLINETEMPLATE=["<setWrapPanels/>"].
BEGIN GPL
SOURCE: s=userSource( id( "GraphDataset" ) )
DATA: intimacy=col( source(s), name( "intimacy" ) )
DATA: time=col( source(s), name( "time" ) )
DATA: id=col( source(s), name( "id" ), unit.category() )
DATA: treatment=col( source(s), name( "treatment" ), unit.category() )
GUIDE: text.title( label( "Spaghetti Plot by Treatment (Not based on multilevel model)" ) )
GUIDE: axis( dim( 1 ), label( "time" ) )
GUIDE: axis( dim( 2 ), label( "intimacy" ) )
GUIDE: axis( dim( 3 ), label( "treatment" ), opposite() )
GUIDE: legend( aesthetic( aesthetic.shape.interior ), null() )
SCALE: linear( dim( 1 ), min( 0 ), max( 15 ) )
SCALE: linear( dim( 2 ), min( 0 ), max( 10 ) )
ELEMENT: line( position( smooth.linear( summary.mode( time * intimacy * treatment ) ) ), shape.interior( id ))
ELEMENT: line( position( smooth.linear( time * intimacy * treatment ) ), color(treatment) )
END GPL.

***************************************
*COMMENT: Basic multilevel growth model with Tx predictor

MIXED intimacy WITH time01 treatment
  /FIXED=time01 treatment time01*treatment | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT time01 | SUBJECT(id) COVTYPE(UN)
  /REPEATED=time | SUBJECT(id) COVTYPE(AR1).

***************************************
*COMMENT: Model for saving BLUPS.

MIXED intimacy WITH time01 treatment
  /FIXED=time01 treatment time01*treatment | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT time01 | SUBJECT(id) COVTYPE(UN)
  /REPEATED=time | SUBJECT(id) COVTYPE(AR1)
  /SAVE = FIXPRED(FIXED) PRED(BLUP).

*Note: BLUP = Best Linear Unbiased Predictions

***************************************
*COMMENT: Spaghetti plots (male and female side-by-side panels) with solid fixed effects lines.
*COMMENT: Based on BLUPs from fitted multilevel model above.

GGRAPH
  /GRAPHDATASET NAME="GraphDataset" VARIABLES= BLUP time id treatment
  /GRAPHSPEC SOURCE=INLINE  INLINETEMPLATE=["<setWrapPanels/>"].
BEGIN GPL
SOURCE: s=userSource( id( "GraphDataset" ) )
DATA: BLUP=col( source(s), name( "BLUP" ) )
DATA: time=col( source(s), name( "time" ) )
DATA: id=col( source(s), name( "id" ), unit.category() )
DATA: treatment=col( source(s), name( "treatment" ), unit.category() )
GUIDE: text.title( label( "Spaghetti Plot by Treatment (Based on BLUPs)" ) )
GUIDE: axis( dim( 1 ), label( "time" ) )
GUIDE: axis( dim( 2 ), label( "BLUP" ) )
GUIDE: axis( dim( 3 ), label( "treatment" ), opposite() )
GUIDE: legend( aesthetic( aesthetic.shape.interior ), null() )
SCALE: linear( dim( 1 ), min( 0 ), max( 15 ) )
SCALE: linear( dim( 2 ), min( 0 ), max( 10 ) )
ELEMENT: line( position( smooth.linear( summary.mode( time * BLUP * treatment ) ) ), shape.interior( id ))
ELEMENT: line( position( smooth.linear( time * BLUP * treatment ) ), color(treatment) )
END GPL.


