*CMZ :  1.00/01 25/01/93  15.55.13  by  TANYA PURLATZ
*-- Author :    Tanya Purlatz   25/01/93
      SUBROUTINE UHFILE
C.
C.    *
C.    *       To open the user's histograms file
C.    *       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^^
C.    *
#include "gckine.inc"
C
	include 'luns.inc'
C
      CALL search_free_unit(LUNTUP)
C
      IF (IKINE.LT.0)
     *  OPEN (UNIT=LUNTUP,FILE='single.dat',ACCESS='DIRECT',
     *     STATUS='NEW',FORM='UNFORMATTED',RECL=1024)
      IF (IKINE.EQ.1)
     *  OPEN (UNIT=LUNTUP,FILE='eeg.dat',ACCESS='DIRECT',
     *     STATUS='NEW',FORM='UNFORMATTED',RECL=1024)
      IF (IKINE.EQ.2)
     *  OPEN (UNIT=LUNTUP,FILE='cosm.dat',ACCESS='DIRECT',
     *     STATUS='NEW',FORM='UNFORMATTED',RECL=1024)
      IF (IKINE.EQ.3)
     *  OPEN (UNIT=LUNTUP,FILE='eer.dat',ACCESS='DIRECT',
     *     STATUS='NEW',FORM='UNFORMATTED',RECL=1024)
      IF (IKINE.EQ.4)
     *  OPEN (UNIT=LUNTUP,FILE='beamp.dat',ACCESS='DIRECT',
     *     STATUS='NEW',FORM='UNFORMATTED',RECL=1024)
      IF (IKINE.EQ.102)
     *  OPEN (UNIT=LUNTUP,FILE='gu2prt.dat',ACCESS='DIRECT',
     *     STATUS='NEW',FORM='UNFORMATTED',RECL=1024)
      IF (IKINE.EQ.103)
     *  OPEN (UNIT=LUNTUP,FILE='gu3prt.dat',ACCESS='DIRECT',
     *     STATUS='NEW',FORM='UNFORMATTED',RECL=1024)
      IF (IKINE.EQ.104)
     *  OPEN (UNIT=LUNTUP,FILE='gu4prt.dat',ACCESS='DIRECT',
     *     STATUS='NEW',FORM='UNFORMATTED',RECL=1024)
         CALL HRFILE(LUNTUP,'DATA','N')
      END
