*
* $Id: ugprotm.F,v 1.1 2007/12/19 04:25:48 malysh Exp $
*
* $Log: ugprotm.F,v $
* Revision 1.1  2007/12/19 04:25:48  malysh
* Исправил ошибку в задании параметров геометрии электродной системы в LKr,
* улучшена (Масленников А) печать объемов при отладке.
*
* Revision 1.1.1.1  1995/10/24 10:20:54  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.30  by  S.Giani
*-- Author :
      SUBROUTINE UGPROTM(NUMB)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Routine to print rotation matrices structure JROTM       *
C.    *       NUMB     Rotation matrix number                          *
C.    *                                                                *
C.    *    ==>Called by : <USER>, GPRINT                               *
C.    *         Author  R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcunit.inc"
C.
C.    ------------------------------------------------------------------
C.
      IF (JROTM.LE.0)  GO TO 99
C
      IF (NUMB  .EQ.0)THEN
         WRITE (CHMAIL,1000)
         CALL GMAIL(0,0)
         N1     = 1
         N2     = IQ(JROTM-2)
      ELSE
         N1     = ABS(NUMB)
         N2     = ABS(NUMB)
      ENDIF
C
      IF(NUMB.GE.0) THEN
         WRITE (CHMAIL,1001)
c         CALL GMAIL(0,1)  !  2nd param = N of blanc lines below
         CALL GMAIL(0,0)
      ENDIF
C
      DO 30 I=N1,N2
      JR = LQ(JROTM-I)
      IF (JR.LE.0)   GO TO 30
C
      IFLAG = Q(JR+10)  !  to recognize simple rotation configurations
c      WRITE (CHMAIL,1002) I,(Q(JR+J),J = 1,9),IFLAG,(Q(JR+J),J = 11,16)
      WRITE (CHMAIL,1003) I,(Q(JR+J),J = 1,9),(Q(JR+J),J = 11,16)
      CALL GMAIL(0,0)
   30 CONTINUE
C
 1000 FORMAT ('0',51('='),1X,'ROTATION MATRICES',2X,50('='))
c 1001 FORMAT ('0','MATRIX',1X,'<',17('-'),' ROTATION MATRIX',1X,17('-')
c     +,'>',' FLAG','  THET1','   PHI1','  THET2','   PHI2','  THET3'
c     +,'   PHI3')
 1001 FORMAT ('MATRIX',1X,'<',26('-'),' ROTATION MATRIX',1X,25('-')
     +,'>','   THET1','    PHI1','   THET2','    PHI2','   THET3'
     +,'    PHI3')
c 1002 FORMAT (' ',I6,9F6.3,I5,6F7.2)
c- 1002 FORMAT ('RotM=',I3,9F8.5,I4,6F8.3)
 1003 FORMAT ('RM',I3,9F8.5,1X,6F8.3)
   99 RETURN
      END
