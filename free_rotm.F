*CMZ :  1.00/01 29/01/93  16.26.25  by  TANYA PURLATZ
*-- Author :    Tanya Purlatz   27/01/93
      SUBROUTINE FREE_ROTMAT_NUMBER(MATNO)
C
C Get unused rotation matrix number (adopted from CLEO-simulation)
C
C  This routine examines the GEANT ROTM data structure and returns
C  in MATNO the first unused rotation matrix number. This number may be
C  used in subsequent GSROTM calls. Note that this routine does not
C  book or reserve the rotation matrix number in any way, if this number
C  is not used up by a call to GSROTM, if called again, this routine
C  will still return the same number as being unused.
C
C
#include "gcbank.inc"
C
      IF( JROTM.LE.0 ) THEN
         MATNO = 1
      ELSE
         DO 10 IMAT = 1, IQ(JROTM-2)
            IF( LQ(JROTM-IMAT).LE.0 ) THEN
               MATNO = IMAT
               GOTO 999
            ENDIF
10       CONTINUE
         MATNO = IQ(JROTM-2) + 1
      ENDIF
C
999   CONTINUE
c	print *,'free_rotm: ',matno
      RETURN
      END
