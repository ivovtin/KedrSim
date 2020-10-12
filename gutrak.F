*CMZ :  1.00/01 27/01/93  16.48.46  by  TANYA PURLATZ
*-- Author :
      SUBROUTINE GUTRAK
#include "gcflag.inc"
C
      IF(ISWIT(10).GE.2.AND.IDEBUG.EQ.1) CALL GPKINE(ITRA)
C++++++++++++++
      CALL GTRACK
C++++++++++++++
      IF (ISWIT(10).EQ.2.AND.IDEBUG.EQ.1) CALL GPCXYZ
      RETURN
      END
