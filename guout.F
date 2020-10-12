        SUBROUTINE GUOUT
c
c   Called at the end of event
c   31.05.99.  S.F.Ganzhur
c
#include "KedrSim/drawpar.inc"
        COMMON /LUN/ LUNPTR
        COMMON /RZFZFILES/IRZFILE,IFZFILE
c
	IF (IAX.GE.1.AND.IAX.LE.3) THEN ! wating before drawing of next picture
	      print *,'Press "RETURN" to continue'
	      READ (5,'(a)') A
	ENDIF

        IF(IFZFILE.NE.1) RETURN
c      
	CALL GFOUT1(LUNPTR,'HEAD', 1, 0, ' ',IER)
c
	CALL GFOUT1(LUNPTR,'VERT', 1, 0, ' ',IER)
c
	CALL GFOUT1(LUNPTR,'KINE', 1, 0, 'S',IER)
c
	CALL GFOUT1(LUNPTR,'HITS', 1, 0, ' ',IER)
c
        
	END
