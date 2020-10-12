      PROGRAM KEDRSIM
c
c  Main program of KEDR simulation 
c
      PARAMETER (NH=100000,NG=1000000)
      COMMON/PAWC/H(NH)
      COMMON/GCBANK/Q(NG)
c
c#include "gcbank.inc"
      include 'extdef.inc'
c      CHARACTER *1 NRELEASE(80),NRELEASE1(9),CHAR_POINT,CHAR_SL
      CHARACTER *1 NRELEASE1(9)
      CHARACTER *80 REL_PATH,CHARG
      CHARACTER *50 TITLE2
      CHARACTER *10 RELEASE
      COMMON/KEDRREL/RELEASE
c
c      DATA CHAR_POINT,CHAR_SL/'.','/'/
      DATA NRELEASE1/' ',' ',' ',' ',' ',' ',' ',' ',' '/    
c
      TITLE2='óompiled at '//COMPDATE
c=====================================================
c
c               Title of job
c
c====================================================
c  Read number of release
c
c      CALL SEARCH_FREE_UNIT(LUNPTR)
c
c      OPEN (UNIT=LUNPTR,FILE='../../.release')
c
c      READ (LUNPTR,101) NRELEASE
c
c      CLOSE (LUNPTR)
c
c      DO 102 I=1,80
c   	IF(NRELEASE(I).NE.CHAR_POINT) GOTO 102
c 	IF(NRELEASE(I-2).EQ.CHAR_SL) IND1=I-1 
c 	IF(NRELEASE(I-3).EQ.CHAR_SL) IND1=I-2 
c 	IF(NRELEASE(I+6 ).EQ.CHAR_SL) IND2=I+5
c 	IF(NRELEASE(I+5 ).EQ.CHAR_SL) IND2=I+4
c 	IF(NRELEASE(I+4 ).EQ.CHAR_SL) IND2=I+3
c        GOTO 103
c 102  CONTINUE  
c               
c 103  DO I=1,IND2-IND1+1
c        NRELEASE1(I)=NRELEASE(IND1+I-1)
c      END DO 
c
	REL_PATH=KEDR_RELEASE
	RLEN=len(KEDR_RELEASE)
	DO I=RLEN-1,1,-1
        	IF(REL_PATH(I:I).EQ.'/') GOTO 77
	ENDDO
77	RELEASE=REL_PATH(I+1:RLEN-1)

	DO I=1,9
		NRELEASE1(I)=RELEASE(I:I)
        ENDDO

	NARG=IARGC()
        IF(NARG.GT.0) THEN
		DO I=1,NARG
	        	CALL GETARG(I,CHARG)
			IF(CHARG(1:2).EQ.'-V'.OR.CHARG(1:2).EQ.'-v') THEN
                        	PRINT *,RELEASE
                                STOP
                         ENDIF
                ENDDO
        ENDIF         
 	PRINT 100,NRELEASE1,TITLE2
c	               
c =====================================================================
C         Allocate memory for ZEBRA and HBOOK
C	print *,' start GZEBRA'
      CALL GZEBRA(NG)
C	print *,' start HLIMIT'
      CALL HLIMIT(-NH)
C
C         Initialize Graphics package
C	print *,' start IGINIT'
C      CALL IGINIT(0)
C
C         Initialisation phase
C	print *,' start UGINIT'
      CALL UGINIT
C
C         Processing phase
C	print *,' start GRUN'
      CALL GRUN
C
C         Termination phase
      CALL UGLAST
C
 100  FORMAT(80(1H*),/,79(1H*),/,79(1H*),/,3(1H*),73X,3(1H*),/,
     +      3(1H*),73X,3(1H*),/,
     +      3(1H*),8X,'KEDRSIM - the detector KEDR simulation. ',
     +     ' Release ',9A1,7X,3(1H*),/,
     +	    3(1H*),73X,3(1H*),/,     
     +      3(1H*),13X,A50,10X,3(1H*),/, 
     +      3(1H*),73X,3(1H*),/,
     +      79(1H*),/,79(1H*))
 101  FORMAT(80A1)   
c
      STOP
      END
