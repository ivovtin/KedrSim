      SUBROUTINE HIS_INIT
c*******************************************************************
c
c  Reads and prepares the initial data for histograms in simulation  
c
c  Author: V.A.Tayursky, 23 Oct 2003, 
c  
c  Corrections: 12 Mar 2004, 22 Mar 2006, 3 Oct 2008
c
c*******************************************************************
      Implicit NONE
c
      Integer *4 IHIS,NCU,NFO,IFOR,ICONTF,NHI1,NHI2,NHIST,NH0
      Integer *4 NH0PR,IWR_HIS     
      CHARACTER *3 PARN      
      Real *4 CUTD
c   NCU - number of cuts, NFO - number of formulas
      COMMON /HIS_SIMUL/ IHIS,PARN(20),NCU,CUTD(9,4),NFO,IFOR(9),
     +                  ICONTF(9,20),NHI1,NHI2,NHIST(18,3),NH0,NH0PR(9),
     +                  IWR_HIS
      CHARACTER *1 SL,ST,PL,CH1,BLNK,TFORM(9,16),E,H,C,F,CL(80)
      CHARACTER *3 NPAR(20),ANM
      CHARACTER *80 CAOUT(20),CA1,HTEXTF
      REAL      *4 AOUT(20),XMIN,XMAX,YMIN,YMAX
      Integer *4 ICUTD(9,4),IAOUT(20),I,J,LUN,NC,NF,KOD,IR,INM,JMAX
      Integer *4 NH,NUMFOR,NUMH,IPAR1,INDCF,IIH,NCHAN,NCHANX,NCHANY
      Integer *4 IPAR2
c
      EQUIVALENCE (CA1,CL), (ICUTD(1,1),CUTD(1,1)), (ANM,CL(1))
      EQUIVALENCE (AOUT(1),IAOUT(1)) 
      SAVE /HIS_SIMUL/
c........
c  Names of parameters
      DATA NPAR/'ENE','MOM','TET','CTE','PHI','TYP','GEN','NPW','VEX',
     +          'VEY','VEZ','TOR','TTR','ACN','ACA','TFN','TFA','RE1',
     +          'RE2','RE3'/          
c
      DATA C,F,H,E,BLNK/'C','F','H','E',' '/
c.......
c  For log. form.
      DATA ST,PL,SL/'*','+','/'/
c
      IF (IHIS.NE.1) RETURN
c
      NCU=0     ! number of cards with cuts
      NFO=0     ! number of cards with formulas
      NHI1=0    ! number of cards with 1-d histograms
      NHI2=0    ! number of cards with 2-d histograms 
      NH=0      ! total number of histograms
      NH0=0     ! counter of cards with prints of data
      IWR_HIS=0 ! for filerw - not to write events from 1-st 1-dim hist. 
c
      DO I=1,20
        PARN(I)=NPAR(I)   ! assign names of parameters
      END DO     
c
      DO I=1,9
        NH0PR(I)=0
        IFOR(I)=0
c
         DO J=1,4 
           ICUTD(I,J)=0
         END DO
c
        DO J=1,20
          ICONTF(I,J)=0
        END DO
c
        DO J=1,16
          TFORM(I,J)=BLNK
        END DO
      END DO
c
      DO I=1,18
        DO J=1,3
          NHIST(I,J)=0
        END DO
      END DO
c
c  Read file for histograms calculation
c
      CALL SEARCH_FREE_UNIT(LUN)
c
      OPEN (UNIT=LUN,FILE='kedr_hist.dat',STATUS='OLD')
c
c   NC        - number of first character elements                      
c   CAOUT(20) - character array *80                                        
c   NF        - number of numbers at card                               
c   AOUT(20)  - real array with transformed data 
c
 10   CALL PstFreeR(LUN,NC,CAOUT,NF,AOUT,KOD) ! read one card 
c
      IF(KOD.EQ.1) GOTO 10  ! card with comment
      IF(KOD.EQ.2) GOTO 40  ! card with error
      CA1=CAOUT(1)
      IF(CL(1).EQ.E) GOTO 2  ! end card
c
      IF(CL(1).EQ.C) GOTO 20 ! card with cut
c
      IF(CL(1).EQ.F) GOTO 30 ! card with log. formula
c
      IF(CL(1).EQ.H) GOTO 35 ! card with histogram
c
40    PRINT 41
      STOP
c--------------------------------------------------------------------
 20   CONTINUE
c
c   Analysis of cuts
c
      NCU=NCU+1  ! cards with cuts
c
      IF(NCU.GT.9) THEN
        PRINT 43
        STOP
      END IF
c
      CA1=CAOUT(1)  ! CA1 (ch*80) eq CL(80) (ch*1)
      IR=0
      CH1=CL(2)
c
      CALL HIS_INIT1(CH1,INM,KOD)
c
      IF(KOD.EQ.0) THEN ! error in card
        PRINT 45
        STOP
      END IF     
c
      IF(KOD.EQ.1) THEN
        ICUTD(NCU,1)=INM    ! number of cut
c
        IF(INM.NE.NCU) THEN ! error: number of cut /= number of card 
 	  PRINT 42,NCU
 	  STOP
 	END IF
c
        CA1=CAOUT(2)    ! name of parameter (used first 3 char.)
c
        CALL HIS_INIT3(ANM,J) ! name of parameter -> number
c
        ICUTD(NCU,2)=J      ! number of parameter
        CUTD(NCU,3)=AOUT(1) ! boundaries on parameter
        CUTD(NCU,4)=AOUT(2)
      END IF
c
      IF(NCU.GE.2) THEN ! are there the same numbers of cuts?
c
        DO I=2,NCU
          JMAX=I-1
c
          DO J=1,JMAX
            IF(ICUTD(I,1).EQ.ICUTD(J,1)) THEN
              PRINT 45
              STOP
            END IF             
          END DO
c
        END DO
c
      END IF
c
      GOTO 10
c
c----------------------------------------------------------------------
 30   CONTINUE      
c
c   Analysis of logical formulas
c
      NFO=NFO+1  ! counter of cards with log. formulas
c
      IF(NFO.GT.9) THEN
        PRINT 44
        STOP
      END IF
c
      CA1=CAOUT(1)
c
      CALL HIS_INIT1(CL(2),NUMFOR,KOD)  ! number of formula
c
      IF(NUMFOR.LT.1.OR.NUMFOR.GT.9) THEN
        PRINT 47
 	STOP
      END IF 
c
      IF(NUMFOR.NE.NFO) THEN
        PRINT 61,NFO
        STOP
      END IF
c
      CA1=CAOUT(2)  ! Formula: <= 16 char. 
c
      DO I=1,16
        TFORM(NFO,I)=CL(I)  ! store text of formula for print out
      END DO
c
      IFOR(NFO)=NUMFOR      ! number of formula
      INDCF=0               ! index of content of formula
c
      DO 31 I=1,16
        IF(CL(I).EQ.C) GOTO 31
        CH1=CL(I)
c
        CALL HIS_INIT1(CH1,INM,KOD)  ! KOD=1: CH1->number INM succesful
c
        IF(KOD.EQ.1.AND.INM.NE.0) THEN
c
          DO J=1,NCU     ! are all cuts in formula permitted? 
            IF(INM.EQ.ICUTD(J,1)) GOTO 34
          END DO
c
          PRINT 33,NUMFOR
          STOP
c
 34       INDCF=INDCF+1
          ICONTF(NFO,INDCF)=INM
        END IF
c
        IF(CL(I).EQ.ST) THEN
 	  INDCF=INDCF+1
          ICONTF(NFO,INDCF)=100
        END IF
c
        IF(CL(I).EQ.PL) THEN
 	  INDCF=INDCF+1   
 	  ICONTF(NFO,INDCF)=200
        END IF
c
        IF(CL(I).EQ.SL) THEN
          INDCF=INDCF+1   
          ICONTF(NFO,INDCF)=300
        END IF
c
 31   CONTINUE
c
      IF(ICONTF(NFO,INDCF).GE.1.AND.ICONTF(NFO,INDCF).LE.9) GOTO 32
      PRINT 33,NUMFOR
      STOP
c
 32   ICONTF(NFO,20)=INDCF
c
      GOTO 10
c--------------------------------------------------------------
 35   CONTINUE
c
c   Analysis of data for histograms    
c
      CA1=CAOUT(1)
c
      CALL HIS_INIT1(CL(2),NUMH,KOD)  ! NUMH: type of histogram (0,1 or 2)
c
      IF(KOD.EQ.0.OR.NUMH.LT.0.OR.NUMH.GT.2) THEN
        PRINT 36
        STOP
      END IF
c
      IF(NUMH.EQ.0) THEN ! Job with prints of data for parameters 
        NH0=NH0+1        
        CA1=CAOUT(2) ! 1-st parameter name (used first 3 char.)
c
 	CALL HIS_INIT3(ANM,IPAR1) ! name of parameter -> it's number
c
        IF(IPAR1.GE.1.AND.IPAR1.LE.13)  NH0PR(1)=1 ! print data on particles
        IF(IPAR1.GE.14.AND.IPAR1.LE.15) NH0PR(2)=1 ! print data on ATC
        IF(IPAR1.GE.16.AND.IPAR1.LE.17) NH0PR(3)=1 ! print data on ToF
        GOTO 10
      END IF
c
      NH=NH+1      ! counter of total number of histograms  
      CA1=CAOUT(2) ! formula
c
      CALL HIS_INIT1(CL(2),NUMFOR,KOD)  ! NUMFOR: number of log. formula (1-9)
c
      IR=0
c
      DO I=1,NFO
        IF(NUMFOR.EQ.IFOR(I)) IR=1      ! whether log. f-la was determined?
      END DO
c
      IF(IR.EQ.0) THEN
        PRINT 38,NH   
        STOP
      END IF
c
      CA1=CAOUT(3) ! 1-st parameter name (used first 3 char.)
c
      CALL HIS_INIT3(ANM,IPAR1) ! name of parameter -> it's number
c
      IF(NUMH.EQ.2) GOTO 37     ! go to 2-dim hist
c
c-----------------------
c
c  1-dim. histograms
c
      NHI1=NHI1+1  ! counter of 1-dim. histograms
c
      IF(NHI1.GT.9) THEN
	IIH=1        ! 1-dim. hist
        PRINT 39,IIH
        STOP
      END IF
c
      CA1=CAOUT(4) ! Text under the histogram
c
      CALL HIS_INIT2(CAOUT,4,NC,HTEXTF)
c
      NCHAN=IAOUT(1) ! channals
      XMIN=AOUT(2)   ! min value
      XMAX=AOUT(3)   ! max value
c
      CALL HBOOK1(NH,HTEXTF,NCHAN,XMIN,XMAX,0.)
c
      NHIST(NH,1)=NUMFOR      ! number of formula
      NHIST(NH,2)=IPAR1       ! number of parameter
      NHIST(NH,3)=0
c
      GOTO 10
c
c-----------------------
c
c  2-dim. histograms
c
 37   NHI2=NHI2+1  ! counter of 2-dim. histograms
c
      IF(NHI2.GT.9) THEN
        IIH=2
        PRINT 39,IIH
        STOP
      END IF
c
      CA1=CAOUT(4) ! 2-nd parameter name (used first 3 char.)
c
      CALL HIS_INIT3(ANM,IPAR2) ! name of parameter -> it's number
c
      CA1=CAOUT(5) ! Text under the histogram
c
      CALL HIS_INIT2(CAOUT,5,NC,HTEXTF)
c
      NCHANX=IAOUT(1) ! channals  in x
      XMIN=AOUT(2)    ! min value in x
      XMAX=AOUT(3)    ! max value in x
      NCHANY=IAOUT(4) ! channals  in y
      YMIN=AOUT(5)    ! min value in y
      YMAX=AOUT(6)    ! max value in y
c
 	CALL HBOOK2(NH,HTEXTF,NCHANX,XMIN,XMAX,NCHANY,YMIN,YMAX,0.)   
c
      NHIST(NH,1)=NUMFOR      ! number of formula
      NHIST(NH,2)=IPAR1	      ! number of parameter in x
      NHIST(NH,3)=IPAR2       ! number of parameter in y
c
      GOTO 10
c
c------------------------------------------------------------------
c
 2    CONTINUE
c
c  Prints of initial data for histograms
c
      PRINT 57                    ! title
c

      PRINT 55                    ! parameters
      PRINT 56,(PARN(I),I=1,17)
c
      PRINT 51, NCU               ! cuts
c
      DO I=1,NCU
        PRINT 52, ICUTD(I,1),ICUTD(I,2),CUTD(I,3),CUTD(I,4)
      END DO
c
      PRINT 53,NFO                ! formulas
c
      DO I=1,NFO
        PRINT 54,IFOR(I),(TFORM(I,J),J=1,16)
      END DO
c
      DO I=1,9
        IF(NH0PR(I).EQ.1) PRINT 66,I
      END DO
c
      PRINT 60,NHI1               ! 1-d histograms
c
      IF(NHI1.GT.0) THEN
        DO I=1,NHI1
          PRINT 59,I,(NHIST(I,J),J=1,2)
        END DO
      END IF
c
      PRINT 64
c
      PRINT 62,NHI2               ! 2-d histograms
c
      IF(NHI2.GT.0) THEN
c
        DO I=1,NHI2
          PRINT 63,I,(NHIST(NHI1+I,J),J=1,3)
        END DO
      END IF
c
c      do i=1,NFO
c        print 100, I, (ICONTF(i,j),j=1,20)
c100     format(/,' Formula N',I3,/,20I4)
c      end do
c
      PRINT 58
c
 33   FORMAT(//,' ERROR in file kedr_hist.dat: FORMULA ',I2)
 36   FORMAT(//,' ERROR in file kedr_hist.dat: HISTOGRAM 1-d or 2-d')
 38   FORMAT(//,' ERROR in file kedr_hist.dat: card for histogram ',I2)
 39   FORMAT(//,' ERROR in file kedr_hist.dat: ',I1,'-d hist-ms >9')       
 41   FORMAT(//,' ERROR in file kedr_hist.dat')
 42   FORMAT(//,' ERROR in file kedr_hist.dat: CUT-card =',I2)
 43   FORMAT(//,' ERROR in file kedr_hist.dat: CUT-cards > 9')
 44   FORMAT(//,' ERROR in file kedr_hist.dat: FORMULAS > 9')
 45   FORMAT(//,' ERROR in file kedr_hist.dat: CUT-card')
 46   FORMAT(//,' ERROR in file kedr_hist.dat: card with formula')
 47   FORMAT(//,' ERROR in file kedr_hist.dat: FORMULA <1 OR > 9') 
 51   FORMAT(/,' Cuts: (',I1,')',/)
 52   FORMAT(' C',I1,': PAR =',I2,' Interval =',1pE12.4,' - ',1pE12.4)
 53   FORMAT(/,' Logical formulas: (',I1,')',/)
 54   FORMAT(' F',I1,': ',16A1)
 55   FORMAT(/,' Parameters: (17)',/)
 56   FORMAT(' 1-',A3,'  2-',A3,'  3-',A3,'  4-',A3,'  5-',A3,'  6-',A3,
     +       '  7-',A3,'  8-',A3,'  9-',A3,//,
     +      ' 10-',A3,' 11-',A3,' 12-',A3,' 13-',A3,' 14-',A3,' 15-',A3,
     +       ' 16-',A3,' 17-',A3,/)  
 57   FORMAT(80(1h*),//,30X,'DATA FOR HISTOGRAMS',/)
 58   FORMAT(80(1h*))
 59   FORMAT(I2,':  F',I1,'  PAR =',I2)
 60   FORMAT(/,' Histograms 1-dim: (',I1,')'/) 
 61   FORMAT(//,' ERROR in file kedr_hist.dat: FORMULA-card= ',I2)
 62   FORMAT(' Histograms 2-dim: (',I1,')'/) 
 63   FORMAT(I2,':  F',I1,'  PAR-x =',I2,'  PAR-y =',I2)
 64   FORMAT(/)
 65   FORMAT(//,' ERROR in file kedr_hist.dat: 2-d hist. number ',i2)
 66   FORMAT(/,' Print parameters of group ',I1)
 67   FORMAT(/)
c
      END

      SUBROUTINE HIS_INIT1(CH,N,KOD)
c*************************************************************
c
c  Character -> number
c
c  Input:  CH    - character *1
c  Output: N     - number 0-9 
c          KOD=1 - O.K.
c 
c*************************************************************
c
      Implicit NONE
c
      Integer *4 KOD,N,I
      CHARACTER *1 CH,INMB(10)
c....... 
c  Integer numbers
      DATA INMB/'0','1','2','3','4','5','6','7','8','9'/  
c
      SAVE INMB
c
      KOD=0
c
      DO I=1,10
  	IF(CH.EQ.INMB(I)) THEN  ! number of cut
          N=I-1
          KOD=1
        END IF
      END DO
c
      END

      SUBROUTINE HIS_INIT2(CA,NC1,NC2,TEXT)
c*************************************************************
c
c  This subroutine forms text under histogram
c
c  Input:  CA     - character array *80
c          NC1    - position of first word of text
c          NC2    - position of last word of text             
c
c  Output: TEXT   - character array *80
c 
c*************************************************************
c      
      Implicit NONE
c
      CHARACTER *1 BL,T1(80),TEXT1(80)
      CHARACTER *80 CA(20),TEXT,T80,TEXT80
c
      Integer *4 NC1,NC2,JMIN,N,JM,J,I
c      
      EQUIVALENCE (T1(1),T80),(TEXT1(1),TEXT80)
c
      DATA BL/' '/
c
      IF(NC1.GT.NC2) THEN 
        PRINT 1
        STOP
      END IF
c
      IF(NC1.EQ.NC2) THEN
        TEXT=CA(NC1)
        RETURN
      END IF
c
      DO I=1,80
        TEXT1(I)=BL
      END DO
c
      JMIN=1
      N=NC1-1
c
 2    N=N+1
      IF(N.GT.NC2) GOTO 4
      T80=CA(N)
c
      DO I=1,80         ! determines lenght of word + 1 blank
        JM=I
        IF(T1(I).EQ.BL) GOTO 3
      END DO
c
 3    DO J=1,JM
c
        IF(JMIN+J-1.GT.80) THEN
          PRINT 5
          STOP
        END IF
c
        TEXT1(JMIN+J-1)=T1(J)
      END DO
c
      JMIN=JMIN+JM
      GOTO 2
c
 4    TEXT=TEXT80
c
 1    FORMAT(//,' Error in HIS_INIT2: NC1>NC2')
 5    FORMAT(//,' Error in HIS_INIT2: length of TEXT1')
      END

      SUBROUTINE HIS_INIT3(NAME,NUMB)
c*************************************************************
c
c  This subroutine gives number of parameter by name  
c
c  Input:  NAME   - character *3
c         
c  Output: NUMB   - number of parameter 
c 
c*************************************************************
c
      Implicit NONE
c
      CHARACTER *3 NAME
      Integer *4 NUMB 
c
      Integer *4 IHIS,NCU,NFO,IFOR,ICONTF,NHI1,NHI2,NHIST,NH0
      Integer *4 NH0PR,IWR_HIS
      CHARACTER *3 PARN
      Real *4 CUTD
      COMMON /HIS_SIMUL/ IHIS,PARN(20),NCU,CUTD(9,4),NFO,IFOR(9),
     +                 ICONTF(9,20),NHI1,NHI2,NHIST(18,3),NH0,NH0PR(9),
     +                IWR_HIS
      Integer *4 IR,J
c      
      IR=0
c
      DO J=1,17
        IF(NAME.EQ.PARN(J)) THEN  ! name of parameter -> it's number
          NUMB=J
          IR=1                    ! whether parameter name was determined?
        END IF
      END DO  
c
      IF(IR.EQ.0) THEN
        PRINT 3,NAME   
        STOP
      END IF
c
 3    FORMAT(/,' ERROR IN HIS_INIT3: wrong name of parameter ', A3,/)
      END