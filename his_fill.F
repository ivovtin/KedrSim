      SUBROUTINE HIS_FILL
c**********************************************************
c
c   Fills histograms
c
c   Author:  V.A.Tayursky,  23 Oct 2003
c
c   Corrections: 13 Mar 2004, 22 Mar 2006, 7 Oct 2008
c
c**********************************************************
c
      Implicit NONE 

      Integer *4 IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
      Integer *4 IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
      COMMON /GCFLAG/ IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)

      Integer *4 nh,kh
      Real *4 ph,vh
      COMMON/lujeth/nh,kh(4000,5),ph(4000,5),vh(4000,5)
c
      Integer *4 IHIS,NCU,NFO,IFOR,ICONTF,NHI1,NHI2,NHIST,NH0,NH0PR
      Integer *4 IWR_HIS   
      Real *4 CUTD
      CHARACTER *3 PARN
      COMMON /HIS_SIMUL/ IHIS,PARN(20),NCU,CUTD(9,4),NFO,IFOR(9),
     +                  ICONTF(9,20),NHI1,NHI2,NHIST(18,3),NH0,NH0PR(9),
     +                  IWR_HIS
      CHARACTER *12 PN
      Real *4 PARAM(20),PI,PX,PY,PZ,PTOT,TETA,TTETA,CTETA,FI
      Real *4 AMASS,EN,X,Y,PMASS,CHAR,TLIF
      Integer *4 ICUTS(9),IFORMS(9),I,MECH,KF,KGEANT,ITYP
      Integer *4 INDX,K,KHI,NGEN,INDPAR
c
c  Names of parameters:
c
c   1-ENE   2-MOM   3-TET  4-CTE  5-PHI  6-TYP  7-GEN   (on particles)
c   8-NPW   9-VEX  10-VEY 11-VEZ 12-TOR 13-TTR          (on particles)
c  14-ACN  15-ACA  16-TFN 17-TFA 18-RE1 19-RE2  20-RE3  (on ACC, TOF, reserv)
c
      PI=3.1415927
c
        IF(NH0.GT.0) THEN
          IF(NH0PR(1).EQ.1) PRINT 81,IEVENT
          IF(NH0PR(1).EQ.0) PRINT 82,IEVENT 
        END IF
c
c----- Parameters are formed from history of event: /lujeth/ ---------
c
      IF(NH.EQ.0) GOTO 30
c
      DO 10 I=1,NH       ! 
c
        MECH = KH(I,1)	 ! number of last mechanism (see/GCTRAK/) on
c                        ! last particle step, 0 if particle reached
c                        ! bounds of detector
        KF = KH(I,2)              ! KF-code
c
        CALL PstPCode1(KF,KGEANT)! KGEANT - GEANT code of particle
c
        ITYP=KGEANT
c
c        N_PARENT = KH(I,3)       ! number of parent
c        N_DAUGTF = KH(I,4)       ! number of first daughter
c        N_DAUGTL = KH(I,5)       ! number of last  daughter
        PX = PH(I,1)              ! Px, GeV/c
        PY = PH(I,2)              ! Py, GeV/c
        PZ = PH(I,3)              ! Pz, GeV/c
c
        PTOT=SQRT(PX**2+PY**2+PZ**2)       ! 2
        TTETA=SQRT(PX**2+PY**2)/ABS(PZ)
 	TETA=ATAN(TTETA)                   ! 3
        IF(PZ.LT.0.) TETA=PI-TETA
        CTETA=COS(TETA)
        TETA=TETA*180/PI
 	FI=ATAN2(PY,PX)*180/PI
        IF(FI.LT.0.) FI=360.+FI            ! 5
c
 	PARAM(1)=PH(I,4)*1000.             ! Energy
   	PARAM(2)=PTOT*1000.
        PARAM(3)=TETA
 	PARAM(4)=CTETA
        PARAM(5)=FI
 	PARAM(6)=1.*ITYP
        INDX=I
c
        CALL HIS_FILL4(INDX,NGEN)  ! number of generation
c
        PARAM(7) =1.*NGEN   ! number of generation
        PARAM(8) =1.*I      ! number of particle in /lujeth/
        PARAM(9) = VH(I,1)  ! vertex x pozition, mm
        PARAM(10)= VH(I,2)  ! vertex y pozition, mm
        PARAM(11)= VH(I,3)  ! vertex z pozition, mm
        PARAM(12)= VH(I,4)  ! time of origin, mm
        PARAM(13)= VH(I,5)  ! time of particle tracking in detector, mm 
c
        IF(NH0PR(1).EQ.1) THEN  
          PRINT 80,I,NGEN,ITYP,PARAM(1),PARAM(2),PARAM(3),PARAM(5),
     +            PARAM(9),PARAM(10),PARAM(11),PARAM(12),PARAM(13),MECH
c
        END IF 
c
 	CALL HIS_FILL1(PARAM,ICUTS)  ! calculation of cuts
c
        CALL HIS_FILL2(ICUTS,IFORMS) ! calculation of log.formula 
c
c        print 100,icuts
c        print 102,iforms

 	EN    = PH(I,4)		! E, GeV
 	AMASS = PH(I,5)		! m, GeV/c**2
c
 	CALL PPAR(KH(I,2),PN,PMASS,CHAR,TLIF)
c
c        print *,'i, mech,pname, parent,N-d1,N-dn=',i,kh(i,1),' ',pn,
c     +       kh(i,3),kh(i,4),kh(i,5)
c        print *,'P,E,m=',SQRT(PH(I,1)**2+PH(I,2)**2+PH(I,3)**2)*1000,
c     +  (ph(i,j)*1000,J=4,5)
c        print *,'x,y,z,time,lifetime=',(vh(i,j),j=1,5)
c
c***************
c        print *,' parameters from /lujth/: i=',i,(param(k),k=1,13)
c

        DO 31 K=1,NHI1               ! fill 1-dim. histogramm
          KF=NHIST(K,1)              ! N of log.formula for this histogram
c
          IF(K.EQ.1) THEN
            IWR_HIS=0 
            IF(IFORMS(KF).EQ.1) IWR_HIS=1 ! for filerw.f: write event
          END IF
c
 	  IF(IFORMS(KF).EQ.0) GOTO 31
 	  INDPAR=NHIST(K,2)          ! N of parameter for this hist.
 	  X=PARAM(INDPAR)
 	  KHI=K
c	                  abs.   ord.	 weight
c	                   | 	  |        |  
 	  CALL HFILL(KHI,  X,	  Y,       1.)
c
 31	CONTINUE
c
        DO 32 K=1,NHI2          ! fill 2-dim. histogramm
          KHI=K+NHI1
 	  KF=NHIST(KHI,1)              ! N of log.formula for this histogram
c
          IF(IFORMS(KF).EQ.0) GOTO 32
 	  INDPAR=NHIST(KHI,2)          ! N of parameter-x for this hist.
          X=PARAM(INDPAR)
c          print *,'his=',i,' 2-dim his=',k     
c          print *, 'indpar=',indpar,' x=',x
 
   	  INDPAR=NHIST(KHI,3)          ! N of parameter-y for this hist.
          Y=PARAM(INDPAR) 
c        print *, 'indpar=',indpar,' y=',y


c                         abs.   ord.    weight
c                          |	  |        |
          CALL HFILL(KHI,  X,	  Y,       1.)
c
 32     CONTINUE
c
 10   CONTINUE  ! end of cycle on calculation of parameters using /lujeth/
c
c-------------------------------------------------------------------------
c
 30   CONTINUE
c
      CALL HIS_FILL5    ! parameters on PMT
c
      CALL HIS_FILL6    ! parameters on ATC
c
 80   FORMAT(I3,I3,I3,F8.1,F8.1,F6.1,F6.1,3F7.1,1PE9.2,1PE9.2,I3)

 81   FORMAT(/,' Event=',I6,/,
     +      '  N Ge Typ E(MeV) P(MeV/c) Teta   Fi',
     +      '        Vertex (mm)    To(mm)   Ttr(mm)  Me',/)
 82   FORMAT(/,' Event=',I6,/)
c
101   format(/,' par  =',7f10.3)
100   format(/,' cuts =',9i2,/)
102   format(/,' forms=',9i2,/)

      END


      SUBROUTINE HIS_FILL1(PARAM,ICUTS)
c*******************************************************************
c
c  Calculation of cuts (1/0) for parameters of event
c
c*******************************************************************
      Implicit NONE

      Integer *4 IHIS,NCU,NFO,IFOR,ICONTF,NHI1,NHI2,NHIST,NH0,NH0PR
      Integer *4 IWR_HIS
      Real *4 CUTD
      CHARACTER *3 PARN
      COMMON /HIS_SIMUL/ IHIS,PARN(20),NCU,CUTD(9,4),NFO,IFOR(9),
     +                  ICONTF(9,20),NHI1,NHI2,NHIST(18,3),NH0,NH0PR(9),
     +                  IWR_HIS 
      Integer *4 I,IPAR,J,ICUTS(9),ICUTD(9,4)
      Real *4 PMIN,PMAX,PARAM(20)

      EQUIVALENCE (ICUTD(1,1),CUTD(1,1))
c
      DO 1 I=1,NCU  ! cycle on cuts 
        PMIN=CUTD(I,3)
        PMAX=CUTD(I,4)
        IPAR=ICUTD(I,2) ! 
c
        DO J=1,17  ! cycle on parameters
c
          IF(IPAR.EQ.J) THEN        ! search for number of parameter
            ICUTS(I)=0
            IF(PARAM(J).GE.PMIN.AND.PARAM(J).LE.PMAX) ICUTS(I)=1
            GOTO 1
          END IF
c
        END DO
c
 1    CONTINUE
c
      END


      SUBROUTINE HIS_FILL2(ICUTS,KODFOR)
c*******************************************************************
c
c  Calculation of values (1/0) of log. formulas for cuts
c
c  Input: ICUTS (1/0) - array of cuts on parameters
c
c  Output: KODFOR (1/0) - array of values of log.formulas for these cuts
c
c  4 Oct 2003
c
c*******************************************************************
      Implicit NONE

      Integer *4 IHIS,NCU,NFO,IFOR,ICONTF,NHI1,NHI2,NHIST,NH0,NH0PR
      Integer *4 IWR_HIS
      Real *4 CUTD
      CHARACTER *3 PARN
      COMMON /HIS_SIMUL/ IHIS,PARN(20),NCU,CUTD(9,4),NFO,IFOR(9),
     +                  ICONTF(9,20),NHI1,NHI2,NHIST(18,3),NH0,NH0PR(9),
     +                  IWR_HIS

      Integer *4 LOG1(20),ICUTS(9),KODFOR(9),I,LNG,J,IL
c
      DO 1 I=1,NFO            ! cycle on log.formulas
        LNG=ICONTF(I,20)      ! number of elements in i-th log.formula
c
        DO J=1,LNG
          LOG1(J)=ICONTF(I,J) ! copy of one log.formula
        END DO
c
        CALL HIS_FILL3(1,LNG,LOG1,ICUTS,IL)  ! IL=1/0 - value of log. formula
c
        KODFOR(I)=IL          ! for output
c
 1    CONTINUE
c
      END


      SUBROUTINE HIS_FILL3(N1,N2,INTLG,KODCUT,IL)
c*********************************************************************
c
c  HIS_FILL3 calculates value of part of logical formula
c
c  Input:
c
c   N1 - 1-st member of log.formula in array INTLG
c   N2 - last member of log.formula in array INTLG
c   INTLG - array with log. formula
c   KODCUT - array of 1/0 values of cuts for given event
c
c  Output:
c
c   IL=1/0 - value of part from N1 to N2 of logical formula
c
c  4 Oct 2003
c
c*********************************************************************
c
      Implicit NONE
c
      Integer *4 N1,N2,INTLG(20),LG(20),KODCUT(9),L,I1,IL,IN,KC
      Integer *4 ISUM,ISUM1,I	
c
c********
c      print *,' '
c      print *,' his_fill3: n1,n2 =',n1,n2
c      print *,' his_fill3: intlg =',intlg
c      print *,' his_fill3: kodcut=',kodcut
c     
      L=0
      I1=0
c
c  Preparation of array LG
c
      DO 1 I=N1,N2
        IN=INTLG(I)                    ! element of log.f-la: N of cut,/,+,*
        IF(IN.GE.100) GOTO 2           ! goto 2 for 100="*", 200="+", 300="/"
c       IF(IN.GT.100) GOTO 2	       ! old
c
        KC=KODCUT(IN)                  ! KC=1/0 - value of cut 
c
        IF(INTLG(I-1).EQ.300) KC=1-KC  
c 
        I1=I1+1
        LG(I1)=KC
c**********      
c      print *,' his_fill3: i,in,kodcut(in),i1,kc:',i,in,kodcut(in),i1,kc

          GOTO 1
c....
c   Symbols: *,+,/
 2      IF(IN.EQ.300) GOTO 1
        I1=I1+1
        LG(I1)=IN     ! 
 1    CONTINUE

c**********
c      print *,' his_fill3: lg:',(lg(ii),ii=1,i1)
c
      IF(I1.GT.1) GOTO 5
c
      IF(I1.EQ.0) THEN
        PRINT 100
        STOP
      END IF
c........
c  one element in log.formula
      IL=LG(I1)
      RETURN
c.........
c   Formula with cuts 1/0 separated by "*" or "+"
 5    ISUM=0   ! for all formula
c
      DO 3 I=1,I1
c
c***********
c      print *,' his_fill3: i,lg(i)=',i,lg(i)

        IF(LG(I).GE.100) GOTO 3   ! "*", "+"
c....
c   LG(I)=1/0
        IF(I.EQ.1) THEN
          ISUM1=LG(1)
          GOTO 32
        END IF
c
 31     IF(LG(I-1).EQ.100) THEN ! "*"
          ISUM1=ISUM1*LG(I)
          GOTO 32
        END IF
c
        IF(LG(I-1).EQ.200) THEN ! +
          ISUM=ISUM+ISUM1
          ISUM1=LG(I)           
        END IF
c
 32     IF(I.EQ.I1) THEN
          ISUM=ISUM+ISUM1
        END IF       
c
 3    CONTINUE
c
c*********
c      print *,' his_fill3: isum1, isum=',isum1,isum

      IL=1
      IF(ISUM.EQ.0) IL=0

100   FORMAT(' Error in HIS_FILL3')
      END


      SUBROUTINE HIS_FILL4(I,NGEN)
c*********************************************************************
c
c  HIS_FILL4 gives number of generation of I-th particle in /lujeth/
c
c  Input:
c
c   I  - index in /lujeth/
c
c  Output:
c
c   NGEN=0,1,2,3,4 - number of generation
c        -1       - generation can not be determined
c
c  9 Oct 2003
c
c*********************************************************************
c
      Implicit NONE

      Integer *4 nh,kh
      Real *4 ph,vh
      common/lujeth/nh,kh(4000,5),ph(4000,5),vh(4000,5)
c
      Integer *4 I,NGEN,I2,I1,I3,I4,I5
c
      NGEN=-1   ! generation can not be determined
c
      IF(NH.EQ.0.OR.I.GT.NH) RETURN
c
      I1 = KH(I,3)	! number of parent
c
      IF(I1.EQ.0) THEN
        NGEN=0
        GOTO 100
      END IF
c
      I2 = KH(I1,3)      ! number of parent
c
      IF(I2.EQ.0) THEN     
 	NGEN=1
        GOTO 100
      END IF
c
      I3 = KH(I2,3)      ! number of parent
c
      IF(I3.EQ.0) THEN       
 	NGEN=2  
        GOTO 100
      END IF 
c
      I4 = KH(I3,3)	 ! number of parent
c
      IF(I4.EQ.0) THEN
        NGEN=3  
        GOTO 100
      END IF
c    
      I5 = KH(I4,3)	 ! number of parent
c
      IF(I5.EQ.0) THEN
        NGEN=4  
        GOTO 100
      END IF    
c
 100  RETURN
      END

      SUBROUTINE HIS_FILL5
c**********************************************************
c
c   Fills histograms on PMT (parameters 11,12)
c
c**********************************************************
c
      Implicit NONE

      Integer *4 IHIS,NCU,NFO,IFOR,ICONTF,NHI1,NHI2,NHIST,NH0,NH0PR
      Integer *4 IWR_HIS
      Real *4 CUTD
      CHARACTER *3 PARN
      COMMON /HIS_SIMUL/ IHIS,PARN(20),NCU,CUTD(9,4),NFO,IFOR(9),
     +                  ICONTF(9,20),NHI1,NHI2,NHIST(18,3),NH0,NH0PR(9),
     +                  IWR_HIS
      Real *4 PHO,PHO1
      Integer *4 NPHOT
      COMMON /SC_PHOT/ PHO(3,128),NPHOT,PHO1(2,256)

      Real *4 PARAM(20),PMT,X,Y
      Integer *4 ICUTS(9),IFORMS(9),IPMT,I,J,K,KHI,KF,INDPAR,IPARAM

      EQUIVALENCE (PMT,IPMT)
c
c*******************
c
c   Time  channals - 1ns=25ch.
c   Ampl. channals - 1pe=5ch. 
c
      DO 1 I=1,NPHOT
c
        DO J=1,15
          PARAM(J)=1.E20
        END DO
c
        PMT=PHO1(1,I)
        IPMT=IPMT+1
c
       	PARAM(16)=1.*IPMT          ! number of PMT (t:1-128, a:129-256) 
c
        PARAM(17)=1.*PHO1(2,I) 
c
        IF(NH0PR(3).EQ.1) THEN
          IPARAM=PARAM(17)+0.5
          IF(IPMT.LE.128) PRINT 101, IPMT,IPARAM
          IF(IPMT.GT.128) PRINT 102, IPMT-128,IPMT,IPARAM
        END IF
c
        CALL HIS_FILL1(PARAM,ICUTS)  ! calculation of cuts
c
c***************
c        print 100, (icuts(k),k=1,NCU)
c
        CALL HIS_FILL2(ICUTS,IFORMS) ! calculation of log.formulas
c
 	DO 41 K=1,NHI1       	     ! fill 1-dim. histogramm
          KF=NHIST(K,1)      	     ! N of log.formula for this histogram
 	  IF(IFORMS(KF).EQ.0) GOTO 41
	  INDPAR=NHIST(K,2)          ! N of paramater for this hist.
 	  X=PARAM(INDPAR)
          KHI=K
c	                  abs.   ord.    weight
c	                   |	  |        |
          CALL HFILL(KHI,  X,	  Y,       1.)
c
 41      CONTINUE

c
        DO 42 K=1,NHI2           ! fill 2-dim. histogramm
          KHI=K+NHI1
          KF=NHIST(KHI,1)		! N of log.formula for this hist.
          IF(IFORMS(KF).EQ.0) GOTO 42
          INDPAR=NHIST(KHI,2)          ! N of paramater-x for this hist.
          X=PARAM(INDPAR)
          INDPAR=NHIST(KHI,3)          ! N of paramater-x for this hist.
          Y=PARAM(INDPAR)
c                         abs.   ord.	 weight
c	                     |	  |        |
 	  CALL HFILL(KHI,  X,     Y,       1.) 
c
 42       CONTINUE
c
 1      CONTINUE   ! end of cycle on PTM
c
 100  format(' HIS_FILL5: cuts=',9i2,/)
 101  format('  N_tof=',I3,'      T =',I10)
 102  format('  N_tof=',I3,'(',I3,')',' A =',I10)
c
      END


      SUBROUTINE HIS_FILL6
c**********************************************************
c
c   Fills histograms on ATC (parameters 9,10)
c
c**********************************************************
c
      Implicit NONE
c
      Integer *4 IHIS,NCU,NFO,IFOR,ICONTF,NHI1,NHI2,NHIST,NH0,NH0PR
      Integer *4 IWR_HIS
      Real *4 CUTD
      CHARACTER *3 PARN
      COMMON /HIS_SIMUL/ IHIS,PARN(20),NCU,CUTD(9,4),NFO,IFOR(9),
     +                  ICONTF(9,20),NHI1,NHI2,NHIST(18,3),NH0,NH0PR(9),
     +                  IWR_HIS
      INTEGER *2 NATC,IATC
      COMMON /ATC_COMW/  NATC,IATC(2,320)       !  here ampl. in channals
      Integer *4 NBGR
      COMMON /ATCCOM/    NBGR(8)
      Real *4 PARAM(20),PI,X,Y
      Integer *4 ICUTS(9),IFORMS(9),I,J,KHI,KF,INDPAR,K
c
      PI=3.1415927
c
c  Parameters on ATC (14,15)
c
      DO I=1,NATC
c
        DO J=1,13
          PARAM(J)=1.E20
        END DO
c
        PARAM(16)=1.E20
        PARAM(17)=1.E20
        PARAM(14)=1.*IATC(1,I)
c
c  IATC(1,.)=1-160: N of counter or shifter 
c  IATC(2,.)=Ampl. in counter or shifter. Ampl. with sign "-" for shifter
c
        PARAM(15)=1.*IATC(2,I) 
        IF(NH0PR(2).EQ.1) PRINT 101, IATC(1,I),IATC(2,I)
c
        CALL HIS_FILL1(PARAM,ICUTS)  ! calculation of cuts
c
c************
c        print 100, (icuts(k),k=1,NCU)
c
        CALL HIS_FILL2(ICUTS,IFORMS) ! calculation of log.formulas
c
 	DO 41 K=1,NHI1       	     ! fill 1-dim. histogramm
          KF=NHIST(K,1)      	     ! N of log.formula for this histogram
 	  IF(IFORMS(KF).EQ.0) GOTO 41
	  INDPAR=NHIST(K,2)          ! N of paramater for this hist.
 	  X=PARAM(INDPAR)
          KHI=K
c	                  abs.   ord.    weight
c	                   |	  |        |
          CALL HFILL(KHI,  X,	  Y,       1.)
c
 41      CONTINUE
c
        DO 42 K=1,NHI2           ! fill 2-dim. histogramm
          KHI=K+NHI1
          KF=NHIST(KHI,1)		! N of log.formula for this hist.
          IF(IFORMS(KF).EQ.0) GOTO 42
          INDPAR=NHIST(KHI,2)          ! N of paramater-x for this hist.
          X=PARAM(INDPAR)
          INDPAR=NHIST(KHI,3)          ! N of paramater-x for this hist.
          Y=PARAM(INDPAR)
c                         abs.   ord.	 weight
c	                     |	  |        |
 	  CALL HFILL(KHI,  X,     Y,       1.) 
c
 42	CONTINUE

      END DO
c
 101  FORMAT('  N_atc=',I3,'      A =',I10)
 100  FORMAT(' HIS_FILL6: cuts=',9i2,/)
c
      END
