      SUBROUTINE TOF_TUBE(LSYS,ICOU,T01,Z1,T02,Z2,DESTEP)        
c************************************************************
c
c   Simulation of signals at PMT
c
c Input parameters:
c
c  LSYS    - system where particle moved:
c     0    - outside system
c     1    - in rigth end-cap 
c     2    - in barrel 
c     3    - in left end-cap
c  ICOU    - number of volume copy = counter (1-32)
c  T01,T02 - time of flights in c*t units before and after displacement
c  Z1, Z2  - corresponding z - coordinates (along counter)
c  DESTEP  - energy loss from GEANT common
c
c Output:
c
c  /SC_PHOT/ numeration of PMT the same as in experiment
c
c  Author: V.I.Yurchenko 
c
c  Last update: 
c               06.02.2001, 17.09.2003. V.A.Tayursky
c
c*************************************************************
      COMMON /SC_DATA/ REFS,REFP,SSP(2,2),APPA(2),A100,EPS,T0,RICH,RIKK
      COMMON /SC_PHOT/ PHO(3,128)     !  here ampl. in GeV 
c
      C=3.E+10       ! velocity of light
      EPSIL=0.15E-09 ! for 0.3 ns time resolution
c
      IF(LSYS.LE.0.OR.LSYS.GT.3.OR.ICOU.LE.0.OR.ICOU.GT.32) THEN
         PRINT *,' TOF_TUBE ERROR: LSYS,ICOU=',LSYS,ICOU
         RETURN
      END IF
c
      I=LSYS         ! system where particle moved
      IF(I.EQ.3) I=1 ! for end-caps
      SS=SSP(1,I)    ! length of end-cap(I=1) or barrel(2) counter
      SP=SSP(2,I)    ! length of end-cap(I=1) or barrel(2) lightguide
c
      Z=0.5*(Z1+Z2)     ! average Z-coord. of particle in counter
      TS0=0.5*(T01+T02) ! average time of particle in counter
c************
c      print *,'tof_tube: z=',z1,z2,z0
c      print *,'tof_tube: t=',t01,t02,ts0

      IF(I.EQ.2) GO TO 552   ! to barrel 
c
c  End-caps: I=1  LSYS=1,3   (1 - right, 3-left) 
c
      DL=0.5*SS-Z
      NN=-48*LSYS+144+ICOU   ! V.Tayursky
      GO TO 660
c
c  Barrel
c
552   CONTINUE               ! barrel, right I=2, NN=65-96
      DL=Z+0.5*SS                       
      NN=I*32+ICOU           ! V.Tayursky
      GO TO 660
c
553   CONTINUE               ! barrel, left, I=3, NN=33-64
      DL=0.5*SS-Z          
      NN=(I-2)*32+ICOU       ! V.Tayursky
c
660   CONTINUE
      IF(NN.LE.0.OR.NN.GT.128) THEN   
        print *,' TOF_TUBE ERROR: ICOU=',NN  
        RETURN                        
      END IF                       
c
      IF(DL.LT.0.) THEN
c        print *,' TOF_TUBE: IEVENT , DL cm',IEVENT,DL
        RETURN                  ! V.Tayursky
      END IF
c
      SOPT=DL*REFS              ! optical length in scint.
      SOPT1=SP*REFP             ! optical length in lightguide (V.Tayursky) 
      TS=TS0+SOPT/C             ! time of arrival of signal 
c      TS=TS0+(SOPT+SOPT1)/C    ! time of arrival of signal with lightguide
      DE=DESTEP*EXP(-DL/APPA(1))           
      TAV=PHO(1,NN)             ! average time for early signals
      DTT=ABS(TAV-TS)           ! difference 
c
      IF(DTT.LT.EPSIL.OR.TAV.EQ.0.) THEN
        SUMSIGN=PHO(2,NN)+DE    ! sum amplitude of early signals
        PHO(1,NN)=(TS*DE+TAV*PHO(2,NN))/SUMSIGN
        PHO(2,NN)=SUMSIGN       ! sum amplitude of early signals
        GO TO 770
      END IF
c
      IF(TAV.LT.TS)  GO TO 770
      PHO(1,NN)=TS
      PHO(2,NN)=DE
c
770   PHO(3,NN)=PHO(3,NN)+DE  ! sum of energy deposition
      IF(I.EQ.1) GO TO 777    ! end-caps
      I=I+1                   ! barrel
      IF(I.EQ.3) GO TO 553    ! barrel, left
c       
777   CONTINUE
c
      RETURN
      END
