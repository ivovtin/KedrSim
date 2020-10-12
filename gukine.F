      SUBROUTINE GUKINE
c*****************************************************************
c
c   GENERATES PRIMARY TRACKS KINEMATICS
c
c   Last changes: see file  0_history
c
c*****************************************************************
c
      IMPLICIT NONE
c
      Integer *4 IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN,
     &           IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)

       Real *4 PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
       COMMON/GCONST/PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS

      Real *4 P,V
      Integer *4 N,K
      COMMON/LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)

      Real *4 PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
      Integer *4 IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD

      Real *4 Sigma_X, Sigma_Y, Sigma_Z,Beam_Theta, Beam_Phi
      Integer *4 Sigbeam_flag
      COMMON /Beam_sigma/ Sigbeam_flag, Sigma_X, Sigma_Y, Sigma_Z,
     +      Beam_Theta, Beam_Phi

      Real *4 EPART,PXPART,PYPART,PZPART
      Integer *4 NTRACK,IPARTICLE
      COMMON /PKIN/ NTRACK,IPARTICLE(50),EPART(50),  ! for output file
     +              PXPART(50),PYPART(50),PZPART(50) ! with primary tracks

      Real *4 BRAT
      Integer *4 MDCY,MDME,KFDP
      COMMON /LUDAT3/   MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)

      Real *4 VER,VERTEX
      COMMON /VER_EVENT/VER(3),VERTEX(3)      ! center of int. area from data
                                              ! real vertex
      Real *4 GENPAR
      Integer *4 KHEP,KGEN
      COMMON /KEDR_GEN/ KHEP,KGEN,GENPAR(20)  ! parameters for generators

      Integer *4 IGENRNG,ISKIP
      COMMON /GEN_RNG/ IGENRNG,ISKIP          ! flag for RNG for generators

      Real *4 RRLU
      Integer *4 MRLU
      COMMON /LUDATR/  MRLU(6),RRLU(100)      ! used for RNG generator

      Real *4 RLU,DUM,S1,S2,S3,S4,PHB,THB,PLAB(3),VERG(3),EACT
      Integer *4 IRLUCALL0,I1,I2,LUCOMP,KGEN0,IPRTRK,I
      Integer *4 IERR,NLUND,IUNIT,ITYPE,NVERT,NT
      Logical *4 LEXIST
c
      SAVE
c
      KGEN0=KGEN        ! save initial value
      IPRTRK=0          ! this parameter =1/0 - for prints of initial tracks
c
      IF(KGEN0.LT.0) THEN
        IPRTRK=1
        KGEN=-KGEN0     ! KGEN should be >0 for calls of generators
      END IF
c
      NLUND=GENPAR(2)   ! ?
c
      IF(IDEBUG.EQ.1) PRINT 1001,IEVENT,NRNDM(1),NRNDM(2)! prints for debugging
c
c-- To make some particles in LUND stable ---------------------------
c
      MDCY(LUCOMP(310),1)=0     ! K0s  
      MDCY(LUCOMP(3222),1)=0    ! Sigma+
      MDCY(LUCOMP(3112),1)=0    ! Sigma-
      MDCY(LUCOMP(3122),1)=0    ! Lambda
      MDCY(LUCOMP(3322),1)=0    ! Xi0   
      MDCY(LUCOMP(3312),1)=0    ! Xi-   
      MDCY(LUCOMP(3334),1)=0    ! Omega-
      MDCY(LUCOMP(130),1)=0     ! K0L
c
c--  Initialization of uniform Jetset generator RLU -----------------
c    (= CERNLIB V115 RANLUX with low luxury=1) 
c
      IF(IEVENT.EQ.1) THEN 
c
c. "G. Marsaglia, A. Zaman and W.-W. Tsang, Stat. Prob. Lett. 9 (1990) 35"
c. Implementation similar to "F. James, Computer Physics Commun. 60 (1990) 329"
c. see in /cern/old/src/mclibs/jetset/jetset/rlu.F
c.
c.      IF(IGENRNG.LT.0) THEN    ! generator state should be read from file
c         The line above would be more logical since 14/12/2007, but 
c         the line below is kept for compatibility
c
        IF(IGENRNG.EQ.1) THEN   ! generator state should be read from file
          INQUIRE (FILE='kedr_gen0.dat',EXIST=LEXIST)  ! is there this file? 
c
          IF(LEXIST) THEN
c
            CALL SEARCH_FREE_UNIT(IUNIT) 
c  
            OPEN(UNIT=IUNIT,FILE='kedr_gen0.dat')  
c
            READ(IUNIT,FMT=115,ERR=111,IOSTAT=IERR) (MRLU(I1),I1=1,5),
     &                                 (RRLU(I2),I2=1,100)
            PRINT 117                 
            CLOSE (IUNIT)
            IRLUCALL0=MRLU(3)
          ELSE
c
 111       PRINT *,' KEDRGEN error: file kedr_gen0.dat, IOSTAT =',IERR
c
          END IF
c
        END IF   !    generator state should be read from file
c
        IF(MRLU(2).EQ.0.and.IGENRNG.GT.0. and.IGENRNG.LT.30082*31329)
     &     MRLU(1)=IGENRNG    
c
c  if put 'IGENRNG.GE.0' takes seed=0 instead def
c  MRLU(1)=seed for one of 30082*31329=942438978 (almost 10**9) independent 
c  sequences, each with period over 10**43
c  30082=2*13*13*89, 2*89=178,  31329=177*177, 177=3*59
c
c   Jetset default seed 0x12DD397=19780503=3*17*387853 differs from
c    Marsaglia/CERN default 0x33b49b1=54217137=3*13*67*20749
c
c   MRLU(2) - if = 0, at next call of RLU(idummy) will restart generator 
c                     with seed=MRLU(1) (MRLU(2)=1 and MRLU(3)=0 after restart)
c        if > 0, number of calls with seed=MRLU(1) is 10**9*(MRLU(2)-1)+MRLU(3)
c   MRLU(4), MRLU(5) - internal variables 
c   MRLU(6) - how many records after beginning are written into the file
c             representing generator state; is used by RLUGET and RLUSET
c
        DO I=1,ISKIP
          DUM=RLU(0)
        END DO
c
        print *,' '
        print *,' gukine.F:  IEVENT=1   MRLU(1:6) are set to'
        print *,' '
        print '(6I11)',(MRLU(I1),I1=1,6)
c
      END IF                             !  IEVENT.EQ.1
c             
      IF(MOD(IEVENT,ITEST).EQ.0.AND.IEVENT.GT.1) THEN
c
        IF(IABS(KGEN).NE.310) THEN
          PRINT *,' BEFORE IEVENT=',IEVENT,
     &    ' NUMBER OF RLU CALLS = 10^9*',MRLU(2)-1,' + ',MRLU(3)
        ELSE
          PRINT *,' BEFORE IEVENT=',IEVENT,' NUMBER OF RLU CALLS= 0'
        END IF      
c
      END IF 
c
c-- Primary tracks simulation: fills /PKIN/ ---------------------------
c
      CALL KEDRGEN(KGEN0,EACT,VERG) ! interface to programs of KedrGen e.t.c.
c                               ! 
c                               vertex of event from generators
c 
c-- Simulation of the vertex of the event ----------------------------
c
      IF(KGEN0.eq.53) GOTO 105  ! for cosmic generator 53 all done
      IF(KGEN0.eq.52) GOTO 104  ! for cosmic generator 52 vertex is defined
c
      CALL VZERO(VERTEX,3)      ! VERTEX(i)=0.
c
      IF (SIGBEAM_FLAG.GE.1) THEN
c
        CALL GRANOR(S1,S2)
c
        CALL GRANOR(S3,S4)
c
        VERTEX(1) = Sigma_X*S1     ! Gaussian distr.
        VERTEX(2) = Sigma_Y*S2
        VERTEX(3) = Sigma_Z*S3
c
        VERTEX(1)=VERTEX(1)+VERG(1)     ! account of vertex from generator
        VERTEX(2)=VERTEX(2)+VERG(2)
        VERTEX(3)=VERTEX(3)+VERG(3)
c
c-- Account of angle between beam axis and Z-axis --------------------
c
        PHB=Beam_Phi*DegRad          ! degrees -> radians
        THB=Beam_Theta*DegRad
c
        CALL KEVROT(THB,PHB,VERTEX)  ! rotation of simulated momenta, vertex
c
        VERTEX(1)=VER(1)+VERTEX(1)   ! VER(i) - center of i.p.
        VERTEX(2)=VER(2)+VERTEX(2)
        VERTEX(3)=VER(3)+VERTEX(3)
      END IF
c
 104  CALL GSVERT(VERTEX,0,0,0,0,NVERT) ! fill vertex of event
c
      DO I=1,NTRACK
        PLAB(1)=PXPART(I)
        PLAB(2)=PYPART(I)
        PLAB(3)=PZPART(I)
        ITYPE=IPARTICLE(I)
c
        IF(IPRTRK.EQ.1) THEN             ! print out init. tracks in lab.
          IF(IEVENT.EQ.1.AND.NTRACK.EQ.1) PRINT 396
          PRINT 397,IEVENT,NTRACK,ITYPE,PLAB,VERTEX
          KGEN=KGEN0
        END IF
c
        CALL GSKINE(PLAB,ITYPE,NVERT,0,0,NT) ! fill primary tracks
c
      END DO
c
c-- For the last event -------------------------------------------------
c
 105  IF(IEVENT.EQ.NEVENT) THEN ! this is the last event
c                                       ! we should write to file
        CALL SEARCH_FREE_UNIT(IUNIT)    ! constants for RNG of events gen-s
c
        OPEN (UNIT=IUNIT,FILE='kedr_gen0.dat')
c
        WRITE(IUNIT,FMT=115, ERR=110,IOSTAT=IERR) (MRLU(I1),I1=1,5),
     &                                 (RRLU(I2),I2=1,100)

        print *,' '
        print *,' gukine.F:  Last IEVENT =',IEVENT,'  MRLU(1:6) copied'
     &         , ' to kedr_gen0.dat ='
        print *,' '
        print '(6I11)',(MRLU(I1),I1=1,6)
c
        PRINT 118
        CLOSE (IUNIT)
c
        GOTO 116
c
 110    PRINT *, ' error when writing file kedr_gen0.dat, IOSTAT =',IERR
c
        STOP
      END IF
c
 116  CONTINUE
c
 115  FORMAT(5Z8,/,10(10Z8,/))   
 117  FORMAT(/,' KEDRSIM read file kedr_gen0.dat from disk',/)
 118  FORMAT(/,' KEDRSIM wrote file kedr_gen0.dat to disk',/) 
 396  FORMAT(/,'  Ev ',' Trk ',' Typ ',7X,'Momentum, GeV/c',18X,
     &       'Vertex, cm',/) 
 397  FORMAT(I5,2I4,3F10.6,3F10.4)
 1001 FORMAT(' __________ EVENT N ',I5,' NRNDM: ',2I12,
     +' _____________')
      RETURN
      END
