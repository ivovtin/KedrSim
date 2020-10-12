      SUBROUTINE TOF_DIGI1
c**********************************************************
c
c   Input:  /SC_PHOT/ -  numbers of counters (PMTs) 
c   in simulation as in experiment:
c
c   1-32   End-cap left  (z > 0),
c   33-64  Barrel  left  (z > 0, in e- direction)
c   65-96  Barrel  right (z < 0)
c   97-128 End-cap right (z < 0),
c
c   Output: /SC_POSL/ -  KLUKVA numbers of counters (PMT)
c
c   Author: V.A.Tayursky, 02.02.2001
c
c*********************************************************
c
      COMMON /SC_POSL/ N,IP(2,256)
      COMMON /SC_PHOT/ PHO(3,128),NPHOT,PHO1(2,256)
      INTEGER *2 N,IP,KODS(256)
      EQUIVALENCE (IPMT,PMT)
c
      SAVE KODS
c
c  Array for transformation KLYUKVA numbers (1-256)  
c  <=> inner numbers (0-31 e.t.c.) of ToF system groups
c
      DATA KODS/
c  0
     *0,1,2,3,4,5,6,7,128,129,130,131,132,133,134,135,
c  1
     *96,97,98,99,100,101,102,103,224,225,226,227,228,229,230,231,
c  2
     *8,9,10,11,12,13,14,15,136,137,138,139,140,141,142,143,
c  3
     *104,105,106,107,108,109,110,111,232,233,234,235,236,237,238,239,
c  4
     *16,17,18,19,20,21,22,23,144,145,146,147,148,149,150,151,
c  5
     *112,113,114,115,116,117,118,119,240,241,242,243,244,245,246,247,
c  6
     *24,25,26,27,28,29,30,31,152,153,154,155,156,157,158,159,
c  7
     *120,121,122,123,124,125,126,127,248,249,250,251,252,253,254,255,
c  8
     *32,33,34,35,36,37,38,39,160,161,162,163,164,165,166,167,
c  9
     *64,65,66,67,68,69,70,71,192,193,194,195,196,197,198,199,
c 10
     *40,41,42,43,44,45,46,47,168,169,170,171,172,173,174,175,
c 11
     *72,73,74,75,76,77,78,79,200,201,202,203,204,205,206,207,
c 12
     *48,49,50,51,52,53,54,55,176,177,178,179,180,181,182,183,
c 13
     *80,81,82,83,84,85,86,87,208,209,210,211,212,213,214,215,
c 14
     *56,57,58,59,60,61,62,63,184,185,186,187,188,189,190,191,
c 15
     *88,89,90,91,92,93,94,95,216,217,218,219,220,221,222,223/
c
c---------------------------------------------------------------
c
      N=0
c
      DO 1 I=1,128
c
        IAMPL=I-1
        ITIME=I+127
c
        IF(PHO(1,I).EQ.0.) GOTO 2
c
        N=N+1
        IP(1,N)=ITIME
        IP(2,N)=PHO(1,I) +0.5  ! time
c
 2      IF(PHO(2,I).EQ.0.) GOTO 1
c
        N=N+1
        IP(1,N)=IAMPL
        IP(2,N)=PHO(2,I)+0.5   ! ampl. of "early" light signal 
 1    CONTINUE
c
      NPHOT=N
c
      DO 8 I=1,NPHOT           ! this is for histograms in simulation
        IPMT=IP(1,I)
        PHO1(1,I)=PMT
        PHO1(2,I)=1.*IP(2,I)
 8    CONTINUE
c
c------------------------------------------------------------------
c
      DO 5 I=1,N   ! transformation to KLYUKVA NN for writing
c                  !                   ( 0 - 255)
        IPI=IP(1,I)
c
        DO 6 J=1,256
          IF(IPI.NE.KODS(J)) GOTO 6
          IP(1,I)=J-1
c......
c  test print
        IPMT=MOD(IPI+1,32)
        IF(IPMT.EQ.0) IPMT=32
c       print 7,IPMT,IPI,IP(1,I)
          GOTO 5
 6      CONTINUE
c
 5    CONTINUE
c
 7    FORMAT(' PMT(1-32): ',I3,'   Exp.(0-255): ',I3,
     +       '   KLYKVA(0-255): ',I3)
c
      RETURN
      END
