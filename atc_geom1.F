      SUBROUTINE ATC_GEOM1(N,NRM,COUN)
c------------------------------------------------------------------
c                                                                 |  
c    ATC_GEOM1 is called from ATC_GEOM - Cher.counters geom.subr. |
c                                                                 |
c                                                                 |
c                                                                 |
c  Al block with aerogel+shifter is being inserted into 'KEDR'    |
c  and name of counter (A001-A160) is assigned to this Al block   |
c                                                                 |
c  Input parameters:                                              |
c                                                                 |
c  N - number of the Cherenkov counter:                           |
c                                                                 |
c         Inner layer              Outer layer                    |
c                                                                 |
c      1-20  - end-cap z<0                                        |
c      21-40 - barrel  z<0         the same + 80                  |
c      41-60 - barrel  z>0                                        |
c      61-80 - end-cap z>0                                        |
c                                                                 |
c  NRM  - number of rotation matrix for this counter              |
c  COUN - array with names of blocks                              |
c                                                                 |
c  Author: V.A.TAyursky                                           |
c                                                                 |
c  Last change 20.05.98   V.A.Tayursky                            |
c                                                                 |
c------------------------------------------------------------------
c
#include "gconst.inc"
c
      include 'medpar.inc'
c
      CHARACTER *4 COUN(160)
c...........
c  Constants
      DFI_BA=18.   !  360/20  angles for fi rotation for barrel
      DFI_EC=18.   !  360/20  angles for fi rotation for endcap
c
100   if(N.lt.1.or.N.gt.160) print 999
      if(N.lt.1.or.N.gt.160) stop
c
      IK=0
      IF(N.LE.20) GOTO 11                ! to end-cap
      IF(N.GE.61.AND.N.LE.100) GOTO 11
      IF(N.GE.141) GOTO 11
c--------------------------------------------------------------
c
      IF(N.GE.101.AND.N.LE.140) GOTO 121
c
c******************** Barrel, inner layer *******************
c
c...N=21-40 (z<0) and 41-60 (z>0)
c
      IK=2
      DFI=DFI_BA*(N-21)
      IF(N.GT.40) DFI=DFI_BA*(N-41)
      X1=0.
      Y1=0.
C
      IF(N.LE.40) THEN
      Z1=-27.6
      IF(MOD(N,2).EQ.0) Z1=-27.1
      ELSE
      Z1=27.6
      IF(MOD(N,2).EQ.1) Z1=27.1
      END IF
C
      GOTO 200
c
c******************* Barrel, outer layer *********************
c
c...N=101-120 (z<0) and 121-140 (z>0)
c
121   IK=2
      DFI=DFI_BA*(N-101)+5.       !  5 Deg. - rotation in dfi   
      IF(N.GT.120) DFI=DFI_BA*(N-121)+5.
      X1=0.
      Y1=0.
C
      IF(N.LE.120) THEN
      Z1=-27.1
      IF(MOD(N,2).EQ.0) Z1=-27.6
      ELSE
      Z1=27.1
      IF(MOD(N,2).EQ.1) Z1=27.6
      END IF
C
      GOTO 200 
C
c------------------------------------------------------------------
11    CONTINUE
c
c********** Endcap, inner and outer layers *****************
c
      IK=1
      RAD=41.925 ! radius to center of trapezoid
      IF(N.GT.80) GOTO 1
c...inner layer
      IF(N.LE.20) DFI=DFI_EC*(N-0.5)
      IF(N.GE.61) DFI=DFI_EC*(N-60.5)
      X1=RAD*COS(DFI*PI/180.)
      Y1=RAD*SIN(DFI*PI/180.)
      IF(N.LE.20) Z1=-60.7
      IF(N.GE.61) Z1= 60.7
      GOTO 200
1     CONTINUE
c...outer layer
      IF(N.LE.100) DFI=DFI_EC*(N-80.5)+5.   ! 5 deg. - shift in dfi
      IF(N.GE.141) DFI=DFI_EC*(N-140.5)+5.
      X1=RAD*COS(DFI*PI/180.)
      Y1=RAD*SIN(DFI*PI/180.)
      IF(N.LE.100) Z1=-68.1
      IF(N.GE.141) Z1= 68.1
c----------------------------------------------------------------
200   N1=NRM  ! Nuber of block described here and rotation matrix
c
      FI1=DFI
c
      IF(IK.EQ.1) CALL GSROTM(N1, 90., 270.+FI1, 0., 0., 90., FI1)
c
      IF(IK.EQ.2) CALL GSROTM(N1, 90., FI1, 90., 90.+FI1, 0., 0.)
c
c                          Vol.name #BL  Moth.vol. Pos. #Rot.m. Flag
c                               |    |    |      / | \   |     |
      IF(IK.NE.0) CALL GSPOS(COUN(N),N1,'KEDR',X1,Y1,Z1, N1, 'ONLY')
c
101   continue
c
999   Format(' ******* ERROR: ATC_GEOM1:  N>160 ***********')
      Return
      End
