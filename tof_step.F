      SUBROUTINE TOF_STEP
c**********************************************************
c
c  Program of analysis of ch. particle displacement for ToF system
c
c  Author: V.I.Yurchenko
c
c  Last update: 06.02.2001, 16.09.2003 V.A.Tayursky
c
c**********************************************************
c
#include "gcvolu.inc"
#include "gckine.inc"
#include "gctrak.inc"
c
      COMMON /SC_DATA/ REFS,REFP,SS(2,2),APPA(2),A100,EPS,T0,RICH,RIKK
      COMMON /SC_NAME/ ESP0,ESL0,BS00
      CHARACTER *4 ESP0,ESL0,BS00, NAMI,NAMES
      DIMENSION R1(3),R2(3),RC1(3),RC2(3)
      SAVE T2
c      
      IF(CHARGE.EQ.0) GO TO 777
c
      CALL UCOPY(RC2(1),RC1(1),3) ! copy 3 words from RC2 to RC1
c
      T1=T2
      IF(INWVOL.EQ.1) T1=TOFG     ! particle entered sc.counter. (V.A.T.)
c
      CALL UCOPY(VECT(1),RC2(1),3)
c
      T2=TOFG                     ! current ToF in c*t units (from /GCTRAK/)
      IF(DESTEP.EQ.0) GO TO 777
c
c  Identification of system
c
      NAMI=NAMES(NLEVEL) ! name of volume 
      LSYS=0
      IF(NAMI.EQ.ESP0) LSYS=1   ! end cap, z<0
      IF(NAMI.EQ.BS00) LSYS=2   ! barrel
      IF(NAMI.EQ.ESL0) LSYS=3   ! end cap, z>0
      IF(LSYS.EQ.0) GO TO 777
c
      CALL GMTOD(RC2,R2,1) ! ref. system transformation
c
      CALL GMTOD(RC1,R1,1)
c
c  Transformation to good coordinates
c
      IF(LSYS.EQ.2) THEN  ! barrel
c
        DO I=1,3,2        
          R1(I)=-R1(I)
          R2(I)=-R2(I)
        END DO
c
      END IF
c
c  Below: T1 -initial time, R1 - initial point;   T2 , R2 - final ones
c
      ICOU=NUMBER(NLEVEL) ! number of volume copy at this level
c
      CALL TOF_TUBE(LSYS,ICOU,T1,R1(3),T2,R2(3),DESTEP)        
c
777   RETURN
      END 

