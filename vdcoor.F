*CMZ :  1.00/01 21/02/96  17.37.55  by  TANYA PURLATZ
*-- Author :
      SUBROUTINE VDCOOR
C
	include 'vd_map.inc'
C
      DIMENSION NLT(3),RL(2,3),DPHI(3),PHIT(320)
      DATA NLT/40,52,64/,RL/67.1,76.332,87.357,96.631,107.528,116.835/
      DATA DPHI/0.156498,0.120428,0.097738/,PI/3.141593/
      JTB=0
      DO 10 K=1,3
      NLIT=NLT(K)
      NLIT2=NLIT/2
      NLIT4=NLIT/4
C...................Odd layer
      PHI0=DPHI(K)*0.5
      J11=1
      J12=NLIT4
      J21=J12+1
      J22=3*J12
      J31=J22+1
      J32=NLIT
      R=0.1*RL(1,K)
      DO 20 I=J11,J12
      JTB=JTB+1
      PHI=PHI0+DPHI(K)*(I-1)
      PHIT(JTB)=PHI
      XP(JTB)=R*SIN(PHI)
      YP(JTB)=R*COS(PHI)
20    CONTINUE
      DO 30 I=J21,J22
      JTB=JTB+1
      PHI=PI-PHI0+DPHI(K)*(I-NLIT2)
      PHIT(JTB)=PHI
      XP(JTB)=R*SIN(PHI)
      YP(JTB)=R*COS(PHI)
30    CONTINUE
      DO 40 I=J31,J32
      JTB=JTB+1
      PHI=2*PI-PHI0+DPHI(K)*(I-NLIT)
      PHIT(JTB)=PHI
      XP(JTB)=R*SIN(PHI)
      YP(JTB)=R*COS(PHI)
40    CONTINUE
C................Even layer......................
      J11=1
      J12=NLIT4+1
      J21=J12+1
      J22=3*NLIT4+1
      J31=J22+1
      J32=NLIT
      R=0.1*RL(2,K)
      DO 50 I=J11,J12
      JTB=JTB+1
      PHI=DPHI(K)*(I-1)
      IF(I.EQ.J12)PHI=0.5*PI
      PHIT(JTB)=PHI
      XP(JTB)=R*SIN(PHI)
      YP(JTB)=R*COS(PHI)
50    CONTINUE
      DO 60 I=J21,J22
      JTB=JTB+1
      PHI=PI+DPHI(K)*(I-NLIT2-1)
	IF(I.EQ.J22)PHI=1.5*PI
      PHIT(JTB)=PHI
      XP(JTB)=R*SIN(PHI)
      YP(JTB)=R*COS(PHI)
60    CONTINUE
      DO 70 I=J31,J32
      JTB=JTB+1
      PHI=2*PI+DPHI(K)*(I-NLIT-1)
      PHIT(JTB)=PHI
      XP(JTB)=R*SIN(PHI)
      YP(JTB)=R*COS(PHI)
70    CONTINUE
10	CONTINUE
      NTUB=JTB
      RETURN
      END
