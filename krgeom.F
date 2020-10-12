*CMZ :  1.00/01 29/01/93  12.49.27  by  TANYA PURLATZ
*-- Author :    MALYSHEV
       SUBROUTINE KRGEOM
C
C ***        LKr calorimeter geometry definition
C
#include "gconst.inc"
C
	include 'medpar.inc'
	include 'lk_geomp.inc'
	include 'lk_xyzrot.inc'
C
      CHARACTER*4 EL(35)
      REAL LKRY(3),MOTH(3),LKRGAS(3)
      DIMENSION STEE(3),VACU(3),ALUM(3),OBEC(3),PARA(5),
     1     ALU1(3),ALU2(3)

C Resize electrode system...
      REL1O=REL1O*G10CTE
      DRLAY=DRLAY*G10CTE
      DREL=DREL*G10CTE
      DROB=DROB*G10CTE
     
C      DATA MOTH/72.89, 152., 149.1/     ! SIZES OF THE MOTHER
      MOTH(1)=RMOTI
      MOTH(2)=RMOTO
      MOTH(3)=DZMOT2
      CALL GSVOLU( 'LKR ' , 'TUBE',MEDVMF,MOTH, 3 , IVOL )
C
C      DATA STEE/72.9, 151.9, 149.0/   ! SIZES OF THE STEEL
      STEE(1)=RSTI
      STEE(2)=RSTO
      STEE(3)=DZST2
      CALL GSVOLU( 'STEE' , 'TUBE',MEDSTE,STEE, 3 , IVOL )
      CALL GSATT('STEE','SEEN',0)
      CALL GSPOS('STEE',1,'LKR ',0.,0.,0.,0,'ONLY')
C
C      DATA VACU/73.0, 150.3, 144.0/   ! SIZES OF THE VACUUM
      VACU(1)=RVACI
      VACU(2)=RVACO
      VACU(3)=DZVAC2
      CALL GSVOLU( 'VACU' , 'TUBE',MEDVMF,VACU, 3 , IVOL )
      CALL GSATT('VACU','SEEN',0)
      CALL GSPOS('VACU',1,'STEE',0.,0.,0.,0,'ONLY')
C
C      DATA ALUM/74.9, 147.7, 135.5/  ! SIZES OF THE ALUMINIUM (at T=293K)
      ALUM(1)=RALI
      ALUM(2)=RALO
      ALUM(3)=DZAL2
      CALL GSVOLU( 'ALUM' , 'TUBE',MEDALF,ALUM, 3 , IVOL )
      CALL GSATT('ALUM','SEEN',0)
C      CALL GSPOS('ALUM',1,'VACU',0.,0.,0.,0,'ONLY')

C     rotation and shifts of cold aluminium cryostat (and LKr+electrodes inside it)
      THETA=XYZROT_LKR(4)
      GAMMA=XYZROT_LKR(5)
      PHI  =XYZROT_LKR(6)

      IF(THETA.NE.0..OR.GAMMA.NE.0..OR.PHI.NE.0.) THEN
	CALL FREE_ROTMAT_NUMBER(NROT)
C COVERSION FROM 'SHAMOV' ANGLES -> 'GEANT' ANGLES
	CALL ACONV(THETA,GAMMA,PHI,THETA1,PHI1,THETA2,PHI2,THETA3,PHI3)
	CALL GSROTM(NROT,THETA1,PHI1,THETA2,PHI2,THETA3,PHI3)
      	CALL GSPOS('ALUM',1,'VACU',XYZROT_LKR(1),XYZROT_LKR(2),
     &     XYZROT_LKR(3),NROT,'ONLY')
      ELSE
      	CALL GSPOS('ALUM',1,'VACU',XYZROT_LKR(1),XYZROT_LKR(2),
     &     XYZROT_LKR(3),0,'ONLY')
      ENDIF

C
C      DATA LKRY/76.5, 146.1, 131.0/  ! SIZES OF THE LKR
C      CALL GSVOLU( 'LKRY' , 'TUBE',MEDLKG,LKRY, 3 , IVOL )
      LKRY(1)=RALI+DRAL
      LKRY(2)=RALO-DRAL
      LKRY(3)=DZAL2-DZAL
      CALL GSVOLU( 'LKRY' , 'TUBE',MEDLKR,LKRY, 3 , IVOL )
      CALL GSPOS('LKRY',1,'ALUM',0.,0.,0.,0,'ONLY')
C
C      DATA OBEC/76.5, 76.644, 131./ ! SIZES OF THE OBECHAIKA
      OBEC(1)=REL1O-DREL-DROB
      OBEC(2)=REL1O-DREL
      OBEC(3)=LKRY(3)
      IF( OBEC(1) .LT. LKRY(1)) 
	1 PRINT *,'******KRGEOM> wrong geom. parameters for LKR calorimeter://',
	2	'OBEC=',(OBEC(I),I=1,3),'  LKRY',(LKRY(I),I=1,3)
      CALL GSVOLU( 'OBEC' , 'TUBE',MEDG10,OBEC, 3 , IVOL )
      CALL GSATT('OBEC','SEEN',0)
      CALL GSPOS('OBEC',1,'LKRY',0.,0.,0.,0,'ONLY')
C
         DATA EL/'EL01','EL02','EL03','EL04','EL05','EL06',
     *                  'EL07','EL08','EL09','EL10','EL11','EL12',
     *                  'EL13','EL14','EL15','EL16','EL17','EL18',
     *                  'EL19','EL20','EL21','EL22','EL23','EL24',
     *                  'EL25','EL26','EL27','EL28','EL29','EL30',
     *                  'EL31','EL32','EL33','EL34','EL35'/
C
      CALL GSVOLU('KRYP', 'TUBE', MEDLKR, LKRY , 3 , IVOL )
      CALL GSATT('KRYP','SEEN',0)
      CALL GSPOS('KRYP',1,'LKRY',0.,0.,0.,0,'MANY')
C
C      RF=76.7+2.*34	! full radius of LKR
      RF=LKRY(2)	! full radius of LKR
      RLIQ=RF-LKGAP	! level of liquid KR
c      print *,' ------------ RF=',RF,' RLIQ=',RLIQ
      DATA LKRGAS/100., 100., 131./ ! SIZES OF THE LKR GAS BOX
      LKRGAS(3)=LKRY(3)
      CALL GSVOLU('LKGA', 'BOX ', MEDLKG, LKRGAS , 3 , IVOL )
      CALL GSATT('LKGA','SEEN',0)
      CALL GSPOS('LKGA',1,'KRYP',0.,LKRY(2)+LKRGAS(2)-LKGAP,0.,0,'MANY') ! INTERSECTION OF GAS BOX WITH LIQUID


      DATA ALU2/124.0, 146.1, 10.7 /  ! SIZES OF THE ALUMINIUM 2 IN LKR
      ALU2(2)=LKRY(2)
      ALU2(1)=ALU2(2)-22.1
C      ZALU2=119.7
      ZALU2=LKRY(3)-ALU2(3)-0.1
      CALL GSVOLU( 'ALU2' , 'TUBE',MEDALF,ALU2, 3 , IVOL )
      CALL GSATT('ALU2','SEEN',0)
      CALL GSPOS('ALU2',1,'LKRY',0.,0., ZALU2,0,'ONLY')
      CALL GSPOS('ALU2',2,'LKRY',0.,0.,-ZALU2,0,'ONLY')


      DATA ALU1/132.0, 146.1, 20.0/  ! SIZES OF THE ALUMINIUM 1 IN LKR
      ALU1(2)=LKRY(2)
      ALU1(1)=ALU1(2)-14.1
C      ZALU1=89.0
      ZALU1=ZALU2-ALU2(3)-ALU1(3)-0.1
      CALL GSVOLU( 'ALU1' , 'TUBE',MEDALF,ALU1, 3 , IVOL )
      CALL GSATT('ALU1','SEEN',0)
      CALL GSPOS('ALU1',1,'LKRY',0.,0., ZALU1,0,'ONLY')
      CALL GSPOS('ALU1',2,'LKRY',0.,0.,-ZALU1,0,'ONLY')
C
C
      DO 111 I=1,35
C        PARA(2)=76.7+2.*(I-1)
        PARA(2)=REL1O+DRLAY*(I-1)
C        PARA(1)=PARA(2)-0.056
        PARA(1)=PARA(2)-DREL
C	PARA(3)=131.
	PARA(3)=LKRY(3)
	IF(PARA(2).GT.ALU2(1)) PARA(3)=ZALU2-ALU2(3)
	IF(PARA(2).GT.ALU1(1)) PARA(3)=ZALU1-ALU1(3)
C	PRINT *,'I=',I,' electrode PARA=',(PARA(J),J=1,3),'  LKRY=',(LKRY(J),J=1,3)
	IF(PARA(2) .GT. LKRY(2)) THEN 
		PRINT *,'******KRGEOM> wrong geom. parameters for LKR calorimeter:'
		PRINT *,'I=',I,' electrode PARA=',(PARA(J),J=1,3),'  LKRY=',(LKRY(J),J=1,3)
	ENDIF
        CALL GSVOLU( EL(I), 'TUBE', MEDSCTU, PARA , 3 , IVOL )
        CALL GSATT(EL(I),'SEEN',0)
111     CALL GSPOS(EL(I),1,'KRYP',0.,0.,0.,0,'ONLY')
C
C 	PRINT *,'******KRGEOMC :'
C 	PRINT *,'MOTH=',(MOTH(I),I=1,3)
C 	PRINT *,'STEE=',(STEE(I),I=1,3)
C 	PRINT *,'VACU=',(VACU(I),I=1,3)
C 	PRINT *,'ALUM=',(ALUM(I),I=1,3)
C 	PRINT *,'LKRY=',(LKRY(I),I=1,3)
C 	PRINT *,'OBEC=',(OBEC(I),I=1,3)
C 	PRINT *,'ALU1=',(ALU1(I),I=1,3)
C 	PRINT *,'ALU2=',(ALU2(I),I=1,3)


      CALL GSORD('LKRY',4) ! obsolete
     
      CALL GSPOS('LKR ',1,'KEDR',0.,0.,0.,0,'ONLY')

      RETURN
      END
