        SUBROUTINE CSI_GEOM
c***********************************************************
c
c  Geometry of CsI calorimeter
c
c  Author :    TANYA PURLATZ  25/01/93
c
c  Last correction: renamed   25.05.2004
c                   A. Shamov 21.07.08 - declarations
c
c**********************************************************
#include "gcflag.inc"
#include "gconst.inc"
#include "medpar.inc"
#include "KedrSim/csisize.inc"
#include "KedrSim/csi_xyzrot.inc"

      REAL *8 M(3,3)
      DIMENSION PAR(11),PH(3),TH(3)
      real DR,SHIFTLONG
      DATA RC/6./,RC1/8./,RC2/8.8/,DR/0.02/,DIS/97./,ELT/1.12/,
     $   CLEN/15./,SHIFTLONG/2.0/
C     RC - размер кристалла 60x60
C     RC1 - вертикальный размер кристалла 60x80
C     DR - расстояние между кристаллами (фольга - 0.01мм *2 - моделируем
C     Al (тефлон 70 микрон,  лавсан 20 микрон и нержавейка 100 микрон
C     примерно на 1/6 поверхности)
C     SHIFTLONG - сдвижка длинных внутренних кристаллов от места встречи
      
C
C  ROTATION MATRICES DEFINITIONS
C
C  REFLECTION W.R.T. (YZ) PLANE: X -> -X
      CALL FREE_ROTMAT_NUMBER(NROT1)
      CALL GSROTM(NROT1,90.,180.,90.,90.,0.,0.)
C  W.R.T. (XZ) PLANE: Y -> -Y
      NROT2=NROT1+1
      CALL GSROTM(NROT2,90.,0.,90.,270.,0.,0.)
C  W.R.T. (XZ) PLANE: Y-AXIS
      NROT3=NROT2+1
      CALL GSROTM(NROT3,90.,180.,90.,90.,180.,0.)
C  90 DEGREES W.R.T. Z-AXIS
      NROT4=NROT3+1
      CALL GSROTM(NROT4,90.,90.,90.,180.,0.,0.)
C  180 DEGREES W.R.T. Z-AXIS
      NROT5=NROT4+1
      IM=1
      IF(ISWIT(2).EQ.-1) THEN
        TH(1)=XYZROT(4,IM)
        PH(1)=XYZROT(5,IM)
        TH(2)=XYZROT(6,IM)
        PH(2)=XYZROT(7,IM)
        TH(3)=XYZROT(8,IM)
        PH(3)=XYZROT(9,IM)
      ELSE
      	CALL UNITM(M)		! set matrix of orts M=1
        CALL SCONV(0.,0.,180.,M)
      	THETA=XYZROT(4,IM)
        GAMMA=XYZROT(5,IM)
        PHI  =XYZROT(6,IM)
        CALL SCONV(THETA,GAMMA,PHI,M)
        CALL VCONV(M,TH,PH)
      ENDIF
      CALL GSROTM(NROT5,TH(1),PH(1),TH(2),PH(2),TH(3),PH(3))
c      print *,'t,g,p=',theta,gamma,phi
c      PRINT *,' M='
c      DO I=1,3
c	PRINT *,M(I,1),M(I,2),M(I,3)
c      ENDDO
c      PRINT *,' T,P='
c      DO I=1,3
c	PRINT *,Th(I),Ph(I)
c      ENDDO
C      CALL GSROTM(NROT5,90.,180.,90.,270.,0.,0.)
C  270 DEGREES W.R.T. Z-AXIS
      NROT6=NROT5+1
      CALL GSROTM(NROT6,90.,270.,90.,0.,0.,0.)
C  180 DEG. W.R.T. Y-AXIS
      NROT7=NROT6+1
      CALL GSROTM(NROT7,90.,180.,90.,90.,180.,0.)
C  180 DEG. W.R.T. X-AXIS, 90 DEG. W.R.T. Z-AXIS CLOCKWISE
      NROT8=NROT7+1
      CALL GSROTM(NROT8,90.,90.,90.,0.,0.,0.)
C  Rotation of module 3 - no rotation if positioning is ideal
      NROT9=NROT8+1
      IM=3
      IF(ISWIT(2).EQ.-1) THEN
        TH(1)=XYZROT(4,IM)
        PH(1)=XYZROT(5,IM)
        TH(2)=XYZROT(6,IM)
        PH(2)=XYZROT(7,IM)
        TH(3)=XYZROT(8,IM)
        PH(3)=XYZROT(9,IM)
      ELSE
      	CALL UNITM(M)		! set matrix of orts M=1
      	THETA=XYZROT(4,IM)
        GAMMA=XYZROT(5,IM)
        PHI  =XYZROT(6,IM)
        CALL SCONV(THETA,GAMMA,PHI,M)
        CALL VCONV(M,TH,PH)
      ENDIF
      CALL GSROTM(NROT9,TH(1),PH(1),TH(2),PH(2),TH(3),PH(3))
C      CALL GSROTM(NROT9,90.,0.,90.,90.,0.,0.)
C  Rotation of module 5 - as for modile 1 (NROT5) if positioning is ideal
C  (remember that CSIR rotated along Y-axis instead of CSIL)
      NROT10=NROT9+1
      IM=5
      IF(ISWIT(2).EQ.-1) THEN
        TH(1)=XYZROT(4,IM)
        PH(1)=XYZROT(5,IM)
        TH(2)=XYZROT(6,IM)
        PH(2)=XYZROT(7,IM)
        TH(3)=XYZROT(8,IM)
        PH(3)=XYZROT(9,IM)
      ELSE
      	CALL UNITM(M)		! set matrix of orts M=1
        CALL SCONV(0.,0.,180.,M)	! rotation along Z
        CALL SCONV(0.,180.,0.,M)	! rotation along Y
      	THETA=XYZROT(4,8)	! take angles from module 8 in Baldin's definition
        GAMMA=XYZROT(5,8)
        PHI  =XYZROT(6,8)
        CALL SCONV(THETA,GAMMA,PHI,M)
        CALL VCONV(M,TH,PH)
      ENDIF
      CALL GSROTM(NROT10,TH(1),PH(1),TH(2),PH(2),TH(3),PH(3))
C      CALL GSROTM(NROT10,90.,180.,90.,270.,0.,0.)
C  Rotation of module 6 - as for modile 2 (NROT14) if positioning is ideal
C  (remember that CSIR rotated along Y-axis instead of CSIL)
      NROT11=NROT10+1
      IM=6
      IF(ISWIT(2).EQ.-1) THEN
        TH(1)=XYZROT(4,IM)
        PH(1)=XYZROT(5,IM)
        TH(2)=XYZROT(6,IM)
        PH(2)=XYZROT(7,IM)
        TH(3)=XYZROT(8,IM)
        PH(3)=XYZROT(9,IM)
      ELSE
      	CALL UNITM(M)		! set matrix of orts M=1
        CALL SCONV(0.,0.,90.,M)	! rotation along Z
        CALL SCONV(0.,180.,0.,M)	! rotation along Y
      	THETA=XYZROT(4,7)	! take angles from module 7 in Baldin's definition
        GAMMA=XYZROT(5,7)
        PHI  =XYZROT(6,7)
        CALL SCONV(THETA,GAMMA,PHI,M)
        CALL VCONV(M,TH,PH)
      ENDIF
      CALL GSROTM(NROT11,TH(1),PH(1),TH(2),PH(2),TH(3),PH(3))
C      CALL GSROTM(NROT11,90.,90.,90.,180.,0.,0.)
C  Rotation of module 7 - as for modile 3 (NROT9) if positioning is ideal
C  (remember that CSIR rotated along Y-axis instead of CSIL)
      NROT12=NROT11+1
      IM=7
      IF(ISWIT(2).EQ.-1) THEN
        TH(1)=XYZROT(4,IM)
        PH(1)=XYZROT(5,IM)
        TH(2)=XYZROT(6,IM)
        PH(2)=XYZROT(7,IM)
        TH(3)=XYZROT(8,IM)
        PH(3)=XYZROT(9,IM)
      ELSE
      	CALL UNITM(M)		! set matrix of orts M=1
        CALL SCONV(0.,180.,0.,M)	! rotation along Y
      	THETA=XYZROT(4,6)	! take angles from module 6 in Baldin's definition
        GAMMA=XYZROT(5,6)
        PHI  =XYZROT(6,6)
        CALL SCONV(THETA,GAMMA,PHI,M)
        CALL VCONV(M,TH,PH)
      ENDIF
      CALL GSROTM(NROT12,TH(1),PH(1),TH(2),PH(2),TH(3),PH(3))
C      CALL GSROTM(NROT12,90.,0.,90.,90.,0.,0.)
C  Rotation of module 8 - as for modile 4 (NROT15) if positioning is ideal
C  module have X>0, Y<0, Z<0 in KEDR detector
C  (remember that CSIR rotated along Y-axis instead of CSIL)
      NROT13=NROT12+1
      IM=8
      IF(ISWIT(2).EQ.-1) THEN
        TH(1)=XYZROT(4,IM)
        PH(1)=XYZROT(5,IM)
        TH(2)=XYZROT(6,IM)
        PH(2)=XYZROT(7,IM)
        TH(3)=XYZROT(8,IM)
        PH(3)=XYZROT(9,IM)
      ELSE
      	CALL UNITM(M)		! set matrix of orts M=1
        CALL SCONV(0.,0.,270.,M)	! rotation along Z
        CALL SCONV(0.,180.,0.,M)	! rotation along Y
    	THETA=XYZROT(4,5)	! take angles from module 5 in Baldin's definition
        GAMMA=XYZROT(5,5)
        PHI  =XYZROT(6,5)
        CALL SCONV(THETA,GAMMA,PHI,M)
        CALL VCONV(M,TH,PH)
      ENDIF
      CALL GSROTM(NROT13,TH(1),PH(1),TH(2),PH(2),TH(3),PH(3))
C      CALL GSROTM(NROT13,90.,270.,90.,0.,0.,0.)
C  Rotation of module 2 
      NROT14=NROT13+1
      IM=2
      IF(ISWIT(2).EQ.-1) THEN
        TH(1)=XYZROT(4,IM)
        PH(1)=XYZROT(5,IM)
        TH(2)=XYZROT(6,IM)
        PH(2)=XYZROT(7,IM)
        TH(3)=XYZROT(8,IM)
        PH(3)=XYZROT(9,IM)
      ELSE
      	CALL UNITM(M)		! set matrix of orts M=1
        CALL SCONV(0.,0.,90.,M)	! rotation along Z
      	THETA=XYZROT(4,IM)
        GAMMA=XYZROT(5,IM)
        PHI  =XYZROT(6,IM)
        CALL SCONV(THETA,GAMMA,PHI,M)
        CALL VCONV(M,TH,PH)
      ENDIF
      CALL GSROTM(NROT14,TH(1),PH(1),TH(2),PH(2),TH(3),PH(3))
C      CALL GSROTM(NROT14,90.,90.,90.,180.,0.,0.)
C  Rotation of module 4 
      NROT15=NROT14+1
      IM=4
      IF(ISWIT(2).EQ.-1) THEN
        TH(1)=XYZROT(4,IM)
        PH(1)=XYZROT(5,IM)
        TH(2)=XYZROT(6,IM)
        PH(2)=XYZROT(7,IM)
        TH(3)=XYZROT(8,IM)
        PH(3)=XYZROT(9,IM)
      ELSE
      	CALL UNITM(M)		! set matrix of orts M=1
        CALL SCONV(0.,0.,270.,M)	! rotation along Z
      	THETA=XYZROT(4,IM)
        GAMMA=XYZROT(5,IM)
        PHI  =XYZROT(6,IM)
        CALL SCONV(THETA,GAMMA,PHI,M)
        CALL VCONV(M,TH,PH)
      ENDIF
      CALL GSROTM(NROT15,TH(1),PH(1),TH(2),PH(2),TH(3),PH(3))
C      CALL GSROTM(NROT14,90.,270.,90.,0.,0.,0.)
C
C     параметры объёмов
      PAR(1)=6.0+DR             !внутренний радиус
      PAR(2)=66.5               !внешний радиус
      PAR(3)=21.+ELT/2.+0.1+SHIFTLONG/2.  !полуширина added 5cm for possibility of ENDB rotation 
      PAR(4)=0.                 !начальный угол для четвертушки
      PAR(5)=90.                !конечный угол для четвертушки
C    ENDCAP (Up and Down quadrants) торец
      CALL GSVOLU('CSIR','TUBE',MEDVMF,PAR,3,IVOL)
      CALL GSATT ('CSIR','SEEN',1) ! объём не виден
      CALL GSVOLU('CSIL','TUBE',MEDVMF,PAR,3,IVOL)
      CALL GSATT ('CSIL','SEEN',1) ! объём не виден      
C    BASIC BLOCK FOR ENDCAP четвертушка
      PAR(1)=6.0+DR             !внутренний радиус
      PAR(2)=9.*(RC+DR)+RC2+DR+3.5 !внешний радиус - равен радиусу обечайки
      PAR(3)=CLEN+SHIFTLONG/2.+DR  !полуширина
      CALL GSVOLU('ENDB','TUBS',MEDVMF,PAR,5,IVOL)
      CALL GSATT ('ENDB','SEEN',0) !объём не виден
      CALL GSATT ('ENDB','COLO',4) 
C     Стеф (выступает примерно на 2 см на внешней стороне - прослойка
C     перед обечайкой
c$$$      PAR(1)=9.*(RC+DR)         !внутренний радиус
c$$$      PAR(2)=9.*(RC+DR)+RC2+DR+3.     !внешний радиус
c$$$      PAR(3)=CLEN               !полуширина 
c$$$      CALL GSVOLU('CSST','TUBE',MEDG10,PAR,3,IVOL)
c$$$      CALL GSATT ('CSST','SEEN',0) ! объём не виден
C     Обечайка 0.5 см дюраля - заменяем для простоты на Al
C     перед обечайкой
      PAR(1)=9.*(RC+DR)+RC2+DR+3.        !внутренний радиус
      PAR(2)=9.*(RC+DR)+RC2+DR+3.5    !внешний радиус
C      PAR(3)=CLEN               !полуширина (старая величина, неправильная)
      IF(ISWIT(3).EQ.0) PAR(3)=45./2               !полуширина (старая величина, правильная)
      IF(ISWIT(3).NE.0) PAR(3)=44.7/2              !полуширина (новая величина, 2014 )
      CALL GSVOLU('CSOB','TUBE',MEDALF,PAR,3,IVOL)
C      CALL GSATT ('CSOB','SEEN',0) ! объём не виден
C     ELECTRONIC FOR CSI
      PAR(1)=6.0+DR             !внутренний радиус
C      PAR(2)=66.5               !внешний радиус
      PAR(2)=64.7               !(новая величина, 2014 )
      PAR(3)=ELT/2.
      CALL GSVOLU('CSEL','TUBE',MEDALF,PAR,3,IVOL)
C     Al plate FOR CSI
      PAR(1)=25.5               !внутренний радиус
      PAR(2)=65.9               !внешний радиус
      PAR(3)=0.4/2
      CALL GSVOLU('CSPL','TUBE',MEDALF,PAR,3,IVOL)
C     Al ring FOR CSI
      PAR(1)=64.8               !внутренний радиус
      PAR(2)=66.0               !внешний радиус
      PAR(3)=1.8/2
      CALL GSVOLU('CSRI','TUBE',MEDALF,PAR,3,IVOL)
C     TWO CRYSTALS OF CSI 60*60 MM.
      PAR(1)=(RC+DR)/2.
      PAR(2)=(RC+DR)/2.
      PAR(3)=CLEN+DR/2
      CALL GSVOLU('BL  ','BOX ',MEDALF,PAR,3,IVOL) ! фольга - моделируем алюминием
      CALL GSATT ('BL  ','SEEN',0) !объём не виден
      PAR(1)=RC/2.
      PAR(2)=RC/2.
      PAR(3)=CLEN/2.
      CALL GSVOLU('CR  ','BOX ',MEDCSI,PAR,3,IVOL) !чувствительный объём
      CALL GSATT ('CR  ','SEEN',1) !объём виден
      CALL GSPOS('CR  ',1,'BL  ',0.,0.,-(CLEN+DR)/2.,0,'ONLY') !внутренний слой
      CALL GSPOS('CR  ',2,'BL  ',0.,0.,(CLEN+DR)/2.,0,'ONLY') !внешний слой
C     LONG INNER CRYSTALS OF CSI 60*60 MM.
C     В невидимый объём помещается блок CsI номером 1, так как структура
C     геометрии CsI четырёхуровневая (торец,модуль,канал,слой)
      PAR(1)=(RC+DR)/2.
      PAR(2)=(RC+DR)/2.
      PAR(3)=CLEN+DR/2.
      CALL GSVOLU('BLIN','BOX ',MEDALF,PAR,3,IVOL) ! фольга - моделируем алюминием
      CALL GSATT ('BLIN','SEEN',0) ! объём не виден
      PAR(1)=(RC)/2.
      PAR(2)=(RC)/2.
      PAR(3)=CLEN
      CALL GSVOLU('CRIN','BOX ',MEDCSI,PAR,3,IVOL)!чувствительный объём
c      CALL GSATT ('CRIN','SEEN',0) !объём не виден
      CALL GSPOS('CRIN',1,'BLIN',0.,0.,0.,0,'ONLY')
C     TWO CSI CRYSTALS 60*80 MM
      PAR(1)=(RC1+DR)/2
      PAR(2)=(RC+DR)/2.
      PAR(3)=CLEN+DR
      CALL GSVOLU('BL80','BOX ',MEDALF,PAR,3,IVOL) ! фольга - моделируем алюминием
      CALL GSATT ('BL80','SEEN',0) !объём не виден
      PAR(1)=RC1/2
      PAR(2)=RC/2.
      PAR(3)=CLEN/2.
      CALL GSVOLU('CR80','BOX ',MEDCSI,PAR,3,IVOL) !чувствительный объём
c      CALL GSATT ('CR80','SEEN',1) !объём виден
      CALL GSPOS('CR80',1,'BL80',0.,0.,-(CLEN+DR)/2.,0,'ONLY') !внутренний слой
      CALL GSPOS('CR80',2,'BL80',0.,0.,(CLEN+DR)/2.,0,'ONLY') !внешний слой
C     TWO CSI CRYSTALS 60*88 MM
      PAR(1)=(RC2+DR)/2
      PAR(2)=(RC+DR)/2.
      PAR(3)=CLEN+DR
      CALL GSVOLU('BL88','BOX ',MEDALF,PAR,3,IVOL) ! фольга - моделируем алюминием
      CALL GSATT ('BL88','SEEN',0) !объём не виден
      PAR(1)=RC2/2
      PAR(2)=RC/2.
      PAR(3)=CLEN/2.
      CALL GSVOLU('CR88','BOX ',MEDCSI,PAR,3,IVOL) !чувствительный объём
c      CALL GSATT ('CR88','SEEN',1) !объём виден
      CALL GSPOS('CR88',1,'BL88',0.,0.,-(CLEN+DR)/2.,0,'ONLY') !внутренний слой
      CALL GSPOS('CR88',2,'BL88',0.,0.,(CLEN+DR)/2.,0,'ONLY') !внешний слой
C    TWO CSI CRYSTALS 60*80 WITH CUT ARE PRESENTED AS THE SUM OF
C     ORDINARY 60*60 BLOCK AND TRAPEZOID AS DESCRIBED BELOW
C     трапезоид с краю, поэтому не заморачиваемся с фольгой вообще
      PAR(1)=CLEN+DR
      PAR(2)=0.
      PAR(3)=0.
      PAR(4)=1.
      PAR(5)=RC/2
      PAR(6)=4./2
      PAR(7)=-ATAN(0.5)*RADDEG
      PAR(8)=PAR(4)
      PAR(9)=PAR(5)
      PAR(10)=PAR(6)
      PAR(11)=PAR(7)
      CALL GSVOLU('BLTR','TRAP',MEDCSI,PAR,11,IVOL)
      CALL GSATT ('BLTR','SEEN',1) 
      CALL GSATT ('BLTR','COLO',4) 
      CALL GSDVN( 'CRTR' ,'BLTR' , 2 , 3 ) !делим блоки пополам

C
C ***          POSITION VOLUMES
C
C      CALL GSDVN( 'CR  ' ,'BL  ' , 2 , 3 ) !делим блоки пополам
C      CALL GSDVN( 'CR80' ,'BL80' , 2 , 3 ) !делим блоки пополам
C      CALL GSDVN( 'CR88' ,'BL88' , 2 , 3 ) !делим блоки пополам

C Blocks coodrinates

C     обечайка - кристаллы замещают стеф.
      Z=-SHIFTLONG/2.0
c$$$      CALL GSPOS( 'CSST' ,1, 'CSIL' , 0.,0.,Z,0,'ONLY')
c$$$      CALL GSPOS( 'CSST' ,2, 'CSIR' , 0.,0.,Z,0,'ONLY')
      IF(ISWIT(3).EQ.0) Z=0               ! старая величина, правильная
      IF(ISWIT(3).NE.0) Z=0.05/2          ! новая величина, 2014 
      CALL GSPOS( 'CSOB' ,1, 'CSIL' , 0.,0.,Z,0,'ONLY')
      IF(ISWIT(2).EQ.-1) THEN
      	CALL GSPOS( 'CSOB' ,2, 'CSIR' , 0.,0.,Z,0,'ONLY')
      ELSE
      	CALL GSPOS( 'CSOB' ,2, 'CSIR' , 0.,0.,-Z,0,'ONLY') ! CSOB NOT ROTATED
      ENDIF


c     Стандартные блоки 1ый ряд (см. csi_mctorl.F)
       Y=0.5*(RC+DR)
       Z=-SHIFTLONG/2.0
       J=1
       DO I=4,9
         X=(I-0.5)*(RC+DR)
         CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH) 
         NCR=IGR*16+ICH
         CALL GSPOS('BL  ',NCR,'ENDB',X,Y,Z,0,'ONLY')
         CRXMC(I,J,3,1)=X
         CRYMC(I,J,3,1)=Y
         CRZMC(1,I,J,3,1)=DIS-CLEN/2.
         CRZMC(2,I,J,3,1)=DIS+CLEN/2.
         CRDX(I,J,3,1)=RC
         CRDY(I,J,3,1)=RC
         CRDZ(I,J,3,1)=CLEN
       enddo
c     Длинный кристалл 6ой группы 1ый ряд (см. csi_mctorl.F)
       Z=SHIFTLONG/2.0
       DO I=3,3
         X=(I-0.5)*(RC+DR)
         CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH) 
         NCR=IGR*16+ICH
         CALL GSPOS('BLIN',NCR,'ENDB',X,Y,Z,0,'ONLY')
         CRXMC(I,J,3,1)=X
         CRYMC(I,J,3,1)=Y
         CRZMC(1,I,J,3,1)=DIS+SHIFTLONG
         CRDX(I,J,3,1)=RC
         CRDY(I,J,3,1)=RC
         CRDZ(I,J,3,1)=CLEN*2.
       enddo

C      стандартные блоки 2ой ряд (см. csi_mctorl.F)
       J=2
       Y=(J-0.5)*(RC+DR)
       Z=-SHIFTLONG/2.0
       DO I=4,9
         X=(I-0.5)*(RC+DR)
         CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)
         NCR=IGR*16+ICH
         CALL GSPOS('BL  ',NCR,'ENDB',X,Y,Z,0,'ONLY')
         CRXMC(I,J,3,1)=X
         CRYMC(I,J,3,1)=Y
         CRZMC(1,I,J,3,1)=DIS-CLEN/2.
         CRZMC(2,I,J,3,1)=DIS+CLEN/2.
         CRDX(I,J,3,1)=RC
         CRDY(I,J,3,1)=RC
         CRDZ(I,J,3,1)=CLEN
       enddo
c     Длинные кристаллы 6ой группы 2ой ряд (см. csi_mctorl.F)
       Z=SHIFTLONG/2.0
       DO I=2,3
         X=(I-0.5)*(RC+DR)
         CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH) 
         NCR=IGR*16+ICH
         CALL GSPOS('BLIN',NCR,'ENDB',X,Y,Z,0,'ONLY')
         CRXMC(I,J,3,1)=X
         CRYMC(I,J,3,1)=Y
         CRZMC(1,I,J,3,1)=DIS+SHIFTLONG
         CRDX(I,J,3,1)=RC
         CRDY(I,J,3,1)=RC
         CRDZ(I,J,3,1)=CLEN*2.
       enddo

C      стандартные блоки 3ий ряд (см. csi_mctorl.F)
       J=3
       Y=(J-0.5)*(RC+DR)
       Z=-SHIFTLONG/2.0
       DO I=4,9
         X=(I-0.5)*(RC+DR)
         CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)
         NCR=IGR*16+ICH
         CALL GSPOS('BL  ',NCR,'ENDB',X,Y,Z,0,'ONLY')
         CRXMC(I,J,3,1)=X
         CRYMC(I,J,3,1)=Y
         CRZMC(1,I,J,3,1)=DIS-CLEN/2.
         CRZMC(2,I,J,3,1)=DIS+CLEN/2.
         CRDX(I,J,3,1)=RC
         CRDY(I,J,3,1)=RC
         CRDZ(I,J,3,1)=CLEN
       enddo
c     Длинные кристаллы 6ой группы 3ий ряд (см. csi_mctorl.F)
       Z=SHIFTLONG/2.0
       DO I=1,3
         X=(I-0.5)*(RC+DR)
         CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH) 
         NCR=IGR*16+ICH
         CALL GSPOS('BLIN',NCR,'ENDB',X,Y,Z,0,'ONLY')
         CRXMC(I,J,3,1)=X
         CRYMC(I,J,3,1)=Y
         CRZMC(1,I,J,3,1)=DIS+SHIFTLONG
         CRDX(I,J,3,1)=RC
         CRDY(I,J,3,1)=RC
         CRDZ(I,J,3,1)=CLEN*2
       enddo

       Z=-SHIFTLONG/2.0         !остальные кристаллы установлены стандартно

C      стандартные блоки 4ый ряд (см. csi_mctorl.F)
       J=4
       Y=(J-0.5)*(RC+DR)
       DO I=1,10
	 X=(I-0.5)*(RC+DR)
         CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)
         NCR=IGR*16+ICH
         CALL GSPOS('BL  ',NCR,'ENDB',X,Y,Z,0,'ONLY')
         CRXMC(I,J,3,1)=X
         CRYMC(I,J,3,1)=Y
         CRZMC(1,I,J,3,1)=DIS-CLEN/2.
         CRZMC(2,I,J,3,1)=DIS+CLEN/2.
         CRDX(I,J,3,1)=RC
         CRDY(I,J,3,1)=RC
         CRDZ(I,J,3,1)=CLEN
       enddo

C      стандартные блоки 5ый и 6ой ряды (см. csi_mctorl.F)
       DO J=5,6
         Y=(J-0.5)*(RC+DR)
         DO I=1,8
           X=(I-0.5)*(RC+DR)
           CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)
           NCR=IGR*16+ICH
           CALL GSPOS('BL  ',NCR,'ENDB',X,Y,Z,0,'ONLY')
           CRXMC(I,J,3,1)=X
           CRYMC(I,J,3,1)=Y
           CRZMC(1,I,J,3,1)=DIS-CLEN/2.
           CRZMC(2,I,J,3,1)=DIS+CLEN/2.
           CRDX(I,J,3,1)=RC
           CRDY(I,J,3,1)=RC
           CRDZ(I,J,3,1)=CLEN
	 enddo
       enddo

C      стандартные блоки 7ой ряд (см. csi_mctorl.F)
       J=7
       Y=(J-0.5)*(RC+DR)
       DO I=1,8
         X=(I-0.5)*(RC+DR)
         CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)
         NCR=IGR*16+ICH
         CALL GSPOS('BL  ',NCR,'ENDB',X,Y,Z,0,'ONLY')
         CRXMC(I,J,3,1)=X
         CRYMC(I,J,3,1)=Y
         CRZMC(1,I,J,3,1)=DIS-CLEN/2.
         CRZMC(2,I,J,3,1)=DIS+CLEN/2.
         CRDX(I,J,3,1)=RC
         CRDY(I,J,3,1)=RC
         CRDZ(I,J,3,1)=CLEN
       enddo

C      стандартные блоки 8ой ряд (см. csi_mctorl.F)
       J=8
       Y=(J-0.5)*(RC+DR)
       DO I=1,7
         X=(I-0.5)*(RC+DR)
         CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)
         NCR=IGR*16+ICH
         CALL GSPOS('BL  ',NCR,'ENDB',X,Y,Z,0,'ONLY')
         CRXMC(I,J,3,1)=X
         CRYMC(I,J,3,1)=Y
         CRZMC(1,I,J,3,1)=DIS-CLEN/2.
         CRZMC(2,I,J,3,1)=DIS+CLEN/2.
         CRDX(I,J,3,1)=RC
         CRDY(I,J,3,1)=RC
         CRDZ(I,J,3,1)=CLEN
       enddo

C      стандартные блоки 9ый ряд (см. csi_mctorl.F)
       J=9
       Y=(J-0.5)*(RC+DR)
       DO I=1,4
         X=(I-0.5)*(RC+DR)
         CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)
         NCR=IGR*16+ICH
         CALL GSPOS('BL  ',NCR,'ENDB',X,Y,Z,0,'ONLY')
         CRXMC(I,J,3,1)=X
         CRYMC(I,J,3,1)=Y
         CRZMC(1,I,J,3,1)=DIS-CLEN/2.
         CRZMC(2,I,J,3,1)=DIS+CLEN/2.
         CRDX(I,J,3,1)=RC
         CRDY(I,J,3,1)=RC
         CRDZ(I,J,3,1)=CLEN
       enddo

C      стандартные блок 10ый ряд (см. csi_mctorl.F)
       I=4
       J=10
       X=(I-0.5)*(RC+DR)
       Y=(J-0.5)*(RC+DR)
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)
       NCR=IGR*16+ICH
       CALL GSPOS('BL  ',NCR,'ENDB',X,Y,Z,0,'ONLY')
       CRXMC(I,J,3,1)=X
       CRYMC(I,J,3,1)=Y
       CRZMC(1,I,J,3,1)=DIS-CLEN/2.
       CRZMC(2,I,J,3,1)=DIS+CLEN/2.
       CRDX(I,J,3,1)=RC
       CRDY(I,J,3,1)=RC
       CRDZ(I,J,3,1)=CLEN

C      нестандартные блоки  внешние кристаллы (см. csi_mctorl.F)
       
C  POSITIONING 60*88 BLOCKS
       I=10
       J=1
       X=(I-1)*(RC+DR)+(RC2+DR)/2.
       Y=(RC+DR)/2.
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)
       NCR=IGR*16+ICH
       CALL GSPOS('BL88',NCR,'ENDB',X,Y,Z,0,'ONLY')
       CRXMC(I,J,3,1)=X
       CRYMC(I,J,3,1)=Y
       CRZMC(1,I,J,3,1)=DIS-CLEN/2.
       CRZMC(2,I,J,3,1)=DIS+CLEN/2.
       CRDX(I,J,3,1)=RC2
       CRDY(I,J,3,1)=RC
       CRDZ(I,J,3,1)=CLEN

       I=10
       J=2
       Y=Y+(RC+DR)
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH
       CALL GSPOS('BL88',NCR,'ENDB',X,Y,Z,0,'ONLY')
       CRXMC(I,J,3,1)=X
       CRYMC(I,J,3,1)=Y
       CRZMC(1,I,J,3,1)=DIS-CLEN/2.
       CRZMC(2,I,J,3,1)=DIS+CLEN/2.
       CRDX(I,J,3,1)=RC2
       CRDY(I,J,3,1)=RC
       CRDZ(I,J,3,1)=CLEN

       I=9
       J=5
       X=X-(RC+DR)
       Y=4.5*(RC+DR)
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH
       CALL GSPOS('BL88',NCR,'ENDB',X,Y,Z,0,'ONLY')
       CRXMC(I,J,3,1)=X
       CRYMC(I,J,3,1)=Y
       CRZMC(1,I,J,3,1)=DIS-CLEN/2.
       CRZMC(2,I,J,3,1)=DIS+CLEN/2.
       CRDX(I,J,3,1)=RC2
       CRDY(I,J,3,1)=RC
       CRDZ(I,J,3,1)=CLEN

       I=5
       J=9
       Y=X
       X=4.5*(RC+DR)
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH 
       CALL GSPOS('BL88',NCR,'ENDB',X,Y,Z,NROT4,'ONLY')
       CRXMC(I,J,3,1)=X
       CRYMC(I,J,3,1)=Y
       CRZMC(1,I,J,3,1)=DIS-CLEN/2.
       CRZMC(2,I,J,3,1)=DIS+CLEN/2.
       CRDX(I,J,3,1)=RC
       CRDY(I,J,3,1)=RC2
       CRDZ(I,J,3,1)=CLEN

       I=2
       J=10
       X=1.5*(RC+DR)
       Y=Y+(RC+DR)
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH
       CALL GSPOS('BL88',NCR,'ENDB',X,Y,Z,NROT4,'ONLY')
       CRXMC(I,J,3,1)=X
       CRYMC(I,J,3,1)=Y
       CRZMC(1,I,J,3,1)=DIS-CLEN/2.
       CRZMC(2,I,J,3,1)=DIS+CLEN/2.
       CRDX(I,J,3,1)=RC
       CRDY(I,J,3,1)=RC2
       CRDZ(I,J,3,1)=CLEN

       I=1
       J=10
       X=X-(RC+DR)
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH
       CALL GSPOS('BL88',NCR,'ENDB',X,Y,Z,NROT4,'ONLY')
       CRXMC(I,J,3,1)=X
       CRYMC(I,J,3,1)=Y
       CRZMC(1,I,J,3,1)=DIS-CLEN/2.
       CRZMC(2,I,J,3,1)=DIS+CLEN/2.
       CRDX(I,J,3,1)=RC
       CRDY(I,J,3,1)=RC2
       CRDZ(I,J,3,1)=CLEN

C  POSITIONING 60*80 BLOCKS
       I=10
       J=3
       X=9*(RC+DR)+(RC1+DR)/2
       Y=2.5*(RC+DR)
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH    
       CALL GSPOS('BL80',NCR,'ENDB',X,Y,Z,0,'ONLY')
       CRXMC(I,J,3,1)=X
       CRYMC(I,J,3,1)=Y
       CRZMC(1,I,J,3,1)=DIS-CLEN/2.
       CRZMC(2,I,J,3,1)=DIS+CLEN/2.
       CRDX(I,J,3,1)=RC1
       CRDY(I,J,3,1)=RC
       CRDZ(I,J,3,1)=CLEN

       I=3
       J=10
       Y=X
       X=2.5*(RC+DR)
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH
       CALL GSPOS('BL80',NCR,'ENDB',X,Y,Z,NROT4,'ONLY')
       CRXMC(I,J,3,1)=X
       CRYMC(I,J,3,1)=Y
       CRZMC(1,I,J,3,1)=DIS-CLEN/2.
       CRZMC(2,I,J,3,1)=DIS+CLEN/2.
       CRDX(I,J,3,1)=RC
       CRDY(I,J,3,1)=RC1
       CRDZ(I,J,3,1)=CLEN

C  POSITIONING CUT BLOCKS
       I=9
       J=6
       X=8.5*(RC+DR)
       Y=5.5*(RC+DR)
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH
       CALL GSPOS('BL  ',NCR,'ENDB',X,Y,Z,0,'ONLY')
       CRXMC(I,6,3,1)=X
       CRYMC(9,6,3,1)=Y
       CRZMC(1,9,6,3,1)=DIS-CLEN/2.
       CRZMC(2,9,6,3,1)=DIS+CLEN/2.
       CRDX(I,J,3,1)=RC
       CRDY(I,J,3,1)=RC
       CRDZ(I,J,3,1)=CLEN
       
       I=6
       J=9
       X=5.5*(RC+DR)
       Y=8.5*(RC+DR)
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH    
       CALL GSPOS('BL  ',NCR,'ENDB',X,Y,Z,0,'ONLY')
       CRXMC(I,J,3,1)=X
       CRYMC(I,J,3,1)=Y
       CRZMC(1,I,J,3,1)=DIS-CLEN/2.
       CRZMC(2,I,J,3,1)=DIS+CLEN/2.
       CRDX(I,J,3,1)=RC
       CRDY(I,J,3,1)=RC
       CRDZ(I,J,3,1)=CLEN

C  TRAPEZOID CONSTITUENTS
       I=9
       J=6
       X=8.5*(RC+DR)-0.5
       Y=6*(RC+DR)+1.-DR/2.
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH 
       CALL GSPOS('BLTR',NCR,'ENDB',X,Y,Z,0,'ONLY')
       CRDY(I,J,3,1)=CRDY(I,J,3,1)+2.

       I=8
       J=7
       X=7.5*(RC+DR)+0.5
       Y=Y+(RC+DR)
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH
       CALL GSPOS('BLTR',NCR,'ENDB',X,Y,Z,NROT1,'ONLY')
       CRDY(I,J,3,1)=CRDY(I,J,3,1)+2.
       
       I=7
       J=8
       X=7*(RC+DR)+1.-DR/2.
       Y=7.5*(RC+DR)+0.5
       CALL CSI_MCTORL(1,3,I,J,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH    
       CALL GSPOS('BLTR',NCR,'ENDB',X,Y,Z,NROT6,'ONLY')
       CRDX(I,J,3,1)=CRDX(I,J,3,1)+2.

       I=6
       J=9
       X=X-(RC+DR)
       Y=8.5*(RC+DR)-0.5
       CALL CSI_MCTORL(1,3,6,9,1,ITRRL,IMDRL,IGR,ICH)   
       NCR=IGR*16+ICH
       CALL GSPOS('BLTR',NCR,'ENDB',X,Y,Z,NROT8,'ONLY')
       CRDX(I,J,3,1)=CRDX(I,J,3,1)+2.

c
c   ENDCAPS POSITIONING
c
C     электроника перед и за модулями
      		IF(ISWIT(3).EQ.0) THEN               ! старое расположение
      CALL GSPOS( 'CSEL' ,1, 'CSIL' , 0.,0.,21.-SHIFTLONG/2.,0,'ONLY')
      CALL GSPOS( 'CSEL' ,2, 'CSIL' , 0.,0.,-21.-SHIFTLONG/2.,0,'ONLY')
      IF(ISWIT(2).EQ.-1) THEN
	CALL GSPOS( 'CSEL' ,3, 'CSIR' , 0.,0.,21.-SHIFTLONG/2.,0,'ONLY')
        CALL GSPOS( 'CSEL' ,4, 'CSIR' , 0.,0.,-21.-SHIFTLONG/2.,0,'ONLY')
      ELSE
	CALL GSPOS( 'CSEL' ,3, 'CSIR' , 0.,0.,21.+SHIFTLONG/2.,0,'ONLY')
        CALL GSPOS( 'CSEL' ,4, 'CSIR' , 0.,0.,-21.+SHIFTLONG/2.,0,'ONLY') ! CSEL NOT ROTATED
      ENDIF
		ENDIF
      		IF(ISWIT(3).NE.0) THEN               ! новое расположение
      CALL GSPOS( 'CSEL' ,1, 'CSIL' , 0.,0.,20.-SHIFTLONG/2.,0,'ONLY')
      CALL GSPOS( 'CSEL' ,2, 'CSIL' , 0.,0.,-20.-SHIFTLONG/2.,0,'ONLY')
      CALL GSPOS( 'CSPL' ,1, 'CSIL' , 0.,0.,-21.3-SHIFTLONG/2.,0,'ONLY')
      CALL GSPOS( 'CSRI' ,1, 'CSIL' , 0.,0.,-21.3-SHIFTLONG/2.+0.4/2+1.8/2,0,'ONLY')
      IF(ISWIT(2).EQ.-1) THEN
	CALL GSPOS( 'CSEL' ,3, 'CSIR' , 0.,0.,20.-SHIFTLONG/2.,0,'ONLY')
        CALL GSPOS( 'CSEL' ,4, 'CSIR' , 0.,0.,-20.-SHIFTLONG/2.,0,'ONLY')
        CALL GSPOS( 'CSPL' ,2, 'CSIR' , 0.,0.,-21.3-SHIFTLONG/2.,0,'ONLY')
        CALL GSPOS( 'CSRI' ,2, 'CSIR' , 0.,0.,-21.3-SHIFTLONG/2.+0.4/2+1.8/2,0,'ONLY')
      ELSE
	CALL GSPOS( 'CSEL' ,3, 'CSIR' , 0.,0.,20.+SHIFTLONG/2.,0,'ONLY')
        CALL GSPOS( 'CSEL' ,4, 'CSIR' , 0.,0.,-20.+SHIFTLONG/2.,0,'ONLY') ! CSEL NOT ROTATED
        CALL GSPOS( 'CSPL' ,2, 'CSIR' , 0.,0.,21.3+SHIFTLONG/2.,0,'ONLY')
        CALL GSPOS( 'CSRI' ,2, 'CSIR' , 0.,0.,21.3+SHIFTLONG/2.-0.4/2-1.8/2,0,'ONLY')
      ENDIF
		ENDIF


C     положение модулей в торце
      XM=XYZROT(1,3)
      YM=XYZROT(2,3)
      ZM=XYZROT(3,3)
      CALL GSPOS( 'ENDB' ,3, 'CSIL' , XM,YM,ZM,NROT9,'ONLY')
      XM=XYZROT(1,2)
      YM=XYZROT(2,2)
      ZM=XYZROT(3,2)
      CALL GSPOS( 'ENDB' ,2, 'CSIL' , XM,YM,ZM,NROT14,'ONLY')
      XM=XYZROT(1,1)
      YM=XYZROT(2,1)
      ZM=XYZROT(3,1)
      CALL GSPOS( 'ENDB' ,1, 'CSIL' , XM,YM,ZM,NROT5,'ONLY')
      XM=XYZROT(1,4)
      YM=XYZROT(2,4)
      ZM=XYZROT(3,4)
      CALL GSPOS( 'ENDB' ,4, 'CSIL' , XM,YM,ZM,NROT15,'ONLY')
      IF(ISWIT(2).EQ.-1) THEN
	IM=5
      ELSE
        IM=8		! take shifts from module 8 in Baldin's definition
      ENDIF
      XM=XYZROT(1,IM)
      YM=XYZROT(2,IM)
      ZM=XYZROT(3,IM)
      CALL GSPOS( 'ENDB' ,5, 'CSIR' , XM,YM,ZM,NROT10,'ONLY')
      IF(ISWIT(2).EQ.-1) THEN
	IM=6
      ELSE
        IM=7		! take shifts from module 7 in Baldin's definition
      ENDIF
      XM=XYZROT(1,IM)
      YM=XYZROT(2,IM)
      ZM=XYZROT(3,IM)
      CALL GSPOS( 'ENDB' ,6, 'CSIR' , XM,YM,ZM,NROT11,'ONLY')
      IF(ISWIT(2).EQ.-1) THEN
	IM=7
      ELSE
        IM=6		! take shifts from module 6 in Baldin's definition
      ENDIF
      XM=XYZROT(1,IM)
      YM=XYZROT(2,IM)
      ZM=XYZROT(3,IM)
      CALL GSPOS( 'ENDB' ,7, 'CSIR' , XM,YM,ZM,NROT12,'ONLY')
      IF(ISWIT(2).EQ.-1) THEN
	IM=8
      ELSE
        IM=5		! take shifts from module 5 in Baldin's definition
      ENDIF
      XM=XYZROT(1,IM)
      YM=XYZROT(2,IM)
      ZM=XYZROT(3,IM)
      CALL GSPOS( 'ENDB' ,8, 'CSIR' , XM,YM,ZM,NROT13,'ONLY')
C     положение торцов в детекторе
      CALL GSPOS( 'CSIL' ,1, 'KEDR' , 0.,0.,DIS+SHIFTLONG/2,0,'ONLY')
      IF(ISWIT(2).EQ.-1) THEN
	CALL GSPOS( 'CSIR' ,1, 'KEDR' , 0.,0.,-DIS-SHIFTLONG/2,NROT3,'ONLY')
      ELSE
      	CALL GSPOS( 'CSIR' ,1, 'KEDR' , 0.,0.,-DIS-SHIFTLONG/2,0,'ONLY')
      ENDIF
C
C define x,y crystal coordinates for the other modules
C
C +Z
      DO J=1,10                 ! Y
        DO I=1,10               ! X
          CRXMC(I,J,1,1)=-CRXMC(I,J,3,1)
          CRYMC(I,J,1,1)=-CRYMC(I,J,3,1)
          CRZMC(1,I,J,1,1)= CRZMC(1,I,J,3,1)
          CRZMC(2,I,J,1,1)= CRZMC(2,I,J,3,1)
          CRDX(I,J,1,1)=CRDX(I,J,3,1)
          CRDY(I,J,1,1)=CRDY(I,J,3,1)
          CRDZ(I,J,1,1)=CRDZ(I,J,3,1)
          CRXMC(I,J,2,1)=-CRYMC(I,J,3,1)
          CRYMC(I,J,2,1)= CRXMC(I,J,3,1)
          CRZMC(1,I,J,2,1)= CRZMC(1,I,J,3,1)
          CRZMC(2,I,J,2,1)= CRZMC(2,I,J,3,1)
          CRDX(I,J,2,1)=CRDY(I,J,3,1)
          CRDY(I,J,2,1)=CRDX(I,J,3,1)
          CRDZ(I,J,2,1)=CRDZ(I,J,3,1)
          CRXMC(I,J,4,1)= CRYMC(I,J,3,1)
          CRYMC(I,J,4,1)=-CRXMC(I,J,3,1)
          CRZMC(1,I,J,4,1)= CRZMC(1,I,J,3,1)
          CRZMC(2,I,J,4,1)= CRZMC(2,I,J,3,1)
          CRDX(I,J,4,1)=CRDY(I,J,3,1)
          CRDY(I,J,4,1)=CRDX(I,J,3,1)
          CRDZ(I,J,4,1)=CRDZ(I,J,3,1)
        ENDDO
      ENDDO                           
C -Z 
      DO J=1,10                 ! Y
        DO I=1,10               ! X
          CRXMC(I,J,4,2)= CRYMC(I,J,1,1)             
          CRYMC(I,J,4,2)= CRXMC(I,J,1,1)
          CRZMC(1,I,J,4,2)=-CRZMC(1,I,J,1,1)
          CRZMC(2,I,J,4,2)=-CRZMC(2,I,J,1,1)
          CRDX(I,J,4,2)=CRDY(I,J,1,1)
          CRDY(I,J,4,2)=CRDX(I,J,1,1)
          CRDZ(I,J,4,2)=CRDZ(I,J,1,1)
          CRXMC(I,J,3,2)=-CRYMC(I,J,2,1)             
          CRYMC(I,J,3,2)=-CRXMC(I,J,2,1)
          CRZMC(1,I,J,3,2)=-CRZMC(1,I,J,2,1)
          CRZMC(2,I,J,3,2)=-CRZMC(2,I,J,2,1)
          CRDX(I,J,3,2)=CRDY(I,J,2,1)
          CRDY(I,J,3,2)=CRDX(I,J,2,1)
          CRDZ(I,J,3,2)=CRDZ(I,J,2,1)
          CRXMC(I,J,2,2)= CRYMC(I,J,3,1)             
          CRYMC(I,J,2,2)= CRXMC(I,J,3,1)
          CRZMC(1,I,J,2,2)=-CRZMC(1,I,J,3,1)
          CRZMC(2,I,J,2,2)=-CRZMC(2,I,J,3,1)
          CRDX(I,J,2,2)=CRDY(I,J,3,1)
          CRDY(I,J,2,2)=CRDX(I,J,3,1)
          CRDZ(I,J,2,2)=CRDZ(I,J,3,1)
          CRXMC(I,J,1,2)=-CRYMC(I,J,4,1)
          CRYMC(I,J,1,2)=-CRXMC(I,J,4,1)
          CRZMC(1,I,J,1,2)=-CRZMC(1,I,J,4,1)
          CRZMC(2,I,J,1,2)=-CRZMC(2,I,J,4,1)
          CRDX(I,J,1,2)=CRDY(I,J,4,1)
          CRDY(I,J,1,2)=CRDX(I,J,4,1)
          CRDZ(I,J,1,2)=CRDZ(I,J,4,1)
        ENDDO
      ENDDO
C  define z coordinates
c$$$      CRZMC(1,1)=DIS-CLEN/2.
c$$$      CRZMC(2,1)=DIS+CLEN/2.
c$$$      CRZMC(1,2)=-CRZMC(1,1)
c$$$      CRZMC(2,2)=-CRZMC(2,1)
C  convert MC coordinates to REAL
      DO ITRMC=1,2
        DO IMDMC=1,4
          DO IBLY=1,10
            DO IBLX=1,10
              DO ICR=1,2
                CALL CSI_MCTORL(ITRMC,IMDMC,IBLX,IBLY,ICR,
     $             ITRRL,IMDRL,IGR,ICH)
                if(ICH.gt.0) then
                  CRXRL(ICH,IGR,IMDRL,ITRRL)=
     $               CRXMC(IBLX,IBLY,IMDMC,ITRMC)
                  CRYRL(ICH,IGR,IMDRL,ITRRL)=
     $               CRYMC(IBLX,IBLY,IMDMC,ITRMC)
                  CRZRL(ICH,IGR,IMDRL,ITRRL)=
     $               CRZMC(ICR,IBLX,IBLY,IMDMC,ITRMC)
                  ncr = (itrrl-1)*16*12*4+(imdrl-1)*16*12+(igr-1)*16
     $               +(ich-1)
c$$$                  print *,'CSICOORD: ',ncr,ICH,IGR,IMDRL,ITRRL,
c$$$     $               CRXRL(ICH,IGR,IMDRL,ITRRL),
c$$$     $               CRYRL(ICH,IGR,IMDRL,ITRRL),
c$$$     $               CRZRL(ICH,IGR,IMDRL,ITRRL),
c$$$     $               CRDX(IBLX,IBLY,IMDMC,ITRMC),
c$$$     $               CRDY(IBLX,IBLY,IMDMC,ITRMC),
c$$$     $               CRDZ(IBLX,IBLY,IMDMC,ITRMC)
                endif
              ENDDO
            ENDDO  
          ENDDO
        ENDDO
      ENDDO
      
      RETURN
      END



