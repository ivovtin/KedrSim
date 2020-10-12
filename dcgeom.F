*CMZU:          21/05/97  15.15.27  by  YURI SKOVPEN
*CMZU:  1.00/04 30/11/96  11.21.11  by  VLADIMIR KOZLOV
*CMZU:  1.00/03 26/11/96  16.06.15  by  VLADIMIR KOZLOV
*-- Author :    Kozlov Vladimir   29/09/92
*  changed Korneliy Todyshev
       SUBROUTINE DCGEOM
C
#include "gconst.inc"
C
	include 'fiel.inc'
C
	include 'medpar.inc'
C
	include 'celpar.inc'
C
	include 'dcwire.inc'
C
      character *80 file

      DIMENSION PAR(15)
      DIMENSION PARKDC(3),PARFI(3),PARFO(3),PARFL(3),PARFOS(3)
      CHARACTER*4 DCLNAM(7),DCCNAM(7),DSHNAM(6),DSHSNM
C
      DIMENSION XSSWIR(8,7),YSSWIR(8,7),XSSWIRZ(8,3),YSSWIRZ(8,3),
     +  XFFWIR(9,2,7),YFFWIR(9,2,7),XFFWIRZ(9,2,3),YFFWIRZ(9,2,3),
     +   XPPWIR(27,7),YPPWIR(27,7),XPPWIRZ(27,3),YPPWIRZ(27,3),
     +   NWSHLD(6),XWSHLD(6),YWSHLD(6),STESIN(3)
C
      DATA DCLNAM /'DCL1','DCL2','DCL3','DCL4','DCL5','DCL6','DCL7'/
      DATA DSHNAM /'DSH1','DSH2','DSH3','DSH4','DSH5','DSH6'/
      DATA DCCNAM /'DCC1','DCC2','DCC3','DCC4','DCC5','DCC6','DCC7'/
C
      DATA PARKDC/12.5, 53.5, 55.5/
      DATA PARFI /12.5, 12.7, 48.5/
      DATA PARFO /53.0, 53.5, 48.5/
      DATA PARFOS /53.2, 53.5, 3.25/ 
      DATA PARFL /12.5, 53.2, 1.0/
C
C PARAMETERS OF SUPER LAYERS
C
C      DIMENSION PARWIRE(3)
C      DATA PARWIRE/ 0.0, 0.0028, 48.5/
      DATA ZCEL/48.5/
C
      DATA NCELL /18,24,30,36,42,48,54/     
      double precision  X,D,DX,DY,CO,SI
      double precision  ANGLEDC
      double precision  WUX,WUY,WUZ,Wnorm,ACEL,XC,YC,PHIC

      save /CELPAR/
C
C==================================================================
C     VOVA'S DEFINITION OF VOLUMES, DC.
C     30 OCTOBER 1996
C     

      IF(FIEL.EQ.0.) ANGLOR=0.
c      ANGLELOR=ANGLOR*DEGRAD*FIEL/18.
      ANGLELOR=3.0*DEGRAD*FIEL/18.
      ANGLEDC=ANGDC
      DFOCUS=BOUND/2.
      RFOCUS=DFOCUS
      TWOBND=2.*BOUND
      BOUND2=BOUND*BOUND
      RELPS2=RELPS*RELPS
      print *,'anglelor :',ANGLELOR
      ANGLEDC=ANGLEDC*DEGRAD
      COSDC=COS(ANGLEDC)
      SINDC=SIN(ANGLEDC)
      COSLOR=COS(ANGLELOR)
      SINLOR=SIN(ANGLELOR)
      TANLOR=TAN(ANGLELOR)
C
      ITIMLC=TIMLCK*GFREQ
      ISIGLC=SIGLCK*GFREQ
      VELGFR=DRFVEL/GFREQ
      TIMDEL=TIMD*GFREQ
      TIMDLT=TIMDEL-INT(TIMDEL)
      IF(TIMDLT.NE.0.) TIMDLT=TIMDLT-1.
      ITMBND=INT(RFOCUS/VELGFR)+1
      XTIM0(1)=0.
      DO I=1,ITMBND
        XTIM0(I+1)=2.*SQRT(RFOCUS*VELGFR*(I+TIMDLT))
        XTIM(I)=(XTIM0(I)+XTIM0(I+1))/2.
      ENDDO
C
C Read connections channels to wires.
C
c      file=kdpath(:index(kdpath,' ')-1)//'connections.dc'
      file='connections_sim.dc'
      OPEN(30,FILE=file,STATUS='OLD',
     +     ERR=741)
      GO TO 742
741   file=KEDR_RELEASE//'KedrSim/connections_sim.dc'
      OPEN(30,FILE=file,STATUS='OLD',
     +     ERR=1001)
742   DO I=1,NWIRES
        READ(30,*) Icr,Ipl,Nch,Ich,knum,krow,kcol,Jnum,Jrow,Jcol,Isec,
     +         L,Ic,Iw
        NW=L*512+Ic*8+Iw
        ICHWIR(1,NW)=Ich
        ICHWIR(2,NW)=Ipl+1
        ICHWIR(3,NW)=Icr
        NCHPL(Ipl+1,Icr)=Nch
      ENDDO
      CLOSE(30)
C
C Read coordinates of wires.
C
c      file=kdpath(:index(kdpath,' ')-1)//'geom.dc'
      file='geom.dc'
      OPEN(30,FILE=file,STATUS='OLD',
     +     ERR=751)
      GO TO 752
751   file=KEDR_RELEASE//'DchGeom/geom.dc'
      OPEN(30,FILE=file,STATUS='OLD',
     +     ERR=1002)
752   DO IL=1,7
        READ(30,*) NLAY,NCELL(IL),NW
C Sensitive wires.
        DO IW=1,8
          READ(30,*) X,Y
          XSSWIR(IW,IL)=(X*COSDC+Y*SINDC)/10.
          YSSWIR(IW,IL)=(Y*COSDC-X*SINDC)/10.
        ENDDO
        DO IW=1,6
          XSWIR(IW,IL)=XSSWIR(1+IW,IL)
          YSWIR(IW,IL)=YSSWIR(1+IW,IL)
        ENDDO
C
C Focus wires (left and right).
        DO IF=1,2
          DO IW=1,9
            READ(30,*) X,Y
            XFFWIR(IW,IF,IL)=(X*COSDC+Y*SINDC)/10.
            YFFWIR(IW,IF,IL)=(Y*COSDC-X*SINDC)/10.
          ENDDO
          DO IW=1,7
            XFWIR(IW,IF,IL)=XFFWIR(1+IW,IF,IL)
            YFWIR(IW,IF,IL)=YFFWIR(1+IW,IF,IL)
          ENDDO
        ENDDO
C
C Field wires.
        IW=27
        IF(IL.EQ.1) IW=23
        DO J=1,IW
          READ(30,*) X,Y
          XPPWIR(J,IL)=(X*COSDC+Y*SINDC)/10.
          YPPWIR(J,IL)=(Y*COSDC-X*SINDC)/10.
        ENDDO
C Left corners of cell.
        IW1=5
        IF(IL.EQ.1) IW1=4
        XPWIR(1,IL)=XPPWIR(IW1,IL)
        YPWIR(1,IL)=YPPWIR(IW1,IL)
        IW1=11
        IF(IL.EQ.1) IW1=9
        XPWIR(2,IL)=XPPWIR(IW1,IL)
        YPWIR(2,IL)=YPPWIR(IW1,IL)
C
C Shield wires.
        IF(IL.NE.7) THEN
          READ(30,*) NWSHLD(IL),X,Y
          XWSHLD(IL) = (X*COSDC+Y*SINDC)/10.
          YWSHLD(IL) = (Y*COSDC-X*SINDC)/10.
        ENDIF
C
C Count CELL parameters.
        CELANG(IL)=TWOPI/NCELL(IL)
        X=XPWIR(2,IL)
        Y=YPWIR(2,IL)
        RADIUS(2,IL)=SQRT(X*X+Y*Y)
        FI0=ATAN2(Y,X)-CELANG(IL)
        IF(FI0.LT.0.) FI0=FI0+TWOPI
        FI0CEL(2,IL)=FI0
        X=XPWIR(1,IL)
        Y=YPWIR(1,IL)
        RADIUS(1,IL)=SQRT(X*X+Y*Y)
        FI0=ATAN2(Y,X)-CELANG(IL)
        F0CEL(IL)=FI0
        IF(FI0.LT.0.) FI0=FI0+TWOPI
        FI0CEL(1,IL)=FI0
        DX=XPWIR(2,IL)-X
        DY=YPWIR(2,IL)-Y
        DXPWIR(IL)=DX
        DYPWIR(IL)=DY
        D=SQRT(DX*DX+DY*DY)
        COPWIR(IL)=DX/D
        SIPWIR(IL)=DY/D
        DPWIR(IL)=D/8.
C
        X=XSWIR(1,IL)
        Y=YSWIR(1,IL)
        DX=(XSWIR(6,IL)-X)/5.
        DY=(YSWIR(6,IL)-Y)/5.
        DXSWIR(IL)=DX
        DYSWIR(IL)=DY
        D=SQRT(DX*DX+DY*DY)
        DSWIR(IL)=D
        CO=DX/D
        COSWIR(IL)=CO
        SI=DY/D
        SISWIR(IL)=SI
c        D=D/2.
        XSWIR0(IL)=X-D*CO
        YSWIR0(IL)=Y-D*SI
        SIWLOR(IL)=SINLOR*SI-COSLOR*CO
        COWLOR(IL)=COSLOR*SI+SINLOR*CO
        DXFWIR(1,IL)=(XFWIR(1,1,IL)-XSWIR0(IL))*COWLOR(IL)+
     +               (YFWIR(1,1,IL)-YSWIR0(IL))*SIWLOR(IL)
        DYFWIR(1,IL)=(YFWIR(1,1,IL)-YSWIR0(IL))*COWLOR(IL)-
     +               (XFWIR(1,1,IL)-XSWIR0(IL))*SIWLOR(IL)
        DXFWIR(2,IL)=(XFWIR(1,2,IL)-XSWIR0(IL))*COWLOR(IL)+
     +               (YFWIR(1,2,IL)-YSWIR0(IL))*SIWLOR(IL)
        DYFWIR(2,IL)=(YFWIR(1,2,IL)-YSWIR0(IL))*COWLOR(IL)-
     +               (XFWIR(1,2,IL)-XSWIR0(IL))*SIWLOR(IL)
        ENDDO
C
C read coordinats on other side in stereo-layers.
      DO JL=1,7
        READ(30,*) N1,N2,N3
        IF(MOD(JL,2).NE.0) THEN
C skip axial layers.
          JSKIP=53
          IF(JL.EQ.1) JSKIP=49
          DO J=1,JSKIP
            READ(30,*) X,Y
          ENDDO
        ELSE
          IL=JL/2
C Sensitive wires.
          DO IW=1,8
            READ(30,*) X,Y
            XSSWIRZ(IW,IL)=(X*COSDC+Y*SINDC)/10.
            YSSWIRZ(IW,IL)=(Y*COSDC-X*SINDC)/10.
          ENDDO
          DO IW=1,6
            XSWIRZ(IW,IL)=XSSWIRZ(1+IW,IL)
            YSWIRZ(IW,IL)=YSSWIRZ(1+IW,IL)
          ENDDO
C
C Focus wires (left and right).
          DO IF=1,2
            DO IW=1,9
              READ(30,*) X,Y
              XFFWIRZ(IW,IF,IL)=(X*COSDC+Y*SINDC)/10.
              YFFWIRZ(IW,IF,IL)=(Y*COSDC-X*SINDC)/10.
            ENDDO
            DO IW=1,7
              XFWIRZ(IW,IF,IL)=XFFWIRZ(1+IW,IF,IL)
              YFWIRZ(IW,IF,IL)=YFFWIRZ(1+IW,IF,IL)
            ENDDO
          ENDDO
C
C Field wires.
          IW=27
          DO J=1,IW
            READ(30,*) X,Y
            XPPWIRZ(J,IL)=(X*COSDC+Y*SINDC)/10.
            YPPWIRZ(J,IL)=(Y*COSDC-X*SINDC)/10.
          ENDDO
C Left corners of cell.
          IW1=5
          XPWIRZ(1,IL)=XPPWIRZ(IW1,IL)
          YPWIRZ(1,IL)=YPPWIRZ(IW1,IL)
          IW1=11
          XPWIRZ(2,IL)=XPPWIRZ(IW1,IL)
          YPWIRZ(2,IL)=YPPWIRZ(IW1,IL)
        ENDIF
        IF(JL.NE.7) READ(30,*) N,X,Y
      ENDDO
C Count CELL parameters.
      DO JL=1,3
        IL=JL*2.
        DO L=1,2
          X=XPWIR(L,IL)
          Y=YPWIR(L,IL)
          XZ=XPWIRZ(L,JL)
          YZ=YPWIRZ(L,JL)
          FIZ0=ATAN2(YZ,XZ)-CELANG(IL)
          FI0=FI0CEL(L,IL)
          IF(FI0.GT.PI) FI0=FI0-TWOPI
          FI0Z=(FIZ0+FI0)/2.
          IF(FI0Z.LT.0.) FI0Z=FI0Z+TWOPI
          STEFI0(L,JL)=FI0Z
          DX=XZ-X
          DY=YZ-Y
          D2=(DX*DX+DY*DY)/4.
          RA=RADIUS(L,IL)
          R2=RA*RA-D2
          R=SQRT(R2)
          D=SQRT(D2)
          STERAD(L,JL)=R
          IF(L.EQ.1) THEN
            STETRN(JL)=(FIZ0-F0CEL(IL))/2.
            STEDIS(JL)=SIGN(D,FI0-FIZ0)
            STEDI(JL)=SIGN(D,STETRN(JL))
            STEANG(JL)=SIGN(ATAN2(STEDI(JL)/ZCEL,1.),STETRN(JL))
            STERA2(JL)=R2
            DXPWIZ(JL)=DX
            DYPWIZ(JL)=DY
            TXPWIZ(JL)=XPWIRZ(2,JL)-XPWIR(2,IL)-DXPWIZ(JL)
            TYPWIZ(JL)=YPWIRZ(2,JL)-YPWIR(2,IL)-DYPWIZ(JL)
            STETAN(JL)=ZCEL*R/STEDI(JL)
            STECOS(JL)=R/RA
            STESIN(JL)=D/RA
          ENDIF
        ENDDO
      ENDDO
C
      CLOSE(30)
C
      CALL GSVOLU( 'KDC ', 'TUBE', MEDDME,   PARKDC,  3, IVOL)
C
      if(IWGEOM.NE.0) then
C    SENS WIRE
      PAR(1)=0.
      PAR(2)=0.0014
      PAR(3)=ZCEL
      CALL GSVOLU('DCSW','TUBE',MEDTUN ,PAR,3,IVOL)
      CALL GSATT('DCSW','SEEN',0)
C    FOCUS WIRE
      PAR(2)=0.0075
      CALL GSVOLU('DCFW','TUBE',MEDTIT ,PAR,3,IVOL)
      CALL GSATT('DCFW','SEEN',0)
C    FIELD WIRE
      CALL GSVOLU('DCPW','TUBE',MEDTIT ,PAR,3,IVOL)
      CALL GSATT('DCPW','SEEN',0)
C    SHIELD WIRE
      PAR(2)=0.0035
      CALL GSVOLU('DCSH','TUBE',MEDTIT ,PAR,3,IVOL)
      CALL GSATT('DCSH','SEEN',0)
      endif
C
C DEFINITION OF CELLS FOR AXIAL SUPER LAYERS
C
      IPI=0
      CALL FREE_ROTMAT_NUMBER(NROTB)
      DO IL=1,7,2
        ACEL=PI/NCELL(IL)
        AWIR=0.02/RADIUS(1,IL)
        RAD1=RADIUS(1,IL)*COS(ACEL)
        PAR(1)=RAD1-0.1
        PAR(2)=(RADIUS(2,IL)+0.05)/COS(ACEL)+0.1
        PAR(3)=ZCEL
        CALL GSVOLU( DCLNAM(IL), 'TUBE', MEDDME, PAR, 3, IVOL)
c        CALL GSATT( DCLNAM(IL), 'SEEN', 0)
C
        PAR(1)=ZCEL
        PAR(2)=0.0
        PAR(3)=0.0
        HCEL=(RADIUS(2,IL)-RAD1)/2.
        PAR(4)=HCEL+0.05
        PAR(5)=(RAD1-0.05)*TAN(ACEL)
        PAR(6)=(RADIUS(2,IL)+0.05)*TAN(ACEL)
        PAR(7)=(ATAN2(YPWIR(1,IL),XPWIR(1,IL))-
     +          ATAN2(DYPWIR(IL),DXPWIR(IL)))*RADDEG
        PAR(8)=PAR(4)
        PAR(9)=PAR(5)
        PAR(10)=PAR(6)
        PAR(11)=PAR(7)
        CALL GSVOLU( DCCNAM(IL), 'TRAP', MEDDME, PAR, 11, IVOL )
C       CALL GSATT(DCCNAM(IL),'SEEN',0)
C
        ANG=ACEL+F0CEL(IL)
        PX=RAD1*COS(ANG)
        PY=RAD1*SIN(ANG)
        PAR7=PAR(7)*DEGRAD
        HC=HCEL/COS(PAR7)
        X0=PX+HC*COS(ANG-PAR7)
        Y0=PY+HC*SIN(ANG-PAR7)
C Add angle to keep field wires inside cell
        PH=(ATAN2(Y0,X0)+AWIR)*RADDEG
        RAD=SQRT(X0*X0+Y0*Y0)

      if(IWGEOM.NE.0) then
C Position wires inside cell
        COSP=COS(PH*DEGRAD)
        SINP=SIN(PH*DEGRAD)
        PX=RAD*COSP
        PY=RAD*SINP
C COS(ph-90)=sin(ph)
C SIN(ph-90)=-cos(ph)
C
C Sensitive wires.
        DO IW=1,8
          X=(XSSWIR(IW,IL)-PX)*SINP-(YSSWIR(IW,IL)-PY)*COSP
          Y=(YSSWIR(IW,IL)-PY)*SINP+(XSSWIR(IW,IL)-PX)*COSP
          CALL GSPOS( 'DCSW', IW+8*(IL-1), DCCNAM(IL),
     +                 X, Y, 0., 0, 'ONLY')
        ENDDO
C
C Focus wires (left and right).
        DO IF=1,2
          DO IW=1,9
            X=(XFFWIR(IW,IF,IL)-PX)*SINP-(YFFWIR(IW,IF,IL)-PY)*COSP
            Y=(YFFWIR(IW,IF,IL)-PY)*SINP+(XFFWIR(IW,IF,IL)-PX)*COSP
            CALL GSPOS( 'DCFW', IW+9*(IF-1)+18*(IL-1), DCCNAM(IL),
     +                   X, Y, 0., 0, 'ONLY')
          ENDDO
        ENDDO
C
C Field wires.
        IW0=27
        IF(IL.EQ.1) IW0=23
        DO IW=1,IW0
          X=(XPPWIR(IW,IL)-PX)*SINP-(YPPWIR(IW,IL)-PY)*COSP
          Y=(YPPWIR(IW,IL)-PY)*SINP+(XPPWIR(IW,IL)-PX)*COSP
          CALL GSPOS( 'DCPW', IW+27*(IL-1), DCCNAM(IL),
     +                 X, Y, 0., 0, 'ONLY')
        ENDDO
      endif
C
        DPH=360./NCELL(IL)
        DO IPHI=1,NCELL(IL)
           CALL GSROTM( NROTB+IPI, 90., PH-90., 90., PH, 0., 0.)
           PX=RAD*COS(PH*DEGRAD)
           PY=RAD*SIN(PH*DEGRAD)
           PH=PH-DPH
           CALL GSPOS( DCCNAM(IL), IPHI, DCLNAM(IL),
     +                 PX, PY, 0., NROTB+IPI, 'ONLY')
           IPI=IPI+1
        ENDDO
        CALL GSPOS(DCLNAM(IL),1,'KDC ',0.,0.,0.,0 ,'ONLY')
      ENDDO
C
C DEFINITION OF CELLS FOR STEREO SUPER LAYERS
C
      DO IL=2,6,2
        JL=IL/2
        DO IW=1,6,1      
          XC=XSWIRZ(IW,JL)
          YC=YSWIRZ(IW,JL)              
          WUX=XSWIR(IW,IL)-XC
          WUY=YSWIR(IW,IL)-YC
          WUZ=2*ZCEL
          Wnorm=SQRT(WUX*WUX+WUY*WUY+WUZ*WUZ)
          WUX= WUX/Wnorm
          WUY= WUY/Wnorm
          WUZ= WUZ/Wnorm
          WUV(1,IW,JL,1)=WUX
          WUV(2,IW,JL,1)=WUY
          WUV(3,IW,JL,1)=WUZ                    
          xswr(IW,JL,1)=XC
          yswr(IW,JL,1)=YC                  
          DO IC=2,NCELL(IL) ,1        
          PHIC=CELANG(IL)*(IC-1)
          WUV(1,IW,JL,IC)=WUX*cos(PHIC)+WUY*sin(PHIC)          
          WUV(2,IW,JL,IC)=WUY*cos(PHIC)-WUX*sin(PHIC) 
          WUV(3,IW,JL,IC)=WUZ
          XSWR(IW,JL,IC)=XC*cos(PHIC)+YC*sin(PHIC)
          YSWR(IW,JL,IC)=YC*cos(PHIC)-XC*sin(PHIC)          
          ENDDO      
          XC=0.5*(XSWIRZ(3,JL)+XSWIRZ(4,JL))
          YC=0.5*(YSWIRZ(3,JL)+YSWIRZ(4,JL))       
          WUX=(XSWIR(3,IL)+XSWIR(4,IL))*0.5-XC
          WUY=(YSWIR(3,IL)+YSWIR(4,IL))*0.5-YC
          WUZ=2*ZCEL          
          Wnorm=SQRT(WUX*WUX+WUY*WUY+WUZ*WUZ)
          WUX= WUX/Wnorm
          WUY= WUY/Wnorm
          WUZ= WUZ/Wnorm          
          DO IC=1,NCELL(IL) ,1        
          PHIC=CELANG(IL)*(IC-1)
          WUV34(1,JL,IC)=WUX*cos(PHIC)+WUY*sin(PHIC)          
          WUV34(2,JL,IC)=WUY*cos(PHIC)-WUX*sin(PHIC) 
          WUV34(3,JL,IC)=WUZ
          ENDDO      
      ENDDO 





        ACEL=PI/NCELL(IL)
        AWIR=0.02/RADIUS(1,IL)
        PAR(1)=STERAD(1,JL)-0.1
        PAR(2)=(RADIUS(2,IL)+0.05)/COS(ACEL)+0.1
        PAR(3)=ZCEL
        CALL GSVOLU( DCLNAM(IL), 'TUBE', MEDDME, PAR, 3, IVOL)
c        CALL GSATT( DCLNAM(IL), 'SEEN', 0)
        PAR(1)=ZCEL
        PAR(3)=0.
        PAR(4)=-STETRN(JL)*RADDEG
        RAD1=RADIUS(1,IL)*COS(ACEL)
        D=STESIN(JL)*(RAD1+RADIUS(2,IL))/2.
        PAR(2)=SIGN(ATAN2(D,ZCEL),STEANG(JL))*RADDEG
        HCEL=(RADIUS(2,IL)-RAD1)/2.
        PAR(5)=HCEL+0.05
        PAR(6)=(RAD1-0.05)*TAN(ACEL)
        PAR(7)=(RADIUS(2,IL)+0.05)*TAN(ACEL)
        PAR(8)=(ATAN2(YPWIR(1,IL),XPWIR(1,IL))-
     +          ATAN2(DYPWIR(IL),DXPWIR(IL)))*RADDEG
        PAR(9)=PAR(5)
        PAR(10)=PAR(6)
        PAR(11)=PAR(7)
        PAR(12)=PAR(8)
        CALL GSVOLU( DCCNAM(IL), 'GTRA', MEDDME, PAR, 12, IVOL )
C       CALL GSATT(DCCNAM(IL),'SEEN',0)
C
        ANG=ACEL+F0CEL(IL)
        PX=RAD1*COS(ANG)
        PY=RAD1*SIN(ANG)
        PAR8=PAR(8)*DEGRAD
        HC=HCEL/COS(PAR8)
        X0=PX+HC*COS(ANG-PAR8)
        Y0=PY+HC*SIN(ANG-PAR8)
C Add angle to keep field wires inside cell
        PH=(ATAN2(Y0,X0)+AWIR)*RADDEG-PAR(4)
        RAD=SQRT(X0*X0+Y0*Y0)*COS(PAR(4)*DEGRAD)

      if(IWGEOM.NE.0) then
C Position wires inside cell
        COSP=COS(PH*DEGRAD)
        SINP=SIN(PH*DEGRAD)
        PX=RAD*COSP
        PY=RAD*SINP
C COS(ph-90)=sin(ph)
C SIN(ph-90)=-cos(ph)
C
C Sensitive wires.
        DO IW=1,8
          XZ=(XSSWIR(IW,IL)+XSSWIRZ(IW,JL))/2.
          YZ=(YSSWIR(IW,IL)+YSSWIRZ(IW,JL))/2.
          X=(XZ-PX)*SINP-(YZ-PY)*COSP
          Y=(YZ-PY)*SINP+(XZ-PX)*COSP
          X1=(XSSWIR(IW,IL)-PX)*SINP-(YSSWIR(IW,IL)-PY)*COSP
          Y1=(YSSWIR(IW,IL)-PY)*SINP+(XSSWIR(IW,IL)-PX)*COSP
          DX=X1-X
          DY=Y1-Y
          FI=ATAN2(DY,DX)*RADDEG
          TET=ATAN2(SQRT(DX*DX+DY*DY),ZCEL)*RADDEG
          CALL GSROTM( NROTB+IPI,90.+TET,FI, 90., 90.+FI, TET, FI)
          CALL GSPOS( 'DCSW', IW+8*(IL-1), DCCNAM(IL),
     +                 X, Y, 0., NROTB+IPI, 'ONLY')
          IPI=IPI+1
        ENDDO
C
C Focus wires (left and right).
        DO IF=1,2
          DO IW=1,9
            XZ=(XFFWIR(IW,IF,IL)+XFFWIRZ(IW,IF,JL))/2.
            YZ=(YFFWIR(IW,IF,IL)+YFFWIRZ(IW,IF,JL))/2.
            X=(XZ-PX)*SINP-(YZ-PY)*COSP
            Y=(YZ-PY)*SINP+(XZ-PX)*COSP
            X1=(XFFWIR(IW,IF,IL)-PX)*SINP-(YFFWIR(IW,IF,IL)-PY)*COSP
            Y1=(YFFWIR(IW,IF,IL)-PY)*SINP+(XFFWIR(IW,IF,IL)-PX)*COSP
            DX=X1-X
            DY=Y1-Y
            FI=ATAN2(DY,DX)*RADDEG
            TET=ATAN2(SQRT(DX*DX+DY*DY),ZCEL)*RADDEG
            CALL GSROTM( NROTB+IPI,90.+TET,FI, 90., 90.+FI, TET, FI)
            CALL GSPOS( 'DCFW', IW+9*(IF-1)+18*(IL-1), DCCNAM(IL),
     +                   X, Y, 0., NROTB+IPI, 'ONLY')
          IPI=IPI+1
          ENDDO
        ENDDO
C
C Field wires.
        DO IW=1,27
          XZ=(XPPWIR(IW,IL)+XPPWIRZ(IW,JL))/2.
          YZ=(YPPWIR(IW,IL)+YPPWIRZ(IW,JL))/2.
          X=(XZ-PX)*SINP-(YZ-PY)*COSP
          Y=(YZ-PY)*SINP+(XZ-PX)*COSP
          X1=(XPPWIR(IW,IL)-PX)*SINP-(YPPWIR(IW,IL)-PY)*COSP
          Y1=(YPPWIR(IW,IL)-PY)*SINP+(XPPWIR(IW,IL)-PX)*COSP
          DX=X1-X
          DY=Y1-Y
          FI=ATAN2(DY,DX)*RADDEG
          TET=ATAN2(SQRT(DX*DX+DY*DY),ZCEL)*RADDEG
          CALL GSROTM( NROTB+IPI,90.+TET,FI, 90., 90.+FI, TET, FI)
          CALL GSPOS( 'DCPW', IW+27*(IL-1), DCCNAM(IL),
     +                 X, Y, 0., NROTB+IPI, 'ONLY')
          IPI=IPI+1
        ENDDO
      endif
C

        DPH=360./NCELL(IL)
        DO IPHI=1,NCELL(IL)
           CALL GSROTM( NROTB+IPI, 90., PH-90., 90., PH, 0., 0.)
           PX=RAD*COS(PH*DEGRAD)
           PY=RAD*SIN(PH*DEGRAD)
           PH=PH-DPH
           CALL GSPOS( DCCNAM(IL), IPHI, DCLNAM(IL),
     +                 PX, PY, 0., NROTB+IPI, 'ONLY')
           IPI=IPI+1
        ENDDO
        CALL GSPOS(DCLNAM(IL),1,'KDC ',0.,0.,0.,0 ,'ONLY')
      ENDDO
      NROTF=NROTB+IPI
C
C Position shield wires
C
      if(IWGEOM.NE.0) then
      IW=0
      DO IL=1,6
	NSec=sqrt(NWSHLD(IL)*1.)
	DPHSec= 360./NSec
        X0=XWSHLD(IL)
        Y0=YWSHLD(IL)
        PH=ATAN2(Y0,X0)*RADDEG
        RAD=SQRT(X0*X0+Y0*Y0)
        DPH=360./NWSHLD(IL)
        IWSec=0
	PAR(1)=RAD-0.02
        PAR(2)=RAD+0.02
        PAR(3)=ZCEL
        CALL GSVOLU( DSHNAM(IL), 'TUBE', MEDDME, PAR, 3, IVOL )
        CALL GSATT( DSHNAM(IL),'SEEN',0)
        CALL GSPOS( DSHNAM(IL),1,'KDC ',0.,0.,0.,0,'ONLY')

	ISec=1
	PAR(4)=PH-0.1
        PAR(5)=PAR(4)+DPHSec
        write(DSHSNM,'(a1,i1,i2.2)') 'D',IL,ISec
        CALL GSVOLU( DSHSNM, 'TUBS', MEDDME, PAR, 5, IVOL )
        CALL GSATT( DSHSNM,'SEEN',0)
        CALL GSPOS( DSHSNM, 1, DSHNAM(IL), 0., 0., 0., 0, 'ONLY') 
c        print *,'sh nam,il,ph,phs(2)=',DSHSNM,il,ph,par(4),par(5)
        DO IWn=1,NWSHLD(IL)
           if(PH.gt.PAR(5)) then
	     ISec=ISec+1
	     PAR(4)=PAR(5)
             PAR(5)=PAR(4)+DPHSec
	     if(PAR(5).gt.360) PAR(5)=PAR(5)-360.
	     write(DSHSNM,'(a1,i1,i2.2)') 'D',IL,ISec
             CALL GSVOLU( DSHSNM, 'TUBS', MEDDME, PAR, 5, IVOL )
             CALL GSATT( DSHSNM,'SEEN',0)
             CALL GSPOS( DSHSNM, 1, DSHNAM(IL), 0., 0., 0., 0, 'ONLY')
c        print *,'sh nam,il,ph,phs(2)=',DSHSNM,il,ph,par(4),par(5)
	   endif
           PX=RAD*COS(PH*DEGRAD)
           PY=RAD*SIN(PH*DEGRAD)
	   IW=IW+1
           CALL GSPOS( 'DCSH', IW, DSHSNM, PX, PY, 0., 0, 'ONLY')
           PH=PH+DPH
        ENDDO
c	print *,'sh il,ph,phs(2)=',il,ph,par(4),par(5)
      ENDDO
      endif
C
C    D-CHAMBER - INNER WALL
         CALL GSVOLU('DCFI','TUBE',MEDCPL,PARFI,3,IVOL)
         PAR(2)=PARFI(2)
         PAR(1)=PAR(2)-0.005
         PAR(3)=PARFI(3)
         CALL GSVOLU('DCI0','TUBE',MEDCOP,PAR,3,IVOL)
      CALL GSATT('DCI0','SEEN',0)
         PAR(2)=PAR(1)
         PAR(1)=PAR(2)-0.04-0.015
         CALL GSVOLU('DCI1','TUBE',MEDG10,PAR,3,IVOL)
      CALL GSATT('DCI1','SEEN',0) 
c         PAR(2)=PAR(1)
c         PAR(1)=PAR(2)-0.015
c         CALL GSVOLU('DCI2','TUBE',MEDMYL,PAR,3,IVOL)
         CALL GSPOS('DCI0',1,'DCFI',0.,0.,0.,0,'ONLY')
         CALL GSPOS('DCI1',1,'DCFI',0.,0.,0.,0,'ONLY')
c         CALL GSPOS('DCI2',1,'DCFI',0.,0.,0.,0,'ONLY')
c      CALL GSATT('DCFI','SEEN',0)
C
C    D-CHAMBER - OUTER WALL
         CALL GSVOLU('DCFO','TUBE',MEDG10,PARFO,3,IVOL)
         PAR(2)=PARFO(2)
         PAR(1)=PAR(2)-0.006
         PAR(3)=PARFI(3)
         CALL GSVOLU('DCO0','TUBE',MEDCOP,PAR,3,IVOL)
      CALL GSATT('DCO0','SEEN',0) 
         CALL GSPOS('DCO0',1,'DCFO',0.,0.,0.,0,'ONLY')
c      CALL GSATT('DCFO','SEEN',0)
         CALL GSVOLU('DCFS','TUBE',MEDG10,PARFOS,3,IVOL)

C
C    D-CHAMBER - ENDCAPS

         CALL GSVOLU('DCFL','TUBE',MEDG10,PARFL,3,IVOL)
c      CALL GSATT('DCFL','SEEN',0)
         PAR(1)=PARFL(1)
         PAR(2)=PARFL(2)
         PAR(3)=0.1
         CALL GSVOLU('DCFN','TUBE',MEDG10,PAR,3,IVOL)

         PAR(4)=0.
         PAR(5)=60.
         CALL GSVOLU('DCFV','TUBS',MEDG10,PAR,5,IVOL)

         PAR(3)=0.001
         CALL GSVOLU('DCFC','TUBE',MEDCOP,PAR,3,IVOL)
         CALL GSPOS('DCFC',1,'DCFN',0.,0.,-0.099,0,'ONLY')
C
      CALL GSROTM( NROTF, 90., 0., 90., 270., 180., 0.)

      CALL GSPOS('DCFI',1,'KDC ',0.,0.,0.,0,'ONLY')
      CALL GSPOS('DCFO',1,'KDC ',0.,0.,0.,0,'ONLY')
      CALL GSPOS('DCFS',1,'KDC ',0.,0., (48.5+PARFOS(3)),0,'ONLY')
      CALL GSPOS('DCFS',2,'KDC ',0.,0.,-(48.5+PARFOS(3)),0,'ONLY')
      CALL GSPOS('DCFL',1,'KDC ',0.,0., 49.5,0,'ONLY')
      CALL GSPOS('DCFL',2,'KDC ',0.,0.,-49.5,0,'ONLY')
      CALL GSPOS('DCFN',1,'KDC ',0.,0.,-51. , 0,'ONLY') 
      CALL GSPOS('DCFN',2,'KDC ',0.,0.,-54.9, 0,'ONLY')
      CALL GSPOS('DCFN',3,'KDC ',0.,0., 54.9,NROTF,'ONLY')

      NROTV1=NROTF+1
      CALL GSROTM( NROTV1, 90., 120., 90., 210., 0., 0.)
      NROTV2=NROTV1+1
      CALL GSROTM( NROTV2, 90., 240., 90., 330., 0., 0.)
      CALL GSPOS('DCFV',1,'KDC ',0.,0., 53.9, 0,'ONLY')
      CALL GSPOS('DCFV',2,'KDC ',0.,0., 53.9, NROTV1,'ONLY')
      CALL GSPOS('DCFV',3,'KDC ',0.,0., 53.9, NROTV2,'ONLY')

      CALL GSPOS( 'KDC ',1,'KEDR' , 0.   , 0.   , 0.   , 0,'ONLY')
C
      RETURN
C
1001  CONTINUE
      PRINT *,'ERROR reading file CONNECTIONS_SIM.DC'
      RETURN
1002  CONTINUE
      PRINT *,'ERROR reading file GEOM.DC'
      STOP
      END
