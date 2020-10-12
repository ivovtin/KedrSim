      SUBROUTINE irgeom
c     IMPLICIT NONE

c      INTEGER MEDG10,MEDVMF,MEDMYL,MEDYOK,MEDALF,MEDSCTU,MEDVDG,
c     +  MEDPLA,MEDLKR,MEDDME,MJCDME,MEDALU,MEDCOP,MEDTUN,MEDAIR,
c     +  MEDVAC,MEDLEAD,MEDCSI,MEDMUG,MEDBER,MEDIRV,MEDYOK
      include 'medpar.inc'
      include 'kedrif.inc'
#include "gconst.inc"
      REAL PAR(10),NROT(8)
      INTEGER IVOL

		real pcon(21),pcon1(21)
c		data pcon/0.,360.,6., 155.,13.5,142.,  160.,13.5,147.,  178.5,13.5,147.,
c     +          178.5,21.,147.,  203.,60.,147.,  260.,112.5,147./
		data pcon/22.5,360.,6., 155.,13.5,142.,  160.,13.5,147.,  178.5,13.5,147.,
     +          178.5,21.,147.,  203.,60.,147.,  260.,112.5,147./
		data pcon1/22.5,360.,6., -260.,112.5,147., -203.,60.,147., -178.5,21.,147.,
     +           -178.5,13.5,147., -160.,13.5,147., -155.,13.5,142./

		if(mod(ifir,10).eq.1) then
c OLD description with "PGON" **********************************
C	1ST LAYER
			PAR(1) = 22.5
			PAR(2) = 360.
			PAR(3) = 8.
			PAR(4) = 2.
			PAR(5) = -260.
			PAR(6) = 180.
			PAR(7) = 203.+0.105
			PAR(8) = 260.
			PAR(9) = PAR(6)
			PAR(10) = PAR(7)
C			CALL GSVOLU('YOK1','PGON',MEDYOK,PAR,10,IVOL)
C			CALL GSPOS('YOK1',1,'KEDR',0.,0.,0.,0,'ONLY')
C			call gsatt('YOK1','COLO',6)
                  
C	2ND LAYER
			PAR(6) = 213.-0.105
			PAR(7) = 236.+0.105
			PAR(9) = PAR(6)
			PAR(10) = PAR(7)
C			CALL GSVOLU('YOK2','PGON',MEDYOK,PAR,10,IVOL)
C			CALL GSPOS('YOK2',1,'KEDR',0.,0.,0.,0,'ONLY')
C			call gsatt('YOK2','COLO',6)
C	3RD LAYER
			PAR(6) = 246.-0.105
			PAR(7) = 269.+0.105
			PAR(9) = PAR(6)
			PAR(10) = PAR(7)
C			CALL GSVOLU('YOK3','PGON',MEDYOK,PAR,10,IVOL)
C			CALL GSPOS('YOK3',1,'KEDR',0.,0.,0.,0,'ONLY')
C			call gsatt('YOK3','COLO',6)
C	4TH LAYER
			PAR(6) = 279.-0.105
			PAR(7) = 299.+0.105
			PAR(9) = PAR(6)
			PAR(10) = PAR(7)
C			CALL GSVOLU('YOK4','PGON',MEDYOK,PAR,10,IVOL)
C			CALL GSPOS('YOK4',1,'KEDR',0.,0.,0.,0,'ONLY')
C			call gsatt('YOK4','COLO',6)
c OLD description with "PGON" **********************************

c NEW description with "TRD1" **********************************
	DO I=1,8
	 	CALL FREE_ROTMAT_NUMBER(NROT(I))
		CALL GSROTM(NROT(I),90.,45.*I-90.,180.,0.,90.,45.*I)
	ENDDO
C	1ST LAYER
	Y1=180.
	Y2=203.+0.105
      PAR(1) = Y1*TAN(22.5*DEGRAD)
      PAR(2) = Y2*TAN(22.5*DEGRAD)
      PAR(3) = 260.
      PAR(4) = (Y2-Y1)/2.
	CALL GSVOLU('YOK1','TRD1',MEDYOK,par,4,ivol)
	CALL GSATT('YOK1','COLO',6)
	DO I=1,8
		X=(Y1+Y2)/2.*COS(45.*I*DEGRAD)
		Y=(Y1+Y2)/2.*SIN(45.*I*DEGRAD)
		CALL GSPOS('YOK1',I,'KEDR',X,Y,0.,NROT(I),'ONLY')
	ENDDO
C	2ND LAYER
	Y1=213.-0.105
	Y2=236.+0.105
      PAR(1) = Y1*TAN(22.5*DEGRAD)
      PAR(2) = Y2*TAN(22.5*DEGRAD)
      PAR(4) = (Y2-Y1)/2.
	CALL GSVOLU('YOK2','TRD1',MEDYOK,par,4,ivol)
	CALL GSATT('YOK2','COLO',6)
	DO I=1,8
		X=(Y1+Y2)/2.*COS(45.*I*DEGRAD)
		Y=(Y1+Y2)/2.*SIN(45.*I*DEGRAD)
		CALL GSPOS('YOK2',I,'KEDR',X,Y,0.,NROT(I),'ONLY')
	ENDDO
C	3RD LAYER
	Y1=246.-0.105
	Y2=269.+0.105
      PAR(1) = Y1*TAN(22.5*DEGRAD)
      PAR(2) = Y2*TAN(22.5*DEGRAD)
      PAR(4) = (Y2-Y1)/2.
	CALL GSVOLU('YOK3','TRD1',MEDYOK,par,4,ivol)
	CALL GSATT('YOK3','COLO',6)
	DO I=1,8
		X=(Y1+Y2)/2.*COS(45.*I*DEGRAD)
		Y=(Y1+Y2)/2.*SIN(45.*I*DEGRAD)
		CALL GSPOS('YOK3',I,'KEDR',X,Y,0.,NROT(I),'ONLY')
	ENDDO
C	4TH LAYER
	Y1=279.-0.105
	Y2=299.+0.105
      PAR(1) = Y1*TAN(22.5*DEGRAD)
      PAR(2) = Y2*TAN(22.5*DEGRAD)
      PAR(4) = (Y2-Y1)/2.
	CALL GSVOLU('YOK4','TRD1',MEDYOK,par,4,ivol)
	CALL GSATT('YOK4','COLO',6)
	DO I=1,8
		X=(Y1+Y2)/2.*COS(45.*I*DEGRAD)
		Y=(Y1+Y2)/2.*SIN(45.*I*DEGRAD)
		CALL GSPOS('YOK4',I,'KEDR',X,Y,0.,NROT(I),'ONLY')
	ENDDO
c NEW description with "TRD1" **********************************
		endif
		if(mod(ifir,100)/10.eq.1) then
C	катушка
			par(1)=156.
			par(2)=178.5
			par(3)=158.
			call gsvolu('MAGN','TUBE',MEDSTE,par,3,ivol)
			call gspos('MAGN',1,'KEDR',0.,0.,0.,0,'ONLY')
c		вакуум
			par(1)=156.6
			par(2)=177.9
			par(3)=152.
			call gsvolu('MAG1','TUBE',MEDVAC,par,3,ivol)
			call gspos('MAG1',1,'MAGN',0.,0.,0.,0,'ONLY')
c		обмотка
			par(1)=162
			par(2)=162.4
			par(3)=142
			call gsvolu('MAG2','TUBE',MEDCOF,par,3,ivol)	! no field
c			call gsvolu('MAG2','TUBE',MEDCOP,par,3,ivol)
			call gspos('MAG2',1,'MAG1',0.,0.,0.,0,'ONLY')
c		то, куда гелий наливают
			par(1)=162.4
			par(2)=165
			call gsvolu('MAG3','TUBE',MEDCOF,par,3,ivol)	! no field
c			call gsvolu('MAG3','TUBE',MEDCOP,par,3,ivol)
			call gspos('MAG3',1,'MAG1',0.,0.,0.,0,'ONLY')
C		жидкий гелий
			par(1)=163.4
			par(2)=164.6
			par(3)=70.6
			call gsvolu('MAG4','TUBE',MEDVAC,par,3,ivol)
			call gspos('MAG4',1,'MAG3',0.,0.,70.8,0,'ONLY')
			call gspos('MAG4',2,'MAG3',0.,0.,-70.8,0,'ONLY')
			call gsatt('MAG4','COLO',4)
		endif
		if(mod(ifir,1000)/100.eq.1) then
C торцы
			call gsvolu('YOK6','PCON',MEDYOK,pcon,21,ivol)
			par(1)=22.5
			par(2)=360.
			par(3)=8.
			par(4)=2.
			par(5)=160.
			par(6)=135.8
			par(7)=180.
			par(8)=260.
			par(9)=par(6)
			par(10)=par(7)
c			call gsvolu('YOK5','PGON',MEDYOK,par,10,ivol)
c NEW description with "TRD1" **********************************
			Y1=par(6)
			Y2=par(7)
		      PAR(1) = Y1*TAN(22.5*DEGRAD)
		      PAR(2) = Y2*TAN(22.5*DEGRAD)
		      PAR(3) = (par(8)-par(5))/2.
		      PAR(4) = (Y2-Y1)/2.
			CALL GSVOLU('YOK5','TRD1',MEDYOK,par,4,ivol)
			DO I=1,8
				X=(Y1+Y2)/2.*COS(45.*I*DEGRAD)
				Y=(Y1+Y2)/2.*SIN(45.*I*DEGRAD)
				CALL GSPOS('YOK5',I,'KEDR',X,Y,(par(8)+PAR(5))/2.,NROT(I),'MANY')
			ENDDO
c NEW description with "TRD1" **********************************
			call gspos('YOK6',1,'KEDR',0.,0.,0.,0,'MANY')
c			call gspos('YOK5',1,'KEDR',0.,0.,0.,0,'MANY')
			par(5)=-par(5)
			par(8)=-par(8)
			pcon(4)=-pcon(4)
			pcon(7)=-pcon(7)
			pcon(10)=-pcon(10)
			pcon(13)=-pcon(13)
			pcon(16)=-pcon(16)
			pcon(19)=-pcon(19)
c NEW description with "TRD1" **********************************
			CALL GSVOLU('YOK7','TRD1',MEDYOK,par,4,ivol)
			DO I=1,8
				X=(Y1+Y2)/2.*COS(45.*I*DEGRAD)
				Y=(Y1+Y2)/2.*SIN(45.*I*DEGRAD)
				CALL GSPOS('YOK7',I,'KEDR',X,Y,(par(8)+PAR(5))/2.,NROT(I),'MANY')
			ENDDO
c NEW description with "TRD1" **********************************
			call gsvolu('YOK8','PCON',MEDYOK,pcon1,21,ivol)
c			call gsvolu('YOK7','PGON',MEDYOK,par,10,ivol)
			call gspos('YOK8',1,'KEDR',0.,0.,0.,0,'MANY')
c			call gspos('YOK7',1,'KEDR',0.,0.,0.,1,'MANY')
			call gsatt('YOK5','COLO',6)
			call gsatt('YOK6','COLO',6)
			call gsatt('YOK7','COLO',6)
			call gsatt('YOK8','COLO',6)
			pcon(3)=3
			pcon(4)=155.
			pcon(5)=60.
			pcon(6)=79.5
			pcon(7)=193.7
			pcon(8)=91.
			pcon(9)=111.4
			pcon(10)=208.3
			pcon(11)=103.5
			pcon(12)=pcon(11)
			call gsvolu('YOK9','PCON',MEDAIR,pcon,12,ivol)
			call gspos('YOK9',1,'YOK6',0.,0.,0.,0,'ONLY')
			pcon1(3)=3
			pcon1(4)=-208.3
			pcon1(5)=103.5
			pcon1(6)=pcon1(5)
			pcon1(7)=-193.7
			pcon1(8)=91.
			pcon1(9)=111.4
			pcon1(10)=-155.
			pcon1(11)=60.
			pcon1(12)=79.5
c			pcon1(4)=-pcon(4)
c			pcon(7)=-pcon(7)
c			pcon(10)=-pcon(10)
			call gsvolu('YOK0','PCON',MEDAIR,pcon1,12,ivol)
			call gspos('YOK0',1,'YOK8',0.,0.,0.,0,'ONLY')
		endif
		return
	
      end
