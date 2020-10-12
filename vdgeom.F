*CMZU:          13/05/97  15.54.58  by  YURI SKOVPEN
*CMZ :  1.00/04 20/02/97  11.03.42  by  TANYA PURLATZ
*CMZ :  1.00/01 27/02/96  10.33.12  by  TANYA PURLATZ
c       16/06/2003 by A. Poluektov - Added VD preamplifiers material
*-- Author :    TANYA PURLATZ   25/01/93
         SUBROUTINE VDGEOM
C    VACUUM CHAMBER AND VERTEX DETECTOR
#include "gconst.inc"
#include "gcflag.inc"
C
	include 'medpar.inc'
C
	include 'vd_map.inc'
C
	include 'vd_par.inc'
C
c      include 'kdpath.inc'
      character *80 file

      DIMENSION PAR(11),CRDMP(20,2)
      DIMENSION NLT(3),RL(2,3)
c      DATA NLT/40,52,64/
      DATA NLT/80,184,312/
      DATA RL/6.71,7.6332,8.7357,9.6631,10.7528,11.6835/
C
C Read KLUKVA SLOTS
c      file=kdpath(:index(kdpath,' ')-1)//'vdslots.tbl'
      file='vdslots.tbl'
      OPEN(UNIT=30,FILE=file, STATUS='OLD',ERR=741)
      GOTO 742
741   file=KEDR_RELEASE//'KedrSim/vdslots_sim.tbl'
      OPEN(UNIT=30,FILE=file, STATUS='OLD',ERR=1001)
742   READ(30,*)
      READ(30,*)(CRDMP(I,1),I=1,10)
      READ(30,*)(CRDMP(I,2),I=1,10)
      READ(30,*)
      READ(30,*)(CRDMP(I,1),I=11,20)
      READ(30,*)(CRDMP(I,2),I=11,20)
      CLOSE(30)
C
c -- Read tube resolutions and efficiencies ---
c      file=kdpath(:index(kdpath,' ')-1)//'vdtubes.tbl'
      file='vdtubes.tbl'
      open(unit=30, file=file,status='OLD',err=743)
      goto 744
743   file=KEDR_RELEASE//'KedrSim/vdtubes.tbl'
      open(unit=30, file=file,status='OLD',err=1003)
744   do iLine=1, 312
        read(30,*) iTube, rRes, rEff
	rVDRes(iTube)=rRes
	rVDEff(iTube)=rEff
      enddo
      close(30)
c ------- Read radius dependencies --------
c      file=kdpath(:index(kdpath,' ')-1)//'vdpar.tbl'
      file='vdpar.tbl'
      open(unit=30, file=file,status='OLD',err=745)
      goto 746
745   file=KEDR_RELEASE//'KedrSim/vdpar.tbl'
      open(unit=30, file=file,status='OLD',err=1004)
746   read(30,*) (rVDRTC(i),i=1,5)
      read(30,*) (rVDRC(i), i=1,4)
      read(30,*) (rVDEC(i), i=1,4)
      read(30,*) rDummy, rT0, rDigNS
      
      if(idebug.eq.1) write (*,*) 'DT=',rDigNS,' T0=',rT0
c -----------------------------------------

c      file=kdpath(:index(kdpath,' ')-1)//'vdtabl.tbl'
      file='vdtabl.tbl'
      OPEN(UNIT=30,FILE=file,STATUS='OLD',ERR=747)
      goto 748
747   file=KEDR_RELEASE//'KedrSim/vdtabl_sim.tbl'
      OPEN(UNIT=30,FILE=file,STATUS='OLD',ERR=1002)
748   NCHT=0
      DO I=1,320
        NCHTON(I)=-1
      ENDDO
      DO NUFO=1,20
        READ(30,*) IPLT
        DO ICH=1,16
          NCHT=NCHT+1
          READ(30,*) JCH, JTB, JKEY
C          IF(JKEY.GT.0) THEN
c            IADRCH(NCHT)="100000+
c     +                 (CRDMP(IPLT,1)-1)*512+CRDMP(IPLT,2)*16+JCH-1
C DAQ format
            IADRCH(NCHT)=(IPLT-1)*16+JCH-1
            NCHTON(NCHT)=JTB
C          ENDIF
        ENDDO
      ENDDO
      CLOSE(30)
C
         PAR(1)=0.
         PAR(2)=0.502
C         PAR(3)=37.
         PAR(3)=1.
         CALL GSVOLU('STRF','TUBE',MEDVDG,PAR,3,IVOL)
         PAR(3)=35.
         CALL GSVOLU('STRM','TUBE',MEDMYL,PAR,3,IVOL)
c         CALL GSATT ('STRM','SEEN',0)
          PAR(2)=0.500
         CALL GSVOLU('STRD','TUBE',MEDVDG,PAR,3,IVOL)
         CALL GSATT ('STRD','SEEN',0)

         PAR(1)=6.0
         PAR(2)=(RL(2,1)+RL(1,2))/2.
         CALL GSVOLU('VRD1','TUBE',MEDAIF,PAR,3,IVOL)
         CALL GSATT ('VRD1','SEEN',0)

         PAR(1)=PAR(2)
         PAR(2)=(RL(2,2)+RL(1,3))/2.
         CALL GSVOLU('VRD2','TUBE',MEDAIF,PAR,3,IVOL)
         CALL GSATT ('VRD2','SEEN',0)

         PAR(1)=PAR(2)
         PAR(2)=12.3
         CALL GSVOLU('VRD3','TUBE',MEDAIF,PAR,3,IVOL)
         CALL GSATT ('VRD3','SEEN',0)

         PAR(1)=6.0
         PAR(2)=12.5
         PAR(3)=59. ! + DC support
         CALL GSVOLU('VRDT','TUBE',MEDAIF,PAR,3,IVOL)
         CALL GSATT ('VRDT','SEEN',0)
C
C Flangz
         PAR(1)=6.0
         PAR(2)=12.3
         PAR(3)=1.4
         CALL GSVOLU('FLNG','TUBE',MEDALF,PAR,3,IVOL)

         PAR(1)=6.0
         PAR(2)=(RL(2,1)+RL(1,2))/2.
         CALL GSVOLU('FLN1','TUBE',MEDALF,PAR,3,IVOL)
         CALL GSATT ('FLN1','SEEN',0)

         PAR(1)=PAR(2)
         PAR(2)=(RL(2,2)+RL(1,3))/2.
         CALL GSVOLU('FLN2','TUBE',MEDALF,PAR,3,IVOL)
         CALL GSATT ('FLN2','SEEN',0)

         PAR(1)=PAR(2)
         PAR(2)=12.3
         CALL GSVOLU('FLN3','TUBE',MEDALF,PAR,3,IVOL)
         CALL GSATT ('FLN3','SEEN',0)

C Electronics parts

         PAR(1)=6.0
         PAR(2)=14.
         PAR(3)=6.
         CALL GSVOLU('ELVD','TUBE',MEDALF,PAR,3,IVOL)

         PAR(1)=6.7
         PAR(2)=13.3
         PAR(3)=5.75
         CALL GSVOLU('ELVA','TUBE',MEDAIF,PAR,3,IVOL)

         PAR(1)=3.3
         PAR(2)=0.25
         PAR(3)=5.75
         CALL GSVOLU('ELVB','BOX ',MEDALF,PAR,3,IVOL)

         PAR(2)=0.1
         CALL GSVOLU('ELVP','BOX ',MEDG10,PAR,3,IVOL)

	 par(1)=3.1                                   ! A.P.
	 par(2)=0.7                                   ! A.P.
	 call gsvolu('ELVE','BOX ',medele,par,3,ivol) ! A.P.

C  POSITIONING VERTEX DETECTOR
C tubes

      CALL GSPOS( 'STRD' ,1, 'STRM' , 0.,0.,0.,0,'ONLY')
C
      CALL VDCOOR
      DO 10 J=1,NLT(1)
        X=-XP(J)
        XP(J)=X
        Y=YP(J)
        CALL GSPOS( 'STRM' ,J, 'VRD1' , X,Y,0.,0,'ONLY')
        CALL GSPOS( 'STRF' ,J, 'FLN1' , X,Y,-0.4,0,'ONLY')
  10  CONTINUE

      DO 20 J=NLT(1)+1,NLT(2)
        X=-XP(J)
        XP(J)=X
        Y=YP(J)
        CALL GSPOS( 'STRM' ,J, 'VRD2' , X,Y,0.,0,'ONLY')
        CALL GSPOS( 'STRF' ,J, 'FLN2' , X,Y,-0.4,0,'ONLY')
  20  CONTINUE

      DO 30 J=NLT(2)+1,NTUB 
        X=-XP(J)
        XP(J)=X
        Y=YP(J)
        CALL GSPOS( 'STRM' ,J, 'VRD3' , X,Y,0.,0,'ONLY') 
        CALL GSPOS( 'STRF' ,J, 'FLN3' , X,Y,-0.4,0,'ONLY')
  30  CONTINUE

c      DO 10 J=1,NTUB
c        X=-XP(J)
c        XP(J)=X
c        Y=YP(J)
c        CALL GSPOS( 'STRM' ,J, 'VRDT' , X,Y,0.,0,'ONLY')
c  10  CONTINUE


      CALL FREE_ROTMAT_NUMBER(NROT0)
      CALL GSROTM(NROT0,90.,0.,90.,270.,180.,0.)

      CALL GSPOS( 'VRDT' ,1, 'KEDR' , 0.,0.,0.,0,'ONLY')
      CALL GSPOS( 'VRD1' ,1, 'VRDT' , 0.,0.,0.,0,'ONLY')    
      CALL GSPOS( 'VRD2' ,1, 'VRDT' , 0.,0.,0.,0,'ONLY')    
      CALL GSPOS( 'VRD3' ,1, 'VRDT' , 0.,0.,0.,0,'ONLY')
c      CALL GSPOS( 'FLNG' ,1, 'VRDT' , 0.,0., 36.4,0,'MANY')
c      CALL GSPOS( 'FLNG' ,2, 'VRDT' , 0.,0.,-36.4,0,'MANY')
      CALL GSPOS( 'FLNG' ,1, 'VRDT' , 0.,0., 36.4,0,'ONLY')
      CALL GSPOS( 'FLNG' ,2, 'VRDT' , 0.,0.,-36.4,NROT0,'ONLY')
      CALL GSPOS( 'FLN1' ,1, 'FLNG' , 0.,0.,0.,0,'ONLY')
      CALL GSPOS( 'FLN2' ,1, 'FLNG' , 0.,0.,0.,0,'ONLY')
      CALL GSPOS( 'FLN3' ,1, 'FLNG' , 0.,0.,0.,0,'ONLY')
C Put electronic's plates 

C      CALL GSPOS( 'ELVD' ,1, 'VRDT' , 0.,0.,65.5,0,'ONLY')
C      CALL GSPOS( 'ELVD' ,2, 'VRDT' , 0.,0.,-65.5,0,'ONLY')
      CALL GSPOS( 'ELVD' ,1, 'KEDR' , 0.,0., 65.5,0,'ONLY')
      CALL GSPOS( 'ELVD' ,2, 'KEDR' , 0.,0.,-65.5,NROT0,'ONLY')
      CALL GSPOS( 'ELVA' ,1, 'ELVD' , 0.,0., 0.05,0,'ONLY')
      CALL GSPOS( 'ELVB' ,1, 'ELVA' , 10.,0.,0.,0,'ONLY')
      CALL GSPOS( 'ELVB' ,2, 'ELVA' ,-10.,0.,0.,0,'ONLY')

      DFPL=17.

      FPL=90.-4*DFPL
      NROTP=NROT0+1
      do i=1,10
        CALL GSROTM(NROTP,90.,FPL,90.,90.+FPL,0.,0.)

        x = 10.*COS(FPL*DEGRAD)
        y = 10.*SIN(FPL*DEGRAD)
        CALL GSPOS( 'ELVP' ,i*2-1, 'ELVA' , x, y, 0., NROTP,'ONLY')  
        CALL GSPOS( 'ELVP' ,i*2  , 'ELVA' ,-x,-y, 0., NROTP,'ONLY')

	x = x + 0.8*sin(fpl*degrad)                                  ! A.P.
	y = y - 0.8*cos(fpl*degrad)                                  ! A.P.
        CALL GSPOS( 'ELVE' ,i*2-1, 'ELVA' , x, y, 0., NROTP,'ONLY')  ! A.P.
        CALL GSPOS( 'ELVE' ,i*2  , 'ELVA' ,-x,-y, 0., NROTP,'ONLY')  ! A.P.

        FPL=FPL+DFPL
	NROTP=NROTP+1
      enddo  
C
C DC support (Zanga)
	 PAR(1)=6.
         PAR(2)=12.45
         PAR(3)=17.8/2.
         CALL GSVOLU('DCZ ','TUBE',MEDAIF,PAR,3,IVOL)

         RZ=41.+17.6/2.
         CALL GSPOS( 'DCZ ' ,1, 'VRDT' , 0.,0., RZ,0,'ONLY')         
         CALL GSPOS( 'DCZ ' ,2, 'VRDT' , 0.,0.,-RZ,NROT0,'ONLY')

         PAR(1)=6.
         PAR(2)=6.5
         PAR(3)=1.
         CALL GSVOLU('DCZ1','TUBE',MEDLAT,PAR,3,IVOL)

         RZ=-8.8+PAR(3)
         CALL GSPOS( 'DCZ1' ,1, 'DCZ ' , 0.,0., RZ,0,'ONLY')

         PAR(1)=11.9
         PAR(2)=12.2
         PAR(3)=3.4
         CALL GSVOLU('DCZ2','TUBE',MEDLAT,PAR,3,IVOL)

         RZ=-8.8+0.8+PAR(3) 
         CALL GSPOS( 'DCZ2' ,1, 'DCZ ' , 0.,0., RZ,0,'ONLY')

         PAR(1)=11.2
         PAR(2)=12.4
         PAR(3)=1.2
         CALL GSVOLU('DCZ3','TUBE',MEDLAT,PAR,3,IVOL)

         RZ=0.
         CALL GSPOS( 'DCZ3' ,1, 'DCZ ' , 0.,0., RZ,0,'ONLY')

         PAR(1)=11.6
         PAR(2)=12.1  
         PAR(3)=1.9
         CALL GSVOLU('DCZ4','TUBE',MEDLAT,PAR,3,IVOL)

         RZ=1.2+PAR(3)
         CALL GSPOS( 'DCZ4' ,1, 'DCZ ' , 0.,0., RZ,0,'ONLY')

         PAR(1)=1.7
         PAR(2)=7.45
         PAR(3)=7.5
         PAR(4)=6.5
         PAR(5)=7.5
         CALL GSVOLU('DCZ5','CONE',MEDLAT,PAR,5,IVOL)

         RZ=3.8+PAR(1)
         CALL GSPOS( 'DCZ5' ,1, 'DCZ ' , 0.,0., RZ,0,'ONLY')

C Spicy 1
         PAR(1)=2.7
         PAR(2)=1.
         PAR(3)=1.
         CALL GSVOLU('DCZ6','BOX ',MEDLAT,PAR,3,IVOL)

         RZ=-8.8+PAR(3)
	 RX=6.5+PAR(1)
         CALL GSPOS( 'DCZ6' ,1, 'DCZ ' , RX, 0., RZ,0,'ONLY')
         CALL GSPOS( 'DCZ6' ,2, 'DCZ ' ,-RX, 0., RZ,0,'ONLY')

         PAR(1)=2.7
         PAR(2)=1.
         PAR(3)=1.
         CALL GSVOLU('DCZ7','BOX ',MEDAIF,PAR,3,IVOL)

         RX=RX/2.
         RY=RX*SQRT(3.)
         NROTZ1=NROTP
         CALL GSROTM(NROTZ1,90.,60.,90.,90.+60.,0.,0.)
         CALL GSPOS( 'DCZ7' ,1, 'DCZ ' , RX, RY, RZ, NROTZ1,'ONLY')
         NROTZ2=NROTZ1+1
         CALL GSROTM(NROTZ2,90.,120.,90.,90.+120.,0.,0.)
         CALL GSPOS( 'DCZ7' ,2, 'DCZ ' ,-RX, RY, RZ, NROTZ2,'ONLY')
         NROTZ3=NROTZ2+1
         CALL GSROTM(NROTZ3,90.,240.,90.,90.+240.,0.,0.)
         CALL GSPOS( 'DCZ7' ,3, 'DCZ ' ,-RX,-RY, RZ, NROTZ3,'ONLY')
         NROTZ4=NROTZ3+1
         CALL GSROTM(NROTZ4,90.,300.,90.,30.,0.,0.)
         CALL GSPOS( 'DCZ7' ,4, 'DCZ ' , RX,-RY, RZ, NROTZ4,'ONLY')

         PAR(1)=1.4         
         PAR(2)=1.          
         PAR(3)=1.
         CALL GSVOLU('DCY1','BOX ',MEDLAT,PAR,3,IVOL)

         PAR(1)=1.3
         PAR(2)=0.5               
         PAR(3)=1.
         CALL GSVOLU('DCY2','BOX ',MEDLAT,PAR,3,IVOL)

         CALL GSPOS('DCY1', 1,'DCZ7', 1.3, 0.,0.,0,'ONLY')   
         CALL GSPOS('DCY2', 1,'DCZ7',-1.4, 0.,0.,0,'ONLY')

C Spicy 2
         PAR(1)=2.05 
         PAR(2)=1.  
         PAR(3)=0.8 
         CALL GSVOLU('DCZ8','BOX ',MEDLAT,PAR,3,IVOL)

         RZ=3.8+PAR(3)
	 RX=7.5+PAR(1)
         CALL GSPOS( 'DCZ8' ,1, 'DCZ ' , RX, 0., RZ,0,'ONLY')
         CALL GSPOS( 'DCZ8' ,2, 'DCZ ' ,-RX, 0., RZ,0,'ONLY')

         PAR(1)=2.05
         PAR(2)=1.
         PAR(3)=0.8
         CALL GSVOLU('DCZ9','BOX ',MEDAIF,PAR,3,IVOL)

         RX=RX/2.
         RY=RX*SQRT(3.)
         CALL GSPOS( 'DCZ9' ,1, 'DCZ ' , RX, RY, RZ, NROTZ1,'ONLY')
         CALL GSPOS( 'DCZ9' ,2, 'DCZ ' ,-RX, RY, RZ, NROTZ2,'ONLY')
         CALL GSPOS( 'DCZ9' ,3, 'DCZ ' ,-RX,-RY, RZ, NROTZ3,'ONLY')
         CALL GSPOS( 'DCZ9' ,4, 'DCZ ' , RX,-RY, RZ, NROTZ4,'ONLY')

         PAR(1)=1.3
         PAR(2)=1.
         PAR(3)=1.
         CALL GSVOLU('DCY3','BOX ',MEDLAT,PAR,3,IVOL)

         PAR(1)=0.75
         PAR(2)=0.5
         PAR(3)=1.
         CALL GSVOLU('DCY4','BOX ',MEDLAT,PAR,3,IVOL)

         CALL GSPOS('DCY3', 1,'DCZ9', 0.75, 0.,0.,0,'ONLY')   
         CALL GSPOS('DCY4', 1,'DCZ9',-1.3, 0.,0.,0,'ONLY') 

C Bolts styazhki
         PAR(1)=0.
         PAR(2)=0.5
         PAR(3)=5.3
         CALL GSVOLU('DCZB','TUBE',MEDLAT,PAR,3,IVOL)

         RZ=-6.8+PAR(3)
	 RY=5.*SQRT(3.)
         RX=5.
         CALL GSPOS( 'DCZB' ,1, 'DCZ ' , RX, RY, RZ,0,'ONLY')
         CALL GSPOS( 'DCZB' ,2, 'DCZ ' ,-RX, RY, RZ,0,'ONLY')
         CALL GSPOS( 'DCZB' ,3, 'DCZ ' , RX,-RY, RZ,0,'ONLY')
         CALL GSPOS( 'DCZB' ,4, 'DCZ ' ,-RX,-RY, RZ,0,'ONLY')
C Vystupy for Vacuum chamber connection on outer part
         PAR(1)=0.5
         PAR(2)=1.3
         PAR(3)=0.8
         CALL GSVOLU('DCZV','BOX ',MEDLAT,PAR,3,IVOL)

         RZ=7.2+PAR(3)
         RY=7./SQRT(2.)
         RX=RY
         NROTZ5=NROTZ4+1
         CALL GSROTM(NROTZ5,90.,45.,90.,135.,0.,0.)
         NROTZ6=NROTZ5+1
         CALL GSROTM(NROTZ6,90.,135.,90.,225.,0.,0.)
         CALL GSPOS( 'DCZV' ,1, 'DCZ ' , RX, RY, RZ,NROTZ5,'ONLY')
         CALL GSPOS( 'DCZV' ,2, 'DCZ ' ,-RX, RY, RZ,NROTZ6,'ONLY')
         CALL GSPOS( 'DCZV' ,3, 'DCZ ' ,-RX,-RY, RZ,NROTZ5,'ONLY')
         CALL GSPOS( 'DCZV' ,4, 'DCZ ' , RX,-RY, RZ,NROTZ6,'ONLY')

C
C NEW VACUUM CHAMBER (Yurchenko) . Placed in main mother volume now (V.K.)
C
C         CALL VACCHAM('VRDT',72.)
C
      RETURN
1001  CONTINUE
      PRINT *,'ERROR reading file VDSLOTS_SIM.TBL'
      RETURN
1002  CONTINUE
      PRINT *,'ERROR reading file VDTABL_SIM.TBL'
      RETURN
1003  continue
      print *,'ERROR reading file VDTUBES.TBL'
      return
1004  continue
      print *,'ERROR reading file VDPAR.TBL'
      return
      END
