      SUBROUTINE UGEOM
c---------------------------------------------------------------
c
c  Author: T.Purlatz
c
c  Changed 02-Apr-1999, V.Tayursky
c          06-Oct-1999, V.Tayursky
c          31-May-2002, pictures in batch mode. V.Tayursky
c          25-May-2004, renamed csgeom -> csi_geom
c
c---------------------------------------------------------------
      include 'kedrif.inc'
      include 'medpar.inc'
#include "gcflag.inc"
#include "gctrak.inc"
      include 'drawpar.inc'
c
      DIMENSION PAR(3)

c
c      -----------------------------------
c    <         DEFINE MOTHER VOLUME        >
c      -----------------------------------
c
      IF(IFTS.EQ.0) THEN
c
c   Mother volume without TS
c
*      PAR(1)=0.
*      PAR(2)=162. ! inside mag. field
*      PAR(3)=155.
      PAR(1)=0.
      PAR(2)=350.  ! with muon system
      PAR(3)=300.
c
      CALL GSVOLU('KEDR','TUBE',MEDAIR,PAR,3,IVOL)
c
      ELSE
c
c   Mother volume with TS
c
*      PAR(1)=250.
*      PAR(2)=162.  ! inside mag. field ?!
*      PAR(3)=2000.
c
*      CALL GSVOLU( 'KEDR' , 'BOX ' ,MEDAIF, PAR, 3 , IVOL )
c
      PAR(1)=0.
      PAR(2)=350.  ! with muon system
      PAR(3)=2000.
c
      CALL GSVOLU('KEDR','TUBE',MEDAIR,PAR,3,IVOL)
c
      END IF

c
c    DEFINE Air with Magnetic field
c
      PAR(1)=0.
      PAR(2)=326.8/2.  ! inside solenoid
      PAR(3)=294./2.
c
      CALL GSVOLU('KEDF','TUBE',MEDAIF,PAR,3,IVOL)
      CALL GSPOS('KEDF',1,'KEDR',0.,0.,0.,0,'MANY')
c
c
c    DEFINE DIFFERENT SYSTEMS
c
c  Vacuum chamber
c
      IF(IFVC.GE.1) CALL VACCHAM('KEDR',155.)
c
c  Vertex detector geometry
c
      IF(IFVD.GE.1) CALL VDGEOM
c
c  Drift chamber
c
      IF(IFDC.GE.1) CALL DCGEOM
c
c  Time of flight
c
      IF(IFTF.GE.1) CALL TOF_GEOM
c
c  Aerogel
c
      IF(IFCH.GE.1) CALL ATC_GEOM
c
c  Liquid Kripton calorimeter
c
      IF(IFKR.GE.1) CALL KRGEOM
c
c  CsI calorimeter
c
      IF(IFCS.GE.1) CALL CSI_GEOM
c
c  Muon system
c
      IF(IFMU.GE.1) CALL MUGEOM
c
c  Iron of magnet yoke and coil
c
      IF(IFIR.GE.1) CALL IRGEOM
c
c  Tagging system geometry
c
      IF(IFTS.GE.1) CALL TS_GEOM
c
c-------------------------------------------------
c
c  Vertex detector hits
c
      IF(IFVD.GE.2) CALL VDHIT
c
c  Drift chamber
c
      IF(IFDC.GE.2) CALL DCHIT
c
c  Time of flight
c
      IF(IFTF.GE.2) CALL TOF_HIT
c
c  Aerogel
c
c  26/10/05 : commented until correct hitization will ve done
c  15/12/2015 : Ovtin I.V,
        IF(IFCH.GE.2) CALL ATC_HITS
c
c  Liquid Kripton calorimeter
c
        IF(IFKR.GE.2) CALL LK_HITD
c
        IF(IFKR.GE.2) CALL LK_DIGD
c
c  CsI calorimeter
c
        IF(IFCS.GE.2) CALL CSI_HIT
c
c  Muon system
c
        IF(IFMU.GE.2) CALL MUHIT
c
c	CALL GSPOS('KEDR', 1, 'KDET',0.,0.,0.,0.,'ONLY')
c
c  Tagging system hits
c
      IF(IFTS.GE.2) CALL TS_HITS
c
c
c      CALL GSATT('KDET','SEEN',0)
c
      CALL GGCLOS ! close geometric bank

c
c        PRINT *,'GGCLOS DONE'

c     if(iswit(4).eq.2) then         ! old
      if(iax.ge.1.and.iax.le.3) then ! new
c
        CALL GSATT('KEDR','SEEN',1)
c
      endif


      END
