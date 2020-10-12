       SUBROUTINE TOF_GEOM
c***********************************************************************
c
c  TOF counters definition
c
c  Author: Tanya Purlatz   25/01/93
c
c  Corrections: 17.01.2001, 16.09.2003. V.A.Tayursky
c
c***********************************************************************
c
#include "gconst.inc"
      include 'medpar.inc'
c
      DIMENSION PAR(11)
c
c  /SC_DATA/ - constants for calculation, /SC_NAME/ names of ToF counters
c
      COMMON/SC_DATA/REFS,REFP,SS(2,2),APPA(2),A100,EPS,T0,RICH,RIKK
      COMMON/SC_NAME/ESP0,ESL0,BS00
      CHARACTER *4   ESP0,ESL0,BS00
c
      DATA ESP0,ESL0,BS00/'ESP0','ESL0','BS00'/   ! Counters
      DATA REFS/1.58/, REFP/1.49/       ! Scit., plex refraction 
      DATA APPA/150.,150./              ! Length of absorption
c
      SS(1,1)=51.7    !  Scint. length of end cap
      SS(2,1)=45.5    !  Plex.  length of end cap
      SS(1,2)=164.
      SS(2,2)=67.
c
c  Energy, GeV on 100 p.e. in PMT
c
c      A100=2.E-03*EXP(-0.5*SS(1,2)/APPA(1)) ! old
      A100=2.E-03*EXP(-0.5*SS(1,2)/APPA(1)-SS(2,2)/APPA(2)) ! V.A.T.
      EPS=0.05   ! threshold, fraction of A100
      T0=2.E-09  ! time of amplifier in ns
      RICH=5.    ! number of channels for 1 p.e.
      RIKK=25.   ! number of channels for 1.E-9 sec
c
c==========================================================
c                   Barrel counters
c==========================================================
c
c  Parameters of tube for barrel counters  
c
c     PAR(1)=69.5  !  old
      PAR(1)=70.1  !  Inner rad. of tube for counters. Corrected by V.A.T. 
      PAR(2)=72.85 !  Outer rad. of tube for counters
      PAR(3)=82.+68. 
c
      CALL GSVOLU('TOFB','TUBE',MEDAIF,PAR,3,IVOL) ! Tube for barrel count.
c
      CALL GSATT ('TOFB','SEEN',0) !
c
c  Parameters of barrel counter (trapezoid)
c
      ANGR=11.31
      PAR(1)=82. ! 1/2 length in z
      PAR(2)=0.
      PAR(3)=0.
      PAR(4)=.5  ! 1/2 thickness of counter
      PAR(5)=7.
      PAR(6)=7.1
      PAR(7)=ANGR
      PAR(8)=.5
      PAR(9)=7.
      PAR(10)=7.1
      PAR(11)=ANGR
      PH=90.
c
      CALL GSVOLU('BS00','TRAP',MEDPLA,PAR,11,IVOL) ! Counter
c
c  Sizes of barrel lighguide are the same as for counter except length
c
      PAR(1)=67./2.   !    1/2 length in z for barrel lightguide
c
      CALL GSVOLU('TFBP','TRAP',MEDPLX,PAR,11,IVOL) ! Lightguide
c
      DZ=82.+PAR(1)  ! shift of center of lightguide
c
      CALL FREE_ROTMAT_NUMBER(NROTB)
c
      NROTB=NROTB-1
c
c      RAD=71.9 ! old
      RAD=71.5 ! distance to center of counter,  corrected by V.A.T.
c
      DO I=1,32
c
        CALL GSROTM(NROTB+I,90.,PH-90.,90.,PH,0.,0.)
c
        DX=RAD*COS(DEGRAD*PH)
        DY=RAD*SIN(DEGRAD*PH)
c 
        CALL GSPOS('BS00',I,'TOFB',DX,DY,0.,NROTB+I,'ONLY') ! Bar. counter
c
        CALL GSPOS('TFBP',I,'TOFB',DX,DY,DZ,NROTB+I,'ONLY') ! Bar. lightguide
c
        CALL GSPOS('TFBP',32+I,'TOFB',DX,DY,-DZ,NROTB+I,'ONLY') ! Lightguide 
c
        PH=PH-11.25
      END DO
c
      NROTE=NROTB+32
c
c  Positioninig barrel ToF counters + lightguides in KEDR
c
      CALL GSPOS( 'TOFB' ,1, 'KEDR' , 0.,0.,0., 0,'ONLY') 
c
c=============================================================
c                  Endcap counters
c=============================================================
c
c   Parameters of tube for end-cap counters
c
      PAR(1)=15. ! inner radius for tube
      PAR(2)=69. ! outer radius for tube
      PAR(3)=1.  ! 1/2 thickness in z
c
      CALL GSVOLU('TOFL','TUBE',MEDAIF,PAR,3,IVOL) ! tube for left 
c                                                  ! counters  
      CALL GSATT ('TOFL','SEEN',0)
c
      CALL GSVOLU('TOFR','TUBE',MEDAIF,PAR,3,IVOL) ! tube for right
c                                                  ! counters
      CALL GSATT ('TOFR','SEEN',0)
c
      TALF=tan(DEGRAD*11.25/2.)
      R1=15.
      R2=66.7
      H1=15.*TALF-0.01
      H2=66.7*TALF-0.01
c
c  Parameters of end-cap counter
c
      PAR(1)=H1              ! X(Zmin)/2
      PAR(2)=H2              ! X(Zmax)/2
      PAR(3)=1.              ! Y/2
      PAR(4)=(R2-R1)/2.      ! Z/2
c
      CALL GSVOLU( 'ESL0', 'TRD1', MEDPLA,PAR,4,IVOL) ! Left End-cap counter
c
      CALL GSVOLU( 'ESP0', 'TRD1', MEDPLA,PAR,4,IVOL) ! Right end-cap counter
c
c  Parameters of edge of end-cap counter
c
      PAR(1)=H2
      PAR(2)=0.
      PAR(3)=0.
      PAR(4)=1.
      PAR(5)=1.
      PAR(6)=0.1
      PAR(7)=ATAN(0.5)*RADDEG  !  Y TO X IS +ANGLE
      PAR(8)=PAR(4)
      PAR(9)=PAR(5)
      PAR(10)=PAR(6)
      PAR(11)=PAR(7)
      REP=R2+1.
      DZ=0.5
c
      CALL GSVOLU( 'TFEC', 'TRAP', MEDPLX,PAR,11,IVOL) ! Edge of counter
c
c      PH=90.            !   old
      PH=-11.25/2-90.    !   V.Tayursky, 29.6.2000
      RE=(R2+R1)/2.
c
c  Positioning left and right endcap counters in tubes
c
      DO I=1,32
c
        CALL GSROTM(NROTE+I,90.,PH-90.,180.,0.,90.,PH)
c
        DX=RE*COS(DEGRAD*PH)
        DY=RE*SIN(DEGRAD*PH)
c
        CALL GSPOS('ESL0',I,'TOFL',DX,DY,0.,NROTE+I,'ONLY') ! Left counters
c
        CALL GSPOS('ESP0',I,'TOFR',DX,DY,0.,NROTE+I,'ONLY') ! Right counters
c
        CALL GSROTM(NROTE+32+I,0.,0.,90.,PH,90.,PH+90.)
c
        CALL GSROTM(NROTE+64+I,180.,0.,90.,PH,90.,PH-90.)
c
        DX=REP*COS(DEGRAD*PH)
        DY=REP*SIN(DEGRAD*PH)
c
        CALL GSPOS('TFEC',I,'TOFL',DX,DY,DZ,NROTE+32+I,'ONLY') ! Edge of cnts
c
        CALL GSPOS('TFEC',I,'TOFR',DX,DY,-DZ,NROTE+64+I,'ONLY')! Edge of cnts
c
c       PH=PH+11.25 ! old
        PH=PH-11.25
c
      END DO   ! end-cap counters
c
c  Endcap lightguides (plex)
c
      PAR(1)=66.6  ! tube for lightguides
      PAR(2)=69.4
      PAR(3)=44.5/2.
c
      CALL GSVOLU('TOEP','TUBE',MEDAIF,PAR,3,IVOL) ! Tube for end-cap lightguide
c
      CALL GSATT ('TOEP','SEEN',0)
c
      PAR(1)=H2
      PAR(2)=1.
      PAR(3)=44.5/2.
c
      CALL GSVOLU('TFEP','BOX ',MEDPLX,PAR,3,IVOL) ! lightguide
c
c      DZ=76.2+PAR(3)  ! old: center of tube for e-c lightguide
      DZ=75.+PAR(3)    ! V.Tayursky, 29.6.2000
c
c  Positioning end-cap lightguides in tube
c  
c      PH=90.          ! old
      PH=-11.25/2-90.  ! V.Tayursky, 29.6.2000
      NROTE1=NROTE+96
c
      DO I=1,32 
        DX=67.7*COS(DEGRAD*PH)
        DY=67.7*SIN(DEGRAD*PH)
c
        CALL GSROTM(NROTE1+I,90.,PH-90.,90.,PH,0.,0.)
c
        CALL GSPOS('TFEP',I,'TOEP',DX,DY,0.,NROTE1+I,'ONLY') ! lightguides
c                                                           ! in tube
c       PH=PH+11.25
        PH=PH-11.25

      END DO ! end-cap lightguides
c
c  Positioning end-cap counters and lightguides in KEDR
c
c      CALL GSPOS( 'TOFL' ,1, 'KEDR' , 0.,0.,75.2,0,'ONLY') ! old
      CALL GSPOS( 'TOFL' ,1, 'KEDR' , 0.,0.,74.,0,'ONLY')   ! V.A.T.
c
c      CALL GSPOS( 'TOFR' ,1, 'KEDR' , 0.,0.,-75.2,0,'ONLY')! old
      CALL GSPOS( 'TOFR' ,1, 'KEDR' , 0.,0.,-74.,0,'ONLY') ! V.A.T
c
      CALL GSPOS( 'TOEP' ,1, 'KEDR' , 0.,0., DZ,0,'ONLY')
c
      CALL GSPOS( 'TOEP' ,2, 'KEDR' , 0.,0.,-DZ,0,'ONLY')
c
      CALL TOF_ZERO
c
      RETURN
      END
