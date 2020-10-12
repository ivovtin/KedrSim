      subroutine ts_geom_tube
c******************************************************
c
*@ :ts_def_tube: define single tube (volume TSTB)
c
c  Author: L.V.Romanov
c
c*********************************************************************
c      implicit none
      include 'medpar.inc'
      include 'ts_cmb.inc'
      real par(3)
      integer ivol

      par(1)=0.
      par(2)=0.28997
      par(3)=10.

      call gsvolu('TSFR','TUBE',MEDFRE,par,3,ivol)

      par(1)=0.29003
      par(2)=0.3
      par(3)=10.

      call gsvolu('TSCT','TUBE',MEDFE0,par,3,ivol)

      par(1)=0.
      par(2)=0.0014
      par(3)=10.

      call gsvolu('WIRE','TUBE',MEDTUN,par,3,ivol)

      par(1)=0.
      par(2)=0.3
      par(3)=10.

      call gsvolu('TSTB','TUBE',MEDAIR,par,3,ivol)
c
c----------------------------------------------------
c
c   Positioning volumes
c
c
      call gspos('TSCT',1,'TSTB',0.,0.,0.,0,'ONLY')
c
      call gspos('TSFR',1,'TSTB',0.,0.,0.,0,'MANY')
c
      call gspos('WIRE',1,'TSTB',0.03,0.,0.,0,'ONLY')
*               
      CALL FREE_ROTMAT_NUMBER(IRM)
c
      irm1=IRM
c
      call gsrotm(irm1,90.,0.,180.,0.,90.,90.)   
c
      CALL FREE_ROTMAT_NUMBER(IRM)
c
      irm2=IRM
c
      call gsrotm(irm2,90.,180.,0.,0.,90.,90.) 
c
      end

