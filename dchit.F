*CMZU:  1.00/03 01/11/96  15.05.57  by  VLADIMIR KOZLOV
*CMZ :  1.00/01 04/02/93  10.13.52  by  TANYA PURLATZ
*CMZ :          25/01/93  01.07.19  by  Tanya Purlatz
*changed Korneliy Todyshev 
      SUBROUTINE DCHIT
#include "KedrSim/celpar.inc"
#include "KedrSim/dchits.inc"
      parameter (NBITALLOC=200,NBITKEEP=300)
      DIMENSION NBITDC(ndimhits),ORIG(ndimhits),FACT(ndimhits)
      CHARACTER*4 NAMEDC(ndimhits)
      DATA NAMEDC /'Xin ','Yin ','Zin ','Xout','Yout','Zout',
     +             'TOF ','Amp','PXin','PYin','PZin',
     +             'BXout','BYout','BZout',
     +             'ITRA','CHARGE','MASS','TrackR','Length',
     +             'PpartIn','PpartOut','EIn','Eout'/
      DATA NBITDC /ndimhits*32/
      DATA ORIG /6*100.,2*0.,        6*1.,  2*10.,1*1.,1*0.,5*0./
      DATA FACT /6*1.E7,1*1.E5,1.E10,6*1.E9,2*1., 1*1.E8,
     & 1*1.E6,1*1.E7,4*1.E8/     
      if(layson(1).gt.0) then
         CALL GSDETV('KDCH','DCC1',2,NBITALLOC,NBITKEEP,ISET,IDET)
         CALL GSDETH('KDCH','DCC1',ndimhits,NAMEDC,NBITDC,ORIG,FACT)
      endif
      if(layson(2).gt.0) then
         CALL GSDETV('KDCH','DCC2',2,NBITALLOC,NBITKEEP,ISET,IDET)
         CALL GSDETH('KDCH','DCC2',ndimhits,NAMEDC,NBITDC,ORIG,FACT)
      endif
      if(layson(3).gt.0) then
         CALL GSDETV('KDCH','DCC3',2,NBITALLOC,NBITKEEP,ISET,IDET)
         CALL GSDETH('KDCH','DCC3',ndimhits,NAMEDC,NBITDC,ORIG,FACT)
      endif
      if(layson(4).gt.0) then
         CALL GSDETV('KDCH','DCC4',2,NBITALLOC,NBITKEEP,ISET,IDET)
         CALL GSDETH('KDCH','DCC4',ndimhits,NAMEDC,NBITDC,ORIG,FACT)
      endif
      if(layson(5).gt.0) then
         CALL GSDETV('KDCH','DCC5',2,NBITALLOC,NBITKEEP,ISET,IDET)
         CALL GSDETH('KDCH','DCC5',ndimhits,NAMEDC,NBITDC,ORIG,FACT)
      endif
      if(layson(6).gt.0) then
         CALL GSDETV('KDCH','DCC6',2,NBITALLOC,NBITKEEP,ISET,IDET)
         CALL GSDETH('KDCH','DCC6',ndimhits,NAMEDC,NBITDC,ORIG,FACT)
      endif
      if(layson(7).gt.0) then
         CALL GSDETV('KDCH','DCC7',2,NBITALLOC,NBITKEEP,ISET,IDET)
         CALL GSDETH('KDCH','DCC7',ndimhits,NAMEDC,NBITDC,ORIG,FACT)
      endif
      RETURN
      END
