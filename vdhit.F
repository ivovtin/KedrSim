      SUBROUTINE VDHIT
#include "KedrSim/vdhits.inc"
      parameter (NBITALLOC=200,NBITKEEP=300)
      DIMENSION NBITS(nvddimhits),ORIG(nvddimhits),FACT(nvddimhits)
      CHARACTER*4 NAMESH(nvddimhits)
      DATA NAMESH /'XTB','YTB',
     +             'Xin','Yin','Zin','Xout','Yout','Zout',
     +             'Amp','PXin','PYin','PZin',
     +             'BXout','BYout','BZout',
     +             'ITRA','CHARGE','MASS','TrackR','Length',
     +             'PpartIn','PpartOut','EIn','Eout'/
      DATA NBITS /nvddimhits*32/
      DATA ORIG /8*100.,1*0., 6*1.,  2*10., 1*1.,1*0.,5*0./
      DATA FACT /8*1.E7,1.E10,6*1.E9,2*1.,  1*1.E8,
     & 1*1.E6,1*1.E7,4*1.E8/     
      CALL GSDETV('KVDH','STRD',1,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETH('KVDH','STRD',nvddimhits,NAMESH,NBITS,ORIG,FACT)

      RETURN
      END
