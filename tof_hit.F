      SUBROUTINE TOF_HIT
#include "KedrSim/tofhits.inc"
c
c  Author: T.Purlatz
c  +changed by O.Beloborodova
c
      parameter (NBITALLOC=200,NBITKEEP=300) ! was 100,100
      DIMENSION NBITTF(ntofdimhits),ORIG(ntofdimhits),FACT(ntofdimhits)
      CHARACTER*4 NAMETF(ntofdimhits)
C
      DATA NAMETF /'X   ','Y   ','Z   ','Xout','Yout','Zout',
     +             'TOF ','Amp ','Xin','Yin','Zin',
     +             'PXin','PYin','PZin','Pin','ITRA','CHARGE',
     +             'MASS','IPART','Count','Ein'/

      DATA NBITTF /ntofdimhits*32/
c      DATA ORIG /6*100.,2*0.,3*100.,4*1.,5*0.,1*1./
      DATA ORIG /6*100.,2*0.,3*100.,3*10.,2*0.,1*3.,3*0.,1*0./
c      DATA FACT /7*1000.,1.E10/
      DATA FACT /6*1.E7,1*1.E5,1*1.E10,3*1.E7,4*1E8,2*1.,1*1.E8,
     & 2*1.,1*1.E8/
C
      CALL GSDETV('KTFH','BS00',3,NBITALLOC,NBITKEEP,ISET,IDET)
c
      CALL GSDETH('KTFH','BS00',ntofdimhits,NAMETF,NBITTF,ORIG,FACT)
c
      CALL GSDETV('KTFH','ESP0',3,NBITALLOC,NBITKEEP,ISET,IDET)
c
      CALL GSDETH('KTFH','ESP0',ntofdimhits,NAMETF,NBITTF,ORIG,FACT)
c
      CALL GSDETV('KTFH','ESL0',3,NBITALLOC,NBITKEEP,ISET,IDET)
c
      CALL GSDETH('KTFH','ESL0',ntofdimhits,NAMETF,NBITTF,ORIG,FACT)
c
      RETURN
      END
