      SUBROUTINE ATC_HITS
c-----------------------------------------------------------------
c                                                                |
c  Defines hits of aerogel threshold counterts                   |
c                                                                |
c                                                                |
c   Author: V.A.Tayursky                                         |
c                                                                |
c   Changed  11.05.2001   V.Tayursky                             |
c   Changed  04.12.2013   I.Ovtin                                |
c   Changed  17.03.2016   I.Ovtin                                |
c-----------------------------------------------------------------
#include "KedrSim/atchits.inc"
c
	DIMENSION NBITSV(1),NBITSH(natcdimhits),ORIG(natcdimhits),FACT(natcdimhits)
	DATA NBITSV/6/
	CHARACTER *4 CHNAMH(15)
c
      DATA ORIG/natcdimhits*100./
      DATA FACT/natcdimhits*100./
      DATA CHNAMH/'Counter','Xint','Yint','Zint', 'Xout', 'Yout',
     +          'Zout','Ia_ch','Ia_sc','Ish_ch',
     +          'Ish_sc','It_ch','It_sc','AtcNAME','AtcNLEVEL'/
c
      DATA NBITSH/natcdimhits*32/
      parameter (NBITALLOC=200,NBITKEEP=300)
c
      IDTYP=4 ! ATC detector type=4 for aerogel and shifter
c
      CALL GSDETV('ATCH','TESI',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETV('ATCH','TEDI',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETV('ATCH','TSSI',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETV('ATCH','TSDI',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETV('ATCH','TLSI',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETV('ATCH','TLDI',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETV('ATCH','TESO',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETV('ATCH','TEDO',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETV('ATCH','TSSO',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETV('ATCH','TSDO',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETV('ATCH','TLSO',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
      CALL GSDETV('ATCH','TLDO',IDTYP,NBITALLOC,NBITKEEP,ISET,IDET)
c
      CALL GSDETH('ATCH','TESI',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
      CALL GSDETH('ATCH','TEDI',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
      CALL GSDETH('ATCH','TSSI',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
      CALL GSDETH('ATCH','TSDI',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
      CALL GSDETH('ATCH','TLSI',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
      CALL GSDETH('ATCH','TLDI',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
      CALL GSDETH('ATCH','TESO',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
      CALL GSDETH('ATCH','TEDO',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
      CALL GSDETH('ATCH','TSSO',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
      CALL GSDETH('ATCH','TSDO',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
      CALL GSDETH('ATCH','TLSO',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
      CALL GSDETH('ATCH','TLDO',natcdimhits,CHNAMH,NBITSH,ORIG,FACT)
c
      RETURN
      END
