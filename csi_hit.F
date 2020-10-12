      SUBROUTINE CSI_HIT
c***************************************************************
c
c  Calculation of hits in CsI calorimeter
c
c  Author :	TANYA PURLATZ   05/01/96
c
c  Called from ugeom
c 
c**************************************************************
      DIMENSION NBIT(14),ORIG(14),FACT(14),NBIT1(3),NBIT2(3),
     +          NBIT3(3),NBIT4(3),NBIT5(3)
      CHARACTER*4 NAME(14),NAM1(3),NAM2(3),NAM3(3),NAM4(3),NAM5(3)
      DATA NAM1/'ENDB','BL  ','CR  '/, NBIT1/3,7,2/
      DATA NAM2/'ENDB','BL80','CR80'/, NBIT2/3,7,2/
      DATA NAM3/'ENDB','BL88','CR88'/, NBIT3/3,7,2/
      DATA NAM4/'ENDB','BLTR','CRTR'/, NBIT4/3,7,2/
      DATA NAM5/'ENDB','BLIN','CRIN'/, NBIT5/3,7,2/


      DATA NAME /'X   ','Y   ','Z   ','Xint','Yint','Zint','Ityp',
     +           'Etot','TOF ','Icry','Ich ','Igr ','Imd ','dE  '/
      DATA NBIT /14*32/
      DATA ORIG /6*1000.,8*0./
      DATA FACT /6*1000.,1.,2*1000.,4*1.,1.E6/
C
      CALL GSDET('CSHM','CR  ',3,NAM1,NBIT1,6,1000,1000,
     +           ISET,IDET)
      CALL GSDET('CSHM','CR80',3,NAM2,NBIT2,6,1000,1000,
     +           ISET,IDET)
      CALL GSDET('CSHM','CR88',3,NAM3,NBIT3,6,1000,1000,
     +           ISET,IDET)
      CALL GSDET('CSHM','CRTR',3,NAM4,NBIT4,6,1000,1000,
     +           ISET,IDET)
      CALL GSDET('CSHM','CRIN',3,NAM5,NBIT5,6,1000,1000,
     +           ISET,IDET)
      CALL GSDETH('CSHM','CR  ',14,NAME,NBIT,ORIG,FACT)
      CALL GSDETH('CSHM','CR80',14,NAME,NBIT,ORIG,FACT)
      CALL GSDETH('CSHM','CR88',14,NAME,NBIT,ORIG,FACT)
      CALL GSDETH('CSHM','CRTR',14,NAME,NBIT,ORIG,FACT)
      CALL GSDETH('CSHM','CRIN',14,NAME,NBIT,ORIG,FACT)
      END
