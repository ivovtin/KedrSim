c      PARAMETER(FBC=4.) ! current for towers are calc. in TDR/FBC time (changed because too short for muons)
      PARAMETER(FBC=1.) ! current for towers are calc. in TDR/FBC time 
      PARAMETER(NHJ=NXC/FBC,NHJS=16)
C      PARAMETER (NTBL=8,NTST=8)
      PARAMETER (NTBL=16,NTST=16)
      COMMON/LK_HITV/HBL(3,-9:9,48,NTBL,NHJ),NTB(NTBL),NTBLA,
     1               HSTZ(-4:4,-124:124,NTST,NHJS),
     2               HSTFI(4,768,NTST,NHJS),NTS(NTST),NTSTA
