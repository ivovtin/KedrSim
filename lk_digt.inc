c      PARAMETER(FBC=4.) ! current for towers are calc. in TDR/FBC time (changed because too short for muons)
      PARAMETER(FBC=1.) ! current for towers are calc. in TDR/FBC time
      PARAMETER(NXC=80,NHJ=NXC/FBC,NHJS=16)
      PARAMETER(NPU=40)
      COMMON/LK_DIGT/CURMIBL(NHJ) ,USHBL(NPU),CDBL(NHJ),
     1        RC2CRTBL(NHJ,NPU),RC2CRTS(NHJS,NPU),
     2        CURMIST(NHJS),USHST(NPU),CDST(NHJS)
