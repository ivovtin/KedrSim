      SUBROUTINE UGMATE
c**************************************************************
c
c   Materials definition; tracking parameters definition
c
c   Changes: 22.12.98.  V.A.Tayursky
c            01.10.99.  V.A.Tayursky
c            10.05.01.  V.A.Tayursky
c	     23.05.2001 V.M.Malyshev : 	Ifield for MEDYOK=0 if
c	 	fieldm=0 (was 2)
c            16.06.2003 A.Poluektov : Added description of
c                       "Electronic parts" material MEDELE
c            29.10.2003 V.A.Tayursky: deleted aerogel crumb
c            25.10.2005 E.A.Kravchenko : Update description of
c                       Aerogel counters materials
c            18.02.2017 I.V.Ovtin : Update description of
c                       Aerogel counters materials
c
c**************************************************************
	include 'medpar.inc'
	include 'fiel.inc'
	include 'lk_geomp.inc'
c
c       MEDIAS & MIXTURES PARAMETERS
c
      CHARACTER*30 NAM
      DIMENSION ACSI(2),ZCSI(2),WCSI(2),ADME(3),ZDME(3),WDME(3),
     +          AMYL(3),ZMYL(3),WMYL(3),ACO2(2),ZCO2(2),WCO2(2),
     +          AIZO(2),ZIZO(2),WIZO(2),AS(3),ZS(3),WMATS(3),
     +          ASCTU(2),ZSCTU(2),WSCTU(2),AF(2),ZF(2),WF(2),
     +          AG10(4),ZG10(4),WG10(4),
     +          ASIL(5),ZSIL(5),WSIL(5),
     +          ALAT(2),ZLAT(2),WLAT(2),
     +          AAER(3),ZAER(3),WAER(3),
     +          ASHI(3),ZSHI(3),WSHI(3),
     +          ATEF(2),ZTEF(2),WTEF(2),
     +          AD16(4),ZD16(4),WD16(4),
     +		APLEX(3),ZPLEX(3),WPLEX(3),
     +		ASCIN(2),ZSCIN(2),WSCIN(2),
     +          A35(2),Z35(2),W35(2),
     +		AELEC(4),ZELEC(4),WELEC(4)      ! A.P.
c
c  DC gas == DME (C2-H6-O)
      DATA  ADME, ZDME, WDME / 12.01, 1.01,  16.00,
     +                            6.,    1.,    8.,
     +                            2.,    6.,    1. /
c
c  CsI  (27.3.98 Changed Acsi(1) from 133.99 to 132.91)
      DATA  ACSI  , ZCSI  , WCSI   / 132.91, 126.90,
     +                   55.,    53.,
     +                   1.,     1.    /
c
c Mylar, C5-H4-O2
      DATA  AMYL, ZMYL, WMYL  /12.01, 1.01, 16.0,
     +                            6.,   1.,  8.0,
     +                            5.,   4.,  2./
c
c Izobutan
      DATA  AIZO, ZIZO, WIZO  /12.01, 1.01, 6., 1., 4., 10./
c
c CO2
      DATA  ACO2, ZCO2, WCO2  /12.01, 16.0, 6., 8.0, 1., 2./
c
c  G10 ( CLEO )
c
* DG10   - G10   = 60% SiO2, 40% epoxy : Rev.Mod.Phys. 56 No2(1984)S35
* AG10     Epoxy = 77% C, 16% O, 7% H by weight : Modern Plastic
* ZG10             Encyclopedia 58(1981-82)10A p.18
* WG10
*
      DATA DG10 /  1.7 /
      DATA AG10 / 16.00, 12.01, 28.09, 1.01  /
      DATA ZG10 /  8.08,  6.00, 14.00, 1.00  /
      DATA WG10 / 0.384, 0.308, 0.280, 0.028 /
c
c  Steel (GEXAM1)
      DATA AS/55.847,58.69,51.9961/
      DATA ZS/26.,28.,24./
      DATA WMATS/0.703964,0.099,0.197/
c
c  Steel (12X18H10T)
c                 Ti      C         Cr        Ni       Fe
       DATA ASIL/ 47.88,  12.011,   51.9961,  58.69,   55.847/
       DATA ZSIL/ 22.,    6.,       24.,      28.,     26.  /
       DATA WSIL/ .01,    .0012,    .18,      .1,      .7088/
c
c  Latun (Cu(62%)+Zn(38%))
       DATA ALAT/ 63.546, 65.39 /
       DATA ZLAT/ 29.,    30.   /
       DATA WLAT/ 0.62,   0.38  /
c
c  Steff + Cu
      DATA ASCTU/63.54,15.4/
      DATA ZSCTU/29.,8.05/
c
c  Aerogel   SiO2(97%) + H2O(3%)
      DATA AAER,ZAER,WAER/ 28.09,16.,1.008,  14.,8.,1.,  0.463,0.536,0.001/
c
c  Shifter (pleks)--- C5H8O2 --------
      DATA ASHI,ZSHI,WSHI/12.01,1.008,16.,  6.,1.,8.,  5.,8.,2./
c
c  Teflon reflecting film -----CF2--------
      DATA ATEF,ZTEF,WTEF/12.01,19.,  6.,9.,  1.,2./
c
c  Duraluminum D16T
c                   Al       Cu     Mg    Mn                                                                                                                                                                                                                                                                                                                                                                    DATA AD16/ 63.546, 65.39 /
       DATA AD16/ 26.982, 63.546, 24.305, 54.938/
       DATA ZD16/ 13.,    29.,    12.,    25.   /
       DATA WD16/  0.936,  0.043,  0.015,  0.006/
c
c  Ceramics
      DATA A35,Z35,W35/28.09,16., 14.,8., 1.,2./

c   PLEX  (C5 H8 O2)
*                  H       C        O
       DATA APLEX/ 1.01,  12.01,   16.  /
       DATA ZPLEX/ 1.  ,   6.  ,    8.  /
       DATA WPLEX/ 8.  ,   5.  ,    2.  /
c  Scintillator
*                  H       C
       DATA ASCIN/ 1.01,  12.01  /
       DATA ZSCIN/ 1.  ,   6.    /
       DATA WSCIN/ 10.  ,  9.    /
c  Electronics parts (30% Cu, 70% Epoxy)
c                    Cu       H       C      O
       DATA AELEC/63.54,   1.01,  12.01,   16.  /            ! A.P.
       DATA ZELEC/29.,     1.,     6.,      8.  /            ! A.P.
       DATA WELEC/0.3,      0.049, 0.539, 0.112 /            ! A.P.
c
c------------------------------------------------------------------
c
c     'NON-STANDART' MATERIALS AND MIXTURES
c
c	print *,' start UGMATE'
c--------------------------------------------------------------------
      NCSI=17
c
      CALL GSMIXT(NCSI,'CSI$',ACSI,ZCSI,4.51,-2,WCSI)
c--------------------------------------------------------------------
c G10
      NG10=18
c
      CALL GSMIXT(NG10,'G10$',AG10,ZG10,DG10,4,WG10)
c--------------------------------------------------------------------
c DC gas DME
      NDME=19
      CALL GSMIXT(NDME,'DME$',ADME,ZDME,.0024,-3,WDME)
c--------------------------------------------------------------------
c Ugleplastic (Carbon 90% + Epoxy 10% =  98% C, density 1.5)
      NCPL=28
c
      CALL GSMATE(NCPL,'CPL$', 12., 6., 1.5, 21.35, 49.9, 0, 0)
c
c--------------------------------------------------------------------
c MYLAR
      NMYL=20
c
      CALL GSMIXT(NMYL,'MYLAR$',AMYL,ZMYL,1.39,-3,WMYL)
c
c--------------------------------------------------------------------
c IZOBUTAN
      NIZO=21
      DENI=2.67E-3
c
      CALL GSMIXT(NIZO,'IZOBUTAN$',AIZO,ZIZO,DENI,-2,WIZO)
c
c--------------------------------------------------------------------
c CO2
      NCO2=22
      DENC=1.977E-3
      CALL GSMIXT(NCO2,'CO2$',ACO2,ZCO2,DENC,-2,WCO2)
c
c--------------------------------------------------------------------
c Gas for VD
      NVDG=23
c
      CALL GFMATE(NIZO,NAM,AF(1),ZF(1),D,R,A,U,NU) ! gives param-s of matt.
c       print *,' call GFMATE NCO2'
c
      CALL GFMATE(NCO2,NAM,AF(2),ZF(2),D,R,A,U,NU)
c
c	print *,' VD MIXT'
      WF(1)=0.15
      WF(2)=0.85
      DEN=WF(1)*DENI+WF(2)*DENC
c
      CALL GSMIXT(NVDG,'GASFORVD$',AF,ZF,DEN,2,WF)
c
c--------------------------------------------------------------------
c TOF Scintillator
c	print *,' TOF MATE'
      NTOF=24
      DENSC=1.032
c
      Call GSMIXT(NTOF,'SCINT$',ASCIN,ZSCIN,DENSC,-2,WSCIN)
c
c--------------------------------------------------------------------
c PLexiglas
      NPLEX=34
      DENSPL=1.18
c
      Call GSMIXT(NPLEX, 'PLEX$',APLEX,ZPLEX,DENSPL,-3,WPLEX)
c
c--------------------------------------------------------------------
c Liquid Krypton
c	print *,' LKR MATE'
      NLKR=25
c
c      CALL GSMATE(NLKR,'LKR$',  83.8,  36.,  2.41,  4.6,  60., 0,0)
c	print *,'LKRRHO=',LKRRHO
c	print *,'LKRRHOG=',LKRRHOG
      CALL GSMATE(NLKR,'LKR$',  83.8,  36.,  LKRRHO,  4.6,  60., 0,0)
c

c--------------------------------------------------------------------
c Gaseous Krypton
      NLKG=47
c
c      CALL GSMATE(NLKG,'LKR GAS$',  83.8,  36.,  2.41/274.2,  4.6*274.2,
      CALL GSMATE(NLKG,'LKR GAS$',  83.8,  36.,  LKRRHOG,  4.6*274.2,
      		  60.*286.5, 0,0)
c
c--------------------------------------------------------------------
c Titan
c	print *,' TIT MATE'
      NTIT=29
c
      CALL GSMATE(NTIT,'TITAN$',  47.88,  22.,  4.54,  3.56,  125., 0,0)
c
c--------------------------------------------------------------------
c STEEL (IRON+NICKEL+CROME)
c	print *,' STEEL MATE'
      NSTE=26
c
      CALL GSMIXT(NSTE,'STEEL$',AS,ZS,7.8,3,WMATS)
c
c--------------------------------------------------------------------
c STEF + CU for LKr calorimeter
c	print *,' STEF+CU MATE'
      NSCTU=27
      ROCU=8.96
      ROST=1.75
      HCU=0.0036
      HST=0.054
      WFT=ROCU*HCU+ROST*HST
      WSCTU(1)=ROCU*HCU/WFT
      WSCTU(2)=ROST*HST/WFT
      DENS=(ROCU*HCU+ROST*HST)/(HST+HCU)
c
      CALL GSMIXT(NSCTU,'STEF+CU$', ASCTU,ZSCTU,DENS,2,WSCTU)
c
c--------------------------------------------------------------------
c  Aerogel blocks, standard      n=1.05   nexp=1.04752
      NAERS=30
c      DENSAER=0.243
      DENSAER=0.2294
c
      CALL GSMIXT(NAERS,'AERS$', AAER,ZAER,DENSAER,3,WAER)
c
c--------------------------------------------------------------------
c  Aerogel blocks, dense     n=1.054   nexp=1.05576
c     SiO2(97%) + H2O(3%)
c      DATA AAER,ZAER,WAER/ 28.09,16.,1.008,  14.,8.,1.,  0.463,0.536,0.001/
      NAERD=36
c      DENSAER1=0.262
      DENSAER1=0.27035
c
      CALL GSMIXT(NAERD,'AERD$', AAER,ZAER,DENSAER1,3,WAER)
c
c---------------------------------------------------------------------
c
c  Shifter (pleks)--- C5H8O2 --------
c      DATA ASHI,ZSHI,WSHI/12.01,1.008,16.,  6.,1.,8.,  5.,8.,2./
      NSHI=31
      DENSSHI=1.18
c
C      DATA ASHI,ZSHI,WSHI/12.01,1.008,16.,  6.,1.,8.,  5.,8.,2./
c
      CALL GSMIXT(NSHI,'SHIFTER$', ASHI,ZSHI,DENSSHI,-3,WSHI)
c
c---------------------------------------------------------------------
c
c  Teflon film, reflector--- CF2 --------
c      DATA ATEF,ZTEF,WTEF/12.01,19.,  6.,9.,  1.,2./
      NTEF=38
      DENTEF=0.628
c
      CALL GSMIXT(NTEF,'TEFLON$', ATEF,ZTEF,DENTEF,-2,WTEF)
c
c----------------------------------------------------------------------
c
c  Duraliminum D16T
c                   Al       Cu     Mg    Mn                                                                                                                                                                                                                                                                                                                                                                    DATA AD16/ 63.546, 65.39 /
c       DATA AD16/ 26.982, 63.546, 24.305, 54.938/
c       DATA ZD16/ 13.,    29.,    12.,    25.   /
c       DATA WD16/  0.936,  0.043,  0.015,  0.006/
      ND16=40
      DEND16T=2.78
      CALL GSMIXT(ND16,'D16T$', AD16,ZD16,DEND16T,4,WD16)
c
c----------------------------------------------------------------------
c Steel for vacuum chamber
      DENSIL=7.85
      NVST=32
      Call GSMIXT(NVST, 'FECH$',ASIL,ZSIL,DENSIL,5,WSIL)
c
c-----------------------------------------------------------------------
c Latun for DC support
      DENLAT=8.27
      NLAT=33
      Call GSMIXT(NLAT, 'LATUN$',ALAT,ZLAT,DENLAT,2,WLAT)
c--------------------------------------------------------------------
c Tine (Sn)
      NTINE=35
      CALL GSMATE(NTINE,'TINE$', 118.71, 50., 7.31, 1.21, 163., 0,0)
c
c----------------------------------------------------------------------
c
c --- Ceramics = SiO2  ---                              RhO=2.2
c
      CALL GSMIXT(43,'CERAMICS(MAG.FLD)$',A35,Z35,2.2,-2,W35)
c
c----------------------------------------------------------------------
c
c --- end-cap ATC electronics: approx. as Al with rho=0.558 g/cm^3
c
      Call GSMATE(44,  'ATCEL$',  27.,  13.,  0.558,  43.,  1000., 0,0)
c
c---------------------------------------------------------------------
      freon_dens=(4*19.+16.)/22.4*0.001 ! for TS
      freon_rl=32./freon_dens
c
      CALL GSMATE(45,'FREON$',18.,8.5,freon_dens,freon_rl,1.,0,0)
c
c---------------------------------------------------------------------
c     Definition of VD electronics parts medium
      NVDE=46
      DENELEC = 80./(1.4*6.6*11.5)                                   ! A.P.
      CALL GSMIXT(NVDE,'VDELEC$', AELEC, ZELEC, DENELEC, 4, WELEC)   ! A.P.
c====================================================================
c===================================================================
c
c     DEFINE TRACKING MEDIA PARAMETERS
c
c--------------------------------------------------------------------
c
c     I. Media inside the magnetic field
c
c--------------------------------------------------------------------
c
      ISVOL  = 0 ! not sensitive volumes
c
		if(fieldmap > 0.5) then
			ifield = 2
		else
      IFIELD = 3
	    endif
      IF(FIEL.EQ.0)IFIELD=0
      FIELDM = FIEL
      TMAXFD = FIEAN
      IF (TMAXFD.LT.1.E-5) TMAXFD=0.5
      DMAXMS = 0.1
      DEEMAX = 0.001
      EPSIL  = 0.01
      STMIN  = 0.1
c
c--- Force AUTOMATIC calculation of tracking parameters...
c (card AUTO 0 in mc.cards should be set!!! Only LKr uses USER parameters!)
      TMAXFD = -TMAXFD
      DMAXMS = -DMAXMS
      DEEMAX = -DEEMAX
      STMIN  = -STMIN
c
c 35 	---- Iron for yoke ---
		IFIELDY=2
		IF(IFIELD.EQ.0) IFIELDY=0
		CALL GSTMED(MEDYOK,'YOK IR$',10,ISVOL, IFIELDY,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN, 0,0)
c     *            FIELDM,TMAXFD,-DMAXMS,-DEEMAX,EPSIL,-STMIN, 0,0)
c
c 1 ----------- Steff (G10) for DC and LKr ---------------------------
      EPSIL  = 0.001
      CALL GSTMED(MEDG10, 'G10$',NG10,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 22 ----------- Ugleplastic for DC  ----------------------------------
      CALL GSTMED(MEDCPL, 'CPL$',NCPL,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 25 ---------- Air with magnetic field --------------------------------
      EPSIL  = 0.01
      CALL GSTMED(MEDAIF, 'AIR(MAG.FLD)$',15,ISVOL,IFIELD,
     *            FIELDM,-TMAXFD,-DMAXMS,-DEEMAX, EPSIL, -STMIN, 0,0)
c
c 2 ----------- Vacuum with magnetic field ---------------------------
      EPSIL  = 0.001
      CALL GSTMED(MEDVMF, 'VACUUM(MAG.FLD)$',16,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 3 ----------- Mylar for vertex detector  ---------------------------
      CALL GSTMED(MEDMYL, 'MYLAR(MAG.FLD)$',NMYL,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 4 ----------- Steel for Kripton calorimeter ------------------------
      EPSIL  = 0.01
      CALL GSTMED(MEDSTE, 'STEEL(MAG.FLD)$',NSTE,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 5 ----------- Aluminium with field for VD, LKr ---------------------
      CALL GSTMED(MEDALF, 'ALUMINIUM(MAG.FLD)$',9,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 6 ----------- G10 + Cu for Kripton calorimeter ---------------------
      CALL GSTMED(MEDSCTU, 'G10+CU(MAG.FLD)$',NSCTU,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c---------------------------------------------------------------------
c 31 --- Ceramics=SiO2  ---                              RhO=2.2
c
      Call GSTMED(MEDCER,'CERAM$'   ,43, 0 , IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
c
c---------------------------------------------------------------------
c 31 --- End-cap ATC electronics (Al with  rho=0.558 g/cm**3)
c
      Call GSTMED(MEDELB,'ATCEL$'   ,44, 0 , IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
c
c---------------------------------------------------------------------
c 37 --- VD electronics (30% Cu + 70% Epoxy with  rho=0.8 g/cm**3)
c
      Call GSTMED(MEDELE,'VDELECTRONICS$',NVDE, 0 , IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
c
c 41 ----- Plex (MCP PMT frame in aerogel counter)--------------------
c
      Call GSTMED(MEDPLXF,'MCPPMTFRAME$',NPLEX, 0 ,IFIELD,
     *             FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 40 ----- Duraliminum D16T-------------------------------------------
c
      Call GSTMED(MEDD16T,'D16T$',ND16, 0 ,IFIELD,
     *             FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c ***changed to having mag.field 4/03/05
c 12 ---
      CALL GSTMED(MEDALU, 'ALUMINIUM$',9,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 13 ---
      CALL GSTMED(MEDCOP, 'COPPER$',11,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 23 ---
      CALL GSTMED(MEDTIT, 'TITAN$',NTIT,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 20 ---
      CALL GSTMED(MEDBER, 'VACTUB BE$',5,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 21 ---
      CALL GSTMED(MEDIRV, 'VACTUB IR$',NVST,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 29 ---
      CALL GSTMED(MEDTIN, 'VACTUB SN$',NTINE,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 24 ---
      CALL GSTMED(MEDLAT, 'RASPORKA DC$',NLAT,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)


c
c===================== Sensitive volumes of matters ==================
c
c
      ISVOL = 1

c 7 -----------  Gas for vertex detector        ----------------------
      TMAXFD=  0.005
      DMAXMS = 0.05
      DEEMAX =  0.001
      STMIN  =  0.05
      EPSIL=0.001
      CALL GSTMED(MEDVDG, 'VDGAS(MAG.FLD)$',NVDG,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 9 ----------- Krypton for calorimeter -----------------------
      TMAXFD = -TMAXFD
      DMAXMS = 0.1
      DEEMAX = 0.001
      STMIN  = 0.05
      EPSIL  = 0.01
      CALL GSTMED(MEDLKR, 'LKRYPTON(MAG.FLD)$',NLKR,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
      CALL GSTMED(MEDLKG, 'GAS KRYPTON (MAG.FLD)$',NLKG,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
      DMAXMS = -DMAXMS
      DEEMAX = -DEEMAX
      STMIN  = -STMIN
c 8 ----------- Scintillator for TOF countersteel --------------------
      CALL GSTMED(MEDPLA, 'SCINT(MAG.FLD)$',NTOF,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 18 ----------CsI----------------------------------------------------
      CALL GSTMED(MEDCSI, 'CSI(MAG.FLD)$',NCSI,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 26 ---------- Aerogel blocks, standard------------------------------
c
      CALL GSTMED(MEDAERS, 'AERS$',NAERS,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 36 ---------- Aerogel blocks, dense---------------------------------
c
      CALL GSTMED(MEDAERD, 'AERD$',NAERD,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 27---------- Shifter -----------------------------------------------
c
      CALL GSTMED(MEDSHF, 'SHIFTER$',NSHI,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 42------ Teflon reflecting film ------------------------------------
c
      DMAXMS1 = 0.05
      EPSIL1  = 0.005
      STMIN1  = 0.05
      CALL GSTMED(MEDTEF, 'TEFLON$',NTEF,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS1,DEEMAX, EPSIL1, STMIN1, 0,0)
c
c 10,11 ----------- Gas for Drift chamber ----------------------------
      TMAXFD=  0.005
      DMAXMS = 0.02
      DEEMAX =  0.001
      STMIN  =  0.02
      EPSIL  =  0.005
      CALL GSTMED(MEDDME, 'DME$',NDME,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c      TMAXFD = .002
c      DMAXMS = 0.01
c      STMIN  = 0.02
c      CALL GSTMED(MJCDME, 'JCDME$',NDME,ISVOL,IFIELD,
c     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c----------------------------------------------------------------------
c
c     I. Media out of the magnetic field
c
c---------------------------------------------------------------------
c
      IFIELD = 0          ! out of field
      FIELDM = 0.
c      FIELDM = FIEL
      TMAXFD = FIEAN
      IF (TMAXFD.LT.1.E-5) TMAXFD=0.5
      DMAXMS = 0.1
      DEEMAX = 0.001
      STMIN  = 0.1
      EPSIL  = 0.01
c
c--- Force AUTOMATIC calculation of tracking parameters...
      TMAXFD = -TMAXFD
      DMAXMS = -DMAXMS
      DEEMAX = -DEEMAX
      STMIN  = -STMIN

c 19 ----------------------------------------------------------------
      ISVOL  = 1 ! sensitive
c
c 19 ---- muon system sensetive media
	   if(fieldmap > 0.5) then
	     ifieldmu = 2
		 fieldm_mu = fiel
	   else
	     ifieldmu = 0
	endif
      CALL GSTMED(MEDMUG, 'MUON TUBES GAS$',15,ISVOL,IFIELDMU,
     *            FIELDM_mu,TMAXFD,DMAXMS,DEEMAX, EPSIL,STMIN, 0,0)
c     *            FIELDM,TMAXFD,-DMAXMS,-DEEMAX, EPSIL,-STMIN, 0,0)
c
c 28 -----TOF Plex (svetovod)-----------------------------------------

      Call GSTMED(MEDPLX,'SCPL$',NPLEX,ISVOL,IFIELD,
     *             FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
c 33 --- Fe without mag. field for TS sensitive
      CALL GSTMED(MEDFE1,'FE(FLD 0)SEN$',10,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c     *            FIELDM,TMAXFD,-DMAXMS,-DEEMAX, EPSIL, -STMIN, 0,0)
c
c---------------------------------------------------------------------

      ISVOL = 0 ! no sensitive

c 38
      CALL GSTMED(MEDCOF, 'COPPER (NO FLD)$',11,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 14 ---
      CALL GSTMED(MEDTUN, 'TUNGSTEN$',12,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 15 ---
      CALL GSTMED(MEDAIR, 'AIR$',15,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c     *            FIELDM,TMAXFD,-DMAXMS,-DEEMAX, EPSIL, -STMIN, 0,0)
c 16 ---
      EPSIL  = 0.001
      CALL GSTMED(MEDVAC, 'VACUUM$',16,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 17 ---
      EPSIL  = 0.01
      CALL GSTMED(MEDLEAD, 'LEAD$',13,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 32 --- freon for TS
      CALL GSTMED(MEDFRE, 'TS FREON$',45,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c 34 --- Fe without mag. field for TS no sensitive
      CALL GSTMED(MEDFE0, 'FE(FLD 0)NOSEN$',10,ISVOL,IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0,0)
c
      END

