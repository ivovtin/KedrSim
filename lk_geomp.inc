      PARAMETER (NSN=2) ! number of neibours for hits on strips
      REAL*8 BFINOR,SFINOR,C,ALPHA
      REAL LKGAP,LKRRHO,LKRRHOG
      COMMON/LK_GEOMP/BFINOR,SFINOR,C,ALPHA,
     1                BLKZ(17,-9:9),ZSTZ(-4:4,-124-NSN:124+NSN)
      COMMON/LK_GAP/LKGAP,LKRRHO,LKRRHOG
      COMMON/LK_SIZES/
	5	G10CTE,	! G10 coefficient of thermal expansion
	5	REL1O,	! Outer radius of first electrode (at T=293K)
	4	DRLAY,	! Electrode system layer thickness (at T=293K)
	5	DREL,	! Electrode thickness (at T=293K)
	7	DROB,	! Obechaika thickness (at T=293K)
	8	RALI,	! Inner radius of aluminium 
	8	RALO,	! Outer radius of aluminium
	1	DZAL2,	! Length/2 of aluminium 
	9	DRAL,	! Aluminium R thickness
	9	DZAL,	! Aluminium flange Z thickness
	2	RVACI,	! Inner radius of vacuum
	3	RVACO,	! Outer radius of vacuum
	4	DZVAC2,	! Length/2 of vacuum 
	5	RSTI,	! Inner radius of steel
	6	RSTO,	! Outer radius of steel
	7	DZST2,	! Length/2 of steel 
	8	RMOTI,	! Inner radius of mother volume
	9	RMOTO,	! Outer radius of mother volume
	1	DZMOT2	! Length/2 of mother volume 
