*CMZU:  1.00/01 16/10/96  17.53.46  by  MALYSHEV V.M.
*-- Author :
      SUBROUTINE LK_CARD
C.
C.    *
C.    *         LK cards definition
C.    *
C.
	include 'lk_digp.inc'
! LK_DIGP:
! Parameters are the same as in PROT-4 experiment (1994).
! Tdr,Tlif - drift and life time (mkS)
! Tau,Taus - blocks and strips shapers times
! Tgmi,Tgma,Tgmis,Tgmas - ADC gate times for blocks and strips
! Gain,Gains - gains for blocks and strips (MeV/channel)
! Iped,Ipeds - pedestals for blocks and strips
! Bnoise,Snoise - blocks and strips noise (MeV)
! Bthr,Bthrn,Sthr - thresholds for amplitudes (in sigmas of noise) 
!			  for writting data to output file
      DATA TDR/9./,TLIF/2.85/,TAU/1.05/,TGMI/0./,TGMA/2.5/,GAIN/.1/,
     *  IPED/70/,BNOISE/1./,TAUS/5./,TGMIS/0./,TGMAS/9./,GAINS/.1/,
     *  IPEDS/70/,SNOISE/.3/,BTHR/2.5/,BTHRN/5./,STHR/2./
C      EQUIVALENCE (ITDR,TDR)
C
	include 'lk_hdsw.inc'
! LK_HDSW:
! 1-on/off block's HITS calculation
! 2-to print on/off (1/0) block's hits&digi
! 3-on/off block's DIGI calculation
! 4-graphs of block's digitization plotting
! 5-on/off Z-strips HITS calculation
! 6-to print on/off (1/0) Z-strips hits&digi
! 7-on/off Z-strips DIGI calculation
! 8-graphs of Z-strips digitization plotting
! 9-on/off FI-strips HITS calculation
! 10-to print on/off (1/0) FI-strips hits&digi
! 11-on/off FI-strips DIGI calculation
! 12-graphs of FI-strips digitization plotting
! 13-1/0 autoscaling is on/off - normalization of amplitudes in towers according to Gain and Gains for Tlif=Tlif or for Tlif=infinity
! 14-1/0 autoscaling is on/off - normalization of amplitudes in strips according to Gain and Gains for Tlif=Tlif or for Tlif=infinity
      DATA ILKHD/1,0,1,0,1,0,1,0,1,0,1,0,1,1,6*0/

                        
	include 'lk_xyzrot.inc'
	include 'lk_geomp.inc'
! LKGAP - gap of gaseous krypton on top of calorimeter
        DATA LKGAP/0./, LKRRHO/2.41/, LKRRHOG/8.79e-3/
	DATA RMOTI/72.89/,RMOTO/152./,DZMOT2/149.1/,
	1    RSTI/72.9/,RSTO/151.9/,DZST2/149./,
	2    RVACI/73./,RVACO/150.3/,DZVAC2/144./,
	3    RALI/74.6/,RALO/147.0/,DZAL2/134.9/,DRAL/1.6/,DZAL/4.5/,
	4    DREL/0.056/,DROB/0.144/,DRLAY/2./,
	5    REL1O/76.7/,G10CTE/0.9977/

C      CALL FFKEY('LKPD',ITDR,14,'M')	! LK DIGI par-s
      CALL FFKEY('LKPD',TDR,17,'M')	! LK DIGI par-s
      CALL FFKEY('LKHD',ILKHD,20,'I')	! LK HITS&DIGI switches
      CALL FFKEY('LKGAP',LKGAP,3,'R')	! LK GAP and density
      CALL FFKEY('LKGEOM',G10CTE,3,'R')	! LK sizes
      CALL FFKEY('LKROT',XYZROT_LKR,6,'R') ! LK SHIFT&ROTATION


      END
