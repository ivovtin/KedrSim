      SUBROUTINE UGCARD
c*******************************************************************
c
c   Read data cards for systems etc
c
c   Corections: 
c
c   30 Apr 2000, V.A.Tayursky. Cards DTB1,DTB2,DTB3, /tabdec/ deleted 
c   25 Sep 2003, V.A.Tayursky. Card "HIST" for histograms added 
c   16 Mar 2004, V.A.Tayursky. Card "RNDG" added
c   25 May 2004, V.A.Tayursky. Renamed csicard -> csi_card
c   10 Oct 2012, V.A.Tayursky. Card "FLUK" added
c
c*******************************************************************
c
#include "gcphys.inc"
      COMMON /HIS_SIMUL/ IHISTOG       ! flag for hist. calculation in sim.
      COMMON /RZFZFILES/ IRZFZ(3)      ! 
      COMMON /GEN_RNG/   IGENRNG,ISKIP ! flag for RNG for event generators
c........
c   KHEP = 0-5 - regimes: 0/1: read/read+print table of particle properties
c   KGEN = number of generator, GENPAR(20) - parameters
      COMMON /KEDR_GEN/ KHEP,KGEN,GENPAR(20) 
      COMMON /KEDR_MBJS/ MOBRJS ! 0/1 - modification of Br Jetset 7.4
c
      REAL *4 RNFLUK
      COMMON /FLUKAP/  RNFLUK  ! version of FLUKA
c
      include 'kedrif.inc'
      include 'ver_event.inc'
      include 'beam_sigma.inc'
      include 'fiel.inc'
      include 'drawpar.inc'
c
      INTEGER *4 IHISTOG,KHEP,KGEN
c
      equivalence (IFIEL,FIEL)
c
      SAVE /FLUKAP/

      DATA Sigbeam_flag /0/
c.........................................................................
c
c     Format: KEDR ifvd ifdc iftf ifch ifkr ifcs ifmu ifir ifts ifvc
c
c     Flags for systems to be included:
c      0 - to swith the system off
c      1 - to include the geometry description
c      2 - to include HITS
c
      CALL FFKEY('KEDR',IFVD,10,'INTEGER')
c
c.........................................................................
c
c     /FIEL/  FIEL, FIEAN (magnetic field in kGs, maximal angle for step)  
c     fieldmap  = 1 to use field maps from current working directory
c
      CALL FFKEY('FIEL',IFIEL,3,'REAL')
c
c.........................................................................
c
c     /BMPR/ Sigbeam_flag, Sigma_X, Sigma_Y, Sigma_Z, Beam_Theta, Beam_Phi
c     Sigbeam_flag (integer) > 0 - to swith on beam spread
c
      CALL FFKEY('BMPR',Sigbeam_flag,6,'MIXED')
c
c.........................................................................
c
      CALL FFKEY('VRTX',vertx,3,'REAL')  ! VRTX Vert_X Vert_Y VErt_Z
c
      CALL VDCARD
c
      CALL DCCARD
c
      CALL LK_CARD
c
      CALL CsI_CARD
c
      CALL ATC_CARD
c
      CALL MUCARD
c
c.........................................................................

      CALL FFKEY('RZFZ',IRZFZ(1),2,'INTEGER') ! creation RZ and FZ files
c
      CALL FFKEY('DCUT',IAX,6,'M')	      ! drawing parameters
c
      CALL FFKEY('MBJS',MOBRJS,1,'INTEGER')   ! modificate Br of Jetset 7.4
c
      CALL FFKEY('HEPS',KHEP,1,'INTEGER')     ! HEP standard for particles 
c
      CALL FFKEY('GENE',KGEN,21,'M')          ! card for parameters of gen.
c
      CALL FFKEY('HIST',IHISTOG,1,'INTEGER')  ! card for histograms  
c
      CALL FFKEY('RNDG',IGENRNG,2,'INTEGER')  ! card for RNG of generators
c
      CALL FFKEY('FLUK',RNFLUK,1,'REAL')      ! card for FLUKA parameters
c
      END
