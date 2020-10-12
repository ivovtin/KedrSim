      SUBROUTINE mu_get_pdata(track, octant, layer)

      IMPLICIT NONE
 
      INTEGER*4 IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN,
     +          IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
#include "gcflag.inc"

      integer MAXMEC,MAXME1
      REAL*4    VECT,GETOT,GEKIN,VOUT,TOFG,GEKRAT,UPWGHT,
     +          DESTEP,DESTEL,SAFETY,SLENG,STEP,SNEXT,SFIELD,POLAR
      INTEGER*4 NMEC,LMEC,NAMEC,NAMEC1,NSTEP,MAXNST,
     +          IGNEXT,INWVOL,ISTOP,IGAUTO,IEKBIN,
     +          ILOSL,IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
#include "gctrak.inc"

      INTEGER*4 NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,
     +          NLEVMX,NLDEV,LINMX
      REAL*4    GTRAN,GRMAT,GONLY,GLX
#include "gcvolu.inc"
		
      INTEGER*4 IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV
#include "gcsets.inc"

#include "gconsp.inc"

C--------------------------------------------------------------
      real*8 track(6)
	  integer*2 octant, layer
	  integer*2 oct(8)
	  
	  data oct/4,3,2,1,8,7,6,5/
	  
	  layer = idet
	  track(1) = vect(4)
	  track(2) = vect(5)
	  track(6) = vect(6)
	  track(4) = vect(1)
	  track(5) = vect(2)
	  track(6) = vect(3)
	  octant = oct(number(nlevel))
		
      END
