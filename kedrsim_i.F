      PROGRAM kedrsim_i
*
*     GEANT main program (interactive version).
*     To link with the MOTIF user interface the routine GPAWPP(NWGEAN,NWPAW) 
*     should be called, whereas the routine GPAW(NWGEAN,NWPAW) gives access 
*     to the basic graphics version.
*
      PARAMETER (NWGEAN=3000000,NWPAW=5000000)
      COMMON/GCBANK/GEANT(NWGEAN)
      COMMON/PAWC/PAW(NWPAW)

      COMMON/KSI/KSIFL

      include 'extdef.inc'
      
      KSIFL=1	! to identify interactive version
      CALL GPAWPP(NWGEAN,NWPAW)

      END
