*KEEP,VDDATA.
C  Output of vertex detetor
C  NHITB - the number of hit tubes
C  INTB(j) - tube identification code
C  TRTB(j) - measured drift time, nsec
C  IDIGVD(2,j) - channel and time from KLUKVA
      integer NHITB, NDIGVD, INTB, IDIGVD         
      real    TRTB
      COMMON/VDDATA/ NHITB,NDIGVD,INTB(400),IDIGVD(800),TRTB(400)
