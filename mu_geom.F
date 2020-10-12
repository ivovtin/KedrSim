      SUBROUTINE MUGEOM

c      IMPLICIT NONE
      include 'medpar.inc'
#include "gconst.inc"

      include 'mu_data.inc'
      include 'mu_par.inc'
      data mu_n_hits/0/

      INTEGER lun
      DATA lun/70/

      REAL PAR
      INTEGER IVOL
      DIMENSION PAR(10), NROT(8)

      integer medium
      parameter(medium=MEDMUG)


c NEW description with "TRD1" **********************************
      DO I=1,8
         CALL FREE_ROTMAT_NUMBER(NROT(I))
         CALL GSROTM(NROT(I),90.,45.*I-90.,180.,0.,90.,45.*I)
      ENDDO

      tan22 = TAN(22.5 * DEGRAD)
C	1ST LAYER
      Y1 = 204.
	  Y2 = Y1 + 8.
      PAR(1) = Y1 * tan22
      PAR(2) = Y2 * tan22
      PAR(3) = 225.
      PAR(4) = (Y2 - Y1) / 2.
      CALL GSVOLU('MSB1', 'TRD1', medium, par, 4, ivol)
      DO I = 1, 8
         X = (Y1 + Y2) / 2. * COS(45. * I * DEGRAD)
         Y = (Y1 + Y2) / 2. * SIN(45. * I * DEGRAD)
         CALL GSPOS('MSB1', I, 'KEDR', X, Y, 0., NROT(I), 'ONLY')
      ENDDO
C	2ND LAYER
      Y1 = 237.
	  Y2 = Y1 + 8.
      PAR(1) = Y1 * tan22
      PAR(2) = Y2 * tan22
      PAR(3) = 225.
      PAR(4) = (Y2-Y1)/2.
      CALL GSVOLU('MSB2','TRD1',medium,par,4,ivol)
      DO I = 1, 8
         X = (Y1 + Y2) / 2. * COS(45. * I * DEGRAD)
         Y = (Y1 + Y2) / 2. * SIN(45. * I * DEGRAD)
         CALL GSPOS('MSB2', I, 'KEDR', X, Y, 0., NROT(I), 'ONLY')
      ENDDO
C	3RD LAYER
      Y1 = 270.
      Y2 = Y1 + 8.
      PAR(1) = Y1 * tan22
      PAR(2) = Y2 * tan22
      PAR(3) = 225.
      PAR(4) = (Y2 - Y1) / 2.
      CALL GSVOLU('MSB3', 'TRD1', medium, par, 4, ivol)
      DO I = 1, 8
         X = (Y1 + Y2) / 2. * COS(45. * I * DEGRAD)
         Y = (Y1 + Y2) / 2. * SIN(45. * I * DEGRAD)
         CALL GSPOS('MSB3', I, 'KEDR', X, Y, 0., NROT(I), 'ONLY')
      ENDDO
C	4TH LAYER
      Y1 = 300.
      Y2 = Y1 + 8.
      PAR(1) = Y1 * tan22
      PAR(2) = Y2 * tan22
      PAR(3) = 250.
      PAR(4) = (Y2 - Y1) / 2.
      CALL GSVOLU('MSB4', 'TRD1', medium, par, 4, ivol)
      DO I = 1, 8
         X = (Y1 + Y2) / 2. * COS(45. * I * DEGRAD)
         Y = (Y1 + Y2) / 2. * SIN(45. * I * DEGRAD)
         CALL GSPOS('MSB4', I, 'KEDR', X, Y, 0., NROT(I), 'ONLY')
      ENDDO

      call mu_default_init()
      if(mu_debug.gt.0) then
	     print *, ' *** INIT MU SYSTEM ***'
		 print *, 'MU DEBUG LEVEL = ', mu_debug
		 print *, 'RESOLUTION = ', mu_resolution
		 print *, 'EFFICIENCY = ', mu_efficiency
		 print *, 'WRITE EVENTS = ', mu_write
      endif

      if(mu_write.gt.0) then
         open(unit=lun,name='./simout/raw.events',form='formatted',status='unknown')
      endif

      return
	  
      end
		
      
      SUBROUTINE MUHIT
		
      IMPLICIT NONE
		
      integer ISET, IDET
	  
      CALL GSDETV('MUON', 'MSB1', 7, 100, 0, ISET, IDET)
      CALL GSDETV('MUON', 'MSB2', 7, 100, 0, ISET, IDET)
      CALL GSDETV('MUON', 'MSB3', 7, 100, 0, ISET, IDET)
      CALL GSDETV('MUON', 'MSB4', 7, 100, 0, ISET, IDET)

      RETURN
		
      END
