      SUBROUTINE CsI_CARD
C
#include "KedrSim/csi_digpar.inc"
#include "KedrSim/csi_xyzrot.inc"
C
      DATA CsI_Threshold/0.001/
C      DATA xyzrot/
C	1	      0., 0., 0.,  90.,180., 90. ,270.,  0.,  0.,
C	2	      0., 0., 0.,  90., 90., 90. ,180.,  0.,  0.,
C	3	      0., 0., 0.,  90.,  0., 90. , 90.,  0.,  0.,
C	4	      0., 0., 0.,  90.,270., 90. ,  0.,  0.,  0.,
C	5	      0., 0., 0.,  90.,180., 90. ,270.,  0.,  0.,
C	6	      0., 0., 0.,  90., 90., 90. ,180.,  0.,  0.,
C	7	      0., 0., 0.,  90.,  0., 90. , 90.,  0.,  0.,
C	8	      0., 0., 0.,  90.,270., 90. ,  0.,  0.,  0. /
      DATA xyzrot/	! new rotation: theta, gamma, phi (ideal positioning)
	1	      0., 0., 0.,   0.,  0.,  0. ,  0.,  0.,  0.,
	2	      0., 0., 0.,   0.,  0.,  0. ,  0.,  0.,  0.,
	3	      0., 0., 0.,   0.,  0.,  0. ,  0.,  0.,  0.,
	4	      0., 0., 0.,   0.,  0.,  0. ,  0.,  0.,  0.,
	5	      0., 0., 0.,   0.,  0.,  0. ,  0.,  0.,  0.,
	6	      0., 0., 0.,   0.,  0.,  0. ,  0.,  0.,  0.,
	7	      0., 0., 0.,   0.,  0.,  0. ,  0.,  0.,  0.,
	8	      0., 0., 0.,   0.,  0.,  0. ,  0.,  0.,  0. /
C
      CALL FFKEY ('CSIT', Csi_Threshold , 1, 'REAL')
C      
      CALL FFKEY ('CSI1', xyzrot(1,1), 9, 'REAL')
      CALL FFKEY ('CSI2', xyzrot(1,2), 9, 'REAL')
      CALL FFKEY ('CSI3', xyzrot(1,3), 9, 'REAL')
      CALL FFKEY ('CSI4', xyzrot(1,4), 9, 'REAL')
      CALL FFKEY ('CSI5', xyzrot(1,5), 9, 'REAL')
      CALL FFKEY ('CSI6', xyzrot(1,6), 9, 'REAL')
      CALL FFKEY ('CSI7', xyzrot(1,7), 9, 'REAL')
      CALL FFKEY ('CSI8', xyzrot(1,8), 9, 'REAL')
C
      RETURN
      END
