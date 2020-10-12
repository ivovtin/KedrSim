*CMZU:          11/04/97  17.26.42  by  YURI SKOVPEN
*CMZU:  1.00/01 16/10/96  17.53.46  by  MALYSHEV V.M.
*-- Author :
      SUBROUTINE LK_HIST
C
C     *       To book some histograms (ilkhd(4),(8),(12) for plottting)
C     *
	include 'lk_hitp.inc'
C
	include 'lk_digp.inc'

      PARAMETER(NPU=40)

C-- towers ---------------
      DT=TDR/NPDR
      DTU0=TGMA/NPU
      DTU=(TGMA-TGMI)/NPU
      CALL HBOOK1(1,'CURRENT FROM MI PART.$'
     *,NPH,-DT/2.,DT*(NPH+.5),0.)
      CALL HBOOK1(2,'SHAPER FUNCTION$'
     *,NPU,-DTU0/2.,TGMA+DTU0/2.,0.)
      CALL HBOOK1(3,'CURRENT, MeV$'
     *,NPH,-DT/2.,DT*(NPH+.5),0.)
      CALL HBOOK1(4,'U AFTER SHAPER, MeV$'
     *,NPU, TGMI-DTU/2.,TGMA+DTU/2.,0.)

C-- strips ---------------------
      DT=TDR/NPDRS
      DTU0=TGMAS/NPU
      DTU=(TGMAS-TGMIS)/NPU
      CALL HBOOK1(5,'CURRENT FROM MI PART.$'
     *,NPHS,-DT/2.,DT*(NPHS+.5), 0.)
      CALL HBOOK1(6,'SHAPER FUNCTION$'
     *, NPU,-DTU0/2.,TGMAS+DTU0/2.,0.)
      CALL HBOOK1(7,'CURRENT, MeV$'
     *,NPHS,-DT/2.,DT*(NPHS+.5),0.)
      CALL HBOOK1(8,'U AFTER SHAPER, MeV$'
     *, NPU,TGMIS-DTU/2.,TGMAS+DTU/2.,0.)

      END
