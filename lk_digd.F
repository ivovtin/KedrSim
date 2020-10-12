*CMZU:  1.00/01 16/10/96  17.53.46  by  MALYSHEV V.M.
*-- Author :
      SUBROUTINE LK_DIGD
C.
C.    *
C.    *      LK DIGI par-s definition
C.    *
C.
C
	include 'lk_digt.inc'
	include 'lk_hdsw.inc'
C
	include 'lk_digp.inc'
C
      PARAMETER(NAUTOC=300,NAUTOU=100)
      DIMENSION UAM(NAUTOU)

      NPDR=NHJ*FBC         ! num of drift points
      NPDRS=NHJS           !

C-- towers -----------------------------------------------
      DT=TDR/NPDR          ! MIcurrent&shaper curve
      DO NC=1,NHJ
        T=DT*(NC-1)
        CURMIBL(NC)=(1-T/TDR)*EXP(-T/TLIF)
      ENDDO
      DTU=TGMA/NPU
      DO NU=1,NPU
        TU=DTU*NU
        USHBL(NU)=RC2CR(TU,TAU)
      ENDDO

      DT=TDR/NPDR          ! charge dissipation
      DO NC=1,NHJ
        T=DT*(NC-1)
        CDBL(NC)=EXP(-T/TLIF)
      ENDDO

      DTU=(TGMA-TGMI)/NPU  ! shaper table
      DO NC=1,NHJ
        T=DT*(NC-1)
        DO NU=1,NPU
          TU=TGMI+DTU*NU
          RC2CRTBL(NC,NU)=RC2CR(TU-T,TAU)
        ENDDO
      ENDDO

! autoscaling
      CALL VZERO(UAM,NAUTOU)
      DT=TDR/3./NAUTOC
      DTU=TDR/3./NAUTOU
      DO NC=1,NAUTOC
        T=DT*(NC-1)
        IF(ILKHD(13).NE.0)CURRENT=1./TDR*(1.-T/TDR)*EXP(-T/TLIF)! 1MeV-current from MI-particle
        IF(ILKHD(13).EQ.0)CURRENT=1./TDR*(1.-T/TDR)	        ! 1MeV-current from MI-particle for Tlif=infinity
        DO NU=1,NAUTOU
          TU=DTU*NU
          UAM(NU)=UAM(NU)+CURRENT*RC2CR(TU-T,TAU)*DT
          IF(NC.EQ.1) UAM(NU)=UAM(NU)-CURRENT*RC2CR(TU-T,TAU)*DT/2.
        ENDDO
      ENDDO
      PRICE=1./VMAX(UAM,NAUTOU)  ! gain MeV/mV

C-----------------------------------------------------

C-- strips -------------------------------------------
      DT=TDR/NPDRS               ! MIcurrent&shaper curve
      DO NC=1,NHJS
        T=DT*(NC-1)
        CURMIST(NC)=(1-T/TDR)*EXP(-T/TLIF)
      ENDDO
      DTU=TGMAS/NPU
      DO NU=1,NPU
        TU=DTU*NU
        USHST(NU)=RC2CR(TU,TAUS)
      ENDDO

      DT=TDR/NPDRS               ! charge dissipation
      DO NC=1,NHJS
        T=DT*(NC-1)
        CDST(NC)=EXP(-T/TLIF)
      ENDDO

      DTU=(TGMAS-TGMIS)/NPU      ! shaper table
      DO NC=1,NHJS
        T=DT*(NC-1)
        DO NU=1,NPU
          TU=TGMIS+DTU*NU
          RC2CRTS(NC,NU)=RC2CR(TU-T,TAUS)
        ENDDO
      ENDDO

! autoscaling
      CALL VZERO(UAM,NAUTOU)
      DT=TDR/NAUTOC
      DTU=TDR/NAUTOU
      DO NC=1,NAUTOC
        T=DT*(NC-1)
        IF(ILKHD(14).NE.0)CURRENT=1./TDR*(1.-T/TDR)*EXP(-T/TLIF)! 1MeV-current from MI-particle
        IF(ILKHD(14).EQ.0)CURRENT=1./TDR*(1.-T/TDR)	        ! 1MeV-current from MI-particle for Tlif=infinity
        DO NU=1,NAUTOU
          TU=DTU*NU
          UAM(NU)=UAM(NU)+CURRENT*RC2CR(TU-T,TAUS)*DT
          IF(NC.EQ.1) UAM(NU)=UAM(NU)-CURRENT*RC2CR(TU-T,TAUS)*DT/2.
        ENDDO
      ENDDO
      PRICES=1./VMAX(UAM,NAUTOU) ! gain MeV/mV

C-----------------------------------------------------

      END
