*CMZU:  1.00/01 16/10/96  17.53.46  by  MALYSHEV V.M.
*-- Author :
      SUBROUTINE LK_HITS
C.
C.    *
C.    *         LK HITS storing
C.    *
C.
#include "gckine.inc"

#include "KedrSim/lk_hitp.inc"

#include "KedrSim/lk_curbl.inc"

#include "KedrSim/lk_hitv.inc"

#include "KedrSim/lk_hdsw.inc"

C ITRA не определена !!!!!!!!!!!!!!!!!!!!

      PARAMETER(NH=4+NHJ,NHS=4+NHJS,NHIMA=2000)
      REAL HITS(NH),HITSS(NHS)
C      REAL HITS2(NH,NHIMA)
C      REAL HITSS2(NHS,NHIMA)
C      INTEGER ITRA2(NHIMA),NUMBV2(1,NHIMA)
      CHARACTER NAMESV*4
C
      IF(ILKHD(1).ne.0) THEN    ! Towers HITS +

C--tower hits-------------------------------------
        CALL GFDET('LK_C','LKRY',NV,NAMESV,NBITSV,
     1     IDTYPE,NWHI,NWDI,ISET,IDET)
        DO NR=1,3
          DO NZ=-9,9
            DO NFI=1,48
              DO NT=1,NTBLA
                IF(HBL(NR,NZ,NFI,NT,1).NE.0.OR.
     1             HBL(NR,NZ,NFI,NT,2).NE.0.) THEN
                  HITS(1)=NR
                  HITS(2)=NZ
                  HITS(3)=NFI
                  HITS(4)=DTBL*(NTB(NT)-1)
                  DO NP=1,NPH
                    HITS(4+NP)=HBL(NR,NZ,NFI,NT,NP)
                    HBL(NR,NZ,NFI,NT,NP)=0.
                  ENDDO
                  QMAX=VMAX(HITS(5),NPH)
                  QMIN=ABS(VMIN(HITS(5),NPH))
                  IF(QMAX.GT.QBLMIN.OR.QMIN.GT.QBLMIN) THEN
                    CALL GSAHIT(ISET,IDET,ITRA,1,HITS,IHIT)
                    IF(IHIT.EQ.0) PRINT *
     $                 ,'****LK_HITS-TOWERS>hit not stored'
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF(ILKHD(2).ne.0) CALL GPHITS('LK_C','LKRY')


      IF(ILKHD(5).ne.0) THEN    ! Z-strips HITS +

C-- Z-strips hits-----------
        CALL GFDET('LK_C','LKRY',NV,NAMESV,NBITSV,
     1     IDTYPE,NWHI,NWDI,ISET,IDET)
        DO NRZ=-4,4
          DO NZZ=-124,124
            DO NT=1,NTSTA
              IF(NRZ.NE.0.AND.IABS(NZZ).LE.129-5*IABS(NRZ)) THEN
                IF(HSTZ(NRZ,NZZ,NT,1).NE.0.OR.
     1             HSTZ(NRZ,NZZ,NT,2).NE.0) THEN
                  HITSS(1)=11   ! to identifire Z-strips hit
                  HITSS(2)=NRZ
                  HITSS(3)=NZZ
                  HITSS(4)=DTST*(NTS(NT)-1)
                  DO NP=1,NPHS
                    HITSS(4+NP)=-HSTZ(NRZ,NZZ,NT,NP) ! current is negative
                    HSTZ(NRZ,NZZ,NT,NP)=0.
                  ENDDO
                  QMAX=VMAX(HITSS(5),NPHS)
                  QMIN=ABS(VMIN(HITSS(5),NPHS))
                  IF(QMAX.GT.QSTMIN.OR.QMIN.GT.QSTMIN) THEN
                    CALL GSAHIT(ISET,IDET+1,ITRA,1,HITSS,IHIT)
                    IF(IHIT.EQ.0) PRINT *,'****>z-hit not stored'
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
        
      ENDIF

      IF(ILKHD(9).ne.0) THEN    ! FI-strips HITS +

C-- FI-strips hits-----------
        CALL GFDET('LK_C','LKRY',NV,NAMESV,NBITSV,
     1     IDTYPE,NWHI,NWDI,ISET,IDET)
        DO NRFI=1,4
          DO NFIFI=1,768
            DO NT=1,NTSTA
              IF(HSTFI(NRFI,NFIFI,NT,1).NE.0.OR.
     1           HSTFI(NRFI,NFIFI,NT,2).NE.0.) THEN
                HITSS(1)=12     ! to identifire FI-strips hit
                HITSS(2)=NRFI
                HITSS(3)=NFIFI
                HITSS(4)=DTST*(NTS(NT)-1)
                DO NP=1,NPHS
                  HITSS(4+NP)=-HSTFI(NRFI,NFIFI,NT,NP) ! current is negative
                  HSTFI(NRFI,NFIFI,NT,NP)=0.
                ENDDO
                QMAX=VMAX(HITSS(5),NPHS)
                QMIN=ABS(VMIN(HITSS(5),NPHS))
                IF(QMAX.GT.QSTMIN.OR.QMIN.GT.QSTMIN) THEN
                  CALL GSAHIT(ISET,IDET+1,ITRA,1,HITSS,IHIT)
                  IF(IHIT.EQ.0) PRINT *,'****>fi-hit not stored'
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
        
      ENDIF

      IF((ILKHD(6).ne.0).OR.(ILKHD(10).ne.0))
     1   CALL GPHITS('LK_C','LKST')

      NTBLA=0
      DO I=1,NTBL
        NTB(I)=0
      ENDDO
C      CALL VZERO(NTB,NTBL)
      NTSTA=0
      DO I=1,NTST
        NTS(I)=0
      ENDDO
C      CALL VZERO(NTS,NTST)

      END
