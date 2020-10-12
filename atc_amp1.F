        SUBROUTINE ATC_AMP1(NPE,AMP)
c*******************************************************************
c                                                                  *
c   This subroutine generates the amplitude from ATC counter       *
c   Other subroutines in this module: ATC_HIST, ATC_SPECN          *                             *
c                                                                  *
c   Input parameter: NPE - avarage number of photoelectrons        *                *
c                                                                  *
c   Output parameter: AMP - amplitude from ATC counter (in p-e)    *
c                                                                  *
c   Author: S.F.Ganzhur                                            *
c                                                                  *
c*******************************************************************
c
c  SPEC(500,I) - ampl. distribution from MCP PMT for integer I pe
c
      COMMON /ATCPMT/ RES, SPEC(500,0:25)
c
c  PMT(500) - array of amplitude distribution from MCP PMT for Npe.
c
      DIMENSION PMT(500), POI(0:25)
      REAL NPE
c........
c-> X axis scale parameters:
c     RES     - Resolution in phe/chennel (maximum 0.1 phe/ch)
c     NPE_MAX - maximum number of photoelectron
c     NCH     - number of channels
c
      DATA ICALL/0/
c
      IF(ICALL.GT.0) GOTO 1
c
      CALL ATC_HIST  ! preparatiion of arrays for ampl.generation
c                   
c**********************
c      PRINT 2, res
2     format(/,' ATC_AMP1: res=',1pe10.2,/)  
c**********************
      ICALL=1
c
1     IF (NPE .LT. 4.) THEN
      NPE_MAX = 12
      ELSE
      NPE_MAX = 3 * NPE
      END IF
c
      IF (NPE_MAX .GT. 25) NPE_MAX = 25
      NCH = NPE_MAX / RES
c.......
c  POI(I) - Poisson distribution for NPE
c
      POI(0)  = EXP(-NPE)
c
      DO J = 1,NPE_MAX
      POI(J)=(NPE**J)*EXP(-NPE)
c
      DO I=1,J
      POI(J)=POI(J)/I
      END DO
c
      END DO
c----------------------------------------------------
c---> PMT(I) calculation
c
      DO I =1,500
      PMT(I) = 0.
      END DO
c
      DO I = 0,NPE_MAX
c
      DO J=1,NCH
      PMT(J)=PMT(J)+POI(I)*SPEC(J,I)
      END DO
c
      END DO
c
c----------------------------------------------------------
c---> Transformation average number of photoelectrons from
c     the ATC counter to the real one taking into account
c     electronics and statistic distribution.
c----------------------------------------------------------
c-> Y axis scale parameter:
c
      RMAX = PMT(1)
c
      DO I = 1,NCH-1
      IF(RMAX .LE. PMT(I+1)) RMAX = PMT(I+1)
      END DO
c........
c-> X generation
c
999   CALL GRNDM(GAMX,1)
c
      GAMX = GAMX * NPE_MAX
      I_HI = (GAMX / RES) + 1
      FUN  = PMT(I_HI)
c........
c-> Y generation
c
      CALL GRNDM(GAMY,1)
c
      GAMY = GAMY * RMAX
c........
c---> Checking of condition
C
      IF(FUN.LT.GAMY) GOTO 999
      AMP = GAMX
      RETURN
      END

