      SUBROUTINE ATC_DIGI
c----------------------------------------------------------
c
c  Preparation of common-block with data from ATC for writing )
c
c   Changed  11.04.2001   V.A.Tayursky
c            14.10.2003   V.A.Tayursky
c	     21.12.2015   I.V.Ovtin
c----------------------------------------------------------
      integer nchdim,nchhmax,nsizehitatc
#if 0
      parameter (nchdim=3,nchhmax=500,nsizehitatc=32000)
#else
      parameter (nchdim=3,nchhmax=500,nsizehitatc=32000)
#endif
#include "gckine.inc"
#include "gctrak.inc"
c
      include 'atcrawhitspar.inc'
      include 'atchits.inc'
      include 'atcrawhits.inc'
c
      PARAMETER (IndAtcCNT=1,IndAtcX1=2,IndAtcY1=3,
     &  IndAtcZ1=4,IndAtcX2=5,IndAtcY2=6,IndAtcZ2=7,
     &  IndAtcIa_ch=8,IndAtcIa_sc=9,IndAtcIsh_ch=10,
     &  IndAtcIsh_sc=11,IndAtcItef_ch=12,IndAtcItef_sc=13,
     &  IndAtcNAME=14,IndAtcNLEVEL=15,NumParHitATC=15)
c
      COMMON /ATCHITS/ HITATC(NumParHitATC,nsizehitatc)
c
      dimension HITS(natcdimhits,nchhmax),NUMBV(nchdim,nchhmax)
      dimension intra(nchhmax)
      dimension NUMVS(nchdim)
      DATA NUMVS/nchdim*0/
      double precision Counter,Xatcin,Yatcin,Zatcin,Xatcout,Yatcout,Zatcout
      double precision Ia_ch,Ia_sc,Ish_ch,Ish_sc,Itef_ch,Itef_sc
      INTEGER I_NAME,I_NLEVEL
c
      CHARACTER*4 ATCCNAM(12)
      DATA ATCCNAM/'TESI','TEDI','TESO','TEDO','TLSI','TLDI',
     +           'TSSI','TSDI','TLSO','TLDO','TSSO','TSDO'/
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc   Извлечение хитов АЧС
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      NRAWHITSATC=0
      NHITSATCW=0
      ITRS=0
c
      J=0
      DO IL=1,12
        CALL GFHITS('ATCH',ATCCNAM(IL),NCHDIM,NATCDIMHITS,NCHHMAX,
     +ITRS,NUMVS,INTRA,NUMBV,HITS,NHITS)
#if 1
        if(NHITS.gt.NCHHMAX) then
           NHITS=NTCHMAX
           print *,'atc_digi: some hits are missing in ',ATCCNAM(IL)
        endif
#endif
c
        DO I=1,NHITS
          Counter = HITS(IatcCounter,I)        !number of counter
          Xatcin = HITS(IatcXin,I)
          Yatcin = HITS(IatcYin,I)
          Zatcin = HITS(IatcZin,I)
          Xatcout = HITS(IatcXout,I)
          Yatcout = HITS(IatcYout,I)
          Zatcout = HITS(IatcZout,I)
          Ia_ch = HITS(IatcIa_ch,I)
          Ia_sc = HITS(IatcIa_sc,I)
          Ish_ch = HITS(IatcIsh_ch,I)
          Ish_sc = HITS(IatcIsh_sc,I)
          Itef_ch = HITS(IatcItef_ch,I)
          Itef_sc = HITS(IatcItef_sc,I)
          I_NAME = HITS(IatcNAME,I)
          I_NLEVEL = HITS(IatcNLEVEL,I)
c
c            print *,'atc_digi: ATCCNAM(IL)=',ATCCNAM(IL)
c            print *,'atc_digi: Counter=',Counter
c            print *,'atc_digi: x1=',Xatcin,'  y1=',Yatcin,'  z1=',Zatcin
c            print *,'atc_digi: x2=',Xatcout,'  y2=',Yatcout,'  z2=',Zatcout
c            print *,'atc_digi: Ia_ch=',Ia_ch,'  Ish_ch=',Ish_ch,'  Itef_ch=',Itef_ch
c            print *,'atc_digi: Ia_sc=',Ia_sc,'  Ish_sc=',Ish_sc,'  Itef_sc=',Itef_sc
c            print *,'atc_digi: I_NAME=',I_NAME,'  I_NLEVEL=',I_NLEVEL
c            Radin=sqrt(Xatcin*Xatcin+Yatcin*Yatcin+Zatcin*Zatcin)
c            Radout=sqrt(Xatcout*Xatcout+Yatcout*Yatcout+Zatcout*Zatcout)
c            print *,'atc_digi: Radin=',Radin,'  Radout=',Radout
C
#if 1
          if(j.ge.nsizehitatc) then
             print *,'atc_digi: too many hits',j,', in',ATCCNAM(IL),
     &            NHITS
             goto 9900
          endif
#endif
          J=J+1
          HITATC(IndAtcCNT,J)  = Counter
          HITATC(IndAtcX1,J)   = Xatcin
          HITATC(IndAtcY1,J)   = Yatcin
          HITATC(IndAtcZ1,J)   = Zatcin
          HITATC(IndAtcX2,J)   = Xatcout
          HITATC(IndAtcY2,J)   = Yatcout
          HITATC(IndAtcZ2,J)   = Zatcout
          HITATC(IndAtcIa_ch,J) = Ia_ch
          HITATC(IndAtcIa_sc,J) = Ia_sc
          HITATC(IndAtcIsh_ch,J) = Ish_ch
          HITATC(IndAtcIsh_sc,J) = Ish_sc
          HITATC(IndAtcItef_ch,J) = Itef_ch
          HITATC(IndAtcItef_sc,J) = Itef_sc
          HITATC(IndAtcNAME,J) = I_NAME
          HITATC(IndAtcNLEVEL,J) = I_NLEVEL
C
          RAWHITSATC(NRAWHITSATC+iatc_CNT) = HITATC(IndAtcCNT,J)
          RAWHITSATC(NRAWHITSATC+ratc_X1)  = HITATC(IndAtcX1,J)
          RAWHITSATC(NRAWHITSATC+ratc_Y1)  = HITATC(IndAtcY1,J)
          RAWHITSATC(NRAWHITSATC+ratc_Z1)  = HITATC(IndAtcZ1,J)
          RAWHITSATC(NRAWHITSATC+ratc_X2)  = HITATC(IndAtcX2,J)
          RAWHITSATC(NRAWHITSATC+ratc_Y2)  = HITATC(IndAtcY2,J)
          RAWHITSATC(NRAWHITSATC+ratc_Z2)  = HITATC(IndAtcZ2,J)
          RAWHITSATC(NRAWHITSATC+ratc_Ia_ch)    = HITATC(IndAtcIa_ch,J)
          RAWHITSATC(NRAWHITSATC+ratc_Ia_sc)    = HITATC(IndAtcIa_sc,J)
          RAWHITSATC(NRAWHITSATC+ratc_Ish_ch)    = HITATC(IndAtcIsh_ch,J)
          RAWHITSATC(NRAWHITSATC+ratc_Ish_sc)    = HITATC(IndAtcIsh_sc,J)
          RAWHITSATC(NRAWHITSATC+ratc_Itef_ch)    = HITATC(IndAtcItef_ch,J)
          RAWHITSATC(NRAWHITSATC+ratc_Itef_sc)    = HITATC(IndAtcItef_sc,J)
          RAWHITSATC(NRAWHITSATC+ratc_I_NAME)    = HITATC(IndAtcNAME,J)
          RAWHITSATC(NRAWHITSATC+ratc_I_NLEVEL)    = HITATC(IndAtcNLEVEL,J)
C
          NRAWHITSATC = NRAWHITSATC + iatc_length
          NHITSATCW = NHITSATCW + 1
c          print *,'NRAWHITSATC=',NRAWHITSATC
        END DO
      END DO
c
c
c
 9900 RETURN
      END
