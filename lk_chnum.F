      SUBROUTINE LK_CHNUM(KR,KZ,KFI,NTST,IC,IM,IN)
C     KFI,KZ/=0  => towers
C     KR<0+TOWERS=> non-feel. ADC
C     KFI=0      => Z  strips
C     KZ=0       => FI strips
      IF(KZ.NE.0.AND.KFI.NE.0) THEN ! towers
        NR=IABS(KR)
        IF(NR.EQ.0.OR.NR.GT.3.OR.IABS(KZ).GT.10-KR.OR.
     +     KFI.LT.1.OR.KFI.GT.48)
     +     PRINT *,'******LK_CHNUM> TOWERS:KR,KZ,KFI=',KR,KZ,KFI
        IF(KZ.LT.0) NZ=KZ+(10-NR)+1
        IF(KZ.GT.0) NZ=KZ+(10-NR)
        IF(NR.EQ.1) NTST=KFI+(NZ-1)*48
        IF(NR.EQ.2) NTST=KFI+(NZ-1)*48+864
        IF(NR.EQ.3) NTST=KFI+(NZ-1)*48+864+768
        IF(NTST.LT.1.OR.NTST.GT.2304)
     +   PRINT *,'******LK_CHNUM> TOWERS:NTST,KR,KZ,KFI=',
     +   NTST,KR,KZ,KFI
        IF(KR.LT.0) NTST=NTST+2304
      ENDIF

      IF(KFI.EQ.0) THEN ! Z-strips
        IF(KR.EQ.0.OR.IABS(KR).GT.4.OR.KZ.EQ.0.
     +     OR.IABS(KZ).GT.129-5*IABS(KR))
     +     PRINT *,'******LK_CHNUM> Z-STRIPS:KR,KZ,KFI=',KR,KZ,KFI
        IF(KR.LT.0) NR=KR+5
        IF(KR.GT.0) NR=KR+4
        NZF=129-5*IABS(KR)
        IF(KZ.LT.0) NZ=KZ+NZF+1
        IF(KZ.GT.0) NZ=KZ+NZF
        IF(NR.EQ.1) NTST=2304*2+NZ
        IF(NR.EQ.2) NTST=2304*2+NZ+218
        IF(NR.EQ.3) NTST=2304*2+NZ+218+228
        IF(NR.EQ.4) NTST=2304*2+NZ+218+228+238
        IF(NR.EQ.5) NTST=2304*2+NZ+218+228+238+248
        IF(NR.EQ.6) NTST=2304*2+NZ+218+228+238+248+248
        IF(NR.EQ.7) NTST=2304*2+NZ+218+228+238+248+248+238
        IF(NR.EQ.8) NTST=2304*2+NZ+218+228+238+248+248+238+228
        IF(NTST.LE.2304*2.OR.NTST.GT.2304*2+1864)
     +   PRINT *,'******LK_CHNUM> Z-STRIPS:NTST,KR,KZ,KFI=',
     +   NTST,KR,KZ,KFI
      ENDIF

      IF(KZ.EQ.0) THEN ! FI-strips
        IF(KR.LT.1.OR.KR.GT.4.OR.KFI.LT.1.OR.KFI.GT.768)
     +   PRINT *,'******LK_CHNUM> FI-STRIPS:KR,KZ,KFI=',KR,KZ,KFI
        NTST=2304*2+1864+KFI+(KR-1)*768
        IF(NTST.LE.2304*2+1864.OR.NTST.GT.2304*2+1864+3072)
     +   PRINT *,'******LK_CHNUM> FI-STRIPS:NTST,KR,KZ,KFI=',
     +   NTST,KR,KZ,KFI
      ENDIF

      IC=(NTST-1)/32/16+1
      IM=(NTST-1-(IC-1)*512)/32+1
      IN=NTST-(IC-1)*512-(IM-1)*32

      END
