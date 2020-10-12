      SUBROUTINE LK_BSN(IC,IM,IN,KR,KZ,KFI)
C     KFI,KZ/=0  => towers
C     KR<0+TOWERS=> non-feel. ADC
C     KFI=0      => Z  strips
C     KZ=0       => FI strips
      NTST=(IC-1)*16*32+(IM-1)*32+IN
      IF(NTST.LE.2304) THEN  ! towers
        NZ=(NTST-1)/48+1
        KFI=NTST-(NZ-1)*48
	IF(NZ.LE.18) KR=1
	IF(NZ.GT.18.AND.NZ.LE.18+16) THEN
          KR=2
          NZ=NZ-18
        ENDIF
	IF(NZ.GT.18+16) THEN
          KR=3
          NZ=NZ-18-16
        ENDIF
        KZ=NZ-(10-KR)-1
        IF(KZ.GE.0) KZ=KZ+1
      ENDIF
      IF(NTST.GT.2304.AND.NTST.LE.2304*2) THEN  ! towers/10
        NTST=NTST-2304
        NZ=(NTST-1)/48+1
        KFI=NTST-(NZ-1)*48
	IF(NZ.LE.18) KR=1
	IF(NZ.GT.18.AND.NZ.LE.18+16) THEN
          KR=2
          NZ=NZ-18
        ENDIF
	IF(NZ.GT.18+16) THEN
          KR=3
          NZ=NZ-18-16
        ENDIF
        KZ=NZ-(10-KR)-1
        IF(KZ.GE.0) KZ=KZ+1
        KR=-KR
      ENDIF

      IF(NTST.GT.2304*2.AND.NTST.LE.2304*2+1864) THEN  ! Z-strips
        NTST=NTST-2304*2
        IF(NTST.LE.218) KR=-4
        IF(NTST.GT.218.AND.NTST.LE.218+228) THEN 
          KR=-3
          NTST=NTST-218
        ENDIF
        IF(NTST.GT.218+228.AND.NTST.LE.218+228+238) THEN
          KR=-2
          NTST=NTST-218-228
        ENDIF
        IF(NTST.GT.218+228+238.AND.NTST.LE.218+228+238+248) THEN
          KR=-1
          NTST=NTST-218-228-238
        ENDIF
        IF(NTST.GT.218+228+238+248.AND.
     +     NTST.LE.218+228+238+248+248) THEN
          KR=1
          NTST=NTST-218-228-238-248
        ENDIF
        IF(NTST.GT.218+228+238+248+248.AND.
     +     NTST.LE.218+228+238+248+248+238) THEN
          KR=2
          NTST=NTST-218-228-238-248-248
        ENDIF
        IF(NTST.GT.218+228+238+248+248+238.AND.
     +     NTST.LE.218+228+238+248+248+238+228) THEN
          KR=3
          NTST=NTST-218-228-238-248-248-238
        ENDIF
        IF(NTST.GT.218+228+238+248+248+238+228.AND.
     +     NTST.LE.218+228+238+248+248+238+228+218) THEN
          KR=4
          NTST=NTST-218-228-238-248-248-238-228
        ENDIF
        KZ=NTST-(129-5*IABS(KR))-1
        IF(KZ.GE.0) KZ=KZ+1
        KFI=0
      ENDIF

      IF(NTST.GT.2304*2+1864) THEN  ! FI-strips
        NTST=NTST-2304*2-1864
        KR=(NTST-1)/768+1
        KFI=NTST-(KR-1)*768
        KZ=0
      ENDIF
     
      END
