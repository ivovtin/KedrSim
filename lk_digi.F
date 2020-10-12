*CMZU:  1.00/03 31/10/96  13.17.48  by  MALYSHEV V.M.
*CMZU:  1.00/01 17/10/96  14.54.23  by  MALYSHEV V.M.
*-- Author :
      SUBROUTINE LK_DIGI
C.
C.    *
C.    *         Digitization
C.    *
C.
#include "gcflag.inc"
C
	include 'lk_eloss.inc'

	include 'lk_edep.inc'
C
	include 'lk_digt.inc'
C
	include 'lk_digp.inc'
C
	include 'lk_digv.inc'
C
	include 'lk_hdsw.inc'
C
	include 'lk_rec.inc'
C	
      PARAMETER(NH=4+NHJ,NHS=4+NHJS)
C      REAL HITS(NH),HITSS(NHS)
      REAL HITS2(NH,NHIMA)
      REAL HITSS2(NHS,NHIMA)
      INTEGER ITRA2(NHIMA),NUMBV2(1,NHIMA)
C
      DIMENSION CURBL(NHJ,NHIMA),CURSTZ(NHJS,NHIMA),CURSTFI(NHJS,NHIMA)

      DIMENSION UAM(NPU),AMPL(9600)
C      CHARACTER NAMESV*4
C      CHARACTER CR*2,CZ*4,CFI*3

      NPH=NHJ              ! num. of t-points in hit
      NPHS=NHJS            !
      NPDR=NHJ*FBC         ! num of drift points
      NPDRS=NHJS           !


      CALL VZERO(AMPL,9600)

            IF(ILKHD(3).ne.0) THEN ! Towers DIGI +

C** --Blocks energy deposition--
      NBL=0
      DO 1 NR=1,3
      DO 1 NZ=-10+NR,10-NR
      DO 1 NFI=1,48
      IF(ABL(NR,NZ,NFI).NE.0.AND.NZ.NE.0) THEN
         NBL=NBL+1
         IRBL(NBL) =NR
         IZBL(NBL) =NZ
         IFIBL(NBL)=NFI
         EBL(NBL)=ABL(NR,NZ,NFI)
         ABL(NR,NZ,NFI)=0.
        IF(ILKHD(2).ne.0) PRINT '(X,A,3I4,E12.4)','NR,NZ,NFI,EBL=',
     1                    NR,NZ,NFI,EBL(NBL)
      ENDIF
1     CONTINUE

C-- correct tower amplitudes ------
      CALL GFHITS('LK_C','LKRY',1,NH,NHIMA,0,1,
     1           ITRA2,NUMBV2,HITS2,NHITS)
c	print *,' lk tower hits: ',nhits
      IF(NHITS.GT.NHIMA) THEN
        PRINT *,'Warning! LK number of produced tower hits >',NHIMA
        NHITS=NHIMA
      ENDIF

      CALL VZERO(CURBL,NHJ*NHIMA)
      NBLHITS=0
      NRO=0
      NZO=0
      NFIO=0

         DO NHI=1,NHITS
      IF(HITS2(1,NHI).LT.10) THEN   ! tower hit
        NR =HITS2(1,NHI)
        NZ =HITS2(2,NHI)
        NFI=HITS2(3,NHI)
        TOF=HITS2(4,NHI)/1000.      ! mkS
        IF(NR.NE.NRO.OR.NZ.NE.NZO.OR.NFI.NE.NFIO) NBLHITS=NBLHITS+1
        KRBL (NBLHITS)=NR
        KZBL (NBLHITS)=NZ
        KFIBL(NBLHITS)=NFI
        NRO=NR
        NZO=NZ
        NFIO=NFI
        NC0=TOF/TDR*NPDR+.5
        DO NC=1,NPH                 ! currents
          NCT=NC+NC0
          IF(NCT.LE.NPH) CURBL(NCT,NBLHITS)=CURBL(NCT,NBLHITS)+
     1                  HITS2(4+NC,NHI)*CDBL(NC)
        ENDDO
      ENDIF
         ENDDO

      IF (NBLHITS.GT.0 .AND. IDEBUG.EQ.1) THEN
      PRINT 101,NBLHITS
      PRINT 102,(KRBL(K),KZBL(K),KFIBL(K),K=1,NBLHITS)
      ENDIF
 101  FORMAT(/3X,'>>>>> LKr calorimeter: ',I4,
     *           ' blocks were hit <<<<<')
 102  FORMAT(4(3I4,8X))

      NBLHA=0
         DO NHI=1,NBLHITS
      NR =KRBL (NHI)
      NZ =KZBL (NHI)
      NFI=KFIBL(NHI)
      CALL VZERO(UAM,NPU)
      DO NU=1,NPU       ! U amplitudes
        DO NC=1,NPH
          CURRENT=CURBL(NC,NHI)/NPDR
          UAM(NU)=UAM(NU)+CURRENT*RC2CRTBL(NC,NU)
            IF(NC.EQ.1) UAM(NU)=UAM(NU)-CURRENT*RC2CRTBL(NC,NU)/2.
        ENDDO
      ENDDO
      AMP=VMAX(UAM,NPU)*PRICE/GAIN	! Ampl in channels ADC
      CALL LKP2G(NR,NZ,NFI,IGIND)
      IF(IGIND.LT.1.OR.IGIND.GT.9600) STOP	! error
      AMPL(IGIND)=AMP
      IF(AMP.LE.0.) GO TO 31  ! no amp. in ADC
      NBLHA=NBLHA+1
      KRBL(NBLHA)=NR
      KZBL(NBLHA)=NZ
      KFIBL(NBLHA)=NFI
      CALL GRANOR(RANOIS,DUM)  ! R+E noise
      AMP=AMP+RANOIS*BNOISE/GAIN
      IAMP=AMP+IPED+0.5
      IF(IAMP.LT.0) IAMP=0
      IF(IAMP.GT.4095) IAMP=4095
      IAMPN=AMP/10.+IPED+.5  ! non-feel. ADC
      IF(IAMPN.LT.0) IAMPN=0
      IF(IAMPN.GT.4095) IAMPN=4095
      IAMBL(NBLHA)=IAMP
      IAMBLN(NBLHA)=IAMPN
      EAM=(IAMP-IPED)*GAIN
      EAMN=(IAMPN-IPED)*GAIN*10.
      IF(ILKHD(2).ne.0) PRINT '(X,A,3I4,2(I5,E12.4))',
     1           'NR,NZ,NFI,IAMP,EAM,IAMPN,EAMN=',
     1                       NR,NZ,NFI,IAMP,EAM,IAMPN,EAMN
31       ENDDO

      NBLHA=0	! 09.04.2004: new paradigm - write towers over pedestals...
      DO IGI=1,9600
		CALL LKG2P(IGI,KR,KZ,KFI)
		IF(KR.GT.0.AND.KZ.NE.0.AND.KFI.NE.0) THEN	! Towers
		CALL GRANOR(RANOIS,DUM)	! R+E noise
                AMP=AMPL(IGI)+RANOIS*BNOISE/GAIN
		IF(AMP.LE.BNOISE*BTHR/GAIN) GOTO 331
		IAMP=AMP+IPED+0.5
		IF(IAMP.LT.0) IAMP=0
		IF(IAMP.GT.4095) IAMP=4095
		NBLHA=NBLHA+1
		KRBL(NBLHA)=KR
		KZBL(NBLHA)=KZ
		KFIBL(NBLHA)=KFI
		IAMBL(NBLHA)=IAMP
		IF(ILKHD(2).ne.0) PRINT '(X,A,3I5,E12.4,E12.4)',
     1                      'KR,KZ,KFI,AMP(MeV),Threshold (MeV)=',
     1                       KR,KZ,KFI,AMP*GAIN,BNOISE*BTHR
331		CALL LKP2G(-KR,KZ,KFI,IGIN)
		IF(AMP.LE.BNOISE*BTHRN/GAIN) GOTO 332
		IAMP=AMP/10.+IPED+0.5
		IF(IAMP.LT.0) IAMP=0
		IF(IAMP.GT.4095) IAMP=4095
		NBLHA=NBLHA+1
		KRBL(NBLHA)=-KR
		KZBL(NBLHA)=KZ
		KFIBL(NBLHA)=KFI
		IAMBL(NBLHA)=IAMP
		IF(ILKHD(2).ne.0) PRINT '(X,A,3I5,E12.4,E12.4)',
     1                      'KR,KZ,KFI,AMP(MeV),Threshold (MeV)=',
     1                       -KR,KZ,KFI,AMP*GAIN,BNOISE*BTHRN
332	    ENDIF
      ENDDO

      NBLHITS=NBLHA

            ENDIF	! end of towers DIGI


            IF(ILKHD(7).ne.0) THEN ! Z- strips DIGI +

C** --Z-strips energy deposition--
      NSTZ=0
      DO NRZ=-4,4
        IF(NRZ.NE.0) THEN
      DO NZZ=-129+5*IABS(NRZ),129-5*IABS(NRZ)
        IF(ASTZ(NRZ,NZZ).NE.0..AND.NZZ.NE.0) THEN
         NSTZ=NSTZ+1
         IRZ(NSTZ)=NRZ
         IZZ(NSTZ)=NZZ
         ESTZ(NSTZ)=ASTZ(NRZ,NZZ)
         ASTZ(NRZ,NZZ)=0.
         IF(ILKHD(6).ne.0) PRINT '(X,A,2I4,E12.4)','NRZ,NZZ,ESTZ=',
     1                       NRZ,NZZ,ESTZ(NSTZ)
        ENDIF
      ENDDO
        ENDIF
      ENDDO


C-- correct Z-strips amplitudes ------
      CALL GFHITS('LK_C','LKST',1,NHS,NHIMA,0,1,
     1           ITRA2,NUMBV2,HITSS2,NHITS)
c	print *,' lk Z-strips hits : ',nhits
      IF(NHITS.GT.NHIMA) THEN
        PRINT *,'Warning! LK number of produced Z-strips hits >',NHIMA
        NHITS=NHIMA
      ENDIF

      CALL VZERO(CURSTZ,NHJS*NHIMA)
      NZHITS=0
      NRZO=0
      NZZO=0

         DO NHI=1,NHITS
      IF(HITSS2(1,NHI).EQ.11) THEN  ! Z-strips hit
        NRZ=HITSS2(2,NHI)
        NZZ=HITSS2(3,NHI)
        TOF=HITSS2(4,NHI)/1000.     ! mkS
        IF(NRZ.NE.NRZO.OR.NZZ.NE.NZZO) NZHITS=NZHITS+1
        KRZ(NZHITS)=NRZ
        KZZ(NZHITS)=NZZ
        NRZO=NRZ
        NZZO=NZZ
        NC0=TOF/TDR*NPDRS+.5
        DO NC=1,NPHS               ! currents
          NCT=NC+NC0
          IF(NCT.LE.NPHS) CURSTZ(NCT,NZHITS)=CURSTZ(NCT,NZHITS)+
     1                   HITSS2(4+NC,NHI)*CDST(NC)
        ENDDO
      ENDIF
         ENDDO

      IF (NZHITS.GT.0 .AND. IDEBUG.EQ.1) THEN
      PRINT 103,NZHITS
      PRINT 104,(KRZ(K),KZZ(K),K=1,NZHITS)
      ENDIF
 103  FORMAT(/3X,'>>>>> LKr calorimeter: ',I4,
     *           ' Z-strips were hit <<<<<')
 104  FORMAT(5(2I4,7X))

      NZHA=0
         DO NHI=1,NZHITS
      NRZ=KRZ(NHI)
      NZZ=KZZ(NHI)
      CALL VZERO(UAM,NPU)
      DO NU=1,NPU       ! U amplitudes
        DO NC=1,NPHS
          CURRENT=CURSTZ(NC,NHI)/NPDRS
          UAM(NU)=UAM(NU)+CURRENT*RC2CRTS(NC,NU)
            IF(NC.EQ.1) UAM(NU)=UAM(NU)-CURRENT*RC2CRTS(NC,NU)/2.
        ENDDO
      ENDDO
      AMP=VMAX(UAM,NPU)*PRICES/GAINS
      CALL LKP2G(NRZ,NZZ,0,IGIND)
      IF(IGIND.LT.1.OR.IGIND.GT.9600) STOP
      AMPL(IGIND)=AMP
      IF(AMP.LE.0.) GO TO 32 ! no amp. in ADC
      NZHA=NZHA+1
      KRZ(NZHA)=NRZ
      KZZ(NZHA)=NZZ
      CALL GRANOR(RANOIS,DUM)  ! R+E noise
      IAMP=AMP+RANOIS*SNOISE/GAINS+IPEDS+.5
      IF(IAMP.LT.0) IAMP=0
      IF(IAMP.GT.4095) IAMP=4095
      IAMSTZ(NZHA)=IAMP
      EAMS=(IAMP-IPEDS)*GAINS
      IF(ILKHD(6).ne.0) PRINT '(X,A,2I4,I5,E12.4)','NRZ,NZZ,IAMP,EAM=',
     1                       NRZ,NZZ,IAMP,EAMS
32       ENDDO

      NZHA=0	! 09.04.2004: new paradigm - write strips over pedestals...
	DO IGI=1,9600
		CALL LKG2P(IGI,KR,KZ,KFI)
		IF(KFI.EQ.0.AND.KZ.NE.0) THEN	! z-strips
		CALL GRANOR(RANOIS,DUM)	! R+E noise
		AMP=AMPL(IGI)+RANOIS*SNOISE/GAINS
		IF(AMP.LE.SNOISE*STHR/GAINS) GOTO 333
		IAMP=AMP+IPED+0.5
		IF(IAMP.LT.0) IAMP=0
		IF(IAMP.GT.4095) IAMP=4095
		NZHA=NZHA+1
		KRZ(NZHA)=KR
		KZZ(NZHA)=KZ
		IAMSTZ(NZHA)=IAMP
		IF(ILKHD(6).ne.0) PRINT '(X,A,2I5,E12.4,E12.4)',
     1                      'KR,KZ,AMP(MeV),Threshold (MeV)=',
     1                       KR,KZ,AMP*GAINS,SNOISE*STHR
333       ENDIF
	ENDDO

      NZHITS=NZHA

            ENDIF

            IF(ILKHD(11).ne.0) THEN   ! FI- strips DIGI +

C** --FI-strips energy deposition--
      NSTFI=0
      DO 3 NRFI=1,4
      DO 3 NFIFI=1,768
      IF(ASTFI(NRFI,NFIFI).NE.0.) THEN
         NSTFI=NSTFI+1
         IRFI(NSTFI) =NRFI
         IFIFI(NSTFI)=NFIFI
         ESTFI(NSTFI)=ASTFI(NRFI,NFIFI)
         ASTFI(NRFI,NFIFI)=0.
         IF(ILKHD(10).ne.0) PRINT '(X,A,2I4,E12.4)','NRFI,NFIFI,ESTFI=',
     1                     NRFI,NFIFI,ESTFI(NSTFI)
      ENDIF
3     CONTINUE


C-- correct FI-strips amplitudes ------
      CALL GFHITS('LK_C','LKST',1,NHS,NHIMA,0,1,
     1           ITRA2,NUMBV2,HITSS2,NHITS)
c	print *,' lk FI-strips hits : ',nhits
      IF(NHITS.GT.NHIMA) THEN
        PRINT *,'Warning! LK number of produced FI-strips hits >',NHIMA
        NHITS=NHIMA
      ENDIF

      CALL VZERO(CURSTFI,NHJS*NHIMA)
      NFIHITS=0
      NRFIO=0
      NFIFIO=0

         DO NHI=1,NHITS
      IF(HITSS2(1,NHI).EQ.12) THEN  ! FI-strips hit
        NRFI =HITSS2(2,NHI)
        NFIFI=HITSS2(3,NHI)
        TOF  =HITSS2(4,NHI)/1000.   ! mkS
        IF(NRFI.NE.NRFIO.OR.NFIFI.NE.NFIFIO) NFIHITS=NFIHITS+1
        KRFI (NFIHITS)=NRFI
        KFIFI(NFIHITS)=NFIFI
        NRFIO=NRFI
        NFIFIO=NFIFI
        NC0=TOF/TDR*NPDRS+.5
        DO NC=1,NPHS                ! currents
          NCT=NC+NC0
          IF(NCT.LE.NPHS) CURSTFI(NCT,NFIHITS)=CURSTFI(NCT,NFIHITS)+
     1                   HITSS2(4+NC,NHI)*CDST(NC)
        ENDDO
      ENDIF
         ENDDO

      IF (NFIHITS.GT.0 .AND. IDEBUG.EQ.1) THEN
      PRINT 105,NZHITS
      PRINT 106,(KRFI(K),KFIFI(K),K=1,NFIHITS)
      ENDIF
 105  FORMAT(/3X,'>>>>> LKr calorimeter: ',I4,
     *           ' FI-strips were hit <<<<<')
 106  FORMAT(5(2I4,7X))

      NFIHA=0
         DO NHI=1,NFIHITS
      NRFI =KRFI (NHI)
      NFIFI=KFIFI(NHI)
      CALL VZERO(UAM,NPU)
      DO NU=1,NPU       ! U amplitudes
        DO NC=1,NPHS
          CURRENT=CURSTFI(NC,NHI)/NPDRS
          UAM(NU)=UAM(NU)+CURRENT*RC2CRTS(NC,NU)
            IF(NC.EQ.1) UAM(NU)=UAM(NU)-CURRENT*RC2CRTS(NC,NU)/2.
        ENDDO
      ENDDO
      AMP=VMAX(UAM,NPU)*PRICES/GAINS
      CALL LKP2G(NRFI,0,NFIFI,IGIND)
      IF(IGIND.LT.1.OR.IGIND.GT.9600) STOP
      AMPL(IGIND)=AMP
      IF(AMP.LE.0.) GO TO 33 ! no amp. in ADC
      NFIHA=NFIHA+1
      KRFI(NFIHA)=NRFI
      KFIFI(NFIHA)=NFIFI
      CALL GRANOR(RANOIS,DUM)  ! R+E noise
      IAMP=AMP+RANOIS*SNOISE/GAINS+IPEDS+.5
      IF(IAMP.LT.0) IAMP=0
      IF(IAMP.GT.4095) IAMP=4095
      IAMSTFI(NFIHA)=IAMP
      EAMS=(IAMP-IPEDS)*GAINS
      IF(ILKHD(10).ne.0) PRINT '(X,A,2I4,I5,E12.4)',
     1                      'NRFI,NFIFI,IAMP,EAM=',
     1                       NRFI,NFIFI,IAMP,EAMS
33       ENDDO

      NFIHA=0	! 09.04.2004: new paradigm - write strips over pedestals...
      DO IGI=1,9600
	  CALL LKG2P(IGI,KR,KZ,KFI)
          IF(KZ.EQ.0.AND.KFI.NE.0) THEN	! phi-strips
		CALL GRANOR(RANOIS,DUM)	! R+E noise
		AMP=AMPL(IGI)+RANOIS*SNOISE/GAINS
		IF(AMP.LE.SNOISE*STHR/GAINS) GOTO 334
		IAMP=AMP+IPED+0.5
		IF(IAMP.LT.0) IAMP=0
		IF(IAMP.GT.4095) IAMP=4095
		NFIHA=NFIHA+1
		KRFI(NFIHA)=KR
		KFIFI(NFIHA)=KFI
		IAMSTFI(NFIHA)=IAMP
		IF(ILKHD(10).ne.0) PRINT '(X,A,2I5,E12.4,E12.4)',
     1                      'KR,KFI,AMP(MeV),Threshold (MeV)=',
     1                       KR,KFI,AMP*GAINS,SNOISE*STHR
334       ENDIF
	ENDDO

      NFIHITS=NFIHA

            ENDIF

C Data in KLUKVA format...
C towers
      DO I=1,NBLHITS
	  CALL LKP2G(KRBL(I),KZBL(I),KFIBL(I),IGIND)
        IBL(I*2-1)=IGIND
        IBL(I*2  )=IAMBL(I)
      ENDDO
cC towers/10
c      DO I=1,NBLHITS
c	CALL LKP2G(-KRBL(I),KZBL(I),KFIBL(I),IGIND)
c        IBL10(I*2-1)=IGIND
c        IBL10(I*2  )=IAMBLN(I)
c      ENDDO
C Z-strips
      DO I=1,NZHITS 
        CALL LKP2G(KRZ(I),KZZ(I),0,IGIND)
        ISTZ(I*2-1)=IGIND
        ISTZ(I*2  )=IAMSTZ(I)
      ENDDO
C FI-strips      
      DO I=1,NFIHITS  
        CALL LKP2G(KRFI(I),0,KFIFI(I),IGIND)
        ISTFI(I*2-1)=IGIND
        ISTFI(I*2  )=IAMSTFI(I)
      ENDDO

      JBL=NBLHITS
      JSTZ=NZHITS
      JSTFI=NFIHITS

      END
