*CMZU:          11/04/97  18.39.14  by  YURI SKOVPEN
*CMZU:  1.00/04 07/02/97  17.56.29  by  YURI SKOVPEN
*CMZ :  1.00/03 29/11/96  11.45.13  by  Unknown
*CMZ :  1.00/01 18/10/96  15.45.17  by  TANYA PURLATZ
*-- Author :
      SUBROUTINE LK_HITD
C.
C.    *
C.    *       LK HITS definition
C.    *
C.
	include 'dconst.inc'

	include 'lk_geomp.inc'

	include 'lk_curbl.inc'

	include 'lk_hitv.inc'

	include 'fgctc.inc'

	include 'lk_hitp.inc'

	include 'lk_digp.inc'

	include 'lk_eloss.inc'

	include 'lk_hdsw.inc'

c      include 'kdpath.inc'
      character *80 file

      PARAMETER (NH=4+NHJ)    ! towers
      CHARACTER *4 NAMESH(NH),NAMESV
      DIMENSION NBITSH(NH),ORIG(NH),FACT(NH)

      PARAMETER (NHS=4+NHJS)     ! strips
      CHARACTER *4 NAMESHS(NHS)
      DIMENSION NBITSHS(NHS),ORIGS(NHS),FACTS(NHS)

      REAL*8 RB,ALN,BN
      CHARACTER*2 CHN

C      PARAMETER(RE1=76.701,DLF=2.,DE=.056,DE2=.028)

      DATA NAMESV/'LKRY'/

C  take krypton parameters from LKGEOM card
      DLF=DRLAY	! electrode system layer thickness 
      DE=DREL	! electrode thickness

C***      Parameters for towers and strips numbers calculation

      PI=3.14159265358979324D0
      TWOPI=2.*PI
      DEGRAD=PI/180.
      BFINOR=TWOPI/48.     ! block angle(fi)
      SFINOR=TWOPI/768.    ! strip angle(fi)
      ALPHA=.46875*DEGRAD  ! strip angle(theta)
      C=PI/24.*1.0485      ! parameter for block's formula

C*** Block's Z-koordinates
      DO NL=1,17           ! layers
c        RB=74.7+4.*NL
        RB=REL1O-DRLAY+DRLAY*2.*NL
        ALN=0.
           DO NZ=1,8
            BN=C*DSQRT(RB*RB+ALN*ALN)
            ALN=ALN+BN
            BLKZ(NL,NZ)=ALN   ! Z coordinates of block
            BLKZ(NL,-NZ)=-ALN
        ENDDO
        BLKZ(NL,0)=0.
C        BLKZ(NL,9)=131.       ! Edge of cal-r on Z
C        BLKZ(NL,-9)=-131.
        BLKZ(NL,9)=DZAL2-DZAL       ! Edge of cal-r on Z (edge of LKr)
        BLKZ(NL,-9)=-BLKZ(NL,9)
      ENDDO

C*** Z-strips Z-coordinates
      DO NL=1,4               ! layers
C        RS=72.7+8.*NL
        RS=REL1O-DRLAY*2.+DRLAY*4.*NL
           DO NZ=0,124+NSN
            ZSTZ( NL, NZ)=RS*DTAN(NZ*ALPHA)  ! Z coordinates of Z-strip
            ZSTZ(-NL, NZ)= ZSTZ(NL,NZ)
            ZSTZ( NL,-NZ)=-ZSTZ(NL,NZ)
            ZSTZ(-NL,-NZ)=-ZSTZ(NL,NZ)
        ENDDO
      ENDDO

C
      CALL search_free_unit(LUNFIL)
C

C-- tower currents table ----------
         IF(ILKHD(1) .NE. 0) THEN    ! towers hits +
c      file=kdpath(:index(kdpath,' ')-1)//'lkr_block_table.dat'
      file='lkr_block_table.dat'
      OPEN(LUNFIL,FILE=file,
     +     STATUS='OLD',FORM='UNFORMATTED',ERR=345)
      GO TO 346
345   file=KEDR_RELEASE//'KEmcData/lkr_block_table.dat'
      OPEN(LUNFIL,FILE=file,
     +     STATUS='OLD',FORM='UNFORMATTED')
346   READ(LUNFIL),NXT,NYT,NZT,D,DY,DZ,ACC,Y1,Z1
      READ(LUNFIL),CUR
      CLOSE(LUNFIL)
!     call vzero(hbl,3*19*48*ntbl*nhj)
         ENDIF

C
      CALL search_free_unit(LUNFIL)
C

C-- strips currents table ----------
         IF((ILKHD(5).NE.0) .OR.(ILKHD(9).NE.0)) THEN ! strips hits +
c      file=kdpath(:index(kdpath,' ')-1)//'lkr_fgc_table.dat'
      file='lkr_fgc_table.dat'
      OPEN(LUNFIL,FILE=file,
     +     STATUS='OLD',FORM='UNFORMATTED',ERR=347)
      GO TO 348
347   file=KEDR_RELEASE//'KEmcData/lkr_fgc_table.dat'
      OPEN(LUNFIL,FILE=file,
     +     STATUS='OLD',FORM='UNFORMATTED')
348   READ(LUNFIL),DF,NAF,DAF,YMF,NYF,DYF
      READ(LUNFIL),CT,ET
      CLOSE(LUNFIL)
         ENDIF

C***      HITS definition
C--towers
      NPDR=NXC       ! number of drift points
      NPH=NHJ        ! number of  J points in HIT
      DL=DLF-DE      ! LKr gap thickness
      DA=DL/NPDR     ! dist. between neibour charge points
      RMIN=2.        ! min. dist to tower edge (in DL units)
      DTBL=100.      ! dt interval for tower hits (nS)

      NBITSV=1
      IDTYPE=5
      NV=1
      CALL GSDET(
     1'LK_C','LKRY',NV,NAMESV,NBITSV,IDTYPE,100,100,ISET,IDET)

      NAMESH(1)='NR'
      NAMESH(2)='NZ'
      NAMESH(3)='NFI'
      NAMESH(4)='TOF'
      DO I=1,NPH
	write(chn,1111) I
C        CALL OTS$CVT_L_TI(I,CHN)
        NAMESH(4+I)='J'//CHN
      ENDDO
1111	format(I2)

      DATA NBITSH/3,5,6,8,NHJ*32/
c      DATA ORIG/0.,10.,0.,0.,NHJ*1000./
      DATA ORIG/0.,10.,0.,0.,NHJ*10000./
      DATA FACT/3*1.,1.,NHJ*1e4/
      FACT(4)=1.0001/DTBL
      QBLMIN=GAIN/4.    ! min possible current in hit (MeV)

      CALL GSDETH('LK_C','LKRY',NH,NAMESH,NBITSH,ORIG,FACT)

C--strips
      NPDRS=NHJS     ! number of drift points
      NPHS=NHJS      ! number of J points in HIT
      DAS=DL/NPDRS   ! dist. between neibour charge points
      RMINS=2.       ! min. dist to tower edge (in DL units)
      DTST=250.      ! dt interval for strip hits (nS)

      CALL GSDETA('LK_C','LKRY','LKST',100,100,IUALI)

      NAMESHS(1)='KSTT'
      NAMESHS(2)='NR'
      NAMESHS(3)='NZFI'
      NAMESHS(4)='TOF'
      DO I=1,NPHS
	write(chn,1111) I
C        CALL OTS$CVT_L_TI(I,CHN)
        NAMESHS(4+I)='JS'//CHN
      ENDDO

      DATA NBITSHS/4,5,10,8,NHJS*32/
      DATA ORIGS/-10.,4.,130.,0.,NHJS*1000./
      DATA FACTS/3*1.,1.,NHJS*1e4/
      FACTS(4)=1.0001/DTST
      QSTMIN=GAINS/4.      ! min possible current in hit (MeV)

      CALL GSDETH('LK_C','LKST',NHS,NAMESHS,NBITSHS,ORIGS,FACTS)

cvk      IF((ILKHD(4).ne.0).OR.(ILKHD(8).ne.0).OR.(ILKHD(12).ne.0))
cvk     + CALL LK_HIST

      END
