        SUBROUTINE GUTREV
c**************************************************************
c
c   Control of particle tracking
c
c
c*-- Author :    ALEX UNDRUS   31/05/93
c
c  Last update:
c
c          28/05/2004    V.A.Tayursky
c
c****************************************************************

      PARAMETER ( NHMAX=500 , NHDIM=15 , NVDIM=4 )
c
#include "gcflag.inc"
#include "gcnum.inc"
#include "gconst.inc"
      include 'kedrif.inc'
      include 'lujeth.inc'

      COMMON /RZFZFILES/ IRZFILE,IFZFILE,ISIMOUT
      COMMON /HIS_SIMUL/ IHISTOG

C      PARAMETER (NSN=2) ! number of neibours for hits on strips
C      REAL*8 BFINOR,SFINOR,Clk,ALPHA
C      COMMON/LK_GEOMP/BFINOR,SFINOR,Clk,ALPHA,
C     1                BLKZ(17,-9:9),ZSTZ(-4:4,-124-NSN:124+NSN)
C
      DIMENSION NUMVS(NVDIM) , NUMBV(NVDIM,NHMAX) ,
     +          HITS(NHDIM,NHMAX) , ITRA(NHMAX)
C     DIMENSION IHT(NHMAX)
C
      integer NHITS

	Dimension vrt(3),pvrt(4),ubuf(20),ubufv(20)
	character *12 pnam

c
      DATA NUMVS/NVDIM*0/
c
      IF(ISWIT(10).GE.1.AND.IDEBUG.EQ.1) THEN
      PRINT *,' =========> Initial kinematic of the event: '
c
        CALL GPVERT(0)
c
        CALL GPKINE(0)
c
      ENDIF
C+++++++
c	print *,' bfinor: ',bfinor
c
         CALL GTREVE
c
c	print *,' bfinor: ',bfinor
C+++++++
      IF(ISWIT(10).GE.1.AND.IDEBUG.EQ.1) THEN
      PRINT *,' =========> Kinematic of the whole event: '
c
        CALL GPVERT(0)
c
        CALL GPKINE(0)
c
      ENDIF

      IF(ISWIT(9).GE.1) THEN
        NH=NTRACK            ! number of entries in history bank
c
        do i=1,ntrack

           call gfkine(i,vrt,pvrt,iprt,nvrt,ubuf,nubuf)

           call ppar(kffromkg(iprt),pnam,pmas,char,tlif)

c		print *,'i,patricle,vertex,pvert,nvertex=',
c	1	i,'  ',pnam,vrt,pvrt,nvrt

           call gfvert(nvrt,vrt,ntb,ntt,tofg,ubufv,nubufv)

           nme=ubuf(3)
c         if(nme.gt.6) nme=6
c         kh(i,1)=0

c         do j=1,nme	 ! save mechanisms
c         if(ubuf(3+j).le.30) kh(i,1)=kh(i,1)+ubuf(3+j)*30**(j-1)
c         enddo

           kh(i,1)=ubuf(3+nme)
           kh(i,2)=kffromkg(iprt)
           kh(i,3)=ntb
           ph(i,1)=pvrt(1)
           ph(i,2)=pvrt(2)
           ph(i,3)=pvrt(3)
           ph(i,4)=pvrt(4)
           ph(i,5)=pmas
           vh(i,1)=vrt(1)*10.   ! mm
           vh(i,2)=vrt(2)*10.
           vh(i,3)=vrt(3)*10.
           vh(i,4)=tofg*clight*10. ! mm
           vh(i,5)=(ubuf(1)-tofg)*clight*10. ! life time in mm
        enddo
c
        do i=1,ntrack           ! daughters and status
           nd2=0
c
           do j=1,ntrack
              if(kh(j,3).eq.i) nd2=j
           end do
c
           nd1=0
c
           do j=ntrack,1,-1
              if(kh(j,3).eq.i) nd1=j
           end do
c
c		kh(i,1)=11
c		if(nd2.eq.0) kh(i,1)=1
           kh(i,4)=nd1
           kh(i,5)=nd2
c
c 		call ppar(kh(i,2),pnam,pmas,char,tlif)
c		print *,'i,patricle,ks,parent,dot1,dot2=',
c	1	i,' ',pnam,kh(i,1),kh(i,3),kh(i,4),kh(i,5)
c		print *,'pvert',(ph(i,j),j=1,5)
c		print *,'vert',(vh(i,j),j=1,5)
        end do
c
c	do i=1,nvertx
c		call gfvert(i,vrt,ntb,ntt,tofg,ubuf,nubuf)
c		print *,'i,vertex,ntbeam,nttarg=',i,vrt,ntb,ntt,tofg
c	enddo
c
      ENDIF
c

      IF (IFCS .GT. 1) THEN
c
c ---> Special routines for CsI
c
c  read hits from ZEBRA bank
c
       CALL CSI_HITR
c
c  convert MC coordinates to REAL
c
       CALL CSI_MCCONRL
c
c --->
      ENDIF

C  PRINT INFORMATION ABOUT HITS
c
         IF(IDEBUG.EQ.1) CALL GPHITS('*','*')
C
C    Hits in Vertex Detector
C
      if(IFVD.gt.1) then
c
      CALL GFHITS('KVDH','STRD',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in VD > ',NHMAX
	NHITS = NHMAX
      ENDIF
c	print *,' call VD_DIG, nhits,idebug=',nhits,idebug
c
      CALL VD_DIG
c
      IF (NHITS.GT.0 .AND . IDEBUG.EQ.1) THEN
      PRINT 101,NHITS
      PRINT *,(NUMBV(4,K),K=1,NHITS)
      ENDIF
      endif

C
C    Hits in Drift Chamber
C
      if(IFDC.gt.1) then
C	print *,' call DCDIGI, idebug',idebug
c
      CALL DCDIGI
c
      CALL GFHITS('KDCH','DCC1',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in DC layer 1 > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'1 layer nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) THEN
      PRINT 102,NHITS
      PRINT *,(NUMBV(4,K),K=1,NHITS)
      ENDIF
C
      CALL GFHITS('KDCH','DCC2',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in DC layer 2 > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'2 layer nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) THEN
      PRINT 103,NHITS
      PRINT *,(NUMBV(1,K),K=1,NHITS)
      ENDIF
C
      CALL GFHITS('KDCH','DCC3',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in DC layer 3 > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'3 layer nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) THEN
      PRINT 104,NHITS
      PRINT *,(NUMBV(1,K),K=1,NHITS)
      ENDIF
C
      CALL GFHITS('KDCH','DCC4',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in DC layer 4 > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'4 layer nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) THEN
      PRINT 105,NHITS
      PRINT *,(NUMBV(1,K),K=1,NHITS)
      ENDIF
C
      CALL GFHITS('KDCH','DCC5',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in DC layer 5 > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'5 layer nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) THEN
      PRINT 106,NHITS
      PRINT *,(NUMBV(1,K),K=1,NHITS)
      ENDIF
C
      CALL GFHITS('KDCH','DCC6',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in DC layer 6 > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'6 layer nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) THEN
      PRINT 107,NHITS
      PRINT *,(NUMBV(1,K),K=1,NHITS)
      ENDIF
C
      CALL GFHITS('KDCH','DCC7',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in DC layer 7 > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'7 layer nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) THEN
      PRINT 108,NHITS
      PRINT *,(NUMBV(1,K),K=1,NHITS)
      ENDIF
      endif
c
c  TOF
c
      if(IFTF.gt.1) then
c
      CALL TOF_DIGI
c
      CALL GFHITS('KTFH','BS00',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in BS00 > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'BS00 nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) PRINT 109,NHITS

      CALL GFHITS('KTFH','ESL0',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in ESL0 > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'ESL0 nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 110,NHITS,NVDIM
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF

      CALL GFHITS('KTFH','ESP0',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
        PRINT *,' Warning!  Nhits in ESP0 > ',NHMAX
        NHITS = NHMAX
      ENDIF
C       print *,'ESP0 nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 110,NHITS,NVDIM
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
      endif
C
C    Hits in LKr calorimeter
C
      IF (IFKR .GT. 1) THEN
C	print *,'call LK_HITS'
c
       CALL LK_HITS
c
C	print *,'call LK_DIGI'
c
       CALL LK_DIGI
c
      ENDIF

c ------- Muon System ----------
c
      if(ifmu.gt.1) then
c
      call mu_ev
c
      endif
c
C--------------- ATC hits -----------------------------------
c
c   Hits in Cherenkov system
c
c  26/12/2015 : correct Ovtin I.V.
      if(IFCH.GT.1) then
c
      CALL ATC_DIGI
c
      CALL GFHITS('ATCH','TESI',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TESI > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TESI nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
c        PRINT 111,NHITS,NVDIM
c        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
        PRINT 111
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c
c2
      CALL GFHITS('ATCH','TEDI',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TEDI > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TEDI nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
c        PRINT 111,NHITS,NVDIM
c        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
        PRINT 111
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c3
      CALL GFHITS('ATCH','TESO',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TESO > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TESO nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 112
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c4
      CALL GFHITS('ATCH','TEDO',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TEDO > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TEDO nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 112
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c5
      CALL GFHITS('ATCH','TLSI',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TLSI > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TLSI nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 113
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c6
      CALL GFHITS('ATCH','TLDI',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TLDI > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TLDI nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 113
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c7
      CALL GFHITS('ATCH','TSSI',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TSSI > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TSSI nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 114
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c8
      CALL GFHITS('ATCH','TSDI',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TSDI > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TSDI nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 114
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c9
      CALL GFHITS('ATCH','TLSO',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TLSO > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TLSO nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 115
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c10
      CALL GFHITS('ATCH','TLDO',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TLDO > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TLDO nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 115
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c11
      CALL GFHITS('ATCH','TSSO',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TSSO > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TSSO nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 116
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c12
      CALL GFHITS('ATCH','TSDO',NVDIM,NHDIM,NHMAX,0,NUMVS,
     * ITRA,NUMBV,HITS,NHITS)
c
      IF (NHITS. GT. NHMAX) THEN
	PRINT *,' Warning!  Nhits in TSDO > ',NHMAX
	NHITS = NHMAX
      ENDIF
C	print *,'TSDO nhits,idebug=',nhits,idebug
      IF (NHITS.GT.0 .AND. IDEBUG.EQ.1) then
        PRINT 116
        PRINT *,((NUMBV(IV1,K),K=1,NHITS),IV1=1,2)
      ENDIF
c
      endif
c
c--------- Histograms and output file ---------------------------
c
 	IF(IHISTOG.EQ.1) CALL HIS_FILL   ! fill hisograms in simulation
c
c  Smth like trigger:
c
      IF(IHISTOG.EQ.1.AND.IWR_HIS.EQ.0) RETURN ! event does not hit 1-st hist.
c
c  ISIMOUT =1/0 - run with/without the output file (var. from /RZFZFILES/)
c
      IF(ISIMOUT.EQ.1) CALL FILERW
c
      RETURN
 101  FORMAT(/3X,'>>>>> Vertex detector: ',I3,' tubes were hit <<<<<')
 102  FORMAT(/3X,'>>>>> Drift chamber, Layer N 1: ',I3,
     +           ' cells were hit <<<<<')
 103  FORMAT(/3X,'>>>>> Drift chamber, Layer N 2: ',I3,
     +           ' cells were hit <<<<<')
 104  FORMAT(/3X,'>>>>> Drift chamber, Layer N 3: ',I3,
     +           ' cells were hit <<<<<')
 105  FORMAT(/3X,'>>>>> Drift chamber, Layer N 4: ',I3,
     +           ' cells were hit <<<<<')
 106  FORMAT(/3X,'>>>>> Drift chamber, Layer N 5: ',I3,
     +           ' cells were hit <<<<<')
 107  FORMAT(/3X,'>>>>> Drift chamber, Layer N 6: ',I3,
     +           ' cells were hit <<<<<')
 108  FORMAT(/3X,'>>>>> Drift chamber, Layer N 7: ',I3,
     +           ' cells were hit <<<<<')
 109  FORMAT(/3X,'>>>>> TOF barrel counters: ',I3,
     +           ' sectors were hit <<<<<')
 110  FORMAT(/3X,'>>>>> TOF edncap counters: ',2I3,
     +           ' sectors were hit <<<<<')
 111  FORMAT(/3X,'>>>>> ATC inner endcap counters were hit',I3)
 112  FORMAT(/3X,'>>>>> ATC outer endcap counters were hit',I3)
 113  FORMAT(/3X,'>>>>> ATC inner long barrel counters were hit',I3)
 114  FORMAT(/3X,'>>>>> ATC inner short barrel counters were hit',I3)
 115  FORMAT(/3X,'>>>>> ATC outer long barrel counters were hit',I3)
 116  FORMAT(/3X,'>>>>> ATC outer short barrel counters were hit',I3)
      END
