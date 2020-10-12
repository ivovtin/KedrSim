      SUBROUTINE GUSTEP
c
c******************************************************************
c
c   User routine called at the end of each tracking step
c   INWVOL is different from 0 when the track has reached
c   a volume boundary
c   ISTOP is different from 0 if the track has stopped
c
c   ==>Called by : GTRACK
c
c  Last update:
c          28/11/2003 V.A.Tayursky
c  Update for ATC:
c          15/12/2015 I.V.Ovtin
c
c******************************************************************
c
!#define _CSIDBG_ 1
c
#include "gconst.inc"
#include "gcflag.inc"
#include "gckine.inc"
#include "gcking.inc"
#include "gcphys.inc"
#include "gcsets.inc"
#include "gctmed.inc"
#include "gctrak.inc"
#include "gcvolu.inc"
#include "KedrSim/medpar.inc"
#include "KedrSim/vd_map.inc"
#include "KedrSim/csisize.inc"
#include "KedrSim/atccom.inc"
#include "KedrSim/atchits.inc"
#include "KedrSim/mu_par.inc"
#include "KedrSim/drawpar.inc"
#include "KedrSim/fiel.inc"
#include "KedrSim/dchits.inc"
#include "KedrSim/vdhits.inc"
#include "KedrSim/tofhits.inc"
#include "ReadNat/re_def.inc"
#include "ReadNat/ss_def.inc"


c	COMMON /GCMATE/ NMAT,NAMATE(5)
#include "gcmate.inc"

        COMMON /KSI/KSIFL
        COMMON /KEDR_GEN/KHEP,KGEN,GENPAR(20) ! input parameters for generators
c      PARAMETER (NSN=2) ! number of neibours for hits on strips
c      REAL*8 BFINOR,SFINOR,Clk,ALPHA
c      COMMON/LK_GEOMP/BFINOR,SFINOR,Clk,ALPHA,
c     1                BLKZ(17,-9:9),ZSTZ(-4:4,-124-NSN:124+NSN)
      dimension HITS(24),UBUF(24)
c
      DATA JPRST/0/,IENT/0/,RM/1000./,Z/0./
      DATA IEVENT0/0/
      double precision Xinp,Yinp,Zinp,Xout,Yout,Zout,PtrIn,PtrOut
      double precision PpartIn,PxIn,PyIn,PzIn,PpartOut,PxOut,PyOut
      double precision TrackRIn,TrackROut,TrackR
      double precision TrLengthIn,TrLengthOut,DLength
      double precision Mpart,EpartIn,EpartOut
c
      SAVE IEVENT0
#ifdef _CSIDBG_
      integer CsIhistBooked/0/,CsIhistEntries/0/
      real ZinCrystal
      save CsIHistBooked,CsIhistEntries
#endif
c
c.   ------------------------------------------------------------------
c     if(BFINOR .eq. 0.) print *,' GUSTEP 1: bfinor=',bfinor
c
#ifdef _CSIDBG_
      if(CsIhistBooked.eq.0) then
         call HBOOK1(903,'CsI abs(z)',400,80.,120.,0.)
         call HBOOK1(911,'CsI [D]x',200,-4.,4.,0.)
         call HBOOK1(912,'CsI [D]y',200,-4.,4.,0.)
         call HBOOK1(913,'CsI [D]z',360,-9.,9.,0.)
         call HBOOK1(921,'E1 CsI [D]x',200,-4.,4.,0.)
         call HBOOK1(922,'E1 CsI [D]y',200,-4.,4.,0.)
         call HBOOK1(923,'E1 CsI [D]z',360,-9.,9.,0.)
         call HBOOK1(931,'E2 CsI [D]x',200,-4.,4.,0.)
         call HBOOK1(932,'E2 CsI [D]y',200,-4.,4.,0.)
         call HBOOK1(933,'E2 CsI [D]z',360,-9.,9.,0.)
         CsIhistBooked=1
      endif
# if _CSIDBG_ > 1
      CALL GPCXYZ
# endif
#else
      IF (ISWIT(10).GE.3.AND.IDEBUG.EQ.1) CALL GPCXYZ  ! prints tracking, etc parameters
#endif
c
	IF (KSIFL.EQ.1.AND.ISWIT(4).EQ.2) THEN	! draw tracks during tracking
	  CALL GDCXYZ
	ENDIF

      IF ((IAX.GE.1.AND.IAX.LE.3).AND.KSIFL.NE.1) THEN
c
c        IF(IEVENT.EQ.1) GOTO 333
        IF(IEVENT.NE.IEVENT0) THEN
c
          call iclrwk(0,1)            !  clear screen
c
          call gdrawc('KEDR',iax,cutval,u0,v0,su,sv) ! picture of KEDR
c
          IEVENT0=IEVENT              ! new
c
        END IF                        ! new
c
333	CALL GDCXYZ  ! drawing tracks on-line
c
      END IF
c
      IF(KGEN.EQ.312) CALL GEN_TPRINTS ! test prints
c
c.   ------------------------------------------------------------------
c   SAVE GENERATED SECONDARIES
c
      IF (NGKINE .GT. 0) CALL KEDRKING
c
c	if(NSTEP.gt.9990) then
c	  print *,'NSTEP > 9990: nmat,numed,idtype,ihset,ihdet,itra: '
c	  print 1111,nmat,numed,idtype,iset,idet,itra
c1111	  format(x,6i)
c	  print *,'x,y,z :',vect(1),vect(2),vect(3)
c	endif
c
c--------------------------------------------------------------------
c
c   Filling HITS for detector systems:
c
c--------------------------------------------------------------------
c
c      IF CHARGED PARTICLE REACHES VD
c
c--------------------------------------------------------------------
c
      IF(CHARGE*ISVOL.NE.0.AND.IDTYPE.EQ.1) THEN
      IF(INWVOL.EQ.1)THEN
      DEPOS = 0.
      IENT=1
      JUV=NUMBER(NLEVEL-1)

      XTB=XP(JUV)
      YTB=YP(JUV)
      X1=VECT(1)
      Y1=VECT(2)
      X=X1-XTB
      Y=Y1-YTB
c      PRINT *,' GUSTEP, Tube ', JUV,' entry point X,Y=',X,Y
      RM=SQRT(X*X+Y*Y)
      Xinp = VECT(1)
      Yinp = VECT(2)
      Zinp = VECT(3)
      TrLengthIn=SLENG
      Mpart=AMASS
      PpartIn=VECT(7)
      EpartIn=GETOT
      PxIn=VECT(4)
      PyIn=VECT(5)
      PzIn=VECT(6)
      PtrIn=PpartIn*sqrt(PxIn*PxIn+PyIn*PyIn)
      if(PtrIn.gt.0) then
        if(FIEL.ne.0) then
          TrackRIn=abs(PtrIn*1000./(0.299792458*FIEL*CHARGE)) ! cm FIEL[kGs]
        else
          TrackRIn=100000. ! cm
        endif
      else
        TrackRIn=0
      endif
      GOTO 111
      ENDIF
      DEPOS = DEPOS + DESTEP
      IF(INWVOL.EQ.0)THEN
       IF(IENT.EQ.1)THEN
       JPRST=JPRST+1
       IENT=0
       ENDIF
      X=VECT(1)-XTB
      Y=VECT(2)-YTB
      R=SQRT(X*X+Y*Y)
         IF(R.LT.RM)THEN
         Z=VECT(3)
         RM=R
         ENDIF
c        IF(JPRST.LE.2)print 1000,JUV,X,Y,R,RM
c1000    FORMAT(2X,' J,X,Y,R,RM=',I3,',',4F10.3)
      GOTO 111
      ENDIF
      DEPOS = DEPOS + DESTEP
c      IF(INWVOL.EQ.2)THEN
      IF ((INWVOL.EQ.2 .OR. ISTOP.NE.0).AND.(DEPOS .GT. 0..or.
     &         ILOSS.EQ.0) ) THEN
      X2=VECT(1)
      Y2=VECT(2)
      RM=SQRT(((X1+X2)*0.5-XTB)**2+((Y1+Y2)*0.5-YTB)**2)
      DY=(Y1+Y2)*0.5-YTB
c      PRINT 3000,JUV,XTB,DY,X1,X2,RM
c3000  FORMAT(1X,'JUV,XT,DY,X1,X2,RM=',I4,F9.4,F8.4,2F9.4,F7.4)
      X=X2-XTB
      Y=Y2-YTB
c     PRINT *,' GUSTEP, Tube ', JUV,' Track ',ITRA,', Part',IPART
c      HITS(1)=XTB
c      HITS(2)=YTB
c      HITS(3)=Z
c      HITS(4)=GETOT
c      HITS(5)=RM
c      HITS(6)=IPART
c      XV=VERT(1)
c      YV=VERT(2)
c      RO=SQRT(XV*XV+YV*YV)
c      HITS(7)=RO
c      HITS(8)=0.
c      HITS(9)=0.
c      HITS(10)=0.

      Xout = VECT(1)
      Yout = VECT(2)
      Zout = VECT(3)
      PpartOut=VECT(7)
      PxOut=VECT(4)
      PyOut=VECT(5)
      PzOut=VECT(6)
      EpartOut=GETOT
      PtrOut=PpartOut*sqrt(PxOut*PxOut+PyOut*PyOut)
      if(PtrOut.gt.0) then
        if(FIEL.ne.0) then
          TrackROut=abs(PtrOut*1000./(0.299792458*FIEL*CHARGE)) ! cm FIEL[kGs]
        else
          TrackROut=100000. ! cm
        endif
      else
        TrackROut=0
      endif
          TrackR=(TrackRIn+TrackROut)*0.5
          TrLengthOut=SLENG
          DLength= TrLengthOut-TrLengthIn
          HITS(IvdXTB) =XTB
          HITS(IvdYTB) =YTB
          HITS(IvdXin) =Xinp
          HITS(IvdYin) =Yinp
          HITS(IvdZin) =Zinp
          HITS(IvdXout)=Xout
          HITS(IvdYout)=Yout
          HITS(IvdZout)=Zout
          HITS(IvdPXin)=PxIn
          HITS(IvdPYin)=PyIn
          HITS(IvdPZin)=PzIn
          HITS(IvdPXout)=PxOut
          HITS(IvdPYout)=PyOut
          HITS(IvdPZout)=PzOut
          HITS(IvdR)    =TrackR
          HITS(IvdTrack)    = ITRA+10000*ISTAK
          HITS(IvdCHARGE)   = CHARGE
          HITS(IvdMASS)     = AMASS
          HITS(IvdAmp)      = DEPOS
          HITS(IvdLen)      = DLength
          HITS(IvdPpOut)    = PpartOut
          HITS(IvdPpIn)     = PpartIn
          HITS(IvdEin)      = EpartIn
          HITS(IvdEout)     = EpartOut
      CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITS,IHIT) ! stores inf. for hit
          DEPOS=0.
c
      ENDIF
        ENDIF
 111  CONTINUE
c-------------------------------------------------------------------
c
c      IF CHARGED PARTICLE REACHES DC
c
c-------------------------------------------------------------------
c

        IF(CHARGE*ISVOL.NE.0.AND.IDTYPE.EQ.2) THEN
          if(abs(vect(5)).lt.1.e-30 .and. abs(vect(4)).lt.1.e-30) then
            print *,'x=',vect(1),'y=',vect(2),'z=',vect(3),'P=',vect(7)
            print *,'Px/P=',vect(4),'Py/P=',vect(5),'Pz/P=',vect(6)
          else
         IF (INWVOL.EQ.1) THEN
           DEPOS = 0.
           Xinp = VECT(1)
           Yinp = VECT(2)
           Zinp = VECT(3)
           HITS(IndTOF)=TOFG*1.E9
           TrLengthIn=SLENG
           Mpart=AMASS
           PpartIn=VECT(7)
           EpartIn=GETOT
           PxIn=VECT(4)
           PyIn=VECT(5)
           PzIn=VECT(6)
           PtrIn=PpartIn*sqrt(PxIn*PxIn+PyIn*PyIn)
        if(PtrIn.gt.0) then
          if(FIEL.ne.0) then
            TrackRIn=abs(PtrIn*1000./(0.299792458*FIEL*CHARGE)) ! cm FIEL[kGs]
          else
            TrackRIn=100000. ! cm FIEL
          endif
        else
             TrackRIn=0
        endif
         ENDIF
         DEPOS = DEPOS + DESTEP
         IF ((INWVOL.EQ.2 .OR. ISTOP.NE.0) .AND.(DEPOS .GT. 0..or.
     &         ILOSS.EQ.0) ) THEN
          Xout = VECT(1)
          Yout = VECT(2)
          Zout = VECT(3)
          PpartOut=VECT(7)
          PxOut=VECT(4)
          PyOut=VECT(5)
          PzOut=VECT(6)
          EpartOut=GETOT
          PtrOut=PpartOut*sqrt(PxOut*PxOut+PyOut*PyOut)
          if(PtrOut.gt.0) then
             if(FIEL.ne.0) then
               TrackROut=abs(PtrOut*1000./(0.299792458*FIEL*CHARGE)) ! cm FIEL[kGs]
             else
               TrackROut=100000. ! cm
             endif
          else
            TrackROut=0
          endif
          TrackR=(TrackRIn+TrackROut)*0.5
          TrLengthOut=SLENG
          DLength= TrLengthOut-TrLengthIn
          HITS(IndXin) = Xinp
          HITS(IndYin) = Yinp
          HITS(IndZin) = Zinp
          HITS(IndXout)=Xout
          HITS(IndYout)=Yout
          HITS(IndZout)=Zout
          HITS(IndPXin)=PxIn
          HITS(IndPYin)=PyIn
          HITS(IndPZin)=PzIn
          HITS(IndPXout)=PxOut
          HITS(IndPYout)=PyOut
          HITS(IndPZout)=PzOut
          HITS(IndR)   = TrackR
          HITS(IndTrack)    = ITRA+10000*ISTAK
          HITS(IndCHARGE)   = CHARGE
          HITS(IndMASS)     = AMASS
          HITS(IndAmp)        = DEPOS
          HITS(IndLength)    = DLength
          HITS(IndPpOut)    = PpartOut
          HITS(IndPpIn)      = PpartIn
          HITS(IndEin)         = EpartIn
          HITS(IndEout)       = EpartOut
          CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITS,IHIT)
          DEPOS = 0.
         ENDIF
         ENDIF
       ENDIF
c
c-----------------------------------------------
c
c        IF CHARGED PARTICLE REACHES ATC
c
c-----------------------------------------------
c
      IF( CHARGE*ISVOL.NE.0. .AND. IDTYPE.EQ.4) then
c
        NAME=NAMES(NLEVEL)
c
	IF(INWVOL.EQ.1) THEN
           Iaer_ch=0.
           Ish_ch=0.
           Itef_ch=0.
           Iaer_sc=0.
           Ish_sc=0.
           Itef_sc=0.
           DEPOS = 0.
           !координаты влета частицы
	   XinpAG = VECT(1)
           YinpAG = VECT(2)
           ZinpAG = VECT(3)
        ENDIF
c
          CALL ATC_AMP(I_CH,IAaer_CH,IAtef_CH,IAsh_CH,IAaer_SC,IAtef_SC,IAsh_SC,I_area)
c         !заполняем массив компонент сигнала -> сцинтилляции и черенковский сигнал
          Iaer_ch=Iaer_ch+IAaer_CH
          Ish_ch=Ish_ch+IAsh_CH
          Itef_ch=Itef_ch+IAtef_CH
          Iaer_sc=Iaer_sc+IAaer_SC
          Ish_sc=Ish_sc+IAsh_SC
          Itef_sc=Itef_sc+IAtef_SC
c
c        !частица вышла из объема или остановилась
         IF ( INWVOL.EQ.2 .OR. ISTOP.NE.0 ) THEN
c         !координаты выхода частицы
	  Xout = VECT(1)
          Yout = VECT(2)
          Zout = VECT(3)
c         !параметры для хит(чувствительных) областей
          HITS(IatcXin)=XinpAG
          HITS(IatcYin)=YinpAG
          HITS(IatcZin)=ZinpAG
          HITS(IatcXout)=Xout
          HITS(IatcYout)=Yout
          HITS(IatcZout)=Zout
 	  HITS(IatcCounter)=I_CH
          HITS(IatcIa_ch)=Iaer_ch
          HITS(IatcIa_sc)=Iaer_sc
          HITS(IatcIsh_ch)=Ish_ch
          HITS(IatcIsh_sc)=Ish_sc
          HITS(IatcItef_ch)=Itef_ch
          HITS(IatcItef_sc)=Itef_sc
          HITS(IatcNAME)=I_area
          HITS(IatcNLEVEL)=NLEVEL
c        !сохранение информации о попадание
         CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITS,IHIT)
c
     	 DEPOS = 0.
	ENDIF
	ENDIF
c
c--------------------------------------------------------------------
c
c      IF CHARGED PARTICLE REACHES TOF
c
c--------------------------------------------------------------------
c
c	if(BFINOR .eq. 0.) print *,' GUSTEP TOF: bfinor=',bfinor
c
      IF(CHARGE*ISVOL.NE.0.AND.IDTYPE.EQ.3) THEN
      IF (INWVOL.EQ.1) THEN
c
         tfDEPOS=0.
         tfXinp = VECT(1)
         tfYinp = VECT(2)
         tfZinp = VECT(3)
         tfTinp =TOFG*1.E9
         tfPXinp = VECT(4)
         tfPYinp = VECT(5)
         tfPZinp = VECT(6)
         tfPinp  = VECT(7)
         tfEpartIn=GETOT
         ItfCOU=NUMBER(NLEVEL) ! number of volume copy at this level
      ENDIF
c
      CALL TOF_STEP  ! simulation scintillation counters amplitudes
c
      tfDEPOS = tfDEPOS + DESTEP

         IF ((INWVOL.EQ.2 .OR. ISTOP.NE.0) .AND. tfDEPOS .GT. 0.) THEN
          tfXout = VECT(1)
          tfYout = VECT(2)
          tfZout = VECT(3)
          HITS(ItofXm)=(tfXinp+tfXout)/2.
          HITS(ItofYm)=(tfYinp+tfYout)/2.
          HITS(ItofZm)=(tfZinp+tfZout)/2.
          HITS(ItofXout)=tfXout
          HITS(ItofYout)=tfYout
          HITS(ItofZout)=tfZout
          HITS(ItofTime)=tfTinp
          HITS(ItofEloss)=tfDEPOS
          HITS(ItofXin)=tfXinp
          HITS(ItofYin)=tfYinp
          HITS(ItofZin)=tfZinp
          HITS(ItofPXin)=tfPXinp
          HITS(ItofPYin)=tfPYinp
          HITS(ItofPZin)=tfPZinp
          HITS(ItofPin)=tfPinp
          HITS(ItofTrack)=ITRA
          HITS(ItofCHARGE)=CHARGE
          HITS(ItofMASS)=AMASS
          HITS(ItofParticle)=IPART
          HITS(ItofCounter)=ItfCOU
          HITS(ItofEpartin)=tfEpartIn
c          print *,'gustep: x1=',tfXinp,'  y1=',tfYinp,'  z1=',tfZinp
c          print *,'gustep: x2=',tfXout,'  y2=',tfYout,'  z2=',tfZout
c          print *,'gustep: tr=',ITRA,'  part=',IPART,'  cou=',ItfCOU
c          print *,'gustep: dE=',tfDEPOS,'  P=',tfPinp,'  Ein=',tfEpartIn
c
          CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITS,IHIT)
c
          tfDEPOS = 0.
         ENDIF
       ENDIF
c----------------------------------------------------------------
c
c      IF PARTICLE REACHES CSI
c
c----------------------------------------------------------------
c
c	if(BFINOR .eq. 0.) print *,' GUSTEP CsI: bfinor=',bfinor
      IF (ISVOL.EQ.1.AND.IDTYPE.EQ.6) THEN

       Nquad = NUMBV(1)  !eight quadrs
       Nbloc = NUMBV(2)  !16*Ngr+Nch
       Ncr = NUMBV(3)    !2 layers
       Ncsi=1
       if(Nquad>4) then
        Ncsi=2
       	Nquad=Nquad-4
       endif


       ichanel=MOD(Nbloc,16) ! Chanel
       igroup =Nbloc/16     ! Group
       if(Ncr.eq.2) igroup=igroup+6
       iquad=Nquad
       if(Ncsi.eq.2) iquad=5-Nquad
       itor=Ncsi

c         print *,'itor,iquad,igroup,ichanel: ',itor,iquad,igroup,ichanel
c       call mctorl(Ncsi,Nquad,Nblx,Nbly,Ncr,itor,iquad,igroup,ichanel)
c      Number of chanel is codding  hear
c      tor - 2 end caps, quad - 4 quads ,
c      group - 12 groups, chanel - 14 chanels

c commented for fast execution
       Icr = (itor-1)*16*12*4+(iquad-1)*16*12+(igroup-1)*16+(ichanel-1)
c       Icr = Ncsi*10000+Nquad*1000+Nbloc*10+Ncr

       HITS(1)=CRXRL(ichanel,igroup,iquad,itor)
       HITS(2)=CRYRL(ichanel,igroup,iquad,itor)
       HITS(3)=CRZRL(ichanel,igroup,iquad,itor)
       HITS(4)=VECT(1)
       HITS(5)=VECT(2)
       HITS(6)=VECT(3)
#ifdef _CSIDBG_
      if(CsIhistBooked.ne.0) then
         if(abs(hits(3)-99.).gt.2..and.
     &        hits(1)**2+hits(2)**2.lt.55.**2) then ! standard crystals
            call HF1(903,hits(6),1.)
            call HF1(911,hits(4)-hits(1),1.)
            call HF1(912,hits(5)-hits(2),1.)
            ZinCrystal=hits(6)-hits(3)
            if(hits(3).lt.0) ZinCrystal=-ZinCrystal
            call HF1(913,ZinCrystal,1.)
            if(abs(hits(3)).lt.100.) then
               call HF1(921,hits(4)-hits(1),DESTEP)
               call HF1(922,hits(5)-hits(2),DESTEP)
               call HF1(923,ZinCrystal,DESTEP)
            else
               call HF1(931,hits(4)-hits(1),DESTEP)
               call HF1(932,hits(5)-hits(2),DESTEP)
               call HF1(933,ZinCrystal,DESTEP)
            endif
         endif
         CsIhistEntries=CsIhistEntries+1
         if(mod(CsIhistEntries,100000).eq.0)then
            print *,'see histograms in "csihist.rz"'
            call HRPUT(0,'csihist.rz','N')
         endif
      endif
# if _CSIDBG_ > 1
       if(INWVOL.eq.1) print *,'++++in',IPART*10000+ITRA*100+ISTAK,
     &      ' E',GETOT*1.e3
       print *,' xyzC',(hits(j),j=1,3)
       print *,' xyz ',(hits(j),j=4,6),' Dz',hits(6)-hits(3)
       if(INWVOL.eq.2) print *,'---out',IPART*10000+ITRA*100+ISTAK,
     &      IGNEXT,' E',GETOT*1.e3
# endif
#endif
       HITS(7)=IPART*10000.+ITRA*100.+ISTAK
       HITS(8) = GETOT
       HITS(9) = TOFG*1.E9
       HITS(10) = Icr
       HITS(11) = ichanel
       HITS(12) = igroup
       HITS(13) = iquad
       HITS(14) = DESTEP
       IF (DESTEP .GT. 1.E-8) then
c
C	CALL GSCHIT(ISET,IDET,1,NUMBV,HITS,1,IHIT)
         CALL GSCHIT(ISET,IDET,ITRA,NUMBV,HITS,1,IHIT)

c
	if(ihit.gt.190) then
	print *,' gustep CsI hit: nh, ec,quad,bl,cr ',
     +		ihit,ncsi,nquad,nbloc,ncr
	print *,'getot,gekin,nstep,destep,step,itra,istak,ipart,ipaold'
	print *,getot,gekin,nstep,destep,step,itra,istak,ipart,ipaold
	endif
       endif
      ENDIF
c------------------------------------------------------------------
c
c      IF PARTICLE REACHES LKr
c
c------------------------------------------------------------------
c
      IF (ISVOL.EQ.1.AND.IDTYPE.EQ.5) CALL LK_HITC
c
c------------------------------------------------------------------
c
c      IF PARTICLE REACHES Muon System
c
c------------------------------------------------------------------
c
      IF(CHARGE.NE.0.AND.isvol.eq.1.and.INWVOL.EQ.1.AND.IDTYPE.EQ.7) THEN
c			print *,'particle No ',ipart
			IF(mu_debug.ge.2.OR.IDEBUG.EQ.1)THEN
				PRINT *,'  '
				print *,' MU SYSTEM IEVENT',ievent
			endif
        	CALL MUON_HIT_FIND
      ENDIF


      IF(ISTOP.NE.0.AND.ISTAK.EQ.0) THEN ! particle stopped, save time
      					 ! and mechanisms
      	IADR=0
	UBUF(1)=TOFG
	UBUF(2)=ISTOP
	UBUF(3)=NMEC
	DO I=1,NMEC
		UBUF(3+I)=LMEC(I)
	ENDDO
      	CALL GSKINU(ITRA,NMEC+3,UBUF,IADR)
c	print *,'**********TOFG,ISTOP,NMEC,LMEC=',
c     *	TOFG,ISTOP,NMEC,LMEC
      ENDIF

      RETURN
      END
