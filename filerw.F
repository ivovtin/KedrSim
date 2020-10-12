      subroutine filerw
      implicit none
c
c   Author :    YURI SKOVPEN   25/11/96
c
c   Last update: 28.05.2001, 12.03.2004 V.A.Tayursky
c                21.07.07 A. Shamov
c
#include "KedrSim/pkin.inc"
#include "KedrSim/ver_event.inc"
#include "KedrSim/csiloss.inc"
#include "KedrSim/csisize.inc"
#include "KedrSim/csiwrite.inc"
#include "KedrSim/vddata.inc"
#include "KedrSim/dcout.inc"
#include "KedrSim/lk_digv.inc"
#include "KedrSim/lk_rec.inc"
#include "KedrSim/tof_posl.inc"
#include "KedrSim/atccom.inc"
#include "KedrSim/mu_data.inc"

#include "ReadNat/mc_def.inc"

#include "ReadNat/mcvd_def.inc"
#include "ReadNat/vdrawhitspar.inc"
#include "KedrSim/vdrawhits.inc"

#include "ReadNat/mcdc_def.inc"
#include "ReadNat/dcrawhitspar.inc"
#include "KedrSim/dcrawhits.inc"

#include "ReadNat/mctof_def.inc"
#include "ReadNat/tofrawhitspar.inc"
#include "KedrSim/tofrawhits.inc"

#include "ReadNat/mcatc_def.inc"
#include "KedrSim/atcrawhitspar.inc"
#include "KedrSim/atcrawhits.inc"

#include "ReadNat/mccsi_def.inc"
#include "ReadNat/csirawhitspar.inc"
#include "KedrSim/csirawhits.inc"

#include "ReadNat/mclkr_def.inc"
#include "ReadNat/lkrrawhitspar.inc"
#include "KedrSim/lkrrawhits.inc"

#include "ReadNat/re_def.inc"
#include "KedrSim/dqfile.inc"
#include "ReadNat/ss_def.inc"

#include "KedrSim/kedrif.inc"

#include "KedrSim/lujeth.inc"
#include "KedrSim/userpar.inc"

#include "lujets.inc"
#include "gtflag.inc"
#include "gcflag.inc"

      integer kffromkg
      external kffromkg

      integer mik,mn1,mn2
      common/marrnd/mik,mn1,mn2

      integer *2 natc,iatc
      common /atc_comw/natc,iatc(2,160)

      integer IHIS,IHDUM,IWR_HIS
      COMMON /HIS_SIMUL/ IHIS,IHDUM(308),IWR_HIS ! + 12.03.2004

      character *12 pn
      real pmass,char,tlif

      integer iptr,i,iw,Loverflow
      integer Ljetset,Lupar,Ldchits,Lvdhits,Ltofhits,Latchits,Ncsiraw,Nlkrraw

      rawl(evl_event)=ievent	! event number
      mcl(mcl_event)=ievent     ! event number

      if(mod(ievent,1000).eq.0)
     *     write(6,*) '-----filerw-----',ievent

      if(IHIS.EQ.1.AND.IWR_HIS.EQ.0) return ! event does not hit 1-st hist.

      iptr=mcc_lhl
      if(ntrack.eq.0) then
         mcdir(2,sr_mc)=0
         mcdir(1,sr_mc)=0
      else
         mcdir(2,sr_mc)=4+ntrack*5+5
         mcdir(1,sr_mc)=iptr+1
         mcl(iptr+1)=ntrack
         iptr=iptr+1

#if 0
         print *,' ievent,ntrack: ',ievent,ntrack
#endif
         do i=1,ntrack
            mcl(iptr+1) = iparticle(i)
#if 0
            print *,'ipart=',iparticle(i)
#endif
            mcl(iptr+2) = epart(i)*1000000. ! Kev
            mcl(iptr+3) = pxpart(i)*1000000. ! Kev
            mcl(iptr+4) = pypart(i)*1000000. ! Kev
            mcl(iptr+5) = pzpart(i)*1000000. ! Kev
            iptr=iptr+5
         end do
C     Vertex
         mcl(iptr+1) = vrtx(1)*10000. ! x, micron
         mcl(iptr+2) = vrtx(2)*10000. ! y
         mcl(iptr+3) = vrtx(3)*10000. ! z
         iptr=iptr+3
c     Randoms
         mcl(iptr+1) = nrndm(1)
         mcl(iptr+2) = nrndm(2)
         mcl(iptr+3) = mik
         mcl(iptr+4) = mn1
         mcl(iptr+5) = mn2
         iptr=iptr+5
      endif

c Kinematics in JETSET format
      if(n.eq.0.and.ntrack.eq.0) then
         mcdir(2,sr_lu)=0
         mcdir(1,sr_lu)=0
      else
         if(n.eq.0) then
	    n=ntrack
	    do i=1,n
               k(i,1)=1
               k(i,2)=kffromkg(iparticle(i))
               k(i,3)=0
               k(i,4)=0
               k(i,5)=0
               p(i,1)=pxpart(i)
               p(i,2)=pypart(i)
               p(i,3)=pzpart(i)
               p(i,4)=epart(i)
               call ppar(k(i,2),pn,pmass,char,tlif)
               p(i,5)=pmass
               v(i,4)=0.
               v(i,5)=0.
	    enddo
         endif
c
         Ljetset=1+n*5*3
         if(iptr+Ljetset.gt.mcc_lleng) then
            print *,'FILERW: event',ievent,' too many tracks',n
            print *,'JETSET record is not saved'
            mcdir(2,sr_lu)=0
            mcdir(1,sr_lu)=0
            goto 190
         endif
         mcdir(2,sr_lu)=Ljetset
         mcdir(1,sr_lu)=iptr+1
         mcl(iptr+1)=n
         iptr=iptr+1
#if 0
         print *,'ntracks=',n
#endif
         do i=1,n
	    mcl(iptr+1) = k(i,1) ! KS
	    mcl(iptr+2) = k(i,2) ! KF
	    mcl(iptr+3) = k(i,3) ! number of parent
	    mcl(iptr+4) = k(i,4) ! number of first daughter
	    mcl(iptr+5) = k(i,5) ! number of last  daughter
	    iptr=iptr+5
	    mcr(iptr+1) = p(i,1) ! Px, GeV/c
	    mcr(iptr+2) = p(i,2) ! Py, GeV/c
	    mcr(iptr+3) = p(i,3) ! Pz, GeV/c
	    mcr(iptr+4) = p(i,4) ! E, GeV
	    mcr(iptr+5) = p(i,5) ! m, GeV/c**2
	    iptr=iptr+5
#if 0
	    if(v(i,1).ne.0.or.v(i,2).ne.0.or.v(i,3).ne.0)
     $           print *,
     $           '*****filerw: warning, Lund vertex position .ne.0: xyz(mm)=',
     $           (v(i,j),j=1,3)
#endif
c     v(i,1-3) may be ne 0 for tau decays, for example...
	    mcr(iptr+1) = vrtx(1)*10. ! vertex x pozition, mm
	    mcr(iptr+2) = vrtx(2)*10. ! vertex y pozition, mm
	    mcr(iptr+3) = vrtx(3)*10. ! vertex z pozition, mm
	    mcr(iptr+4) = v(i,4) ! time of origin, mm/c
	    mcr(iptr+5) = v(i,5) ! proper lifetime, mm/c (0 if no decay)
	    iptr=iptr+5
#if 0
	    call ppar(k(i,2),pn,pmass,char,tlif)
	    print *,'i,ks,pname,k3,k4,k5,=',i,k(i,1),'  ',pn,k(i,3),k(i,4),k(i,5)
	    print *,'px,py,pz,E,m=',(p(i,j),j=1,5)
	    print *,'x,y,z,time,lifetime=',vrtx(1)*10,vrtx(2)*10,vrtx(3)*10,
     $           v(i,4),v(i,5)
	    print *,'v(1,2,3)=',v(i,1),v(i,2),v(i,3)
#endif
         end do
         n=0
      endif
 190  continue

c History in JETSET format
      if(nh.eq.0) then
         mcdir(2,sr_hi)=0
         mcdir(1,sr_hi)=0
      else
         Ljetset=1+n*5*3
         if(iptr+Ljetset.gt.mcc_lleng) then
            print *,'FILERW: event',ievent,' too many tracks',n
            print *,'history in JETSET format is not saved'
            mcdir(2,sr_lu)=0
            mcdir(1,sr_lu)=0
            goto 290
         endif
         mcdir(2,sr_hi)=Ljetset
         mcdir(1,sr_hi)=iptr+1
         mcl(iptr+1)=nh
         iptr=iptr+1
#if 0
         print *,'n history tracks=',nh
#endif
         do i=1,nh
	    mcl(iptr+1) = kh(i,1) ! KS - number of last mechanism (see /GCTRAK/)
                                ! on last particle step, 0 if particle reached
                                ! bounds of detector
	    mcl(iptr+2) = kh(i,2)	! KF
	    mcl(iptr+3) = kh(i,3) ! number of parent
	    mcl(iptr+4) = kh(i,4) ! number of first daughter
	    mcl(iptr+5) = kh(i,5) ! number of last  daughter
	    iptr=iptr+5
	    mcr(iptr+1) = ph(i,1) ! Px, GeV/c
	    mcr(iptr+2) = ph(i,2) ! Py, GeV/c
	    mcr(iptr+3) = ph(i,3) ! Pz, GeV/c
	    mcr(iptr+4) = ph(i,4) ! E, GeV
	    mcr(iptr+5) = ph(i,5) ! m, GeV/c**2
	    iptr=iptr+5
	    mcr(iptr+1) = vh(i,1) ! vertex x pozition, mm
	    mcr(iptr+2) = vh(i,2) ! vertex y pozition, mm
	    mcr(iptr+3) = vh(i,3) ! vertex z pozition, mm
	    mcr(iptr+4) = vh(i,4) ! time of origin, mm/c
	    mcr(iptr+5) = vh(i,5) ! time of particle tracking in detector, mm/c
	    iptr=iptr+5
	    call ppar(kh(i,2),pn,pmass,char,tlif)
#if 0
	    print *,'i,ks,pname,k3,k4,k5,=',i,kh(i,1),'  ',pn,
     $           kh(i,3),kh(i,4),kh(i,5)
	    print *,'px,py,pz,E,m=',(ph(i,j),j=1,5)
	    print *,'x,y,z,time,lifetime=',(vh(i,j),j=1,5)
#endif
         end do
         nh=0
      endif
 290  continue

c Users parameters
      if(nu.eq.0) then
         mcdir(2,sr_us)=0
         mcdir(1,sr_us)=0
      else
         Lupar=1+nu
         if(iptr+Lupar.gt.mcc_lleng) then
            print *,'FILERW event',ievent,' too many user parameters',nu
            print *,'user parameter are not saved'
            mcdir(2,sr_us)=0
            mcdir(1,sr_us)=0
            goto 390
         endif
         mcdir(2,sr_us)=Lupar
         mcdir(1,sr_us)=iptr+1
         mcl(iptr+1)=nu
         iptr=iptr+1
         do i=1,nu
            mcr(iptr+i) = upar(i) ! 'user' parameter
         end do
         iptr=iptr+nu
      endif
 390  continue
      mcl(mcl_length)=iptr*2	! record length in 2 bytes (1st word in record)
      if (mcl(mcl_length).gt.mcc_wleng) then
         print *,
     $        '***** filerw> record length > max length',mcl(mcl_length),mcc_wleng
         stop
      endif
#if 0
     p        rint *,' rec length ',iptr
#endif
c
C    Write to data file (need swap bytes)

      iw = fwritef(mcl(mcl_length),4,1,ifile)
      iw = fwritef(mcw(mcw_recid),2,2,ifile)
      iw = fwritef(mcb(mcb_recfmt),1,4,ifile)
      iw = fwritef(mcl(mcl_run),4,9,ifile)
      iw = fwritef(mcdir,2,mcc_dirl*2,ifile)
      iw = fwritef(mcl(mcc_lhl+1),4,iptr-mcc_lhl,ifile)
#if 0
      print *,'rec was writting'
#endif
c
      if(IFVD.ge.2.and.IFVD.lt.4) then
         iptr=mcvd_rhl
         Lvdhits=NumRawParVD*NHITSVDW
         if(iptr+Lvdhits.gt.mcvd_rleng) then
            print *,'FILERW: event',ievent,' too many VD raw hits',NHITSVDW
            NHITSVDW=(mcvd_rleng-iptr)/NumRawParVD
            print *,'trucate number of hits to',NHITSVDW
            NRAWHITSVD=NHITSVDW*NumRawParVD
         endif
         mcVDhitsL(mcvdl_Nhits) = NHITSVDW
         do i=1,NRAWHITSVD,NumRawParVD
            mcVDhitsR(iptr+IvdTube)  =RAWHITSVD(i+IvdTube-1)
            mcVDhitsR(iptr+IvdAmpl)  =RAWHITSVD(i+IvdAmpl-1)
            mcVDhitsR(iptr+IvdLength)=RAWHITSVD(i+IvdLength-1)
            mcVDhitsR(iptr+IvdXG)=RAWHITSVD(i+IvdXG-1)
            mcVDhitsR(iptr+IvdYG)=RAWHITSVD(i+IvdYG-1)
            mcVDhitsR(iptr+IvdZG)=RAWHITSVD(i+IvdZG-1)
            mcVDhitsR(iptr+IvdVx)=RAWHITSVD(i+IvdVx-1)
            mcVDhitsR(iptr+IvdVy)=RAWHITSVD(i+IvdVy-1)
            mcVDhitsR(iptr+IvdVz)=RAWHITSVD(i+IvdVz-1)
            mcVDhitsR(iptr+IvdTr)=RAWHITSVD(i+IvdTr-1)
            mcVDhitsR(iptr+IvdE)=RAWHITSVD(i+IvdE-1)
            mcVDhitsR(iptr+IvdM)=RAWHITSVD(i+IvdM-1)
            mcVDhitsR(iptr+IvdP)=RAWHITSVD(i+IvdP-1)
            iptr=iptr+NumRawParVD
         enddo

         mcVDhitsL(mcvdl_length)=iptr*2	! record length in 2 bytes (1st word in record)
         mcVDhitsW(mcvdw_recid)=1102
         mcVDhitsL(mcvdl_event)=ievent ! event number
         mcVDhitsL(mcvdl_zero)=0 ! zero longword
         mcVDhitsW(mcvdw_hdrl)=mcvd_whl

         iw = fwritef(mcVDhitsL(mcvdl_length),4,1,ifile)
         iw = fwritef(mcVDhitsW(mcvdw_recid),2,1,ifile)
         iw = fwritef(mcVDhitsW(mcvdw_hdrl),2,1,ifile)
         iw = fwritef(mcVDhitsB(mcvdb_recfmt),1,4,ifile)
         iw = fwritef(mcVDhitsL(mcvdl_event),4,1,ifile) ! event number
         iw = fwritef(mcVDhitsL(mcvdl_Nhits),4,1,ifile)
#ifdef KEEPBUG
         iw = fwritef(mcVDhitsL(mcvdl_zero_1),4,1,ifile)
#else
         iw = fwritef(mcVDhitsL(mcvdl_zero),4,1,ifile)
#endif
         iw = fwritef(mcVDhitsR(mcvdr_hits),4,iptr-mcvd_rhl,ifile)

      endif

c ---------- MCDC ----------
      if(IFDC.ge.2.and.IFDC.lt.4) then
         iptr=mcdc_rhl
         Ldchits=NumRawPar*NHITS
         if(iptr+Ldchits.gt.mcdc_rleng) then
            print *,'FILERW: event',ievent,' too many DC raw hits',NHITS
            NHITS=(mcdc_rleng-iptr)/NumRawPar
            print *,'trucate number of hits to',NHITS
            NRAWHITSDC=NHITS*NumRawPar
         endif
         mcDChitsL(mcdcl_Nhits)=NHITS
         do i=1,NRAWHITSDC,NumRawPar
            mcDChitsR(iptr+IndWIRE)=RAWHITSDC(i+IndWIRE-1)
            mcDChitsR(iptr+IndADC)=RAWHITSDC(i+IndADC-1)
            mcDChitsR(iptr+IndXG)=RAWHITSDC(i+IndXG-1)
            mcDChitsR(iptr+IndYG)=RAWHITSDC(i+IndYG-1)
            mcDChitsR(iptr+IndZG)=RAWHITSDC(i+IndZG-1)
            mcDChitsR(iptr+IndVx)=RAWHITSDC(i+IndVx-1)
            mcDChitsR(iptr+IndVy)=RAWHITSDC(i+IndVy-1)
            mcDChitsR(iptr+IndVz)=RAWHITSDC(i+IndVz-1)
            mcDChitsR(iptr+IndTr)=RAWHITSDC(i+IndTr-1)
            mcDChitsR(iptr+IndE)=RAWHITSDC(i+IndE-1)
            mcDChitsR(iptr+IndM)=RAWHITSDC(i+IndM-1)
            mcDChitsR(iptr+IndP)=RAWHITSDC(i+IndP-1)
            mcDChitsR(iptr+IndRhodE)=RAWHITSDC(i+IndRhodE-1)
            iptr=iptr+NumRawPar
            if(iptr.ge.75000) print *,'WARNING: raw event MCDC too large'
         enddo
         mcDChitsL(mcdcl_length)=iptr*2	! record length in 2 bytes (1st word in record)
         mcDChitsW(mcdcw_recid)=1103
         mcDChitsL(mcdcl_event)=ievent ! event number
         mcDChitsL(mcdcl_zero)=0 ! zero lonword
         mcDChitsW(mcdcw_hdrl)=mcdc_whl

         iw = fwritef(mcDChitsL(mcdcl_length),4,1,ifile)
         iw = fwritef(mcDChitsW(mcdcw_recid),2,1,ifile)
         iw = fwritef(mcDChitsW(mcdcw_hdrl),2,1,ifile)
         iw = fwritef(mcDChitsB(mcdcb_recfmt),1,4,ifile)
         iw = fwritef(mcDChitsL(mcdcl_event),4,1,ifile) ! event number
         iw = fwritef(mcDChitsL(mcdcl_Nhits),4,1,ifile)
#ifdef KEEPBUG
         iw = fwritef(mcDChitsL(mcdcl_zero_1),4,1,ifile)
#else
         iw = fwritef(mcDChitsL(mcdcl_zero),4,1,ifile)
#endif
         iw = fwritef(mcDChitsR(mcdcr_hits),4,iptr-mcdc_rhl,ifile)

      endif
c
c ---------- MCATC ----------
      if(IFCH.ge.2.and.IFCH.lt.4) then
         iptr=mcatc_rhl
	  Latchits=iatc_length*NHITSATCW
c          print *,'ATC event',Latchits
c
         if(iptr+Latchits.gt.mcatc_rleng) then
            print *,'FILERW: event',ievent,' too many ATC raw hits',
     &           NHITSATCW
            NHITSATCW=(mcatc_rleng-iptr)/iatc_length
            print *,'trucate number of hits to',NHITSATCW
            NRAWHITSATC=NHITSATCW*iatc_length
         endif
         mcATChitsL(mcatcl_Nhits)=NHITSATCW
c         print *,'ATC NHITSATCW=',NHITSATCW,' iatc_length=',iatc_length,' NRAWHITSATC=',NRAWHITSATC
         do i=1,NRAWHITSATC,iatc_length
            mcATChitsR(iptr+iatc_CNT)=RAWHITSATC(i+iatc_CNT-1)
            mcATChitsR(iptr+ratc_X1)=RAWHITSATC(i+ratc_X1-1)
            mcATChitsR(iptr+ratc_Y1)=RAWHITSATC(i+ratc_Y1-1)
            mcATChitsR(iptr+ratc_Z1)=RAWHITSATC(i+ratc_Z1-1)
            mcATChitsR(iptr+ratc_X2)=RAWHITSATC(i+ratc_X2-1)
            mcATChitsR(iptr+ratc_Y2)=RAWHITSATC(i+ratc_Y2-1)
            mcATChitsR(iptr+ratc_Z2)=RAWHITSATC(i+ratc_Z2-1)
            mcATChitsR(iptr+ratc_Ia_ch)=RAWHITSATC(i+ratc_Ia_ch-1)
            mcATChitsR(iptr+ratc_Ia_sc)=RAWHITSATC(i+ratc_Ia_sc-1)
            mcATChitsR(iptr+ratc_Ish_ch)=RAWHITSATC(i+ratc_Ish_ch-1)
            mcATChitsR(iptr+ratc_Ish_sc)=RAWHITSATC(i+ratc_Ish_sc-1)
            mcATChitsR(iptr+ratc_Itef_ch)=RAWHITSATC(i+ratc_Itef_ch-1)
            mcATChitsR(iptr+ratc_Itef_sc)=RAWHITSATC(i+ratc_Itef_sc-1)
            mcATChitsR(iptr+ratc_I_NAME)=RAWHITSATC(i+ratc_I_NAME-1)
            mcATChitsR(iptr+ratc_I_NLEVEL)=RAWHITSATC(i+ratc_I_NLEVEL-1)
c
#if 0
            iptr=iptr+iatc_length
c            print *,'ATC filerw: x1=',mcATChitsR(iptr+2),
c     &           '  y1=',mcATChitsR(iptr+3),'  z1=',
c     &           mcATChitsR(iptr+4)
c            print *,'ATC filerw: x2=',mcATChitsR(iptr+5),
c     &           '  y2=',mcATChitsR(iptr+6),'  z2=',
c     &           mcATChitsR(iptr+7)
c
#endif
            iptr=iptr+iatc_length
c
            if(iptr.ge.75000) print * ,
     &           'WARNING: raw event MCATC too large'
c
         enddo
c
         mcATChitsL(mcatcl_length)=iptr*2
         mcATChitsW(mcatcw_recid)=1106
         mcATChitsL(mcatcl_event)=ievent
         mcATChitsL(mcatcl_zero)=0
         mcATChitsW(mcatcw_hdrl)=mcatc_whl
c
         iw = fwritef(mcATChitsL(mcatcl_length),4,1,ifile)
         iw = fwritef(mcATChitsW(mcatcw_recid),2,1,ifile)
         iw = fwritef(mcATChitsW(mcatcw_hdrl),2,1,ifile)
         iw = fwritef(mcATChitsB(mcatcb_recfmt),1,4,ifile)
         iw = fwritef(mcATChitsL(mcatcl_event),4,1,ifile)
         iw = fwritef(mcATChitsL(mcatcl_Nhits),4,1,ifile)
#ifdef KEEPBUG
         iw = fwritef(mcATChitsL(mcatcl_zero_1),4,1,ifile)
#else
         iw = fwritef(mcATChitsL(mcatcl_zero),4,1,ifile)
#endif
         iw = fwritef(mcATChitsR(mcatcr_hits),4,iptr-mcatc_rhl,ifile)
c
      endif
c
c ---------- MCTOF ----------
      if(IFTF.ge.2.and.IFTF.lt.4) then
         iptr=mctof_rhl
#if 0
         print *,'filerw: NHITSTOFW=',NHITSTOFW
         print *,'filerw: NRAWHITSTOF=',NRAWHITSTOF
#endif

         Ltofhits=itof_length*NHITSTOFW
         if(iptr+Ltofhits.gt.mctof_rleng) then
            print *,'FILERW: event',ievent,' too many TOF raw hits',
     &           NHITSTOFW
            NHITSTOFW=(mctof_rleng-iptr)/itof_length
            print *,'trucate number of hits to',NHITSTOFW
            NRAWHITSTOF=NHITSTOFW*itof_length
         endif
         mcTOFhitsL(mctofl_Nhits)=NHITSTOFW
         do i=1,NRAWHITSTOF,itof_length
            mcTOFhitsR(iptr+itof_PMT)=RAWHITSTOF(i+itof_PMT-1)
            mcTOFhitsR(iptr+rtof_X1)=RAWHITSTOF(i+rtof_X1-1)
            mcTOFhitsR(iptr+rtof_Y1)=RAWHITSTOF(i+rtof_Y1-1)
            mcTOFhitsR(iptr+rtof_Z1)=RAWHITSTOF(i+rtof_Z1-1)
            mcTOFhitsR(iptr+rtof_X2)=RAWHITSTOF(i+rtof_X2-1)
            mcTOFhitsR(iptr+rtof_Y2)=RAWHITSTOF(i+rtof_Y2-1)
            mcTOFhitsR(iptr+rtof_Z2)=RAWHITSTOF(i+rtof_Z2-1)
            mcTOFhitsR(iptr+rtof_TIME)=RAWHITSTOF(i+rtof_TIME-1)
            mcTOFhitsR(iptr+rtof_BETA)=RAWHITSTOF(i+rtof_BETA-1)
            mcTOFhitsR(iptr+rtof_ELOSS)=RAWHITSTOF(i+rtof_ELOSS-1)
            mcTOFhitsR(iptr+itof_PARTICLE)=RAWHITSTOF(i+itof_PARTICLE-1)
            mcTOFhitsR(iptr+itof_TYPE)=RAWHITSTOF(i+itof_TYPE-1)
#if 0
            iptr=iptr+itof_length
            print *,'filerw: x1=',mcTOFhitsR(iptr+rtof_X1),
     &           '  y1=',mcTOFhitsR(iptr+rtof_Y1),'  z1=',
     &           mcTOFhitsR(iptr+rtof_Z1)
            print *,'filerw: x2=',mcTOFhitsR(iptr+rtof_X2),
     &           '  y2=',mcTOFhitsR(iptr+rtof_Y2),'  z2=',
     &           mcTOFhitsR(iptr+rtof_Z2)
            print *,'filerw: tr=',mcTOFhitsR(iptr+itof_PARTICLE),
     &           '  part=',mcTOFhitsR(iptr+itof_TYPE),
     &           '  cou=',mcTOFhitsR(iptr+itof_PMT)
            print *,'filerw: dE=',mcTOFhitsR(iptr+rtof_ELOSS),
     &           '  beta=',mcTOFhitsR(iptr+rtof_BETA),
     &           '  T=',mcTOFhitsR(iptr+rtof_TIME)
            print *,'filerw1: x1=',RAWHITSTOF(i-1+rtof_X1),
     &           '  y1=',RAWHITSTOF(i-1+rtof_Y1),'  z1=',
     &           RAWHITSTOF(i-1+rtof_Z1)
            print *,'filerw1: x2=',RAWHITSTOF(i-1+rtof_X2),
     &           '  y2=',RAWHITSTOF(i-1+rtof_Y2),'  z2=',
     &           RAWHITSTOF(i-1+rtof_Z2)
            print *,'filerw1: tr=',RAWHITSTOF(i-1+itof_PARTICLE),
     &           '  part=',RAWHITSTOF(i-1+itof_TYPE),
     &           '  cou=',RAWHITSTOF(i-1+itof_PMT)
            print *,'filerw1: dE=',RAWHITSTOF(i-1+rtof_ELOSS),
     &           '  beta=',RAWHITSTOF(i-1+rtof_BETA),
     &           '  T=',RAWHITSTOF(i-1+rtof_TIME)
c
#endif
            iptr=iptr+itof_length
c
            if(iptr.ge.75000) print * ,
     &           'WARNING: raw event MCTOF too large'
c
         enddo
         mcTOFhitsL(mctofl_length)=iptr*2 ! record length in 2 bytes (1st word in record)
         mcTOFhitsW(mctofw_recid)=1104
         mcTOFhitsL(mctofl_event)=ievent ! event number
         mcTOFhitsL(mctofl_zero)=0 ! zero longword
         mcTOFhitsW(mctofw_hdrl)=mctof_whl

         iw = fwritef(mcTOFhitsL(mctofl_length),4,1,ifile)
         iw = fwritef(mcTOFhitsW(mctofw_recid),2,1,ifile)
         iw = fwritef(mcTOFhitsW(mctofw_hdrl),2,1,ifile)
         iw = fwritef(mcTOFhitsB(mctofb_recfmt),1,4,ifile)
         iw = fwritef(mcTOFhitsL(mctofl_event),4,1,ifile) ! event number
         iw = fwritef(mcTOFhitsL(mctofl_Nhits),4,1,ifile)
#ifdef KEEPBUG
         iw = fwritef(mcTOFhitsL(mctofl_zero_1),4,1,ifile)
#else
         iw = fwritef(mcTOFhitsL(mctofl_zero),4,1,ifile)
#endif
         iw = fwritef(mcTOFhitsR(mctofr_hits),4,iptr-mctof_rhl,ifile)

      endif

c ---------- MCCSI ----------
      if(IFCS.ge.3.and.IFCS.lt.4) then
         iptr=mccsi_rhl
         Ncsiraw=NRAWHITSCSI/NumCsIRawPar
         if(iptr+NRAWHITSCSI.gt.mccsi_rleng) then
            print *,'FILERW: event',ievent,' too many CsI raw hits',
     &           Ncsiraw
            Ncsiraw=(mccsi_rleng-iptr)/NumCsIRawPar
            print *,'trucate number of hits to',Ncsiraw
         endif
         mcCSIhitsL(mccsil_Nhits)=Ncsiraw
         do i=0,NRAWHITSCSI-NumCsIRawPar,NumCsIRawPar
            mcCSIhitsR(iptr+IrawCsIchan)=
     $           ss_lcn(1,ss_csi1)+RAWHITSCSI(i+IrawCsIchan)
            mcCSIhitsR(iptr+IrawCsIITRA)=RAWHITSCSI(i+IrawCsIITRA)
            mcCSIhitsR(iptr+IrawCsIEnergy)=RAWHITSCSI(i+IrawCsIEnergy)
#if 0
            print *,ievent,iptr,
     $           mcCSIhitsR(iptr+IrawCsichan),
     $           mcCSIhitsR(iptr+IrawCsiITRA),
     $           mcCSIhitsR(iptr+IrawCsiEnergy)/1.E6
#endif
            iptr=iptr+NumCsIRawPar
            if(iptr.ge.75000)
     $           print *,'WARNING: raw event MCCSI too large'
         enddo
         mcCSIhitsL(mccsil_length)=iptr*2 ! record length in 2 bytes (1st word in record)
         mcCSIhitsW(mccsiw_recid)=1105 ! mccsi id
         mcCSIhitsL(mccsil_event)=ievent ! event number
         mcCSIhitsL(mccsil_zero)=0 ! zero longword
         mcCSIhitsW(mccsiw_hdrl)=mccsi_whl

         iw = fwritef(mcCSIhitsL(mccsil_length),4,1,ifile)
         iw = fwritef(mcCSIhitsW(mccsiw_recid),2,1,ifile)
         iw = fwritef(mcCSIhitsW(mccsiw_hdrl),2,1,ifile)
         iw = fwritef(mcCSIhitsB(mccsib_recfmt),1,4,ifile)
         iw = fwritef(mcCSIhitsL(mccsil_event),4,1,ifile) ! event number
         iw = fwritef(mcCSIhitsL(mccsil_Nhits),4,1,ifile)
#ifdef KEEPBUG
         iw = fwritef(mcCSIhitsL(mccsil_zero_1),4,1,ifile)
#else
         iw = fwritef(mcCSIhitsL(mccsil_zero),4,1,ifile)
#endif
         iw = fwritef(mcCSIhitsR(mccsir_hits),4,iptr-mccsi_rhl,ifile)

      endif


c ---------- MCLKR ----------
      if(IFKR.ge.3.and.IFKR.lt.4) then
         iptr=mclkr_rhl
         Nlkrraw=NRAWHITSLKR/NumLKrRawPar
         if(iptr+NRAWHITSLKR.gt.mccsi_rleng) then
            print *,'FILERW: event',ievent,' too many LKR raw hits',
     &           Nlkrraw
            Nlkrraw=(mclkr_rleng-iptr)/NumLKrRawPar
            print *,'trucate number of hits to',Nlkrraw
         endif
         mcLKRhitsL(mclkrl_Nhits)=Nlkrraw
         do i=0,NRAWHITSLKR-NumLKrRawPar,NumLKrRawPar
            mcLKRhitsR(iptr+IrawLKrchan)=
     $           RAWHITSLKR(i+IrawLKrchan)+ss_lcn(1,ss_lkr)-1
            mcLKRhitsR(iptr+IrawLKrITRA)=RAWHITSLKR(i+IrawLKrITRA)
            mcLKRhitsR(iptr+IrawLKrEnergy)=RAWHITSLKR(i+IrawLKrEnergy)
#if 0
            print *,ievent,iptr,
     $           mcLKRhitsR(iptr+IrawLKrchan),
     $           mcLKRhitsR(iptr+IrawLKrITRA),
     $           mcLKRhitsR(iptr+IrawLKrEnergy)/1.E6
#endif
            iptr=iptr+NumLKrRawPar
            if(iptr.ge.75000)
     $           print *,'WARNING: raw event MCLKR too large'
         enddo
         mcLKRhitsL(mclkrl_length)=iptr*2 ! record length in 2 bytes (1st word in record)
         mcLKRhitsW(mclkrw_recid)=1107 ! mclkr id
         mcLKRhitsL(mclkrl_event)=ievent ! event number
         mcLKRhitsL(mclkrl_zero)=0 ! zero longword
         mcLKRhitsW(mclkrw_hdrl)=mclkr_whl

         iw = fwritef(mcLKRhitsL(mclkrl_length),4,1,ifile)
         iw = fwritef(mcLKRhitsW(mclkrw_recid),2,1,ifile)
         iw = fwritef(mcLKRhitsW(mclkrw_hdrl),2,1,ifile)
         iw = fwritef(mcLKRhitsB(mclkrb_recfmt),1,4,ifile)
         iw = fwritef(mcLKRhitsL(mclkrl_event),4,1,ifile) ! event number
         iw = fwritef(mcLKRhitsL(mclkrl_Nhits),4,1,ifile)
#ifdef KEEPBUG
         iw = fwritef(mcLKRhitsL(mclkrl_zero_1),4,1,ifile)
#else
         iw = fwritef(mcLKRhitsL(mclkrl_zero),4,1,ifile)
#endif
         iw = fwritef(mcLKRhitsR(mclkrr_hits),4,iptr-mclkr_rhl,ifile)

      endif

c ==== raw data record ====

c clear directory
      Loverflow=0
      do i=1,evc_dirl
         rrdir(1,i)=evc_whl+1
         rrdir(2,i)=0
      enddo

c ---------- VD ----------

#if 0
      print *,'ndigvd: ',ndigvd
#endif
#ifdef ZEB
      write(22) ndigvd,(idigvd(i),i=1,ndigvd)
#endif
      iptr=evc_whl
      if(iptr+ndigvd*2.gt.evc_wleng) then
         print *,'FILERW: event buffer overflow'
         ndigvd=(evc_wleng-iptr)/2
         print *,'truncate VD data to',ndigvd*2
         Loverflow=1
      endif
      rrdir(2,ss_vd)=ndigvd
      if(ndigvd.eq.0) then
         rrdir(1,ss_vd)=0
      else
         rrdir(1,ss_vd)=iptr+1
      endif
      do i=1,ndigvd,2
         raww(iptr+1)=-(idigvd(i)+ss_lcn(1,ss_vd))
         raww(iptr+2)=idigvd(i+1)
         iptr=iptr+2
      enddo
      if(Loverflow.ne.0) goto 490

c ---------- DC ----------

#if 0
      print *,'ndigdc: ',ndigdc
#endif
#ifdef ZEB
      write(22) ndigdc,(idigdc(i),i=1,ndigdc)
#endif
      rrdir(2,ss_dc)=ndigdc
      if(ndigdc.eq.0) then
         rrdir(1,ss_dc)=0
      else
         rrdir(1,ss_dc)=iptr+1
      endif
      if(iptr+ndigdc*2.gt.evc_wleng) then
         print *,'FILERW: event buffer overflow'
         ndigvd=(evc_wleng-iptr)/2
         print *,'truncate DC data to',ndigdc*2
         Loverflow=1
      endif
      do i=1,ndigdc,2
         raww(iptr+1)=-(idigdc(i)+ss_lcn(1,ss_dc))
         raww(iptr+2)=idigdc(i+1)
         iptr=iptr+2
      enddo
      if(Loverflow.ne.0) goto 490

c
c----------- ATC ---------
c
      if(iptr+natc*2.gt.evc_wleng) then
         print *,'FILERW: event buffer overflow'
         natc=(evc_wleng-iptr)/2
         print *,'truncate ATC data to',natc*2
      endif
      rrdir(2,ss_atc)=natc*2
      if(natc.eq.0) then
         rrdir(1,ss_atc)=0
      else
         rrdir(1,ss_atc)=iptr+1
      endif
c
      do i=1,natc
         raww(iptr+1)=-(iatc(1,i)+ss_lcn(1,ss_atc))
         raww(iptr+2)=iatc(2,i)
         iptr=iptr+2
      end do
      if(Loverflow.ne.0) goto 490
c
c ---------- ToF ----------

#if 0
      print *,'ntof: ',ntof
#endif
      if(iptr+ntof*2.gt.evc_wleng) then
         print *,'FILERW: event buffer overflow'
         ntof=(evc_wleng-iptr)/2
         print *,'truncate TOF data to',ntof*2
      endif
      rrdir(2,ss_tof)=ntof*2
      if(ntof.eq.0) then
         rrdir(1,ss_tof)=0
      else
         rrdir(1,ss_tof)=iptr+1
      endif
      do i=1,ntof
         raww(iptr+1)=-(itof(1,i)+ss_lcn(1,ss_tof))
         raww(iptr+2)=itof(2,i)
         iptr=iptr+2
      enddo
      if(Loverflow.ne.0) goto 490
c
c ---------- LKr ----------

C   Towers
      if(iptr+(JBL+JSTZ+JSTFI)*2.gt.evc_wleng) then
         print *,'FILERW: event buffer overflow'
         Loverflow=1
         if(iptr+(JBL+JSTZ)*2.lt.evc_wleng) then
            JSTFI=(evc_wleng-(iptr+(JBL+JSTZ)*2))/2
            print *,'truncate LKR phi-strip data to',JSTFI*2
         elseif(iptr+JBL*2.lt.evc_wleng) then
            JSTZ=(evc_wleng-(iptr+JBL*2))/2
            JSTFI=0
            print *,'truncate LKR z-strip data to',JSTZ*2
            print *,'skip phi-strip data'
         elseif(iptr.lt.evc_wleng) then
            JBL=(evc_wleng-iptr)/2
            JSTZ=0
            JSTFI=0
            print *,'truncate LKR tower data to',JBL*2
            print *,'skip phi-strip data'
         else
            print *,'skip LKR data'
            JBL=0
            JSTZ=0
            JSTFI=0
         endif
      endif
      rrdir(2,ss_lkr)=(JBL+JSTZ+JSTFI)*2
      if((JBL+JSTZ+JSTFI).eq.0 ) then
         rrdir(1,ss_lkr)=0
      else
         rrdir(1,ss_lkr)=iptr+1
      endif
C   Tower's amplitude
      do i=1,JBL
         raww(iptr+1)=-(IBL(i*2-1)+ss_lcn(1,ss_lkr)-1)
         raww(iptr+2)=IBL(i*2)
         iptr=iptr+2
      enddo
C   Z strips
      do i=1,JSTZ
         raww(iptr+1)=-(ISTZ(i*2-1)+ss_lcn(1,ss_lkr)-1)
         raww(iptr+2)=ISTZ(i*2)
         iptr=iptr+2
      enddo
C   FI strips
      do i=1,JSTFI
         raww(iptr+1)=-(ISTFI(i*2-1)+ss_lcn(1,ss_lkr)-1)
         raww(iptr+2)=ISTFI(i*2)
         iptr=iptr+2
      enddo
      if(Loverflow.ne.0) goto 490

#ifdef ZEB
      write(22) nblhits,nzhits,nfihits

      if(nblhits.gt.0)then
         do i=1,nblhits
            a=(iambl(i)-70)*0.1
            write(22) krbl(i),kzbl(i),kfibl(i),a
         end do
      endif

      if(nzhits.gt.0)then
         do i=1,nzhits
            a=(iamstz(i)-70)*0.1
            write(22) krz(i),kzz(i),a
         end do
      endif

      if(nfihits.gt.0)then
         do i=1,nfihits
            a=(iamstfi(i)-70)*0.1
            write(22) krfi(i),kfifi(i),a
         end do
      endif
#endif

c ---------- CsI ----------

#if 0
      print *,' CsNcr1, CsNcr2 ',NCsNcr(1),NCsNcr(2)
      print *,ss_csi1,ss_lcn(1,ss_csi1),ss_csi2,ss_lcn(1,ss_csi2)
#endif
      if(iptr+NCsNcr(1)*2.gt.evc_wleng) then
         print *,'FILERW: event buffer overflow'
         NCsNcr(1)=(evc_wleng-iptr)/2
         print *,'truncate CsI1 data to',NCsNcr(1)*2
         Loverflow=1
      endif
      rrdir(2,ss_csi1)=NCsNcr(1)*2
      if(NCsNcr(1).eq.0) then
         rrdir(1,ss_csi1)=0
      else
         rrdir(1,ss_csi1)=iptr+1
      endif
      do i=1,NCsNcr(1)
         raww(iptr+1)=-(ICsIcr(i,1)+ss_lcn(1,ss_csi1))
#if 0
         print *,ICsIcr(i,1),raww(iptr+1)
#endif
         raww(iptr+2)=ICsAmp(i,1)
         iptr=iptr+2
      enddo
      if(Loverflow.ne.0) goto 490

      if(iptr+NCsNcr(2)*2.gt.evc_wleng) then
         print *,'FILERW: event buffer overflow'
         NCsNcr(2)=(evc_wleng-iptr)/2
         print *,'truncate CsI-l to',NCsNcr(2)*2
         Loverflow=1
      endif
      rrdir(2,ss_csi2)=NCsNcr(2)*2
      if(NCsNcr(2).eq.0) then
         rrdir(1,ss_csi2)=0
      else
         rrdir(1,ss_csi2)=iptr+1
      endif
      do i=1,NCsNcr(2)
         raww(iptr+1)=-(ICsIcr(i,2)+ss_lcn(1,ss_csi2))
#if 0
         print *,ICsIcr(i,2),raww(iptr+1)
#endif
         raww(iptr+2)=ICsAmp(i,2)
         iptr=iptr+2
      enddo
      if(Loverflow.ne.0) goto 490

c ---------- Muon system ----------

      if(iptr+mu_hits.gt.evc_wleng) then
         print *,'FILERW: event buffer overflow'
         mu_hits=(evc_wleng-iptr)/2
         print *,'truncate MU data',mu_hits*2
      endif
      rrdir(2,ss_mu)=mu_hits*2
      if(mu_hits.eq.0 ) then
         rrdir(1,ss_mu)=0
      else
         rrdir(1,ss_mu)=iptr+1
      endif
      do i=1,mu_hits
         raww(iptr+1)= -mu_ch_hit(i)
         raww(iptr+2)=mu_ch_time(i)
         iptr=iptr+2
      enddo
      if(Loverflow.ne.0) goto 490
c-----------------------------------------------------------------------

#ifdef ZEB
      ncsis=0
      ncsi1=0
      ncsi2=0
      do i=1,4
         do j=1,12
            do k=1,15
               if(enloss(1,i,j,k).gt.0.)ncsi1=ncsi1+1
#if 0
               if(enloss(1,i,j,k).gt.0.) then
                  ncsi1=ncsi1+1
                  do i1=i+1,4
                     if(enloss(1,i1,j,k).gt.0) then
                        ncsis=ncsis+1
                        print *,'i,j,k,i1,ncsis: ',i,j,k,i1,ncsis
                        print *,'eloss,eloss1: ',
     &                       enloss(1,i,j,k),enloss(1,i1,j,k)
                     endif
                  enddo
               endif
#endif
               if(enloss(2,i,j,k).gt.0.) ncsi2=ncsi2+1
            end do
         end do
      end do
#if 0
      print *,'ncsi1,ncsi2:',ncsi1,ncsi2
#endif
      write(22)ncsi1
      if(ncsi1.gt.0)then
         do i=1,4
            do j=1,12
               do k=1,15
                  if(enloss(1,i,j,k).gt.0.)
     &                 write(22) i,j,k,enloss(1,i,j,k)
               end do
            end do
         end do
      endif

      write(22) ncsi2
      if(ncsi2.gt.0)then
         do i=1,4
            do j=1,12
               do k=1,15
                  if(enloss(2,i,j,k).gt.0.)
     &                 write(22) i,j,k,enloss(2,i,j,k)
               end do
            end do
         end do
      endif
#endif

#if 0
C Check integrity of data fo DC
      ndc=rrdir(2,ss_dc)
      if(ndc.gt.0) then
         ip=rrdir(1,ss_dc)
         do i=1,ndc,2
            iadr=-raww(ip)
            if(iadr.lt.ss_lcn(1,ss_dc).or.iadr.gt.ss_lcn(2,ss_dc)) then
               print *,' DC wrong adr:',iadr-ss_lcn(1,ss_dc),i
            endif
            ip=ip+2
         enddo
      endif
#endif
c
 490  rawl(evl_length)=iptr	! record length in words (1st word in record)

#if 0
      print *,' rec length ',iptr
#endif
C   Write to data file (need swap bytes)

      iw = fwritef(rawl(evl_length),4,1,ifile)
      iw = fwritef(raww(evw_recid),2,2,ifile)
      iw = fwritef(rawb(evb_recfmt),1,4,ifile)
      iw = fwritef(rawl(evl_run),4,23,ifile)
      iw = fwritef(rrdir,2,evc_dirl*2,ifile)
      iw = fwritef(raww(evc_whl+1),2,iptr-evc_whl,ifile)
#if 0
      print *,'rec was writting'
#endif

      return
      end
