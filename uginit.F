      SUBROUTINE UGINIT
c*****************************************************************
c
c  Corrections: 
c
c  30 Apr 2002, V.A.Tayursky: Modification of Br (cards DTB1,...) deleted            
c  31 May 2002, V.A.Tayursky: Corrected drawing in batch mode 
c  21 Jan 2003, V.A.Tayursky: Corrected operations for negative KGEN
c  20 Nov 2003, V.A.Tayursky: Added possibility not to write output file,
c                             call pst_init instead call pst_pprop
c  11 Mar 2005, V.A.Tayursky: added call ludat2m
c  10 Aug 2007, V.A.Tayursky: added reading kedr_jsnew.dat
c  17 Jun 2008, V.A.Tayursky: renamed PST_INIT -> PstInit
c  14 Dec 2012, V.A.Tayursky: added gcphys.inc, print out GHEISHA
c  31 Jan 2014, V.A.Tayursky: changed length variable FILE 40 -> 41 bite
c*****************************************************************
c
#include "gcflag.inc"
#include "gckine.inc"
#include "gclist.inc"
#include "gcphys.inc"
#include "KedrSim/kedrif.inc"
#include "KedrSim/fiel.inc"  
#include "ReadNat/rr_def.inc"
#include "ReadNat/re_def.inc"
#include "ReadNat/mc_def.inc"
#include "ReadNat/mccard_def.inc"
#include "KedrSim/dqfile.inc"
#include "KedrSim/drawpar.inc"
#include "KedrSim/atccom.inc"
#include "KedrSim/ver_event.inc"
#include "KedrSim/beam_sigma.inc"
c
c     COMMON /GCTIME/    TIMINT,TIMEND,ITIME,IGDATE,IGTIME
#include "gctime.inc"
      COMMON /QUEST/     IQUEST(100)
      COMMON /LUN/       LUNPTR
      COMMON /RZFZFILES/ IRZFILE,IFZFILE,ISIMOUT
      COMMON /KEDR_GEN/  KHEP,KGEN,GENPAR(20)
      COMMON /KSI/       KSIFL
      COMMON /KEDR_MBJS/ MOBRJS
      COMMON /FLUKAP/  RNFLUK  ! version of FLUKA
c
      character *40 ter
      character *41 file
      character *21 nampart
      dimension ub(20)
      integer sor_lng
c
      CALL GINIT
c
c     Redefine input/output streams for FFGO package
c
      IF(KSIFL.EQ.1) THEN 
      	OPEN(9,FILE='mc.cards')
      	CALL FFSET('LINP',9)
      ENDIF
c
      CALL UGCARD   ! define non-standard cards
c
      CALL GFFGO    ! read user & GEANT cards
c
      IF(KSIFL.EQ.1) CLOSE (9)
c
      IF(IHADR.EQ.4.AND.ABS(RNFLUK).LT.0.1) RNFLUK=1.0 ! 1-st version of FLUKA 
c
c     GEANT's ZEBRA areas initialization
c
      CALL GZINIT
c
c     For drawing in batch and interactive modes
c		
      IF(KSIFL.EQ.1) THEN     ! interactive mode
c
        CALL GDINIT
c
        ELSEIF(IAX.GE.1.AND.IAX.LE.3) THEN
c
        CALL HPLINT(1)
c
        CALL GDINIT

c       Redirect input from mc.cards to terminal
        CALL GETENVF('SSH_TTY',TER)
        OPEN(5,FILE=TER)
c
      END IF
c
      CALL GPART     ! create 'JPART' data structure
c
      CALL GMATE     ! create 'JMATE' data structure
c
c     'JMATE' structure (materials description) + geometry description
c
      CALL UGMATE
c
      CALL UGEOM

c === magnetic field setup
	  if(FIELDmap > 0.5) then
	  	call set_gufld_complex
	  else
	    call set_gufld_simple
	  endif


c
c  Creation of file with KEDR geometry for ROOT graphics
c
      IF(IRZFILE.EQ.1) CALL GRFILE(1,'simout/geometry.rz','NO')
c
c     Prepare cross-sections and energy loss tables for all used materials
c
      CALL GPHYSI
c
      CALL HIS_INIT      ! define user's histograms
c
      ISIMOUT=1        ! to write output file sim...? 
      IF(IDRUN.LT.0) ISIMOUT=0
      NRUN=IABS(IDRUN) ! run number: in file name simXXXXXX.dat, XXXXXX=NRUN
c
      IKINE=IABS(KGEN)            
c
      DO I=1,10
 	PKINE(I)=GENPAR(I)
      END DO
c
      ENERGY=GENPAR(1)       
      IF(IABS(KGEN).eq.1) THEN     ! single particle
        IKINE=GENPAR(1)+0.5
c
        call gfpart(ikine,nampart,itrtype,pamass,pcharge,ptlife,ub,nwb)      
c
        ENERGY=SQRT(GENPAR(2)**2+PAMASS**2)
      END IF

c fill SOR event record header
      sorw(rrw_recid)=rrc_sor  		! record ID = 100 - SOR
      sorw(rrw_hdrl)=rrc_whl    	! header length in words (16 bits)
      sorb(rrb_recfmt)=2		! record format
      sorb(rrb_dirl)=rrc_dirl  		! directory length in partitions
      sorw(rrw_run)=nrun		! run number
      sorb(rrb_minute)=MOD(IGTIME,100)  !   current minute
      sorb(rrb_hour)=IGTIME/100         !   current hour
      sorb(rrb_rtype)=64                ! run type (0-DAQ,64-SIMULATION)
      sorb(rrb_day)=MOD(IGDATE,100)     !   current day
      sorb(rrb_month)=MOD(IGDATE/100,100)    !   current month
      sorb(rrb_year)=IGDATE/10000       !   current year (last 2 digits)
      sorb(rrb_year_1)=0                !   current year (first 2 digits)
      sorl(rrl_energy)=int(energy*1000000.) ! energy in KeV
      sorl(rrl_events)=0		! number of events (0 for SOR)
      sorw(rrw_field)=fiel*1000.	! magnetic field in gauss
c
c fill SOR event record directories
      do i=1,rrc_dirl
        sordir(1,i)=0
        sordir(2,i)=0
      enddo
      sordir(1,1)=rrc_whl+1		! "TRIGGER" record
      sordir(2,1)=0			
      sordir(1,2)=rrc_whl+sordir(2,1)+1	! record of beam center and sigma	
      sordir(2,2)=8*4/2                 ! length of 8 real values in 2-byte words
      iptr=rrc_lhl+sordir(2,1)/2	! current pointer in 4-byte words
      sorf(iptr+1)=vertx
      sorf(iptr+2)=verty
      sorf(iptr+3)=vertz
      sorf(iptr+4)=Sigma_X*Sigbeam_flag
      sorf(iptr+5)=Sigma_Y*Sigbeam_flag
      sorf(iptr+6)=Sigma_Z*Sigbeam_flag
      sorf(iptr+7)=Beam_Theta
      sorf(iptr+8)=Beam_Phi
c
c
c write to file SOR header and directories
      sor_lng=rrc_whl+SORDIR(2,1)+SORDIR(2,2)
c
c fill part of EOR event record header, the rest in uglast.f
      eorw(rrw_recid)=rrc_eor           ! record ID = 101 - EOR
      eorw(rrw_hdrl)=rrc_whl            ! header length in words (16 bits)
      eorb(rrb_recfmt)=1                ! record format
      eorb(rrb_dirl)=rrc_dirl           ! directory length in partitions
      eorw(rrw_run)=nrun                ! run number
      eorb(rrb_rtype)=64                ! run type (0-DAQ,64-SIMULATION)
      eorl(rrl_energy)=int(energy*1000000.) ! energy in KeV
      eorl(rrl_events)=0                ! number of events (0 for SOR)
      eorw(rrw_field)=fiel              ! magnetic field in gauss
c
      do i=1,rrc_dirl
        eordir(1,i)=0
        eordir(2,i)=0
      enddo
c
c fill event record header
      raww(evw_recid)=1  		! record ID = 1
      raww(evw_hdrl)=evc_whl    	! header length in words (16 bits)
      rawb(evb_recfmt)=3		! record format
      rawb(evb_dirl)=evc_dirl  		! directory length in partitions
      rawl(evl_run)=nrun		! run number
      rawl(evl_energy)=int(energy*1000000.) ! energy in KeV
c
      do i=1,evc_dirl
        rrdir(1,i)=0
        rrdir(2,i)=0
      enddo
c
c fill monte carlo record header
      mcw(mcw_recid)=1001  	! record ID = 1001
      mcw(mcw_hdrl)=mcc_whl    	! header length in words (16 bits)
      mcb(mcb_recfmt)=1		! record format
      mcb(mcb_dirl)=mcc_dirl  	! directory length in partitions
      mcl(mcl_run)=nrun		! run number
      mcl(mcl_energy)=int(energy*1000000.) ! energy in KeV
c
      do i=1,mcc_dirl
        mcdir(1,i)=0
        mcdir(2,i)=0
      enddo
c
      IF(ISIMOUT.EQ.1) THEN
        write(file,5) 'simout/sim',nrun,'.dat'
5       format(a10,i6.6,a4)
        write(6,*) '=====     Output file     ====='
        write(6,10) file
10      format(a)
        ifile=fopenf(file,'w')
        if(ifile.lt.0) then
	print *,'Can not open output file ',file,
     +          ' or dir simout/ does not exist'
	stop
      endif     
      
c
c write to simxxxxxx.dat SOR record
      sorl(rrl_length)=sor_lng  ! record length in words (1st word in record)
c
c       print *,' SOR rec length ',sor_lng
c Write to data file (need swap bytes)
c
      iw = fwritef(sorl(rrl_length),4,1,ifile)
      iw = fwritef(sorw(rrw_recid),2,1,ifile)
      iw = fwritef(sorw(rrw_hdrl),2,1,ifile)
      iw = fwritef(sorb(rrb_recfmt),1,2,ifile)
      iw = fwritef(sorw(rrw_run),2,1,ifile)
      iw = fwritef(sorb(rrb_minute),1,8,ifile)
      iw = fwritef(sorl(rrl_energy),4,12,ifile)
      iw = fwritef(sorw(rrw_field),2,1,ifile)
      iw = fwritef(sorb(rrb_zero_2),1,18,ifile)
      iw = fwritef(sordir,2,rrc_dirl*2,ifile)
      iw = fwritef(sorf(rrc_lhl+1),4,(sor_lng-rrc_whl)/2,ifile)
c	print *, 'SOR length=',sorl(rrl_length)*2,' data length=',(sor_lng-rrc_whl)/2*4,' status=',iw
c      iw = fwritef(sorw(rrc_whl+1),2,sor_lng-rrc_whl,ifile)
c
      call writemccard
      END IF
c 
      IF(IFZFILE.EQ.1) THEN ! creation of rz-files
c
        CALL SEARCH_FREE_UNIT(LUNPTR)
c
        CALL CFOPEN(LUNPTR,0,900,'w ',0,'simout/event.fz',ISTAT)
c
        IQUEST(1) = LUNPTR
c
        CALL GOPEN(LUNPTR,'OXL',900,IER)
c 
        print *, 'UGINIT lun= ', LUNPTR
      END IF
c
      IF(MOBRJS.EQ.1) THEN
c
c  Modification of particle properties and decays table of jetset 7.4 
c
        CALL SEARCH_FREE_UNIT(LU1)
c
        FILE='kedr_jsnew.dat'
        OPEN (lu1,FILE = file,status='old',err=641)
c
        GOTO 642
c
 641    file=KEDR_RELEASE//'KedrSim/kedr_jsnew.dat'
c
          OPEN (lu1,FILE = file,status='old')
c   
 642    CALL LUUPNEW(2,LU1)     ! обновление параметров частиц
c
        CLOSE(LU1)
c
        CALL LUBRNORM           ! нормируем суммы <<бранчингов>> к 1

      END IF
c
c                    ! reads table of particle properties,                
      CALL PstInit   ! preparation of table for PhSp simulation with GenPST
c
      CALL LUDAT2M   ! modification of data for particles of jetset7.4
c                    ! according to table kedr_part.dat
c
      IF(MOBRJS.EQ.2) CALL LULIST(12) ! print new table of jetset 7.4
c
      CALL GLOOK('VOLU',LPRIN,NPRIN,IPRES)
      IF(IPRES.NE.0) THEN
c         CALL GPVOLU(0)
         CALL UGPVOLU(0)
      ENDIF
      CALL GLOOK('MATE',LPRIN,NPRIN,IPRES)
      IF(IPRES.NE.0) THEN
         CALL GPMATE(0)
      ENDIF
      CALL GLOOK('TMED',LPRIN,NPRIN,IPRES)
      IF(IPRES.NE.0) THEN
         CALL GPTMED(0)
      ENDIF
c
      RETURN
      END
