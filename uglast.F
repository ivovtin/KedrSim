      SUBROUTINE UGLAST
c*************************************************************
c
c   Corrections: 31.05.1999. S.F.Ganzhur,V.A.Tayursky
c                27.09.2003. V.A.Tayursky
c                22.03.2004. V.A.Tayursky
c
c************************************************************
c
#include "gcflag.inc"
#include "gckine.inc"
      include 'dqfile.inc'
      include 'ReadNat/rr_def.inc'
c
      COMMON /GCTIME/    TIMINT,TIMEND,ITIME,IGDATE,IGTIME
      COMMON /QUEST/     IQUEST(100)
      COMMON /LUN/       LUNPTR
      COMMON /RZFZFILES/ IRZFILE,IFZFILE,ISIMOUT
      COMMON /HIS_SIMUL/ IHISTOG
c
      CALL GLAST
c
c fill EOR event record header
c
      eorb(rrb_minute)=MOD(IGTIME,100)  !   current minute
      eorb(rrb_hour)=IGTIME/100         !   current hour
      eorb(rrb_day)=MOD(IGDATE,100)     !   current day
      eorb(rrb_month)=MOD(IGDATE/100,100) !   current month
      eorb(rrb_year)=IGDATE/10000       !   current year (last 2 digits)
      eorb(rrb_year_1)=0                !   current year (first 2 digits)
      eorl(rrl_events)=ievent           ! number of events (0 for SOR)
c
      do i=1,rrc_dirl
        eordir(1,i)=0
        eordir(2,i)=0
      enddo
c
c write to file only header of EOR now!
      eor_lng=rrc_whl
c
c write to simxxxxxx.dat EOR record
      eorl(rrl_length)=eor_lng  ! record length in words (1st word in record)

c       print *,' EOR rec length ',sor_lng
c Write to data file (need swap bytes)
c
      IF(ISIMOUT.EQ.1) THEN
        iw = fwritef(eorl(rrl_length),4,1,ifile)
        iw = fwritef(eorw(rrw_recid),2,2,ifile)
        iw = fwritef(eorb(rrb_recfmt),1,2,ifile)
        iw = fwritef(eorw(rrw_run),2,1,ifile)
        iw = fwritef(eorb(rrb_minute),1,8,ifile)
        iw = fwritef(eorl(rrl_energy),4,12,ifile)
        iw = fwritef(eorw(rrw_field),2,1,ifile)
        iw = fwritef(eorb(rrb_zero_2),1,18,ifile)
        iw = fwritef(eordir,2,rrc_dirl*2,ifile)
c      iw = fwritef(eorw(rrc_whl+1),2,eor_lng-rrc_whl,ifile)

c Close data file
        ierr=fclosef(ifile)
      END IF
c
c      call rdmout(seed)
c      open(21,file='random.dat',status='old')
c      write(21,1)seed
c1     format(2x,z8)
c      close(21)
c
      IF(IFZFILE.EQ.1) CALL GCLOSE(LUNPTR,IER)  ! used for FZ file
c
      IF( IHISTOG.EQ.1) THEN   ! otput file with histograms
c
        CALL SEARCH_FREE_UNIT(LU)
c
        CALL HROPEN(LU,'hbook','hist.res','N',1024,ISTAT)
c
        CALL HROUT(0,ICYCLE,' ')   
c
        CALL HREND('hbook')
c
        PRINT 10
        close(lu)
c
      END IF
c
 10   FORMAT(/,' KEDRSIM wrote file hist.res to disk',/)
c
      END
