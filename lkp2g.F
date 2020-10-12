      subroutine lkp2g(kr,kz,kfi,igind)
c converts physical indexes to general index
c     kfi,kz/=0  => towers
c     kr<0+towers=> non-feel. adc
c     kfi=0      => z  strips
c     kz=0       => fi strips

      integer *2 ibuf(3,9600),ibuf24(24),igr
      common/p2g/icall,ibuf
      save/p2g/

c     include 'kdpath.inc'
      character *80 file

      lun=10
      if(icall.ne.1) then ! first call
c	 print *,'lkr channels database reading...'
c         file=kdpath(:index(kdpath,' ')-1)//'lkr_cdb.dat'
         file='lkr_cdb.dat'
         open(lun,file=file,status='old',err=741)
	 goto 742
741      file=KEDR_RELEASE//'KEmcData/lkr_cdb.dat'
         open(lun,file=file,status='old')
742      do i=1,9600
	    read (lun,*) ibuf24,ibuf(3,i),ibuf(2,i),ibuf(1,i),igr
	 enddo
	 close(lun)
c	 print *,'OK'
	 icall=1
       endif
	    
      if(kz.ne.0.and.kfi.ne.0) then ! towers
        nr=iabs(kr)
        if(nr.eq.0.or.nr.gt.3.or.iabs(kz).gt.10-nr.or.
     +     kfi.lt.1.or.kfi.gt.48) then
          print *,'******lkp2g-wrong call> towers:kr,kz,kfi=',kr,kz,kfi
	  return
        endif
      endif

      if(kfi.eq.0) then ! z-strips
        if(kr.eq.0.or.iabs(kr).gt.4.or.kz.eq.0.
     +     or.iabs(kz).gt.129-5*iabs(kr)) then
          print *,'******lkp2g-wrong call> z-strips:kr,kz,kfi=',kr,kz,kfi
	  return
	endif
      endif

      if(kz.eq.0) then ! fi-strips
        if(kr.lt.1.or.kr.gt.4.or.kfi.lt.1.or.kfi.gt.768) then
          print *,'******lkp2g-wrong call> fi-strips:kr,kz,kfi=',kr,kz,kfi
	  return
	endif
      endif

      do i=1,9600
        if(ibuf(1,i).eq.kr.and.ibuf(2,i).eq.kz.and.ibuf(3,i).eq.kfi) then
	   igind=i
	   return
	endif   
      enddo
      
      print *,'******lkp2g-conversion error> kr,kz,kfi=',kr,kz,kfi

      end
