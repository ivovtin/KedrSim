      subroutine lkg2p(igind,kr,kz,kfi)
c converts general index to physical indexes
c     kfi,kz/=0  => towers
c     kr<0+towers=> non-feel. adc
c     kfi=0      => z  strips
c     kz=0       => fi strips

      integer *2 ibuf(3,9600),ibuf24(24),igr
      common/p2g/icall,ibuf
      save/p2g/

c      include 'kdpath.inc'
      character *80 file
       
      lun=10
      if(icall.ne.1) then ! first call
c        file=kdpath(:index(kdpath,' ')-1)//'lkr_cdb.dat'
        file='lkr_cdb.dat'
        open(lun,file=file,status='old',err=741)
	goto 742
741     file=KEDR_RELEASE//'KEmcData/lkr_cdb.dat'
        open(lun,file=file,status='old')
742	do i=1,9600
	    read (lun,*) ibuf24,ibuf(3,i),ibuf(2,i),ibuf(1,i),igr
	enddo
	close(lun)
	icall=1
      endif

	kr=0
	kz=0
	kfi=0

	if(igind.gt.9600.or.igind.lt.1) return
c         print *,'******lkg2p-wrong gind> igind=',igind
      if(ibuf(1,igind).eq.999) return

	kr=ibuf(1,igind)
	kz=ibuf(2,igind)
	kfi=ibuf(3,igind)

	end
