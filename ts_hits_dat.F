      subroutine ts_hits_dat
c
c  Call from ts_hits
c
c      implicit none
      include 'ts_cmb.inc'
      integer k_hd

      data nt_plane/24,12,12,12/
      data d_shift/4*0.3,4*0.3/

      nt_hd(1)=nt_plane(1)*12
      kt0_hd(1)=1
      kt9_hd(1)=nt_hd(1)

      do k_hd=2,4
        nt_hd(k_hd)=nt_plane(k_hd)*12
        kt0_hd(k_hd)= kt0_hd(k_hd-1)+nt_hd(k_hd-1)
        kt9_hd(k_hd)= kt9_hd(k_hd-1)+nt_hd(k_hd)
      enddo

      end
