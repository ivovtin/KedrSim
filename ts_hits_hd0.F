*++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine ts_hits_hd0
c***************************************************************
c
c  Call from ts_hits
c
c  Author: L.Romanov
c
c***************************************************************
c      implicit none
      include 'ts_cmb.inc'
      integer i,k_tube,k_arm,k_hd,k_layer,k_odd
      real d0

C   XC0 - CENTERS OF TUBES;      XC - WIRES.
      RC=3.

      do k_layer=1,6

        do k_odd=1,2
          z_plane(2*(k_layer-1)+k_odd)=48.*k_layer+7.*k_odd-55.
        enddo                

      enddo

      do k_arm=1,2
        k_tube=0

        do k_hd=1,4
          d0=d_shift(k_hd,k_arm)

          do k_layer=1,6
            d0=-d0

            do  k_odd=1,2

              do  i=1,nt_plane(k_hd)
                k_tube=k_tube+1
                zyc(k_tube,k_arm)=z_plane(2*(k_layer-1)+k_odd)
                xc0(k_tube,k_arm)=8.*i+4.*(k_odd-1)
                xc(k_tube ,k_arm)=xc0(k_tube,k_arm)+d0
              enddo

            enddo

          enddo

        enddo                     ! on k_hd

      enddo                     ! on k_arm

      end
