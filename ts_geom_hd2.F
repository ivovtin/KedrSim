      subroutine ts_geom_hd2
c*****************************************************************
c
*@ :ts_geom_hd2: define 288 tube hodoscope; tube coordinates are 
*@  calculated here
*@
c
c   Author: L.Romanov
c
c***************************************************************** 
c      implicit none
      include 'ts_cmb.inc'
c      real zyc,xc0,   by
      integer i,i1,i2,i3,  nsl   ,k_rot

      i=0
      NSL=24

      DO I1=1,6
        IF(i1/2*2.eq.i1) THEN
          k_rot=irm2
        ELSE
          k_rot=irm1
        ENDIF

        DO  I2=1,2

          DO  I3=1,NSL
            I=I+1
            XC0_S=(8.*I3+4.*(i2-1))*0.1       -13.  !
            ZYC_S=(48.*I1+7.*I2-55.)*0.1      -12.5 !
            by=0.

            call gspos('TSTB',i,'TSG2',xc0_S,by,zyc_S,k_rot,'ONLY')

          enddo

        enddo

      enddo

      END
