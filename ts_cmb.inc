*-*- Fortran -*-
*  z_plane Z-coordinates of planes
*  nt_plane(k_hd) - number of tubes in plane k_hd
*  nt_hd(k_hd)  - number of tubes in hodoscope k_hd
*  kt0_hd(k_hd) - begin of hodoscope k_hd in (720,k_arm)
*  kt0_hd(k_hd) -  end of hodoscope k_hd in (720,k_arm)
*! calculated in ts_tra_dat.f and ts_hd0_geom.f(withiut corrections)
*  irm1,irm2 - numbers of rotation matrixes
*
      real xc,zyc,xc0,rc,z_plane,d_shift
      integer nt_plane,nt_hd,kt0_hd,kt9_hd,irm1,irm2
      common/ts_hd0/xc(720,2),zyc(720,2),xc0(720,2)
     *, d_shift(4,2)
*    the same for elctron and positron side
     *,rc   ! tube radius
     *,z_plane(12)
     *,nt_plane(4)
     *,nt_hd(4)
     *,kt0_hd(4),kt9_hd(4),irm1,irm2

