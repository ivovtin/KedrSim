*CMZU:          21/05/97  11.21.35  by  YURI SKOVPEN
*CMZU:  1.00/01 16/10/96  17.53.46  by  MALYSHEV V.M.
*-- Author :
      function cfcurt(z1,z2,y,a)
C
	include 'lk_curbl.inc'
C
	include 'lk_hitp.inc'
C
      if(y.lt.0) print *,'****cfcurt>y=',y
      if(a.lt.0.or.a.gt.dl) print *,'****cfcurt>a=',a
      if(z1.gt.z2) print *,'****cfcurt>z1,z2=',z1,z2
      ix=a/da+1.
      if(ix.gt.nxc) ix=nxc
      iy=y/dy+.5
      if(iy.gt.nyc) iy=nyc
      iz1=abs(z1)/dz+.5
      if(iz1.gt.nzc) iz1=nzc
      iz2=abs(z2)/dz+.5
      if(iz2.gt.nzc) iz2=nzc
      if(z1.ge.0.and.z2.ge.0) cfcurt=cur(ix,iy,iz2)-cur(ix,iy,iz1)
      if(z1.lt.0.and.z2.ge.0) cfcurt=cur(ix,iy,iz2)-cur(ix,iy,0)+
     1                               cur(ix,iy,iz1)-cur(ix,iy,0)
      if(z1.lt.0.and.z2.lt.0) cfcurt=cur(ix,iy,iz1)-cur(ix,iy,iz2)
      cfcurt=-cfcurt*nxc   ! charge drifts from electrode
      end
