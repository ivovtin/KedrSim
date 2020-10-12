*CMZU:  1.00/01 16/10/96  17.53.46  by  MALYSHEV V.M.
*-- Author :
      function fgct(y1,y2,a)
C define current,induced on strip (Pospelov_G); fgc=J(t)*Tdr=dQ/da*d
C y1,y2 - strip sizes (a.u.)
C a - distance from strip
C d - size of gap
C q=1,t_drift=1; table used
	include 'fgctc.inc'
C
      i=a/daf+.5
      if(i.lt.0) i=0
      if(i.gt.na) i=na
      c=ct(i)
      i=abs(y1)/dyf+.5
      if(i.gt.ny) i=ny
      if(y1.lt.0.) i=-i
      e1=et(i)
      i=abs(y2)/dyf+.5
      if(i.gt.ny) i=ny
      if(y2.lt.0.) i=-i
      e2=et(i)
      fgct=(e2*c-1.)/(2*e2*c-1.-e2*e2)-(e1*c-1.)/(2*e1*c-1.-e1*e1)
      end
