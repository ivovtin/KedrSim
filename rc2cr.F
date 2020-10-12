*CMZU:  1.00/01 16/10/96  17.53.46  by  MALYSHEV V.M.
*-- Author :
      Function RC2CR(t,tau)
c-----------------------------------------
c   RC-2CR shaper,LAPLAS method,
c         2      3
c   F(P)=P /(1+P),
c-----------------------------------------
      if(t.lt.0.) then
        rc2cr=0
        return
      endif
      x=t/tau
      rc2cr=exp(-x)*x*(1-x/2)
      end
