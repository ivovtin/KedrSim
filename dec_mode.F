      subroutine dec_mode(n_d,J,e_gamma)
      dimension p_d(8), en_gamma(6)
      data en_gamma/0.285,0.254,0.233,0.202,0.178,0.165/
c     J=       0    1    2
c       D1
      data p_d/0.60,0.92,0.936,  1.,
c       D2
     *            0.51,0.77,0.784,  1./
c
      x=rndm(dummy)
      do 1 i=1,4
      if(x.gt.p_d(4*(n_d-1)+i))goto 1
      J=i-1
      goto 2
1     continue
2     continue
c
      if(n_d.eq.1)then
      if(J.eq.0)e_gamma=en_gamma(1)
        if(J.eq.1)e_gamma=en_gamma(2)
      if(J.eq.2)e_gamma=en_gamma(3)
      endif
c
      if(n_d.eq.2)then
      if(J.eq.0)e_gamma=en_gamma(4)
      if(J.eq.1)e_gamma=en_gamma(5)
      if(J.eq.2)e_gamma=en_gamma(6)
      endif
c
      return
      end
