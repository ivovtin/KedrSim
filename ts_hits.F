      subroutine ts_hits
c
c  Defines hits of Tagging system
c
c  Author: L.Romanov
c
c
c      implicit none
      integer iset,idet

*-----e_--------------------------------------
c
      integer e_nhit
      parameter(e_nhit=9)

      real e_orig(e_nhit),e_fact(e_nhit)
      integer e_nbit(e_nhit)
      character *5 e_chhit(e_nhit)

*________________hodoscope______________________
c
      integer h_nhit,h_nv
      parameter(h_nhit=6)
      parameter(h_nv=3)
      real h_orig(h_nhit),h_fact(h_nhit)
      integer h_nbit(h_nhit),h_nbitsv(h_nv)
      character *4 h_chhit(h_nhit),h_nmsv(h_nv)

*-----e_--------------------------------------
c
      data e_chhit/'E_X ','E_Y ','E_Z ','E_SX1','E_SY1',
     *             'E_SX2','E_SY2','E_PF','E_TR'/
      data e_nbit/2*20,16,4*20,2*16/ 
      data e_orig/250.,250.,1.,1.,1.,1.,1.,0.,0./ 
      data e_fact/2*1000.,10.,4*80000.,1000.,1./
      data h_nmsv/'MOTH','TSG*','TSTB'/
      data h_nbitsv/3,3,10/

*________________hodoscope______________________
c
      data h_chhit/'H_R0','H_RR','H_T ','H_IT','H_TR','H_E'/
      data h_nbit/6*16/ 
      data h_orig/10.,10.,0.,0.,500.,0./
      data h_fact/2*1000.,1.,1.,1.,1000./
c
      call gsdetv('TSEN','TSD1',2,100,100,iset,idet)
c
      call gsdetv('TSEN','TSD2',2,100,100,iset,idet)
c
      call gsdeth('TSEN','TSD1',e_nhit,e_chhit,e_nbit,e_orig,e_fact)
c
      call gsdeth('TSEN','TSD2',e_nhit,e_chhit,e_nbit,e_orig,e_fact)
c
*________________hodoscoph______________________
c
*      call gsdet     
*('TSHD','TSTB',h_nv,h_nmsv,h_nbitsv,1,1000,1000,iset,idet)
c
      call gsdetv('TSHD','TSFR',1,1000,1000,iset,idet)
c
      call gsdeth('TSHD','TSFR',h_nhit,h_chhit,h_nbit,h_orig,h_fact)
c
*      call gpsets('*','*')
c
      call ts_hits_dat
c
      call ts_hits_hd0
c
      end
