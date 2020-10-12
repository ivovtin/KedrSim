      SUBROUTINE WRITEMCCARD

#include "gcflag.inc"
#include "gccuts.inc"
#include "gcphys.inc"
#include "ReadNat/mccard_def.inc"
#include "ReadNat/mccardindex.inc"
#include "KedrSim/celpar.inc"
#include "KedrSim/csi_xyzrot.inc"
#include "KedrSim/lk_digp.inc"
#include "KedrSim/lk_geomp.inc"
#include "KedrSim/lk_xyzrot.inc"
#include "KedrSim/mu_par.inc"
#include "KedrSim/dqfile.inc"       
       integer i
       mccardL(mccdl_length)=mccd_whl+mccd_fullS*2
       mccardW(mccdw_recid)=1002        
       mccardW(mccdw_hdrl)=mccd_whl
       if(iswit(2).eq.-1) then
       	 mccardW(mccdw_ver)=0	! old CSI rotation parameters
       else
       	 mccardW(mccdw_ver)=1	! new CSI rotation parameters
       endif
       mccardW(mccdw_empty)=0
       mccardR(mccd_cuts)=CUTGAM
       mccardR(mccd_cuts+1)=CUTELE
       mccardR(mccd_cuts+2)=CUTNEU
       mccardR(mccd_cuts+3)=CUTHAD
       mccardR(mccd_cuts+4)=CUTMUO
       mccardR(mccd_cuts+5)=BCUTE
       mccardR(mccd_cuts+6)=BCUTM
       mccardR(mccd_cuts+7)=DCUTE 
       mccardR(mccd_cuts+8)=DCUTM
       mccardR(mccd_cuts+9)=PPCUTM
       mccardR(mccd_cuts+10)=TOFMAX
       do i=1,5
              mccardR(mccd_cuts+10+i)=GCUTS(i)
       enddo
! DC CARD
       mccardR(ICardDRFVEL)=DRFVEL
       mccardR(ICardAngDC)=ANGDC
       mccardR(ICardAngLor)=ANGLOR
       mccardR(mccd_dc+3)=BOUND
       mccardR(mccd_dc+4)=RELPS
       mccardR(mccd_dc+5)=GFREQ
       mccardR(mccd_dc+6)=TIMD
       mccardR(mccd_dc+7)=TIMLCK
       mccardR(mccd_dc+8)=SIGLCK
 ! CSI CARD     
       do i=1,mccd_csiS
         mccardR(mccd_csi1+i-1)=xyzrot(i,1)
         mccardR(mccd_csi2+i-1)=xyzrot(i,2)
         mccardR(mccd_csi3+i-1)=xyzrot(i,3)
         mccardR(mccd_csi4+i-1)=xyzrot(i,4)
         mccardR(mccd_csi5+i-1)=xyzrot(i,5)
         mccardR(mccd_csi6+i-1)=xyzrot(i,6)
         mccardR(mccd_csi7+i-1)=xyzrot(i,7)
         mccardR(mccd_csi8+i-1)=xyzrot(i,8)                
       enddo
 ! LKR CARD     
 ! DIG
         mccardR(mccd_lkrdig)=TDR
         mccardR(mccd_lkrdig+1)=TLIF
         mccardR(mccd_lkrdig+2)=TAU
         mccardR(mccd_lkrdig+3)=TGMI
         mccardR(mccd_lkrdig+4)=TGMA 
         mccardR(mccd_lkrdig+5)=GAIN 
         mccardR(mccd_lkrdig+6)=IPED
         mccardR(mccd_lkrdig+7)=BNOISE
         mccardR(mccd_lkrdig+8)=TAUS
         mccardR(mccd_lkrdig+9)=TGMIS
         mccardR(mccd_lkrdig+10)=TGMAS
         mccardR(mccd_lkrdig+11)=GAINS
         mccardR(mccd_lkrdig+12)=IPEDS
         mccardR(mccd_lkrdig+13)=SNOISE
         mccardR(mccd_lkrdig+14)=BTHR
         mccardR(mccd_lkrdig+15)=BTHRN
         mccardR(mccd_lkrdig+16)=STHR
! GAP
         mccardR(mccd_lkrgap)=LKGAP
! ROT
         do i=1,mccd_lkrrotS
           mccardR(mccd_lkrrot+i-1)=xyzrot_lkr(i)
         enddo
! LOSS
          mccardR(mccd_loss)=ILOSS
! MU 
                
          mccardR(mccd_mu)=mu_resolution
          mccardR(mccd_mu+1)=mu_efficiency
          iw = fwritef(mccardL(mccdl_length),4,1,ifile)
          iw = fwritef(mccardW(mccdw_recid),2,1,ifile)
          iw = fwritef(mccardW(mccdw_hdrl),2,1,ifile)
          iw = fwritef(mccardW(mccdw_ver),2,1,ifile)
          iw = fwritef(mccardW(mccdw_empty),2,1,ifile)          
          iw = fwritef(mccardR(mccdr_rbegin),4,mccd_fullS,ifile)
      END
