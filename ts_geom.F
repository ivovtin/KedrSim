      subroutine ts_geom
c*****************************************************************
c
c  Geometry of tagging system
c
c  Author: L.V.Romanov
c
c  Changed: 04.10.99.  V.A.Tayursky
c
c*****************************************************************
c
*z-axis along the positron beam, so Z_el negative.
c
c      implicit none
      include 'ts_cmb.inc'
      include 'medpar.inc'
      integer i,ib,    ivol,  iset,idet, k_arm

      real     freon_dens,freon_rl
     *, dlx(4),dlz,dlz1,    tetb,  pi_grad,tet_vch
     *,  par(10),  bx,by,bz,  btx,btz,     z_tsg(4) 
     *, x_tsg_el0(4),x_tsg_pos0(4)
     *, t_tsg_0(4),t_cor_el(4),t_cor_pos(4)

      character *4 nam_gd(4), nam_fre(4)   

      real x_vch_el(4),x_vch_pos(4),z_vch_el(4),z_vch_pos(4)
     *, x_fre_el(4),z_fre_el(4), dlx_fre(4) ,dlz_fre,sh_fre
     *, x_fre_pos(4),z_fre_pos(4)
     *,x_stp_el(4),x_stp_pos(4),z_stp_el(4),z_stp_pos(4)
     *,dx_stop,dz_stop,sh_stop(4)
      real x_tsg_el(4),x_tsg_pos(4),     z_tsg_el(4) ,z_tsg_pos(4)
     *,      t_tsg(4,2) 

*  godoscope position                                  
      data z_tsg/900.,1010.,1300.,1720./  
      data x_tsg_el0/13.8,22.4, 53.4, 109.8/
      data x_tsg_pos0/13.8,22.4, 53.4, 109.5/
      data dlx/3*6.52,12.52/
      data dlx_fre/3*7.3,13.2/
      data t_tsg_0/ 0.1219, 0.1002, 0.1722, 0.1493/
      data t_cor_el/-0.0225, 0.0030, 0.029,  0.0094/
      data t_cor_pos/ 0.0   , 0.0   , 0.029  ,  0.00/

* detector parameter                                   
      data nam_gd/3*'TSG1','TSG2'/ 
      data nam_fre/3*'TSD1','TSD2'/
      data dlz/23./ dlz1/32./ 
      data dlz_fre/5./ 
      data sh_fre/0./
      data dz_stop /85./ 
      data sh_stop/3*5.,5./ 
      data dx_stop/12./
c
      pi_grad=180./3.1415927

      do i=1,4
        t_tsg(i,1) =(atan(t_tsg_0(i))+ t_cor_el(i))*pi_grad
        t_tsg(i,2)=(atan(t_tsg_0(i))+t_cor_pos(i))*pi_grad
        tetb=t_tsg(i,2)/pi_grad   !!!!!!!!!! really uncorrected

        x_tsg_el(i) =x_tsg_el0(i)  + dlx(i)*cos(tetb) + dlz*sin(tetb)
        x_tsg_pos(i)=x_tsg_pos0(i) + dlx(i)*cos(tetb) + dlz*sin(tetb)
        
        z_tsg_el(i) =-z_tsg(i)-dlz*cos(tetb) + dlx(i)*sin(tetb)
        z_tsg_pos(i)= z_tsg(i)+dlz*cos(tetb) - dlx(i)*sin(tetb)

        x_vch_el(i) =x_tsg_el0(i)   - 2.*cos(tetb) + dlz1*sin(tetb)
        x_vch_pos(i)=x_tsg_pos0(i)  - 2.*cos(tetb) + dlz1*sin(tetb)

        z_vch_el(i) =-z_tsg(i)  - dlz1*cos(tetb)
        z_vch_pos(i)= z_tsg(i)  + dlz1*cos(tetb)

        bx=sh_fre+dlx_fre(i)
        x_fre_el(i) =x_tsg_el0(i)  + bx*cos(tetb) + dlz_fre*sin(tetb)
        x_fre_pos(i)=x_tsg_pos0(i) + bx*cos(tetb) + dlz_fre*sin(tetb)

        z_fre_el(i) =-z_tsg(i) + bx*sin(tetb) - dlz_fre*cos(tetb)
        z_fre_pos(i)= z_tsg(i) - bx*sin(tetb) + dlz_fre*cos(tetb)

        bx=sh_stop(i)+dx_stop
        x_stp_el(i) =x_tsg_el0(i)  + bx*cos(tetb) + dz_stop*sin(tetb)
        x_stp_pos(i)=x_tsg_pos0(i) + bx*cos(tetb) + dz_stop*sin(tetb)

        z_stp_el(i) =-z_tsg(i) + bx*sin(tetb) - dz_stop*cos(tetb)
        z_stp_pos(i)= z_tsg(i) - bx*sin(tetb) + dz_stop*cos(tetb)

      enddo
*                                                        
c      call gmate

c      freon_dens=(4*19.+16.)/22.4*0.001
c      freon_rl=32./freon_dens

c      call gsmate(21,'freon',18.,8.5,freon_dens,freon_rl,1.,0.,0)
c      call gstmed(1,'plbm',13,0,0,0.,0.,  0.01 ,0.01, 0.001, 0.01,0.,0)
c      call gstmed(2,'feru',10,0,0,0.,0.,  0.003,0.01, 0.001, 0.01,0.,0)
c      call gstmed(3,'air ',15,0,0,0.,0.,  1.5,  0.02, 0.001, 1.4, 0.,0)
c      call gstmed(4,'freo',21,1,0,0.,0.,  0.5,  0.02, 0.001, 0.4, 0.,0)
c      call gstmed(5,'fdet',10,1,0,0.,0.,  0.01, 0.02, 0.001, 0.01,0.,0)
c      call gstmed(6,'vacu',16,0,0,0.,0.,  10.0, 0.02, 0.001, 10.0,0.,0)
c      call gstmed(7,'wolf',12,0,0,0.,0.,  0.01, 0.01,0.0001,0.0001,0.,0)
*                                                                
c      par(1)=250. !  mother molume
c      par(2)=40.
c      par(3)=2000.

c      call gsvolu('MOTH','BOX ',6,par,3,ivol) ! MOTH -> KEDR

      par(1)=dlx(1) !
      par(2)=10.2
      par(3)=13.

      call gsvolu('TSG1','BOX ',MEDAIR,par,3,ivol)

      par(1)=dlx(4) !
      par(2)=10.2
      par(3)=13.

      call gsvolu('TSG2','BOX ',MEDAIR,par,3,ivol)

      call ts_geom_tube 

      call ts_geom_hd1  

      call ts_geom_hd2  

      par(1)=dlx_fre(1)
      par(2)=10.2
      par(3)=0.015

      call gsvolu('TSD1','BOX ',MEDFE1,par,3,ivol)

      par(1)=dlx_fre(4)
      par(2)=10.2
      par(3)=0.015

      call gsvolu('TSD2','BOX ',MEDFE1,par,3,ivol)
*                                 
      par(1)=dx_stop
      par(2)=20.
      par(3)=26.

      call gsvolu('STOP','BOX ',MEDLEAD,par,3,ivol)
*                                 
      par(1)=2
      par(2)=10.4
      par(3)=26.

      call gsvolu('VACH','BOX ',MEDVAC,par,3,ivol)

      par(1)=0.015
      par(2)=10.2
      par(3)=1.5

      call gsvolu('VCH1','BOX ',MEDFE0,par,3,ivol)

      par(1)=0.015
      par(2)=10.2
      par(3)=22.

      call gsvolu('VCH2','BOX ',MEDFE0,par,3,ivol)

*******************************************************
c
c   Positioning volumes
c
******************************************************
c
      call gspos('VCH2',1,'VACH',1.4,0.,0.,0,'ONLY')
c
      tet_vch=asin(5./30.)*pi_grad
      btx=90.+tet_vch
      btz=tet_vch
c
      CALL FREE_ROTMAT_NUMBER(IRM)
c
      call gsrotm(IRM,btx,0.,90.,90.,btz,0.)  ! 31->IRM   
c
      bz=22.+1.5
      bx=1.4+0.25+0.015
c
      call gspos('VCH1',1,'VACH',bx,0.,bz,IRM,'ONLY')
c
      btx=90.-tet_vch
      btz=-tet_vch
c
      CALL FREE_ROTMAT_NUMBER(IRM)
c
      call gsrotm(IRM,btx,0.,90.,90.,btz,0.)   
c
      call gspos('VCH1',2,'VACH',bx,0.,-bz,IRM,'ONLY')
c
*******************************************************
c
      do i=1,4     
        k_arm=1   
        btx=90.-t_tsg(i,k_arm)
        btz=180.-t_tsg(i,k_arm)
c
        CALL FREE_ROTMAT_NUMBER(IRM)
c
        call gsrotm(IRM,btx,0.,90.,-90.,btz,0.)    

        call gspos(nam_gd(i),i,'KEDR',x_tsg_el(i)
     *   ,0.,z_tsg_el(i),IRM,'ONLY')

        call gspos(nam_fre(i),i,'KEDR',x_fre_el(i)
     *   ,0.,z_fre_el(i),IRM,'ONLY')

        call gspos('VACH',i,'KEDR',x_vch_el(i)
     *   ,0.,z_vch_el(i),IRM,'MANY')

        call gspos('STOP',i,'KEDR',x_stp_el(i)
     *   ,0.,z_stp_el(i),IRM,'STOP')

*________________
*
      k_arm=2
c
      CALL FREE_ROTMAT_NUMBER(IRM)
c
      ib=IRM              
      btx=90.+t_tsg(i,k_arm)
      btz=t_tsg(i,k_arm)
c
      call gsrotm(ib,btx,0.,90.,90.,btz,0.)   
c
      call gspos(nam_gd(i),ib,'KEDR',
     *              x_tsg_pos(i),0.,z_tsg_pos(i),ib,'ONLY')
c
      call gspos(nam_fre(i),ib,'KEDR',x_fre_pos(i)
     *   ,0.,z_fre_pos(i),ib,'ONLY')
c
      call gspos('VACH',ib,'KEDR',x_vch_pos(i)
     *   ,0.,z_vch_pos(i),ib,'MANY')
c
      call gspos('STOP',ib,'KEDR',x_stp_pos(i)
     *   ,0.,z_stp_pos(i),ib,'ONLY')
c
      end do
 
c      call ts_def_hit ->ts_hits, call from ugeom

c      call ggclos

      end
