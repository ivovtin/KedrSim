      SUBROUTINE VD_DIG     
      integer nvdim,nhmax,ntub
      parameter (nvdim=4,nhmax=500,ntub=312)
     
C
#include "KedrSim/fiel.inc"
#include "ReadNat/vdrawhitspar.inc"
C
      include 'vddata.inc'
C     
      include 'vd_par.inc'           
C
      include 'vdhits.inc'
C
      include 'vdrawhits.inc'
    
C     
      PARAMETER (IndTube=1,IndAmpl=2,IndLength=3,
     &  IndX_G=4,IndY_G=5,IndZ_G=6,IndBetaX=7,IndBetaY=8,
     &  IndBetaZ=9,IndNTr=10,IndEp=11,IndMp=12,
     &  IndMom=13,NumParHitVD=13)                    
      COMMON /VDHITS/ HITVD(NumParHitVD,ntub)
      real drtime
      INTEGER   ISTB(NTUB)
      INTEGER   NUMVS(nvdim)
      real RTTB(NTUB)
      dimension HITS(nvddimhits,nhmax),NUMBV(nvdim,nhmax),intra(nhmax)
      DATA NUMVS/nvdim*0/
      real rRad, rURan
      integer iTime
      integer ITR
      double precision XTB,YTB,Xin,Yin,Zin,Xout,Yout,Zout,DX,DY,DZ
      double precision Dt,Dsq,DLength  
      double precision VbetaIn(3),VbetaOut(3)
      double precision VortIn(3),VortOut(3)      
      double precision Vbeta(3) 
      double precision TrackR,PtrIn,Ap,sinAp,cosAp,delta,dphiA
      double precision RcosA,Sz,VtrOrtIn,VtrOrtOut,VtrOrt,Sc,Shit         
      double precision vdeltax,vdeltay,Xav,Yav      
      double precision Xr0G,Yr0G                 
      double precision Xr0,Yr0
      double precision Sx,Sy,Str,Sa
      double precision XpG,YpG,ZpG
      double precision XpT,YpT,ZpT     
      double precision test1,test2
      double precision Discr     
      double precision Mpart,Ppart,Epart,TLength,dEdx,Ampl
      double precision PCharge,PChargeSign
      double precision RMIN,Rprec
      integer I,JTB,ICHT,iTim
      double precision VB(3)
  
c======================
      VB(1)=0
      VB(2)=0
      VB(3)=1.0
      if(FIEL.lt.0) VB(3)=-1.0
      NRAWHITSVD=0
      NHITSVDW=0
      ITRS=0
      CALL GFHITS('KVDH','STRD',NVDIM,NVDDIMHITS,NHMAX,ITRS,NUMVS,INTRA,
     +NUMBV,HITS,NHITS)
C:::::::::::::::::::::::::::::: SUBROUTINE IS THE FOLLOWING:
C                CLEAN ARRAYS
      DO I=1,NTUB
      INTB(I)=0
      TRTB(I)=0
      ISTB(I)=0
      RTTB(I)=0
      ENDDO
      NHITB=0
      NDIGVD=0
c   print *,'VD nhits=',NHITS
      IF(NHITS.eq.0)return
      IF(NHITS.GT.NHMAX) THEN
	PRINT *,' Warning! VD nhits > ',nhmax
	nhits=nhmax
      ENDIF
      DO 10 I=1,NHITS
C:::::::::::::::::::::::: OPEN HITS :::::::::::::
           JTB=NUMBV(4,I)
c        print *,'jtb=',JTB
           Rprec=0
           sinAp=0
           TrackR=0
           XTB=HITS(IvdXTB,I)
           YTB=HITS(IvdYTB,I)
           Xin=HITS(IvdXin,I)
           Yin=HITS(IvdYin,I)
           Zin=HITS(IvdZin,I)
           Xout=HITS(IvdXout,I)
           Yout=HITS(IvdYout,I)
           Zout=HITS(IvdZout,I)
           Epart=HITS(IvdEin,I)
           RMIN=sqrt((Xin-XTB)*(Xin-XTB)+(Yin-YTB)*(Yin-YTB))
           VbetaIn(1)=HITS(IvdPXin,I)*HITS(IvdPpIn,I)/Epart
           VbetaIn(2)=HITS(IvdPYin,I)*HITS(IvdPpIn,I)/Epart
           VbetaIn(3)=HITS(IvdPZin,I)*HITS(IvdPpIn,I)/Epart
           VbetaOut(1)=HITS(IvdPXout,I)*HITS(IvdPpOut,I)/HITS(IvdEout,I)
           VbetaOut(2)=HITS(IvdPYout,I)*HITS(IvdPpOut,I)/HITS(IvdEout,I)
           VbetaOut(3)=HITS(IvdPZout,I)*HITS(IvdPpOut,I)/HITS(IvdEout,I)
           PtrIn=sqrt(HITS(IvdPXin,I)*HITS(IvdPXin,I)
     &    +HITS(IvdPYin,I)*HITS(IvdPYin,I))
           TrackR=HITS(IvdR,I)
           Ampl=HITS(IvdAmp,I)
           ITR=HITS(IvdTrack,I)              
           PCharge=HITS(IvdCHARGE,I)
           PChargeSign=1
           if(PCharge.lt.0) PChargeSign=-1
           VtrOrtIn=sqrt(VbetaIn(1)*VbetaIn(1)+VbetaIn(2)*VbetaIn(2))
           VortIn(1)=VbetaIn(1)/VtrOrtIn
           VortIn(2)=VbetaIn(2)/VtrOrtIn         
           VtrOrtOut=sqrt(VbetaOut(1)*VbetaOut(1)+
     &     VbetaOut(2)*VbetaOut(2))
           VortOut(1)=VbetaOut(1)/VtrOrtOut
           VortOut(2)=VbetaOut(2)/VtrOrtOut              
           if(PtrIn.gt.0) then            
             Sz = HITS(IvdPZin,I)/PtrIn
             Sx =  HITS(IvdPXin,I)/PtrIn  
             Sy =  HITS(IvdPYin,I)/PtrIn                 
             Sa = -TrackR*PChargeSign*VB(3)    
           endif
           Mpart=HITS(IvdMASS,I)
           Ppart=HITS(IvdPpIn,I)
           TLength=HITS(IvdLen,I)
           if(TLength.ne.0) then
             dEdx=Ampl/TLength
           else
             dEdx=0  
           endif  
           DX=(Xout-Xin)
           DY=(Yout-Yin)
           DZ=(Zout-Zin)
           Dsq=DX*DX+DY*DY
           Dt=SQRT(Dsq)              
           DLength=SQRT(DX*DX+DY*DY+DZ*DZ)          
           Xav=(Xout+Xin)*0.5
           Yav=(Yout+Yin)*0.5
           XpG=Xav  
           YpG=Yav 
           IF(FIEL.NE.0.) THEN
            if(TrackR.gt.0) then
             sinAp=0.5*Dt/TrackR 
             if(sinAp>1.0) sinAp=1
             cosAp=sqrt(1-sinAp*sinAp)
             Ap=asin(sinAp)                                  
             RcosA=TrackR*cosAp
             if( sinAp .NE. 0) then                  
               Shit=ABS(TrackR*Ap)
               delta=0.5*Dt*tan(Ap*0.5)
               vdeltax= DY/Dt*PChargeSign*VB(3)
               vdeltay=-DX/Dt*PChargeSign*VB(3)
               Xav=(Xout+Xin)*0.5
               Yav=(Yout+Yin)*0.5
               XpG=XpG  - delta*vdeltax
               YpG=YpG  - delta*vdeltay                   
               XpT=XpG-XTB
               YpT=YpG-YTB
               Xr0G=Xav  + vdeltax*RcosA
               Yr0G=Yav  + vdeltay*RcosA
               Xr0=Xr0G-XTB
               Yr0=Yr0G-YTB
               Rprec=sqrt(XpT*XpT+YpT*YpT)
               test1=sqrt((XpT-Xr0)*(XpT-Xr0)+
     &              (YpT-Yr0)*(YpT-Yr0))-TrackR
             endif              ! sinAp>0           
           endif ! TrackR >0           
          endif  ! FIEL!=0           
          DX=(XpG-Xin)
          DY=(YpG-Yin)             
          Dt=SQRT(DX*DX+DY*DY)*0.5
          IF(FIEL.EQ.0..OR.sinAp.EQ.0.) THEN
            Sc=2*Dt
            ZpG=Zin+Sz*Sc
            Vbeta(1)=VbetaIn(1)
            Vbeta(2)=VbetaIn(2)
            Vbeta(3)=VbetaIn(3)
          ELSE
             Sc=2.*ASIN(Dt/TrackR)*TrackR    
             ZpG=Zin+Sc*Sz
             if(Sz.ne.0) then                
                Str= (ZpG-Zin)/Sz   
                dphiA=-Str*PChargeSign*VB(3)/TrackR 
                sinAp=sin(dphiA)
                cosAp=cos(dphiA)  
                Vbeta(1)=VbetaIn(1)*cosAp-VbetaIn(2)*sinAp
                Vbeta(2)=VbetaIn(2)*cosAp+VbetaIn(1)*sinAp
                Vbeta(3)=VbetaIn(3)     
            endif
         endif   
          
          if(Rprec.gt.0) then
            RMIN=Rprec                           
          else  
            RMIN=SQRT(((Xin+Xout)*0.5-XTB)**2+((Yin+Yout)*0.5-YTB)**2)
          endif
C:::::::::::::::::::::::::::::::::::::::::::::::::
           ISTB(JTB)=ISTB(JTB)+1
           IF(ISTB(JTB).GT.1)THEN
             IF(RMIN.LT.RTTB(JTB)) then
               RTTB(JTB)=RMIN   
               HITVD(IvdTube,JTB)=JTB
               HITVD(IndAmpl,JTB)=Ampl
               HITVD(IndLength,JTB)=TLength
               HITVD(IndX_G,JTB)=XpG
               HITVD(IndY_G,JTB)=YpG
               HITVD(IndZ_G,JTB)=ZpG
               HITVD(IndBetaX,JTB) = Vbeta(1)
               HITVD(IndBetaY,JTB) = Vbeta(2)
               HITVD(IndBetaZ,JTB) = Vbeta(3)
               HITVD(IndNtr,JTB)    = ITR
               HITVD(IndEp,JTB)     = Epart
               HITVD(IndMp,JTB)     = Mpart
               HITVD(IndMom,JTB)    = Ppart      
             ENDIF              
           ELSE
             RTTB(JTB)=RMIN
             HITVD(IvdTube,JTB)=JTB
             HITVD(IndAmpl,JTB)=Ampl
             HITVD(IndLength,JTB)=TLength
             HITVD(IndX_G,JTB)=XpG
             HITVD(IndY_G,JTB)=YpG
             HITVD(IndZ_G,JTB)=ZpG
             HITVD(IndBetaX,JTB) = Vbeta(1)
             HITVD(IndBetaY,JTB) = Vbeta(2)
             HITVD(IndBetaZ,JTB) = Vbeta(3)
             HITVD(IndNtr,JTB)    = ITR
             HITVD(IndEp,JTB)     = Epart
             HITVD(IndMp,JTB)     = Mpart
             HITVD(IndMom,JTB)    = Ppart
           ENDIF
10    CONTINUE
      DO I=1,NTUB
        IF(ISTB(I).GT.0)THEN
           NHITB=NHITB+1
           INTB(NHITB)=I
           TRTB(NHITB)=RTTB(I)
        ENDIF
      ENDDO
C ---- KLUKVA data
      DO ICHT=1,NCHT       
        I=NCHTON(ICHT)               
        IF(I.GT.0 .AND. ISTB(I).GT.0)THEN
	  rRad = rttb(i)
c	  rURan = rndm(rRad)
c	  call grndm(rURan,1)
c	  if (rURan .lt. tubeff(i, rad)) then
c!!! changed	    call rsmear(i, rRad)
	    iTim = (rT0 - drtime(rRad))/rDigNS            
c	    write (*,*) 'R=',rRad,' T=',iTim*2.
c     print *,'R=',rRad,' T=',iTim,'dig:',rDigNS,'drt:',drtime(rRad)                          
	    if (iTim.gt.0 .and. iTim.le.4095) then
             
             RAWHITSVD(NRAWHITSVD+IvdTube)  = HITVD(IndTube,I) ! wire number           
             RAWHITSVD(NRAWHITSVD+IvdAmpl)  = HITVD(IndAmpl,I)*10**9 ! eV
             RAWHITSVD(NRAWHITSVD+IvdLength)= HITVD(IndLength,I) ! cm
             RAWHITSVD(NRAWHITSVD+IvdXG)    = HITVD(IndX_G,I) ! X cm
             RAWHITSVD(NRAWHITSVD+IvdYG)    = HITVD(IndY_G,I) ! Y
             RAWHITSVD(NRAWHITSVD+IvdZG)    = HITVD(IndZ_G,I) ! Z
             RAWHITSVD(NRAWHITSVD+IvdVx)    = HITVD(IndBetaX,I) ! BetaX         v/c
             RAWHITSVD(NRAWHITSVD+IvdVy)    = HITVD(IndBetaY,I) ! BetaY
             RAWHITSVD(NRAWHITSVD+IvdVz)    = HITVD(IndBetaZ,I) ! BetaZ
             RAWHITSVD(NRAWHITSVD+IvdTR)    = HITVD(IndNtr,I) ! Track number 
             RAWHITSVD(NRAWHITSVD+IvdE)     = HITVD(IndEp,I)*1000 ! Energy MeV
             RAWHITSVD(NRAWHITSVD+IvdM)     = HITVD(IndMp,I)*1000 ! Mass Mev
             RAWHITSVD(NRAWHITSVD+IvdP)     = HITVD(IndMom,I)*1000 !Momentum MeV/c 
             NRAWHITSVD=NRAWHITSVD+NumRawParVD        
             NHITSVDW= NHITSVDW+1
             IDIGVD(NDIGVD+1)=IADRCH(ICHT)
             IDIGVD(NDIGVD+2)=MOD(ITIM, "10000)
             NDIGVD=NDIGVD+2
              
              
              
	    endif  ! iTim limit check

c	  endif  ! rURan .lt. tubeff

        ENDIF
      ENDDO
      RETURN
      END
c---------------------------------------------------------------
c Subroutine rsmear(itube, rad) smears "rad" to simulate the spacial 
c resolution of a tube
c   Author:   A. Poluektov
c---------------------------------------------------------------
	subroutine rsmear(itube, rad)
	real rad, rDRad, rRRes
	integer itube
        include 'vd_par.inc'
c	call norran(rDRad)
	call granor(rDRad,dum)
	rRRes = rVDRes(itube)*(rVDRC(1)+rad*(rVDRC(2)+rad*(rVDRC(3)+
	1	rVDRC(4)*rad)))
	rad = abs(rad+rDRad*rRRes)
	return
	end
c---------------------------------------------------------------
c Function tubeff(itube, rad) returns a value of efficiency 
c for a given tube at given radius
c    Author:  A. Poluektov
c---------------------------------------------------------------
	real function tubeff(itube, rad)
	include 'vd_par.inc'
	tubeff=rVDEff(itube)*(rVDEC(1)+rad*(rVDEC(2)+rad*(rVDEC(3)+
	1	rVDEC(4)*rad)))
	end

c---------------------------------------------------------------
c Function drtime returns a drift time for a given drift
c distance 'rad' 
c   Author: A. Poluektov
c---------------------------------------------------------------
	real function drtime(rad)
	real rad
	include 'vd_par.inc'
	drtime = rVDRTC(1)+rad*(rVDRTC(2)+rad*(rVDRTC(3)+rad*
	1   		(rVDRTC(4)+rad*rVDRTC(5))))
	end
c---------------------------------------------------------------
