*CMZU:          21/05/97  11.24.05  by  YURI SKOVPEN
*CMZU:  1.00/04 04/02/97  14.22.26  by  VLADIMIR KOZLOV
*CMZU:  1.00/03 21/11/96  13.39.58  by  VLADIMIR KOZLOV
*-- Author :    VLADIMIR KOZLOV   04/11/96
*    programm was changed by  Korneliy Todyshev
        SUBROUTINE DCDIGI
c**************************************************************
c 
c
c   Changes: 
c	     24.05.2001 V.M.Malyshev : 	HITDC(4,3000)->
c	HITDC(4,30000) to avoid segm. fault when lots of hits
c
c**************************************************************
#include "gconst.inc"         
#include "gcphys.inc"         
#include "KedrSim/fiel.inc"
#include "KedrSim/celpar.inc"
#include "KedrSim/dcwire.inc"
#include "KedrSim/dchits.inc" 
#include "KedrSim/dcout.inc"
#include "ReadNat/dcrawhitspar.inc"     
#include "KedrSim/dcrawhits.inc"
#include "KedrSim/dctamhits.inc"  
        PARAMETER (NHMAX=500,NTMAX=20,NVMAX=5,NVDIM=2,NHDIM=23)        
        PARAMETER (IndIW=1,IndTmin=2,IndTmax=3,IndAmpl=4,
     &  IndX_G=5,IndY_G=6,IndZ_G=7,IndBetaX=8,IndBetaY=9,
     &  IndBetaZ=10,IndNTr=11,IndEp=12,IndMp=13,
     &  IndMom=14,InddEdx=15,NumParHitDC=15 )                             
        DIMENSION ITRACK(NHMAX),NUMVS(NVDIM),NUMBV(NVDIM,NHMAX),
     &  HITS(NHDIM,NHMAX)   
        DIMENSION NTIM(MAXCH),TMIN(MAXDIG,MAXCH),
     +            ITIM(MAXDIG,MAXCH)
        DIMENSION   VXcr(7),VYcr(7),VZcr(7)
        double precision   VxDr(7),VyDr(7),VxDrC(7),VyDrC(7),tanVDr  
        double precision   XMidleHitG(7),YMidleHitG(7),ZMidleHitG(7)        
        PARAMETER (NSW=513*8)
        COMMON /DCHITS/ NHITDC,HITDC(NumParHitDC,30000),
     &    LINKDC(101,MAXCH,MAXPL,MAXCR)
        integer*  2 NUsedHits(30000)
        double precision Zcr(7),CFcr(7),Xcr(7),Ycr(7) 
        double precision Xin,Yin,Zin,Xout,Yout,Zout,Dt,Dsq
        double precision DX,DY,DZ,DZ0,DX0,DY0
        double precision VbetaIn(3),VbetaOut(3)
        double precision VortIn(3),VortOut(3),TA2,TA3
        double precision Xi1,Yi1,Xo1,Yo1
        double precision Xi2,Yi2,Xo2,Yo2         
        double precision DYSW,DXSW,XSW0Z,YSW0Z
        double precision TrackR,PtrIn,Ap,sinAp,cosAp,delta,dphiA
        double precision RcosA,Sz,VtrOrtIn,VtrOrtOut,VtrOrt,Sc            
        double precision vdeltax,vdeltay,Xav,Yav
        double precision vdeltaxC,vdeltayC
        double precision vdeltaxCL,vdeltayCL
        double precision Xr0G,Yr0G,Xr0C,Yr0C       
        double precision Xr0,Yr0
        double precision Sx,Sy,Str,Sa
        double precision XpG,YpG,ZpG,XpCZ,YpCZ
        double precision XpC,YpC,Xp,Yp
        double precision Xc,Yc
        double precision Xdrf1,Xdrf2
        double precision diffY
        integer          NcW,Nstep
        double precision test1,test2
        double precision Discr     
        double precision ANG                
        double precision Mpart,Ppart,Epart,TLength,dEdx,Ampl,vbeta
        double precision PCharge,PChargeSign
        double precision TWX,TWY,Tnorm
        double precision TWR(1:3),VDrf(1:3),VDrfN,tanlor
        double precision VLOR(1:3),VB(1:3)
        double precision WU(1:3),VE(1:3),CosTN,SinTN        
        integer ITR
        integer NSkiped
        logical Fdbg/.false./
        logical XoutR/.false./
        logical YoutR/.false./
        logical ZoutR/.false./
        
        integer IcrossFlag(1:6)
        data    NUsedHits/30000*0/
        CHARACTER*4 DCCNAM(7)
        DATA DCCNAM /'DCC1','DCC2','DCC3','DCC4','DCC5','DCC6','DCC7'/
C
C ALL VALID NUMBERS (FOR GFHITS)
C
        DATA NUMVS /NVDIM*0/
C
C=====================================================================
C
        VB(1)=0
        VB(2)=0
        VB(3)=1
        if(FIEL.lt.0) VB(3)=-1.0
        NHITS=0
        NHITDC=0
        NDIGDC=0
        NSkiped=0
C!
        NRAWHITSDC=0
        DO I=1,MAXCR
         DO J=1,MAXPL
          DO K=1,MAXCH
           LINKDC(101,K,J,I)=0
          ENDDO
         ENDDO
        ENDDO
        do i=1,30000
          NUsedHits(i)=0
        enddo
C
c      PRINT *,'EVENT:' 
        DO IL=1,7,2                   
	  if(layson(IL).gt.0) then
          CALL GFHITS('KDCH',DCCNAM(IL),NVDIM,NHDIM,NHMAX,0,NUMVS,
     +                 ITRACK,NUMBV,HITS,NHIT)
	  IF(NHIT.GT.NHMAX) THEN
	    print *,' Warning! DC nhits in layer ',IL,' > ',NHMAX
	    NHIT=NHMAX 
	  ENDIF
          IF(NHIT.GT.0) THEN
            DO NH=1,NHIT
              Xin=HITS(IndXin,NH)
              Yin=HITS(IndYin,NH)
              Zin=HITS(IndZin,NH)
              Xout=HITS(IndXout,NH)
              Yout=HITS(IndYout,NH)
              Zout=HITS(IndZout,NH)
              NCEL=NUMBV(1,NH)
              Epart=HITS(IndEin,NH)
         VbetaIn(1)=HITS(IndPXin,NH)*HITS(IndPpIn,NH)/Epart
         VbetaIn(2)=HITS(IndPYin,NH)*HITS(IndPpIn,NH)/Epart
         VbetaIn(3)=HITS(IndPZin,NH)*HITS(IndPpIn,NH)/Epart
        VbetaOut(1)=HITS(IndPXout,NH)*HITS(IndPpOut,NH)/HITS(IndEout,NH)
        VbetaOut(2)=HITS(IndPYout,NH)*HITS(IndPpOut,NH)/HITS(IndEout,NH)
        VbetaOut(3)=HITS(IndPZout,NH)*HITS(IndPpOut,NH)/HITS(IndEout,NH)
        PtrIn=sqrt(HITS(IndPXin,NH)*HITS(IndPXin,NH)
     &    +HITS(IndPYin,NH)*HITS(IndPYin,NH))
              TrackR=HITS(IndR,NH)
              Ampl=HITS(IndAmp,NH)
              ITR=HITS(IndTrack,NH)              
              PCharge=HITS(IndCHARGE,NH)
              PChargeSign=1
              if(PCharge.lt.0) PChargeSign=-1
              VtrOrtIn=sqrt(VbetaIn(1)*VbetaIn(1)+VbetaIn(2)*VbetaIn(2))
              VortIn(1)=VbetaIn(1)/VtrOrtIn
              VortIn(2)=VbetaIn(2)/VtrOrtIn         
              VtrOrtOut=sqrt(VbetaOut(1)*VbetaOut(1)+
     &        VbetaOut(2)*VbetaOut(2))
              VortOut(1)=VbetaOut(1)/VtrOrtOut
              VortOut(2)=VbetaOut(2)/VtrOrtOut              
              if(PtrIn.gt.0) then
                Sz =  HITS(IndPZin,NH)/PtrIn
                Sx =  HITS(IndPXin,NH)/PtrIn  
                Sy =  HITS(IndPYin,NH)/PtrIn                 
                Sa = -TrackR*PChargeSign*VB(3)    
               endif
              Mpart=HITS(IndMASS,NH)
              Ppart=HITS(IndPpIn,NH)
              TLength=HITS(IndLength,NH)
              if(TLength.ne.0) then
                dEdx=Ampl/TLength
              else
                dEdx=0  
              endif  
              if(Fdbg)  then
              print *, 'Index Track:',ITR
              print *, 'R:',TrackR
              print *, ' LAYER:',IL
              print *, ' NCEL:',NCEL              
              print *, ' ptr:',sqrt(HITS(IndPXin,NH)**2+HITS(IndPYin,NH)**2)
     &     *Ppart
               print *, ' p:',sqrt(HITS(IndPXin,NH)**2+
     &   HITS(IndPYin,NH)**2+ HITS(IndPZin,NH)**2)*Ppart 
               print *, ' pz:',HITS(IndPZin,NH)*Ppart
               print *, ' Epart:',Epart
              endif

              vbeta=sqrt(VbetaIn(1)*VbetaIn(1)+VbetaIn(2)*VbetaIn(2)
     &         +VbetaIn(3)*VbetaIn(3))
              ANG=CELANG(IL)*(NCEL-1)
              Xi1=Xin*COS(ANG)-Yin*SIN(ANG)
              Yi1=Xin*SIN(ANG)+Yin*COS(ANG)
              Xo1=Xout*COS(ANG)-Yout*SIN(ANG)
              Yo1=Xout*SIN(ANG)+Yout*COS(ANG)
C Convert to system of sense wire in direction of drift
              Xi2=(Xi1-XSWIR0(IL))*COWLOR(IL)+
     +            (Yi1-YSWIR0(IL))*SIWLOR(IL)
              Yi2=(Yi1-YSWIR0(IL))*COWLOR(IL)-
     +            (Xi1-XSWIR0(IL))*SIWLOR(IL)
              Xo2=(Xo1-XSWIR0(IL))*COWLOR(IL)+
     +            (Yo1-YSWIR0(IL))*SIWLOR(IL)
              Yo2=(Yo1-YSWIR0(IL))*COWLOR(IL)-
     +            (Xo1-XSWIR0(IL))*SIWLOR(IL)
              DY = (Yo2-Yi2)
              TA2= 0.
              TA3= 0.
              if(DY .NE. 0.) then
                TA2=(Xo2-Xi2)/DY
                TA3=(Zout-Zin)/DY
              endif
C Determine  which sense wires hitted
              DYSW=DSWIR(IL)*COSLOR
              DXSW=DSWIR(IL)*SINLOR
              NSW1=1
              Ymin=min(Yi2,Yo2)
              Ymax=max(Yi2,Yo2)              
              IF(Ymin.GT.0.) NSW1=Ymin/DYSW+1.
              NSW2=Ymax/DYSW+1.
              IF(NSW2.GT.6) NSW2=6
              NcW=NSW2-NSW1
              if(NcW.ge.0) Nstep=1
              else Nstep=-1
              IF(NSW1.LE.6.AND. NSW2.GT.0) THEN
C Length of this track part
               DX=(Xout-Xin)
               DY=(Yout-Yin)
               DZ=(Zout-Zin)
               Dsq=DX*DX+DY*DY
               Dt=SQRT(Dsq)             
               DLength=SQRT(DX*DX+DY*DY+DZ*DZ)

               IF(FIEL.NE.0.) THEN
                 if(TrackR.gt.0) then
                 sinAp=0.5*Dt/TrackR 
                 if(sinAp>1.0) sinAp=1
                 cosAp=sqrt(1-sinAp*sinAp)                 
                 Ap=asin(sinAp)                                  
                 RcosA=TrackR*cosAp
                 if( sinAp .NE. 0) then                  
                   delta=0.5*Dt*tan(Ap*0.5)
                   vdeltax=  DY/Dt*PChargeSign*VB(3)
                   vdeltay=-DX/Dt*PChargeSign*VB(3)
                   vdeltaxC=vdeltax*COS(ANG)-vdeltay*SIN(ANG)
                   vdeltayC=vdeltay*COS(ANG)+vdeltax*SIN(ANG)
                   vdeltaxCL=vdeltaxC*COWLOR(IL)+vdeltayC*SIWLOR(IL)
                   vdeltayCL=vdeltayC*COWLOR(IL)-vdeltaxC*SIWLOR(IL)
                   Xav=(Xout+Xin)*0.5
                   Yav=(Yout+Yin)*0.5
                   XpG=Xav - delta*vdeltax
                   YpG=Yav  - delta*vdeltay                   
              XpC=XpG*COS(ANG)-YpG*SIN(ANG)
              YpC=XpG*SIN(ANG)+YpG*COS(ANG)
              Xp=(XpC-XSWIR0(IL))*COWLOR(IL)+(YpC-YSWIR0(IL))*SIWLOR(IL)
              Yp=(YpC-YSWIR0(IL))*COWLOR(IL)-(XpC-XSWIR0(IL))*SIWLOR(IL)
              Xr0G=Xav  + vdeltax*RcosA
              Yr0G=Yav  + vdeltay*RcosA
              Xr0C=Xr0G*COS(ANG)-Yr0G*SIN(ANG)
              Yr0C=Xr0G*SIN(ANG)+Yr0G*COS(ANG)              
              Xr0=(Xr0C-XSWIR0(IL))*COWLOR(IL)+
     &              (Yr0C-YSWIR0(IL))*SIWLOR(IL)
              Yr0=(Yr0C-YSWIR0(IL))*COWLOR(IL)-
     &              (Xr0C-XSWIR0(IL))*SIWLOR(IL)
                   test2=sqrt((Xp-Xr0)*(Xp-Xr0)+
     &              (Yp-Yr0)*(Yp-Yr0))-TrackR
                 ENDIF !sinAp!=0
                ENDIF ! TrackR>0
              ENDIF   ! FIEL>0
             if(Fdbg) then
             test1=sqrt((Xin-Xr0G)**2+
     &           (Yin-Yr0G)**2)-TrackR
             test2=sqrt((Xout-Xr0G)**2+
     &           (Yout-Yr0G)**2)-TrackR            
             print *,'X0:', Xr0G
             print *,'Y0:', Yr0G
             print *,'vdeltax:', vdeltax
             print *,'vdeltay:', vdeltay
             endif
       if((abs(test1).gt.0.00001.or.abs(test2).gt.0.00001).and.Fdbg)then
               print *,'test1:',test1
               print *,'test2:',test 2               
       endif                       
               DO NW=NSW1,NSW2,Nstep
                Yc=DYSW*NW              
                IF(FIEL.EQ.0. .OR.sinAp .eq. 0.) THEN
C Cross point of straight track and drift line
                  Xc=Xi2+(Yc-Yi2)*TA2
                ELSE ! FIEL
C Cross point of orbit track and drift line
                  Discr=TrackR*TrackR-(Yc-Yr0)*(Yc-Yr0)
                  if(Discr.ge.0) then
                    if(vdeltaxCL.le.0) then
                       Xc=Xr0+sqrt(Discr)
                    else
                       Xc=Xr0-sqrt(Discr)                 
                    endif                      
                    IcrossFlag(NW)=1
                  else
                    IcrossFlag(NW)=0
                  endif  
                ENDIF ! FIEL
                Xcr(NW)=Xc-DXSW*NW
                Ycr(NW)=0
                XpC=Xc*COWLOR(IL)-Yc*SIWLOR(IL)+XSWIR0(IL)
                YpC=Yc*COWLOR(IL)+Xc*SIWLOR(IL)+YSWIR0(IL)
                XpG=XpC*COS(ANG)+YpC*SIN(ANG)
                YpG=YpC*COS(ANG)-XpC*SIN(ANG)
c                test2=sqrt((XpG-Xr0G)*(XpG-Xr0G)+
c     &             (YpG-Yr0G)*(YpG-Yr0G))-TrackR                                           
                DX=(XpG-Xin)
                DY=(YpG-Yin)             
                Dt=SQRT(DX*DX+DY*DY)*0.5
                IF(FIEL.EQ.0..OR.sinAp.EQ.0.) THEN
                  Sc=2*Dt
                  ZpG=Zin+Sz*Sc
                  VXcr(NW)=VbetaIn(1)
                  VYcr(NW)=VbetaIn(2)
                  VZcr(NW)=VbetaIn(3)
                ELSE
                Sc=2.*ASIN(Dt/TrackR)*TrackR    
                ZpG=Zin+Sc*Sz
                if(Sz.ne.0) then                
                Str= (ZpG-Zin)/Sz   
                dphiA=-Str*PChargeSign*VB(3)/TrackR 
                sinAp=sin(dphiA)
                cosAp=cos(dphiA)  
                VXcr(NW)=VbetaIn(1)*cosAp-VbetaIn(2)*sinAp
                VYcr(NW)=VbetaIn(2)*cosAp+VbetaIn(1)*sinAp
                VZcr(NW)=VbetaIn(3)             
c                XpG=Xin+Sa*(Sx*sinAp-Sy*(1-cosAp))
c                YpG=Yin+Sa*(Sy*sinAp+Sx*(1-cosAp))
c                  test1=sqrt((XpG-Xr0G)**2+
c     &           (YpG-Yr0G)**2)-TrackR

                if(abs(test1).gt.0.00001.and.Fdbg) then      
                PRINT *,'CHARGE:',PCharge                
                print *,'TrackR:',TrackR
                PRINT *,'XpC:',XpC                
                PRINT *,'YpC:',YpC
                PRINT *,'Xr0G:',Xr0G                
                PRINT *,'Yr0G:',Yr0G
                PRINT *,'test2:',test2                
                test1=sqrt((XpG-Xr0G)*(XpG-Xr0G)+
     &              (YpG-Yr0G)*(YpG-Yr0G))-TrackR
                   print *,'test1:',test1                                   
                   test1=sqrt((XpC-Xr0C)*(XpC-Xr0C)+
     &              (YpC-Yr0C)*(YpC-Yr0C))-TrackR
                     print *,'testc:',test1
                print *,'Xin:',Xin
                print *,'Yin:',Yin
                print *,'Xout:',Xout
                print *,'Yout:',Yout                   
                print *,'XpG:',XpG
                print *,'YpG:',YpG                     
                print *,'Xp:',Xp
                print *,'Yp:',Yp
                print *,'Xr0:',Xr0
                print *,'Yr0:',Yr0                   
                endif              
             endif        
           ENDIF                               
              VtrOrt=sqrt(VXcr(NW)*VXcr(NW)+VYcr(NW)*VYcr(NW))
              VxDrC(NW)=VXcr(NW)*COS(ANG)-VYcr(NW)*SIN(ANG)
              VxDrC(NW)=VYcr(NW)*COS(ANG)+VXcr(NW)*SIN(ANG)
              VxDr(NW)=VxDrC(NW)*COWLOR(IL)+VyDrC(NW)*SIWLOR(IL)
              VyDr(NW)=VyDrC(NW)*COWLOR(IL)-VxDrC(NW)*SIWLOR(IL)
              VxDr(NW)=VxDr(1)/VtrOrt
              VyDr(NW)=VyDr(1)/VtrOrt              
              XMidleHitG(NW)=XpG
              YMidleHitG(NW)=YpG     
              ZMidleHitG(NW)=ZpG               
c              if((ZpG-Zin)*(Zout-ZpG).lt.-0.1*abs(DZ).or.
c     &        (XpG-Xin)*(Xout-XpG).lt.-0.01*abs(DX).or.
c     &        (YpG-Yin)*(Yout-YpG).lt.-0.01*abs(DY)) then             
               XoutR=((XpG-Xin)*(Xout-XpG).lt.0.and.
     &        (abs(XpG-Xin).gt.0.05.or.abs(XpG-Xout).gt.0.05))
              YoutR=((YpG-Yin)*(Yout-YpG).lt.0.and.
     &        (abs(YpG-Yin).gt.0.05.or.abs(YpG-Yout).gt.0.05))
              ZoutR=((ZpG-Zin)*(Zout-ZpG).lt.0.and.
     &        (abs(ZpG-Zin).gt.0.1.or.abs(ZpG-Zout).gt.0.1))
              
              if(XoutR.or.YoutR.or.ZoutR) then             
                IcrossFlag(NW)=0
                Nskiped=Nskiped+1
               endif
          ENDDO ! NW                    
               DO NW=NSW1,NSW2,Nstep
                if(ICrossFlag(NW).eq.1)  then                   
                 if(VyDr(NW).ne.0) then  
               tanVDr=VxDr(NW)/VyDr(NW)
               Xdrf1=Xcr(NW)+RFOCUS*tanVDr
               Xdrf2=Xcr(NW)-RFOCUS*tanVDr               
               Xmin=min(abs(Xdrf1),abs(Xdrf2))
               Xmax=max(abs(Xdrf1),abs(Xdrf2))
                 else
                   Xmin=Xcr(NW)
                   Xmax=Xcr(NW)                                     
                 ENDIF                                     
                IF(Xmin*Xmax.LT.0.) THEN
C     If crossed center of cell to calculate distance to sense wire
                   Dmin=abs(Xcr(NW)*VyDr(NW))
                   Dmax=sqrt(Xmax*Xmax+0.25*BOUND2)
                ELSE
C     Both hits outside focus region on the same side of cell
C     Check min distance due to cylindr field near sense wire
                  Dmin=ABS(Xmin)
                  Dmax=ABS(Xmax)
                ENDIF
                if(Dmin.lt.RFOCUS) then
                    Tmini=6.9*Dmin*Dmin
                else
                    Tmini=(Dmin-RFOCUS)/DRFVEL+6.9* RFOCUS*RFOCUS
                endif
                if(Dmax.lt.RFOCUS) then
                    Tmax=6.9*Dmax*Dmax
                else
                    Tmax=(Dmax-RFOCUS)/DRFVEL+6.9 * RFOCUS*RFOCUS
                endif               

                Dc=(Xmax-Xmin)*(Xmax-Xmin)+BOUND2
                if(TLength.ne.0.) Ampl=dEdx*SQRT(Dc)
                IW=512*IL+8*NCEL+NW
                ICH=ICHWIR(1,IW)
                IPL=ICHWIR(2,IW)
                ICR=ICHWIR(3,IW)
                NLH=LINKDC(101,ICH,IPL,ICR)+1
                IF(NLH.LE.100) then
c              .AND. Tmini.LT.TIMD) THEN
                  LINKDC(101,ICH,IPL,ICR)=NLH
                  NHITDC=NHITDC+1
                  LINKDC(NLH,ICH,IPL,ICR)=NHITDC
                  HITDC(IndIW,NHITDC)=IW
                  HITDC(IndTmin,NHITDC)=Tmini
                  HITDC(IndTmax,NHITDC)=Tmax
                  HITDC(IndAmpl,NHITDC)=Ampl
                  HITDC(IndX_G,NHITDC)=XMidleHitG(NW)
                  HITDC(IndY_G,NHITDC)=YMidleHitG(NW)
                  HITDC(IndZ_G,NHITDC)=ZMidleHitG(NW)
                  HITDC(IndBetaX, NHITDC) = VXcr(NW)
                  HITDC(IndBetaY, NHITDC) = VYcr(NW)
                  HITDC(IndBetaZ,NHITDC)  = VZcr(NW)
                  HITDC(IndNtr,NHITDC)    = ITR
                  HITDC(IndEp,NHITDC)     = Epart
                  HITDC(IndMp,NHITDC)     = Mpart
                  HITDC(IndMom,NHITDC)    = Ppart
                  HITDC(InddEdx,NHITDC)   = dEdx                               
c                ELSE
c                  PRINT *,'Axial L:MORE THEN 100 HITS ON ONE WIRE '
                ENDIF ! NLH
                endif ! cross wire area
               ENDDO ! NW
              ENDIF ! NSW1, NSW2
            ENDDO ! NH
          ENDIF ! NHIT
          ENDIF ! LAYSON  
C
        ENDDO ! IL
C
C Stereo layers
C
        DO IL=2,6,2
c          print *,'layer:',IL
	  if(layson(IL).gt.0) then
          CALL GFHITS('KDCH',DCCNAM(IL),NVDIM,NHDIM,NHMAX,0,NUMVS,
     +                 ITRACK,NUMBV,HITS,NHIT)
C
	  IF(NHIT.GT.NHMAX) THEN
	    print *,' Warning! DC nhits in layer ',IL,' > ',NHMAX
	    NHIT=NHMAX 
	  ENDIF
          IF(NHIT.GT.0) THEN
            JL=IL/2            
            DO NH=1,NHIT
              Xin=HITS(IndXin,NH)
              Yin=HITS(IndYin,NH)
              Zin=HITS(IndZin,NH)
              Xout=HITS(IndXout,NH)
              Yout=HITS(IndYout,NH)
              Zout=HITS(IndZout,NH)
              NCEL=NUMBV(1,NH) 
              Epart=HITS(IndEin,NH)
         VbetaIn(1)=HITS(IndPXin,NH)*HITS(IndPpIn,NH)/Epart
         VbetaIn(2)=HITS(IndPYin,NH)*HITS(IndPpIn,NH)/Epart
         VbetaIn(3)=HITS(IndPZin,NH)*HITS(IndPpIn,NH)/Epart
        VbetaOut(1)=HITS(IndPXout,NH)*HITS(IndPpOut,NH)/HITS(IndEout,NH)
        VbetaOut(2)=HITS(IndPYout,NH)*HITS(IndPpOut,NH)/HITS(IndEout,NH)
        VbetaOut(3)=HITS(IndPZout,NH)*HITS(IndPpOut,NH)/HITS(IndEout,NH)
          PtrIn=sqrt(HITS(IndPXin,NH)*HITS(IndPXin,NH)
     &    +HITS(IndPYin,NH)*HITS(IndPYin,NH))
              TrackR=HITS(IndR,NH)
              Ampl=HITS(IndAmp,NH)
              ITR=HITS(IndTrack,NH)
              PCharge=HITS(IndCHARGE,NH)
              PChargeSign=1
              if(PCharge.lt.0) PChargeSign=-1      
              VtrOrtIn=sqrt(VbetaIn(1)*VbetaIn(1)+VbetaIn(2)*VbetaIn(2))
              VortIn(1)=VbetaIn(1)/VtrOrtIn
              VortIn(2)=VbetaIn(2)/VtrOrtIn         
              VtrOrtOut=sqrt(VbetaOut(1)*VbetaOut(1)+
     &       VbetaOut(2)*VbetaOut(2))
              VortOut(1)=VbetaOut(1)/VtrOrtOut
              VortOut(2)=VbetaOut(2)/VtrOrtOut              
              if(PtrIn.gt.0) then
                Sz =  HITS(IndPZin,NH)/PtrIn
                Sx =  HITS(IndPXin,NH)/PtrIn  
                Sy =  HITS(IndPYin,NH)/PtrIn                 
                Sa = -TrackR*PChargeSign*VB(3)    
              endif
              Mpart=HITS(IndMASS,NH)
              Ppart=HITS(IndPpIn,NH)     
              TLength=HITS(IndLength,NH)
              if(TLength.ne.0) then
                dEdx=Ampl/TLength
              else
                dEdx=0  
              endif  
C To first cell
              ANG=CELANG(IL)*(NCEL-1)              
C Projection on z=+ZCEL
             XSW0Z=0.5*((XSWR(3,JL,NCEL)+XSWR(4,JL,NCEL))+
     &       (WUV(1,3,JL,NCEL)/WUV(3,3,JL,NCEL)+
     &       WUV(1,4,JL,NCEL)/WUV(3,4,JL,NCEL))*(Zin+ZCEL))
             YSW0Z=0.5*((YSWR(3,JL,NCEL)+YSWR(4,JL,NCEL))+
     &       (WUV(2,3,JL,NCEL)/WUV(3,3,JL,NCEL)+
     &       WUV(2,4,JL,NCEL)/WUV(3,4,JL,NCEL))*(Zin+ZCEL))
              Xi1=Xin-XSW0Z
              Yi1=Yin-YSW0Z
             TWX=((XSWR(6,JL,NCEL)-XSWR(1,JL,NCEL))+(Zin+ZCEL)*
     &      (WUV(1,6,JL,NCEL)/WUV(3,6,JL,NCEL) -
     &        WUV(1,1,JL,NCEL)/WUV(3,1,JL,NCEL)))*0.2
             TWY=((YSWR(6,JL,NCEL)-YSWR(1,JL,NCEL))+(Zin+ZCEL)*
     &      (WUV(2,6,JL,NCEL)/WUV(3,6,JL,NCEL) -
     &        WUV(2,1,JL,NCEL)/WUV(3,1,JL,NCEL)))*0.2                   
              Tnorm=sqrt(TWX*TWX+TWY*TWY)
              TWX=TWX/Tnorm
              TWY=TWY/Tnorm              
              TWR(1)=TWX
              TWR(2)=TWY
              TWR(3)=0              
              WU(1)=WUV34(1,JL,NCEL)
              WU(2)=WUV34(2,JL,NCEL)
              WU(3)=WUV34(3,JL,NCEL)
             call VecPro(TWR,WU,VE)              
             call VecPro(VE,VB,VLOR)
              tanlor=abs(tan(anglelor*WU(3)))
              VDrfN=0
              do iv=1,3
                VDrf(iv)=VE(iv)-tanlor*VLOR(i)
                VDrfN=VDrfN+VDrf(iv)*VDrf(iv)
              enddo  
              VDrfN=sqrt(VDrfN)           
              do iv=1,3
                VDrf(iv)=VDrf(iv)/VDrfN
              enddo             
              call VecPro(WU,VDrf,TWR)              
              Xi2=Xi1*VDrf(1)+Yi1*VDrf(2)
              Yi2=Xi1*TWR(1)+Yi1*TWR(2)             
              XSW0Z=0.5*((XSWR(3,JL,NCEL)+XSWR(4,JL,NCEL))+
     &       (WUV(1,3,JL,NCEL)/WUV(3,3,JL,NCEL)+
     &       WUV(1,4,JL,NCEL)/WUV(3,4,JL,NCEL))*(Zout+ZCEL))
             YSW0Z=0.5*((YSWR(3,JL,NCEL)+YSWR(4,JL,NCEL))+
     &       (WUV(2,3,JL,NCEL)/WUV(3,3,JL,NCEL)+
     &       WUV(2,4,JL,NCEL)/WUV(3,4,JL,NCEL))*(Zout+ZCEL))
              Xo1=Xout-XSW0Z
              Yo1=Yout-YSW0Z
              Xo2=Xo1*VDrf(1)+Yo1*VDrf(2)
              Yo2=Xo1*TWR(1)+Yo1*TWR(2)
              DY = (Yo2-Yi2)
              TA2= 0.
              TA3= 0.
              if(DY .NE. 0.) THEN
                TA2=(Xo2-Xi2)/DY
                TA3=(Zout-Zin)/DY
              endif
C Determine  which sense wires hitted
              CosTN=TWR(1)*TWX+TWR(2)*TWY
              SinTN=sqrt(1-CosTN*CosTN)
              DYSW=Tnorm*CosTN
              DXSW=Tnorm*SinTN
              NSW1=1              
              Ymin=min(Yi2,Yo2)
              Ymax=max(Yi2,Yo2)   
              IF(Ymin.GT.-3.5*DYSW)  NSW1=(Ymin+3.5*DYSW)/DYSW+1
              NSW2=(Ymax+3.5*DYSW)/DYSW+1
              IF(NSW2.GT.6) NSW2=6
      

        

              NcW=NSW2-NSW1
              if(NcW.ge.0) Nstep=1
              else Nstep=-1
              IF(NSW2.LE.6 .AND. NSW1.GT.0) THEN
C Length of this track part
               DX=(Xo2-Xi2)
               DY=(Yo2-Yi2)
               DX0=(Xout-Xin)
               DY0=(Yout-Yin)
               DZ0=(Zout-Zin)
               Dt=SQRT(DX*DX+DY*DY)
               dphiA=(Zout-Zin)/Sz                             
               DO NW=NSW1,NSW2,Nstep                   
               DO it=1,1                 
               if(it.eq.1) then
cccc                Yc=DYSW*(NW-3.5)                      
               Yc=DYSW*(NW-3.5)                           
               else 
                 Yc=Yc+diffY 
               endif                 
               ZpG=Zin+TA3*(Yc-Yi2)                 
               Str=(ZpG-Zin)/Sz   
               dphiA=-Str*PChargeSign*VB(3)/TrackR
               sinAp=sin(dphiA)
               cosAp=cos(dphiA)                   
               XpG=Xin+Sa*(Sx*sinAp-Sy*(1-cosAp))
               YpG=Yin+Sa*(Sy*sinAp+Sx*(1-cosAp))                    
              XSW0Z=0.5*((XSWR(3,JL,NCEL)+XSWR(4,JL,NCEL))+
     &       (WUV(1,3,JL,NCEL)/WUV(3,3,JL,NCEL)+
     &       WUV(1,4,JL,NCEL)/WUV(3,4,JL,NCEL))*(ZpG+ZCEL))
             YSW0Z=0.5*((YSWR(3,JL,NCEL)+YSWR(4,JL,NCEL))+
     &       (WUV(2,3,JL,NCEL)/WUV(3,3,JL,NCEL)+
     &       WUV(2,4,JL,NCEL)/WUV(3,4,JL,NCEL))*(ZpG+ZCEL))
              XpCZ=XpG - XSW0Z
              YpCZ=YpG - YSW0Z
               XpC=XpCZ *VDrf(1)+YpCZ*VDrf(2)
              YpC=XpCZ *TWR(1)+YpCZ*TWR(2)              
              diffY=Yc-YpC               
            enddo ! IT    
ccc           Xc=XpC-DXSW*(NW-3.5)        
ccc           Yc=YpC-DYSW*(NW-3.5)              
            Xc=XpC-DXSW*(NW-3.5)        
            Yc=YpC-DYSW*(NW-3.5)              
               Xcr(NW)=Xc
               Ycr(NW)=Yc
               Zcr(NW)=ZpG
               XMidleHitG(NW)=XpG
               YMidleHitG(NW)=YpG         
               ZMidleHitG(NW)=ZpG               
               XoutR=((XpG-Xin)*(Xout-XpG).lt.0.and.
     &        (abs(XpG-Xin).gt.0.05.or.abs(XpG-Xout).gt.0.05))
              YoutR=((YpG-Yin)*(Yout-YpG).lt.0.and.
     &        (abs(YpG-Yin).gt.0.05.or.abs(YpG-Yout).gt.0.05))
              ZoutR=((ZpG-Zin)*(Zout-ZpG).lt.0.and.
     &        (abs(ZpG-Zin).gt.0.2.or.abs(ZpG-Zout).gt.0.2))
              
              if(XoutR.or.YoutR.or.ZoutR) then             
c           if((ZpG-Zin)*(Zout-ZpG).lt.-0.1*abs(DZ0).or.
c     &        (XpG-Xin)*(Xout-XpG).lt.-0.1*abs(DX0).or.
c     &        (YpG-Yin)*(Yout-YpG).lt.-0.1*abs(DY0)) then             
               IcrossFlag(NW)=0              
                Nskiped=Nskiped+1
          else 
             IcrossFlag(NW)=1                                     
          endif
           if(Sz.ne.0) then
               Str= (ZpG-Zin)/Sz   
               dphiA=-Str*PChargeSign*VB(3)/TrackR
               sinAp=sin(dphiA)
               cosAp=cos(dphiA)  
               VXcr(NW)=VbetaIn(1)*cosAp-VbetaIn(2)*sinAp
               VYcr(NW)=VbetaIn(2)*cosAp+VbetaIn(1)*sinAp
               VZcr(NW)=VbetaIn(3)               
          endif 
              VtrOrt=sqrt(VXcr(NW)*VXcr(NW)+VYcr(NW)*VYcr(NW))
              VxDr(NW)=(VXcr(NW)*VDrf(1)+VYcr(NW)*VDrf(2))/VtrOrt
              VyDr(NW)=(VXcr(NW)*TWR(1)+VYcr(NW)*TWR(2))/VtrOrt
              F=ATAN(Zcr(NW)/STETAN(JL))
              CFcr(NW)=(STECOS(JL)/COS(F))**2               
            ENDDO               ! NW                                    
               DO NW=NSW1,NSW2,Nstep
               if(IcrossFlag(NW).eq.1) then  
               if(VyDr(NW).ne.0) then  
               tanVDr=VxDr(NW)/VyDr(NW)               
               Xdrf1=(Xcr(NW)+Tnorm*tanVDr)*CFcr(NW)
               Xdrf2=(Xcr(NW)-Tnorm*tanVDr)*CFcr(NW)       
               Xmin=min(abs(Xdrf1),abs(Xdrf2))
               Xmax=max(abs(Xdrf1),abs(Xdrf2))
               else
                   Xmin=Xcr(NW)
                   Xmax=Xcr(NW)                                     
               endif                                     
                IF(Xmin*Xmax.LT.0.) THEN
C     If crossed center of cell to calculate distance to sense wire
                   Dmin=abs(Xcr(NW)*VyDr(NW))
                   Dmax=sqrt(Xmax*Xmax+0.25*BOUND2)
                ELSE
C BOth hits outside focus region on the same side of cell
C Check min distance due to cylindr field near sense wire     
                  Dmin=ABS(Xmin)
                  Dmax=ABS(Xmax)
                ENDIF        
                if(Dmin.lt.RFOCUS) then
                    Tmini=6.9*Dmin*Dmin
                else
                    Tmini=(Dmin-RFOCUS)/DRFVEL+6.9* RFOCUS*RFOCUS
                endif
                if(Dmax.lt.RFOCUS) then
                    Tmax=6.9*Dmax*Dmax
                else
                    Tmax=(Dmax-RFOCUS)/DRFVEL+6.9* RFOCUS*RFOCUS
                endif               
                Dc=(Xmax-Xmin)*(Xmax-Xmin)+BOUND2
                Ampl=HITS(IndAmp,NH)
                if(TLength.ne.0.) Ampl=dEdx*SQRT(Dc)
                IW=512*IL+8*NCEL+NW
                ICH=ICHWIR(1,IW)
                IPL=ICHWIR(2,IW)
                ICR=ICHWIR(3,IW)
                NLH=LINKDC(101,ICH,IPL,ICR)+1
                IF(NLH.LE.100) then
c              .AND. Tmini.LT.TIMD) THEN
                  LINKDC(101,ICH,IPL,ICR)=NLH                  
                  NHITDC=NHITDC+1
                  LINKDC(NLH,ICH,IPL,ICR)=NHITDC
                  HITDC(IndIW,NHITDC)=IW
                  HITDC(IndTmin,NHITDC)=Tmini
                  HITDC(IndTmax,NHITDC)=Tmax 
                  HITDC(IndAmpl,NHITDC)=Ampl
                  HITDC(IndX_G,NHITDC)=XMidleHitG(NW)
                  HITDC(IndY_G,NHITDC)=YMidleHitG(NW)
                  HITDC(IndZ_G,NHITDC)=ZMidleHitG(NW)
                  HITDC(IndBetaX,NHITDC) = VXcr(NW)
                  HITDC(IndBetaY,NHITDC) = VYcr(NW)
                  HITDC(IndBetaZ,NHITDC) = VZcr(NW)
                  HITDC(IndNtr,NHITDC)   = ITR
                  HITDC(IndEp,NHITDC)   = Epart
                  HITDC(IndMp,NHITDC)   = Mpart
                  HITDC(IndMom,NHITDC)  = Ppart         
                  HITDC(InddEdx,NHITDC)  = dEdx                           
c                ELSE
c                  PRINT *,'Stereo L:MORE THEN 100 HITS ON ONE WIRE'
                ENDIF ! NLH           
                ENDIF             ! Cross wire area
               ENDDO ! NW             
             ENDIF              ! NSW1 NSW2
            ENDDO ! NH
          ENDIF ! NHIT
          ENDIF ! layson
C
        ENDDO ! IL
C
        DO ICR=1,MAXCR
          DO IPL=1,MAXPL
            NCH=NCHPL(IPL,ICR)
            NHCH=0
            NnonZeroHits=0
!  cycle by socket's of TAM card
            DO ICH=1,NCH
              NTIM(ICH)=0
              NLH=LINKDC(101,ICH,IPL,ICR)
!  if number hits at choosen ADC >0
              IF(NLH.NE.0) THEN
                TIMIN=1000.
!  Find preliminary minimum of time ADC & corresponding global nhitdc
!  cycle by socket's hits
                DO LH=1,NLH
                  NH=LINKDC(LH,ICH,IPL,ICR)
                  DRFTIM=HITDC(IndTmin,NH)
                  IF(DRFTIM.LT.TIMIN) THEN
                    TIMIN=DRFTIM
                    NHMIN=NH
                  ENDIF
                ENDDO
100             CONTINUE
                 NHCH= NHCH+1
                 NTIM(ICH)= NTIM(ICH)+1
                 TMIN(NTIM(ICH),ICH)=TIMIN  ! minimum time corresponded of this number
                 ITIM(NTIM(ICH),ICH)=NHMIN  ! global nhitsdc  corresponded of this number ???
                 IF(NHCH.LT.MAXDIG) THEN
                  TIMDE=TIMIN+TIMLCK
                  TIMIN=1000.
                  DO LH=1,NLH
                    NH=LINKDC(LH,ICH,IPL,ICR)
                    if(NUsedHits(NH).eq.0) then
                    NnonZeroHits=NnonZeroHits+1
                    TAMDrfTime(NnonZeroHits)=HITDC(IndTmin,NH)
                    NGHitDC(NnonZeroHits) = NH
                    NIchTAM(NnonZeroHits) = ICH
                    IF(HITDC(IndTmax,NH).GT.TIMDE) THEN     ! hitdc(3,NH)= max time
                      IF(HITDC(IndTmin,NH).LT.TIMIN) THEN   ! hitdc(2,NH)= min time
                       TIMIN=HITDC(IndTmin,NH)
                       if(TIMIN.LT.TIMDE) TAMDrfTime(NnonZeroHits)=TIMDE
                       NHMIN0=NH
                      ENDIF
                    ENDIF
                    NUsedHits(NH)=1
                    endif
                  ENDDO ! LH
                  IF(TIMIN.NE.1000.) THEN
                    IF(TIMIN.LT.TIMDE) THEN
                      TIMIN=TIMDE
                    ELSE
                      NHMIN=NHMIN0
                    ENDIF
                    GOTO 100
                  ENDIF
                ENDIF ! NHCH
              ENDIF ! NLH
            ENDDO ! ICH
! ok we found minimal drift time  for all sockets
! find minimal time for TAM card
             IF(NHCH.NE.0) THEN
             call SortHitsByTime()
             IDIG=0
             do ii=1,NHUse
                 IF(TAMDrfTime(ii).LT.TIMD) then
                    NHG=NGHitDC(ii)
                    IchTAM=NIchTAM(ii)
                    IW=HITDC(IndIW,NHG)
                    ITIME=GFREQ*(TIMD-TAMDrfTime(ii))                    
                    if(ILOSS.ne.0) then
                        Ampl=HITDC(IndAmpl,NHG)*10**9
                    else
                        Ampl=1024  
                    endif  
                    IADR=16*12*(ICR-1)+12*(IPL-1)+IDIG
                    IBCH=2**(IchTAM-1)
                    IDIGDC(NDIGDC+1)=IADR
                    IDIGDC(NDIGDC+2)=IBCH
                    IDIGDC(NDIGDC+3)=IADR+1
                    IDIGDC(NDIGDC+4)=ITIME
                    IDIGDC(NDIGDC+5)=IADR+2                 
                    if(Ampl.GT.32767.) Ampl=32767.
                    IDIGDC(NDIGDC+6)=Ampl        ! Ampl                    
C! modification 17.10.02 Todyshev
                    if(NRAWHITSDC.le.75000) then 
               RAWHITSDC(NRAWHITSDC+IndWIRE) =IW ! wire number
               RAWHITSDC(NRAWHITSDC+IndADC) =HITDC(IndAmpl,NHG)*10**9 ! eV
               RAWHITSDC(NRAWHITSDC+IndXG) =HITDC(IndX_G,NHG) ! X cm
               RAWHITSDC(NRAWHITSDC+IndYG) =HITDC(IndY_G,NHG) ! Y
               RAWHITSDC(NRAWHITSDC+IndZG) =HITDC(IndZ_G,NHG) ! Z
               RAWHITSDC(NRAWHITSDC+IndVx) =HITDC(IndBetaX,NHG) ! BetaX         v/c
               RAWHITSDC(NRAWHITSDC+IndVy) =HITDC(IndBetaY,NHG) ! BetaY
               RAWHITSDC(NRAWHITSDC+IndVz) =HITDC(IndBetaZ,NHG) ! BetaZ
               RAWHITSDC(NRAWHITSDC+IndTR) =HITDC(IndNtr,NHG) ! Track number 
               RAWHITSDC(NRAWHITSDC+IndE)  =HITDC(IndEp,NHG) *1000 ! Energy MeV
               RAWHITSDC(NRAWHITSDC+IndM)  =HITDC(IndMp,NHG) *1000 ! Mass Mev
               RAWHITSDC(NRAWHITSDC+IndP)  =HITDC(IndMom,NHG) *1000 !Momentum MeV/c 
               RAWHITSDC(NRAWHITSDC+IndRhodE)=HITDC(InddEdx,NHG)*10**9 
!     Energy loss density  eV/cm 
                    NRAWHITSDC=NRAWHITSDC+NumRawPar
                    endif
                    NDIGDC=NDIGDC+6
                    NHITS=NHITS+1                                                    
                    IDIG=IDIG+3
                 ENDIF
             enddo            
           ENDIF ! NHCH
          ENDDO ! IPL
        ENDDO ! ICR
        if(Fdbg) then
         print *,'Nskiped:',Nskiped
         print *,'Nhits:',NHITS
        endif
        RETURN
        END
        subroutine SortHitsByTime()
        include 'dctamhits.inc'
        integer i,j
        real TimeMinOld
        integer NHitOld,NIchOld
        integer imax
        NHUse=0
        if(NnonZeroHits.gt.MaxNHitTAM)  then
        imax=MaxNHitTAM
        else
        imax=NnonZeroHits
        endif
        if(NnonZeroHits.le.4) then
        NHUse=NnonZeroHits
        else
        NHUse=4
        endif
        do i=1,NHUse
        TimeMinOld=TAMDrfTime(i)
        NhitOld=NGHitDC(i)
        NIchOld=NIchTAM(i)
        do j=i+1,imax
        if(TimeMinOld.gt.TAMDrfTime(j)) then
                TimeMinOld    =TAMDrfTime(j)
                TAMDrfTime(j) =TAMDrfTime(i)
                TAMDrfTime(i) =TimeMinOld
                NhitOld=NGHitDC(j)
                NGHitDC(j)=NGHitDC(i)
                NGHitDC(i)=NhitOld
                NIchOld   =NIchTAM(j)
                NIchTAM(j)=NIchTAM(i)
                NIchTAM(i)=NIchOld
        endif
        enddo
        enddo
        end

      	subroutine VecPro(a,b,c)
	implicit none
	double precision a(1:3),b(1:3),c(1:3)
	c(1)=a(2)*b(3)-a(3)*b(2)
	c(2)=a(3)*b(1)-a(1)*b(3)
	c(3)=a(1)*b(2)-a(2)*b(1)
	end

