      SUBROUTINE TOF_DIGI
c*************************************************************
c
c  Digitization
c
c  Author: V.I.Yurchenko
c
c  Changed: 
c
c  19.01.2001, 22.04.2003, 16.09.2003.   V.A.Tayursky
c  01.03.2007  O.L.Beloborodova
c
c*************************************************************
      integer ntfdim,ntfhmax,nsizehittof
#if 0
      parameter (ntfdim=3,ntfhmax=500,nsizehittof=256)
#else
c   by sense ntfhmax <= nsizehittof
      parameter (ntfdim=3,ntfhmax=500,nsizehittof=500)
#endif
#include "ReadNat/tofrawhitspar.inc"
c
      include 'tofhits.inc'
c
      include 'tofrawhits.inc'
c
      PARAMETER (IndTofPMT=1,IndTofX1=2,IndTofY1=3,
     &  IndTofZ1=4,IndTofX2=5,IndTofY2=6,IndTofZ2=7,IndTofTime=8,
     &  IndTofBeta=9,IndTofEloss=10,IndTofParticle=11,IndTofType=12,
     &  NumParHitTOF=12)                    
      COMMON /TOFHITS/ HITTOF(NumParHitTOF,nsizehittof)
c
      dimension HITS(ntofdimhits,ntfhmax),NUMBV(ntfdim,ntfhmax)
      dimension intra(ntfhmax)
      dimension NUMVS(ntfdim)
      DATA NUMVS/ntfdim*0/
      double precision Xin,Yin,Zin,Xout,Yout,Zout        
      double precision PXin,PYin,PZin,Pin,Timein,Counter
      double precision Mpart,Pcharge,Eloss,Ppart,Epart
c
      COMMON /SC_PHOT/ PHO(3,128)
      COMMON /SC_DATA/ REFS,REFP,SS(2,2),APPA(2),A100,EPS,T0,RICH,RIKK
c
      CHARACTER*4 TOFCNAM(3)
      DATA TOFCNAM/'ESL0','BS00','ESP0'/
      DO 1 I=1,128               ! cycle on all PMT
        IF(PHO(1,I).EQ.0.) THEN
c
          DO K=1,3
            PHO(K,I)=0.
  	  END DO
c
          GOTO 1 
        END IF
c
        AVE=PHO(2,I)*100./A100   ! average number of p.e. for Poisson 
c 
        CALL GPOISS(AVE,NPH,1)   ! GEANT Poisson generator (V.A.T.)
c
        PHO(2,I)=NPH*RICH        ! ampl. in channel number 
        AVE=PHO(3,I)*100./A100   ! average number of p.e. for Poisson  
c
        CALL GPOISS(AVE,NPH,1)   ! GEANT Poisson gen-r (V.A.T.)
c
        PHO(3,I)=NPH*RICH        ! ampl. in channel number
        IF(PHO(3,I).LT.PHO(2,I)) PHO(3,I)=PHO(2,I)
        THRE=EPS*100             ! THRE - threshold in number of p.e. (5)
c
        IF(PHO(3,I).LE.THRE) THEN
c
          DO K=1,3
            PHO(K,I)=0.
          END DO
c
        ELSE                     ! pho(3,i) >= thre (threshold) 
c
          IF(PHO(2,I).EQ.0.) THEN
c           PRINT *,' TOF_DIGI: ',IEVENT,PHO(1,I),PHO(2,I),PHO(3,I)
            GOTO 2
          END IF
c
c**********
c          print *,'tof_digi: pho(1,i),pho(2,i),T0=',pho(1,i),pho(2,i),T0
          PHO(1,I)=1500.-(PHO(1,I)+THRE/PHO(2,I)*T0)*RIKK/1.E-09
c
 2        IF(PHO(2,I).EQ.0.OR.PHO(1,I).LT.0.) THEN
c
            DO K=1,3
              PHO(K,I)=0.
            END DO
c
          ENDIF
c
          IF(PHO(1,I).GT.4095.) PHO(1,I)=4095.
          IF(PHO(2,I).GT.4095.) PHO(2,I)=4095.
          IF(PHO(3,I).GT.4095.) PHO(3,I)=4095.
c
        END IF
c**********
c      print 999,pho(1,i),pho(2,i),pho(3,i),i
999   format(' TOF_DIGI: PHO(1-3,I)=',3f10.3,'  I =',i4) 
c
 1    CONTINUE
c
      CALL TOF_DIGI1
c
      CALL TOF_ZERO
c
      NRAWHITSTOF=0
      NHITSTOFW=0
      ITRS=0
c
      J=0
      DO IL=1,3      
        CALL GFHITS('KTFH',TOFCNAM(IL),NTFDIM,NTOFDIMHITS,NTFHMAX,
     +ITRS,NUMVS,INTRA,NUMBV,HITS,NHITS)
#if 1
        if(NHITS.gt.NTFHMAX) then
           NHITS=NTFHMAX
           print *,'tof_digi: some hits are missing in ',TOFCNAM(IL)
        endif
#endif
c
        DO I=1,NHITS
          Xin = HITS(ItofXin,I)  
          Yin = HITS(ItofYin,I)  
          Zin = HITS(ItofZin,I)  
          Xout = HITS(ItofXout,I)  
          Yout = HITS(ItofYout,I)  
          Zout = HITS(ItofZout,I)  
c         PXin = HITS(ItofPXin,I)  
c         PYin = HITS(ItofPYin,I)  
c         PZin = HITS(ItofPZin,I)  
          Pin = HITS(ItofPin,I)  
          Epart = HITS(ItofEpartin,I)  
	  Beta = Pin/Epart
	  Eloss = HITS(ItofEloss,I)
          Timein = HITS(ItofTime,I)  
          Counter = HITS(ItofCounter,I)+(IL-1)*32
          Ttrack = HITS(ItofTrack,I)  
          Ttype = HITS(ItofParticle,I)  
c          print *,'tof_digi: x1=',Xin,'  y1=',Yin,'  z1=',Zin
c          print *,'tof_digi: x2=',Xout,'  y2=',Yout,'  z2=',Zout,
c     &    '  T=',Timein
c          print *,'tof_digi: tr=',Ttrack,'  part=',Ttype,' cou=',Counter
c          print *,'tof_digi: dE=',Eloss,'  P=',Pin,'  Ein=',Epart,
c     &    '  beta=',Beta
          
C
#if 1
          if(j.ge.nsizehittof) then
             print *,'tof_digi: too many hits',j,', in',TOFCNAM(IL),
     &            NHITS
             goto 9900
          endif
#endif
          J=J+1
          HITTOF(IndTofPMT,J)  = Counter
          HITTOF(IndTofX1,J)   = Xin
          HITTOF(IndTofY1,J)   = Yin
          HITTOF(IndTofZ1,J)   = Zin
          HITTOF(IndTofX2,J)   = Xout
          HITTOF(IndTofY2,J)   = Yout
          HITTOF(IndTofZ2,J)   = Zout
          HITTOF(IndTofTime,J)    = Timein
          HITTOF(IndTofBeta,J)    = Beta
          HITTOF(IndTofEloss,J)   = Eloss
          HITTOF(IndTofParticle,J)= Ttrack
          HITTOF(IndTofType,J)    = Ttype
C
          RAWHITSTOF(NRAWHITSTOF+itof_PMT) = HITTOF(IndTofPMT,J)   
          RAWHITSTOF(NRAWHITSTOF+rtof_X1)  = HITTOF(IndTofX1,J)   
          RAWHITSTOF(NRAWHITSTOF+rtof_Y1)  = HITTOF(IndTofY1,J)   
          RAWHITSTOF(NRAWHITSTOF+rtof_Z1)  = HITTOF(IndTofZ1,J)   
          RAWHITSTOF(NRAWHITSTOF+rtof_X2)  = HITTOF(IndTofX2,J)   
          RAWHITSTOF(NRAWHITSTOF+rtof_Y2)  = HITTOF(IndTofY2,J)   
          RAWHITSTOF(NRAWHITSTOF+rtof_Z2)  = HITTOF(IndTofZ2,J)   
          RAWHITSTOF(NRAWHITSTOF+rtof_TIME)     = HITTOF(IndTofTime,J)    
          RAWHITSTOF(NRAWHITSTOF+rtof_BETA)     = HITTOF(IndTofBeta,J)    
          RAWHITSTOF(NRAWHITSTOF+rtof_ELOSS)    = HITTOF(IndTofEloss,J)   
          RAWHITSTOF(NRAWHITSTOF+itof_PARTICLE)=HITTOF(IndTofParticle,J)
          RAWHITSTOF(NRAWHITSTOF+itof_TYPE)     = HITTOF(IndTofType,J)    
c
c           print *,'tof_digi1: tr=',HITTOF(IndTofParticle,J),
c     &     '  part=',HITTOF(IndTofType,J),
c     &     '  cou=',HITTOF(IndTofPMT,J)
c          print *,'tof_digi2: x1=',RAWHITSTOF(NRAWHITSTOF+rtof_X1),
c     &     '  y1=',RAWHITSTOF(NRAWHITSTOF+rtof_Y1),'  z1=',
c     &     RAWHITSTOF(NRAWHITSTOF+rtof_Z1)
c           print *,'tof_digi2: x2=',RAWHITSTOF(NRAWHITSTOF+rtof_X2),
c     &     '  y2=',RAWHITSTOF(NRAWHITSTOF+rtof_Y2),'  z2=',
c     &     RAWHITSTOF(NRAWHITSTOF+rtof_Z2)
c           print *,'tof_digi2: tr=',RAWHITSTOF(NRAWHITSTOF+itof_PARTICLE),
c     &     '  part=',RAWHITSTOF(NRAWHITSTOF+itof_TYPE),
c     &     '  cou=',RAWHITSTOF(NRAWHITSTOF+itof_PMT)
c           print *,'tof_digi2: dE=',RAWHITSTOF(NRAWHITSTOF+rtof_ELOSS),
c     &     '  beta=',RAWHITSTOF(NRAWHITSTOF+rtof_BETA),
c     &     '  T=',RAWHITSTOF(NRAWHITSTOF+rtof_TIME)

C
           NRAWHITSTOF = NRAWHITSTOF + itof_length
           NHITSTOFW = NHITSTOFW + 1
        END DO
      END DO
C
 9900 RETURN
      END
