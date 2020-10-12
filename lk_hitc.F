*CMZU:  1.00/01 16/10/96  17.53.45  by  MALYSHEV V.M.
*-- Author :
      SUBROUTINE LK_HITC
C      integer IEVENT,IEVENT !номер события и номер трека

c**************************************************************
c     *
c     *       LKr HITS calculation
c     *
c
c Changes:
c
c 23.05.2001 V.M.Malyshev : 	Warning print if NL<0
c	removed (may appear when photon fly toward center, in 
c	this case GEANT defines volume incorrect, but it does
c	not lead to error, because hit coordinates calculates
c	this subroutine)
c.
c**************************************************************
#include "gctrak.inc"
#include "gctmed.inc"
#include "gcflag.inc"
#include "gckine.inc"

#include "KedrSim/lk_geomp.inc"
#include "KedrSim/dconst.inc"
#include "KedrSim/lk_hitp.inc"
#include "KedrSim/lk_digp.inc"
#include "KedrSim/lk_curbl.inc"
#include "KedrSim/lk_hitv.inc"
#include "KedrSim/lk_eloss.inc"
#include "KedrSim/lk_hdsw.inc"
#include "KedrSim/lkrrawhits.inc"

#include "ReadNat/lkrrawhitspar.inc"

      PARAMETER(STEPMAX=.25)
C      PARAMETER(RE1=76.701,DLF=2.,DE=.056,DE2=.028)

      REAL*8 X,Y,Z,R,FI,ZA,RS

      REAL DVECT(3)	! coordinates of point in Daughter Ref. System

      integer icounter
      DATA Icounter/0/	  
      DATA NRAWHITSLKR/0/

      DIMENSION KBLR(9,3)  ! UP layer's number in tower
      DATA KBLR/
     1  8 , 8, 8, 8, 7, 7, 6, 6, 8,
     2  13,13,12,12,11,11, 9, 9, 8,
     3  17,17,17,17,14,14,12, 9, 8 /
c     3  17,17,16,16,14,14,12, 9, 8 / this is corrected 29.04.2003

C Bad cells in calorimeter... (R,FI,Z, Number of dead layers). 
C Dead layers appeared during assembling of electrode system, so they are present in all runs...
      PARAMETER(NBADCELLS=7)
      DIMENSION NBAD(4,NBADCELLS)
      DATA NBAD/
     1  3, 36, 1, 1,
     2  2, 11, 4, 1,
     3  2, 16, 8, 1,
     4  1, 43,-9, 4,
     5  3, 42,-4, 2,
     6  1, 38,-7, 1,
     7  3, 33,-7, 1/

      SAVE icounter

C  take krypton parameters from LKGEOM card
      RE1=REL1O+0.001
      DLF=DRLAY
      DE=DREL
      DE2=DE/2.

C     Сброс массива RAWHITSLKR
      if (icounter.ne.IEVENT) then
        NRAWHITSLKR=0
        icounter=IEVENT
      endif

      CALL GMTOD(VECT,DVECT,1)	! coordinates MRS->DRS
c      print *,'vect= ',vect(1),vect(2),vect(3)
c      print *,'dvect=',dvect(1),dvect(2),dvect(3)

      IF(STEP.LT.STEPMAX) THEN
        X=XO+(DVECT(1)-XO)/2.
        Y=YO+(DVECT(2)-YO)/2.
        Z=ZO+(DVECT(3)-ZO)/2.
      ELSE     ! may be photoeffect or another point-like ionisation
        X=DVECT(1)
        Y=DVECT(2)
        Z=DVECT(3)
      ENDIF
      XOLD=XO
      YOLD=YO
      XNEW=DVECT(1)
      YNEW=DVECT(2)
      XO=DVECT(1)
      YO=DVECT(2)
      ZO=DVECT(3)

      IF(DESTEP.EQ.0.) RETURN
      IF(NUMED.NE.9) RETURN

      DESTEPM=DESTEP*1000. ! MeV

      R=DSQRT(X*X+Y*Y)
      FI=DACOS(X/R)
      IF(Y.LT.0.) FI=TWOPI-FI
      NL2=(R-RE1)/DLF+1.    ! 2-sm. layer's number
      RLN=NL2*DLF+RE1       ! radius of next 2-sm. layer
      DR=RLN-R
      IF(DR.LT.DE) RETURN  ! inside electrode


C ***          Energy deposited

C***  ---------- in blocks ----------------

      NFI=FI/BFINOR+1.          ! FI number
      IF(NFI.GT.48) then
	print *,'Warning! LK_hitc nfi,fi,bfinor: ',nfi,fi,bfinor
	NFI=48
      endif
      NL=(R-RE1)/2./DLF+1.      ! layer's number
c     IF(NL.LT.0) PRINT *,'****LK-HITC>NL=',NL	! may appear when photon fly to
c						! center (GEANT bug!)
      IF(NL.LE.0.OR.NL.GT.17) RETURN
      ZA=DABS(Z)
      DO I=1,8
        IF(ZA.LE.BLKZ(NL,I)) GOTO 2
      ENDDO
      NZ=9                      ! tower at the edge
      GOTO 3
 2    NZ=I                      ! Z number

 3    DO I=1,3
        IF(NL.LE.KBLR(NZ,I)) GOTO 4
      ENDDO
      RETURN
 4    NR=I

      IF(Z.LT.0) NZ=-NZ

      if(NR.gt.3 .or. NR.lt.1 .or. NZ.lt.-9 .or. NZ.gt.9
     +	 .or. NFI.lt.1 .or. NFI.gt.48) then
        print *,' x,y,r:',x,y,r
        print *,' nr,nz,nfi,fi,bfinor: ',nr,nz,nfi,fi,bfinor
      endif

      ABL(NR,NZ,NFI)=ABL(NR,NZ,NFI)+DESTEPM

C Check for dead cells... (R,FI,Z, N dead layers)
      DO I=1,NBADCELLS
	NRBAD=NBAD(1,I)
	NFIBAD=NBAD(2,I)
	NZBAD=NBAD(3,I)
	NDEADL=NBAD(4,I)
	NLBAD1=1
	IF(NRBAD.GT.1) NLBAD1=KBLR(ABS(NZ),NRBAD-1)+1
	NLBAD2=NLBAD1+NDEADL-1
	IF(NR.EQ.NRBAD.AND.NFI.EQ.NFIBAD.AND.NZ.EQ.NZBAD.AND.
     1     NL.GE.NLBAD1.AND.NL.LE.NLBAD2) GOTO 1001
      ENDDO





C     -------------------------------------------------------------------
C     заполнение  RAWHITSLKR - для MC истории Е.М.Балдин 23.06.2005 20:32
C     -------------------------------------------------------------------
      iflag=0
C     определение номера канала - выдрано из контектса
      if (ILKHD(1).ne.0) THEN   ! Tower Hits + 
        CALL LKP2G(NR,NZ,NFI,ichannel)
      elseif  (ILKHD(5).ne.0) THEN    ! Z-strips Hits +
        NRS=(R-RE1+DLF)/2./DLF +1. ! layer's number
        IF(NRS.GT.8) goto 1100
        RS=RE1+(NRS-1)*2.*DLF   ! strip's radius
        IF(MOD(NRS,2).NE.0) GOTO 1105 ! FI strip
        NRZ=NRS/2.              ! Z strips R number
        NZZ=DATAN(ZA/RS)/ALPHA+1. ! Z strips Z number
        IF(NZZ.GT.(129-5*NRZ)) GOTO 1100
        IF(Y.LT.0.) NRZ=-NRZ
        IF(Z.LT.0.) NZZ=-NZZ
        CALL LKP2G(NRZ,NZZ,0,ichannel)
      elseif(ILKHD(9).ne.0) THEN    ! FI-strips Hits +
 1105   NRFI=NRS/2.+1.          ! FI strips R  number
        NFIFI=FI/SFINOR+1.      ! FI strips FI number
        IF(NFIFI.EQ.769) NFIFI=768
        CALL LKP2G(NRFI,0,NFIFI,ichannel)
      endif
C     заполнение массива каналов
C     поиск если канал уже существует и энергию надо добавить
      DO I=0,NRAWHITSLKR-NumLKrRawPar,NumLkrRawPar
        if (RAWHITSLKR(I+IrawLkrchan).eq.ichannel.and.
     $     RAWHITSLKR(I+IrawLkrITRA).eq.ITRA) then
          iflag=1
          RAWHITSLKR(I+IrawLkrEnergy)=RAWHITSLKR(I+IrawLkrEnergy)+
     $       DESTEP*1.E6    
        endif
      enddo
C     Вставка нового канала
      if (iflag.eq.0) then
        if(NRAWHITSLKR.le.32768) then 
c$$$          print *,ichannel,ITRA,DESTEP*1.E6,NRAWHITSLKR+IrawLkrchan
c$$$     $       ,NRAWHITSLKR+IrawLkrITRA,NRAWHITSLKR+IrawLkrEnergy
          RAWHITSLKR(NRAWHITSLKR+IrawLkrchan) =  ichannel ! номер канала
          RAWHITSLKR(NRAWHITSLKR+IrawLkrITRA) = ITRA ! номер частицы
          RAWHITSLKR(NRAWHITSLKR+IrawLkrEnergy) = DESTEP*1.E6 ! энергия в кэв
          NRAWHITSLKR=NRAWHITSLKR+NumLkrRawPar
        else
          print *,'Error lkr_hitr NRAWHITSLKR gt 32768: '
          NRAWHITSLKR=32768
        endif
      endif

 1100 continue
C     -------------------------------------------------------------------

      IF(ILKHD(1).ne.0) THEN    ! Tower Hits +

        IF(TOFG*1e6.GT.TDR/FBC) GOTO 1001
        KT=TOFG*1E9/DTBL+1.5    ! Tofg correction
        DO NT=1,NTBL
          IF(KT.EQ.NTB(NT)) GOTO 21
        ENDDO
        NTBLA=NTBLA+1
        IF(NTBLA.GT.NTBL) THEN	! very many hits with different time
        	PRINT *,'****LK_HITC> Warning: very many BLOCK hits with different time : NTBLA=',NTBLA
		NTBLA=NTBLA-1
                GOTO 1001
        ENDIF	
        NTB(NTBLA)=KT
        NT=NTBLA

 21     CONTINUE
        IF(NT.LE.NTBL) THEN

C-- Y1,Y2,Z1,Z2 for currents calculation ---------------
C y1,y2..
          RL=RE1-DLF+DLF*2.*NL  ! R of layer
          Y1=((NFI-1)*BFINOR-FI)*RL
          Y2=Y1+BFINOR*RL
          if(y1.gt.0.or.y2.lt.-1e-3) print *,'y1,y2=',y1,y2
          
          YMI=AMIN1(ABS(Y1),ABS(Y2))
C z1,z2..
          IF(Z.GE.0) THEN
            Z1=BLKZ(NL,NZ-1)-Z
            Z2=BLKZ(NL,NZ)-Z
          ELSE
            Z1=BLKZ(NL,NZ)-Z
            Z2=BLKZ(NL,NZ+1)-Z
          ENDIF
          ZMI=AMIN1(ABS(Z1),ABS(Z2))
C--------------------------------------------------
C-- neibours --------------------------------------
          NEIBY1=0
          NEIBY2=0
          NEIBZ1=0
          NEIBZ2=0

          IF(ABS(Y1)/DL.LT.RMIN) THEN
            NEIBY1=1
            NFIN=NFI-1
            IF(NFIN.EQ.0) NFIN=48
          ENDIF
          IF(ABS(Y2)/DL.LT.RMIN) THEN
            NEIBY2=1
            NFIN=NFI+1
            IF(NFIN.EQ.49) NFIN=1
          ENDIF

          IF(ABS(Z1)/DL.LT.RMIN) THEN
            NEIBZ1=1
            NZN=NZ-1
            IF(NZN.EQ.0) NZN=-1
            IF(NZN.EQ.-11+NR) NEIBZ1=0 ! tower at the edge
          ENDIF
          IF(ABS(Z2)/DL.LT.RMIN) THEN
            NEIBZ2=1
            NZN=NZ+1
            IF(NZN.EQ.0) NZN=1
            IF(NZN.EQ.11-NR) NEIBZ2=0 ! tower at the edge
          ENDIF

          NEIBSUM=NEIBY1+NEIBY2+NEIBZ1+NEIBZ2
          IF(NEIBSUM.GT.2) PRINT *,'****LK_HITC-TOWERS>NEIBSUM=',NEIBSUM
C------------------------------------------------------------------

C--currents ------------------------------------------------------
          RO=SQRT(XOLD*XOLD+YOLD*YOLD)
          RN=SQRT(XNEW*XNEW+YNEW*YNEW)
          ACO=ABS(RO-RL+DE2)-DE2
          IF(ACO.LE.0 ) ACO=1E-5 ! may be ins. electrode
          IF(ACO.GE.DL) ACO=DL-1E-5 !
          ACN=ABS(RN-RL+DE2)-DE2
          IF(ACN.LE.0 ) ACN=1E-5 ! may be ins. electrode
          IF(ACN.GE.DL) ACN=DL-1E-5 !
          ACMAX=AMAX1(ACO,ACN)
          ACMIN=AMIN1(ACO,ACN)
          DAC=ACMAX-ACMIN
          IXMAX=ACMAX/DA+1.
          IXMIN=ACMIN/DA+1.
          DAMAX=AMOD(ACMAX,DA)
          DAMIN=AMOD(DL-ACMIN,DA)
          IF(DAC.NE.0) THEN
            DSTMAX=DESTEPM*DAMAX/DAC
            DSTMIN=DESTEPM*DAMIN/DAC
            DSTF  =DESTEPM*DA/DAC
          ENDIF
          NDIV=IXMAX-IXMIN+1


          DO IDIV=1,NDIV
            DESTEPMT=DSTF
            IF(IDIV.EQ.1   ) DESTEPMT=DSTMAX
            IF(IDIV.EQ.NDIV) DESTEPMT=DSTMIN
            IF(NDIV.EQ.1   ) DESTEPMT=DESTEPM
            IXF=IXMAX-IDIV+1
            IY=YMI/DY+.5
            IF(IY.GT.NYC) IY=NYC
            IZ=ZMI/DZ+.5
            IF(IZ.GT.NZC) IZ=NZC
            if(iy.lt.0.or.iy.gt.100) print *,'iy=',iy
            if(iz.lt.0.or.iz.gt.100) print *,'iz=',iz
            DO NP=1,NPH
              IX=IXF-NP+1
              IF(IX.GT.0) THEN
c     if(ix.gt.nxc) print *,'x,y,z,xc,yc,zc',ix,iy,iz,nxc,nyc,nzc
                HBL(NR,NZ,NFI,NT,NP)=HBL(NR,NZ,NFI,NT,NP)+
     1             DESTEPMT*CUR(IX,IY,IZ)*NPDR
                IF((NEIBY1.ne.0).OR.(NEIBY2.ne.0)) THEN
                  HBL(NR,NZ,NFIN,NT,NP)=HBL(NR,NZ,NFIN,NT,NP)+
     1               DESTEPMT*(CUR(IX,NYC,IZ )-CUR(IX,IY,IZ))*NPDR
                ENDIF
                IF((NEIBZ1.ne.0).OR.(NEIBZ2.ne.0)) THEN
                  HBL(NR,NZN,NFI,NT,NP)=HBL(NR,NZN,NFI,NT,NP)+
     1               DESTEPMT*(CUR(IX,IY ,NZC)-CUR(IX,IY,IZ))*NPDR
                ENDIF
                IF(NEIBSUM.EQ.2) THEN
                  HBL(NR,NZN,NFIN,NT,NP)=HBL(NR,NZN,NFIN,NT,NP)+
     1               DESTEPMT*(CUR(IX,NYC,NZC)-CUR(IX,NYC,IZ)-
     2               CUR(IX,IY ,NZC)+CUR(IX,IY ,IZ))*NPDR
                ENDIF
              ENDIF
            ENDDO
            
          ENDDO
          
        ENDIF

 1001 ENDIF
C------------------------------------------------------------------

C*** --------------- on strips ------------------

      NRS=(R-RE1+DLF)/2./DLF +1. ! layer's number
      IF(NRS.GT.8) RETURN
      RS=RE1+(NRS-1)*2.*DLF           ! strip's radius
      IF(MOD(NRS,2).NE.0) GOTO 5 ! FI strip

C..Z-strips
      NRZ=NRS/2.                      ! Z strips R number
      NZZ=DATAN(ZA/RS)/ALPHA+1.       ! Z strips Z number
      IF(NZZ.GT.(129-5*NRZ)) RETURN
      IF(Y.LT.0.) NRZ=-NRZ
      IF(Z.LT.0.) NZZ=-NZZ

      ASTZ(NRZ,NZZ)=ASTZ(NRZ,NZZ)+DESTEPM


      IF(ILKHD(5).ne.0) THEN    ! Z-strips Hits +

        IF(TOFG*1E6.GT.TDR) GOTO 1002
        KT=TOFG*1E9/DTST+1.5    ! Tofg correction
        DO NT=1,NTST
          IF(KT.EQ.NTS(NT)) GOTO 22
        ENDDO
        NTSTA=NTSTA+1
        IF(NTSTA.GT.NTST) THEN
        	PRINT *,'****LK_HITC> Warning: very many Z-hits with different time : NTSTA=',NTSTA
		NTSTA=NTSTA-1
                GOTO 1002
        ENDIF
        NTS(NTSTA)=KT
        NT=NTSTA

 22     CONTINUE

        IF(NT.LE.NTST) THEN

C-- Z-strips currents calculation --------------------------------
          NZZ0=NZZ
          IF(NZZ0.LT.0) NZZ0=NZZ+1 ! for ZSTZ
          
          AC=R-RS               ! init. dist. to Z-strip electrode
          IF(AC.LT.0) AC=-AC-DE
          IF(AC.GT.DL.OR.AC.LT.0) PRINT *,'****LK_HITC-Z>AC=',AC
          AC=AC-DAS
          DO NP=1,NPHS
            AC=AC+DAS
            IF(AC.LE.DL) THEN
              DO IS=-NSN,NSN    ! NSN neibour strips
                Z1=ZSTZ(NRZ,NZZ0+IS-1)-Z
                Z2=ZSTZ(NRZ,NZZ0+IS  )-Z
                NZZN=NZZ+IS
                IF(NZZ.EQ.0.OR.IABS(NZZ).GT.124) PRINT *,'****NZZ=',NZZ
                IF(NZZN.GE.0.AND.NZZ.LT.0) NZZN=NZZN+1
                IF(NZZN.LE.0.AND.NZZ.GT.0) NZZN=NZZN-1
                IF(IABS(NZZN).GT.129-5*IABS(NRZ)) GOTO 11 ! edge
                IF(ABS(Y).GE.RMINS*DL) THEN
                  HSTZ(NRZ,NZZN,NT,NP)=HSTZ(NRZ,NZZN,NT,NP)+
     1               DESTEPM*FGCT(Z1,Z2,AC)
                ELSE            ! near y=0 plane
                  CUR0=CFCURT(Z1,Z2,ABS(REAL(Y)),AC)
                  HSTZ( NRZ,NZZN,NT,NP)=HSTZ( NRZ,NZZN,NT,NP)+
     1               DESTEPM*CUR0
                  HSTZ(-NRZ,NZZN,NT,NP)=HSTZ(-NRZ,NZZN,NT,NP)+
     1               DESTEPM*(CFCURT(Z1,Z2,100.,AC)-CUR0)
                ENDIF
 11           ENDDO
            ENDIF
          ENDDO
          
        ENDIF

 1002 ENDIF

      RETURN
C------------------------------------------------------------------------
C..FI
 5    NRFI=NRS/2.+1.            ! FI strips R  number
      NFIFI=FI/SFINOR+1.        ! FI strips FI number
      IF(NFIFI.EQ.769) NFIFI=768

      ASTFI(NRFI,NFIFI)=ASTFI(NRFI,NFIFI)+DESTEPM


      IF(ILKHD(9).ne.0) THEN    ! FI-strips Hits +
        
        IF(TOFG*1E6.GT.TDR) GOTO 1003
        KT=TOFG*1E9/DTST+1.5    ! Tofg correction
        DO NT=1,NTST
          IF(KT.EQ.NTS(NT)) GOTO 23
        ENDDO
        NTSTA=NTSTA+1
        IF(NTSTA.GT.NTST) THEN
        	PRINT *,'****LK_HITC> Warning: very many FI-hits with different time : NTSTA=',NTSTA
		NTSTA=NTSTA-1
                GOTO 1003
	ENDIF
        NTS(NTSTA)=KT
        NT=NTSTA

 23     CONTINUE

        IF(NT.LE.NTST) THEN

C-- FI-strips currents calculation --------------------------------
          AC=R-RS               ! init. dist. to Z-strip electrode
          IF(AC.LT.0) AC=-AC-DE
          IF(AC.GT.DL.OR.AC.LT.0) PRINT *,'****LK_HITC-FI>AC=',AC
          AC=AC-DAS
          DO NP=1,NPHS
            AC=AC+DAS
            IF(AC.LE.DL) THEN
              DO IS=-NSN,NSN    ! NSN neibour strips
                Y1=((NFIFI+IS-1)*SFINOR-FI)*RS
                Y2=Y1+SFINOR*RS
                IF(NFIFI.LE.0.OR.NFIFI.GT.768) PRINT *,'****NFIFI='
     $             ,NFIFI
                NFIFIN=MOD(NFIFI+IS,768)
                IF(NFIFIN.LE.0) NFIFIN=NFIFIN+768
                HSTFI(NRFI,NFIFIN,NT,NP)=HSTFI(NRFI,NFIFIN,NT,NP)+
     1             DESTEPM*FGCT(Y1,Y2,AC)
              ENDDO
            ENDIF
          ENDDO

        ENDIF

 1003 ENDIF
C------------------------------------------------------------------------

      END
