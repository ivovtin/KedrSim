      SUBROUTINE CsI_HITR
C.
C.    *
C.    *   read hits data from bank, fill enloss array for all crystals
C.    *   ^^^^^^^^^^^^^^^^^^^^^^^^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C. last modification: 21.07.08 by A.Shamov;
C    light collection ununiformity along z (#if _CSILIGHTCOLLECTION_)
C.

#define _CSILIGHTCOLLECTION_ 1

#include "KedrSim/csiloss.inc"
#include "ReadNat/csirawhitspar.inc"     
#include "KedrSim/csirawhits.inc"

C
      DIMENSION ITRA(200),HITS(14,200),NV(3,200),NVENDS(3)
      DATA NVENDS/0,0,0/

      save NVENDS

#if _CSILIGHTCOLLECTION_
      real LightColFact,DeltaZ
      integer LightColLoaded/0/
      real LightColData(14,12,4,2)
      save LightColLoaded,LightColData
#endif


C  Get information about hits
      call uzero(enloss,1,1440)

#if _CSILIGHTCOLLECTION_
      if(LightColLoaded.eq.0) then
         call csi_ReadLightColData(LightColData)
         LightColLoaded=1
      endif
#endif

C GETTING INF. ABOUT EN. LOSSES FROM CRISTALS

      IHITS=0
      NRAWHITSCSI=0

      CALL GFHITS('CSHM','CR  ',3,14,200,0,NVENDS,ITRA,NV,HITS,NHITS)
	if(NHITS.gt.200) then
	  print *,'csi CR hits: ',nhits
	  nhits=200
	endif
      IHITS=IHITS+NHITS
      DO I=1,NHITS
C       itrrl=NV(1,I)
       if(HITS(6,I).gt.0) itrrl=1	! z>0, CSIL
       if(HITS(6,I).lt.0) itrrl=2	! z<0, CSIR
       imdrl=HITS(13,I)
       igr  =HITS(12,I)
       ich  =HITS(11,I)
#if _CSILIGHTCOLLECTION_
       DeltaZ=HITS(6,i)-HITS(3,i)
       if(HITS(3,i).lt.0) DeltaZ=-DeltaZ
       LightColFact=1.+DeltaZ*LightColData(ich,igr,imdrl,itrrl)
# if 0
       print *,'xyzC',(hits(j,i),j=1,3),' xyz',(hits(j,i),j=4,6),
     &      ' Dz',DeltaZ,' f',LightColFact
# endif
#else
       LightColFact=1.
#endif
       enloss(ich,igr,imdrl,itrrl)=enloss(ich,igr,imdrl,itrrl)+
     $    HITS(14,I)*LightColFact

C     заполнение  RAWHITSCSI - для истории
       if(NRAWHITSCSI.le.10000) then 
         RAWHITSCSI(NRAWHITSCSI+IrawCsichan) =  HITS(10,I) ! номер канала
         RAWHITSCSI(NRAWHITSCSI+IrawCsIITRA) = ITRA(I)
C(HITS(7,I)-
C     $      (HITS(7,I)/10000)*10000)/100 ! номер частицы
         RAWHITSCSI(NRAWHITSCSI+IrawCsIEnergy) = HITS(14,I)*1.E6 ! энергия в кэв
         NRAWHITSCSI=NRAWHITSCSI+NumCsIRawPar
       else
         print *,'Error csi_hitr NRAWHITSCSI gt 10000: '
         NRAWHITSCSI=10000
       endif

      ENDDO

      CALL GFHITS('CSHM','CRIN',3,14,200,0,NVENDS,ITRA,NV,HITS,NHITS)
	if(NHITS.gt.200) then
	  print *,'csi CR hits: ',nhits
	  nhits=200
	endif
      IHITS=IHITS+NHITS
      DO I=1,NHITS
C       itrrl=NV(1,I)
       if(HITS(6,I).gt.0) itrrl=1	! z>0, CSIL
       if(HITS(6,I).lt.0) itrrl=2	! z<0, CSIR
       imdrl=HITS(13,I)
       igr  =HITS(12,I)
       ich  =HITS(11,I)
#if _CSILIGHTCOLLECTION_
       DeltaZ=HITS(6,i)-HITS(3,i)
       if(HITS(3,i).lt.0) DeltaZ=-DeltaZ
       LightColFact=1.+DeltaZ*LightColData(ich,igr,imdrl,itrrl)
#else
       LightColFact=1.
#endif
       enloss(ich,igr,imdrl,itrrl)=enloss(ich,igr,imdrl,itrrl)+
     $    HITS(14,I)*LightColFact

C     заполнение  RAWHITSCSI - для истории
       if(NRAWHITSCSI.le.10000) then 
         RAWHITSCSI(NRAWHITSCSI+IrawCsichan) =  HITS(10,I) ! номер канала
         RAWHITSCSI(NRAWHITSCSI+IrawCsIITRA) = ITRA(I)
         RAWHITSCSI(NRAWHITSCSI+IrawCsIEnergy) = HITS(14,I)*1.E6 ! энергия в кэв
         NRAWHITSCSI=NRAWHITSCSI+NumCsIRawPar
       else
         print *,'Error csi_hitr NRAWHITSCSI gt 10000: '
         NRAWHITSCSI=10000
       endif
      ENDDO

      CALL GFHITS('CSHM','CR80',4,14,200,0,NVENDS,ITRA,NV,HITS,NHITS)
	if(nhits .gt. 200) then
	  print *,' csi CR80 hits: ',nhits
	  nhits=200
	endif
      IHITS=IHITS+NHITS
      DO I=1,NHITS
C       itrrl=NV(1,I)
       if(HITS(6,I).gt.0) itrrl=1	! z>0, CSIL
       if(HITS(6,I).lt.0) itrrl=2	! z<0, CSIR
       imdrl=HITS(13,I)
       igr  =HITS(12,I)
       ich  =HITS(11,I)
#if _CSILIGHTCOLLECTION_
       DeltaZ=HITS(6,i)-HITS(3,i)
       if(HITS(3,i).lt.0) DeltaZ=-DeltaZ
       LightColFact=1.+DeltaZ*LightColData(ich,igr,imdrl,itrrl)
#else
       LightColFact=1.
#endif
       enloss(ich,igr,imdrl,itrrl)=enloss(ich,igr,imdrl,itrrl)+
     $    HITS(14,I)*LightColFact

C     заполнение  RAWHITSCSI- для истории
       if(NRAWHITSCSI.le.10000) then 
         RAWHITSCSI(NRAWHITSCSI+IrawCsichan) =  HITS(10,I) ! номер канала
         RAWHITSCSI(NRAWHITSCSI+IrawCsIITRA) = ITRA(I) ! номер частицы
         RAWHITSCSI(NRAWHITSCSI+IrawCsIEnergy) = HITS(14,I)*1.E6 ! энергия в кэв
         NRAWHITSCSI=NRAWHITSCSI+NumCsIRawPar
       else
         print *,'Error csi_hitr NRAWHITSCSI gt 10000: '
         NRAWHITSCSI=10000
       endif

      ENDDO

      CALL GFHITS('CSHM','CR88',4,14,200,0,NVENDS,ITRA,NV,HITS,NHITS)
	if(nhits .gt. 200) then
	  print *,' csi CR88 hits: ',nhits
	  nhits=200
	endif
      IHITS=IHITS+NHITS
      DO I=1,NHITS
C       itrrl=NV(1,I)
       if(HITS(6,I).gt.0) itrrl=1	! z>0, CSIL
       if(HITS(6,I).lt.0) itrrl=2	! z<0, CSIR
       imdrl=HITS(13,I)
       igr  =HITS(12,I)
       ich  =HITS(11,I)
#if _CSILIGHTCOLLECTION_
       DeltaZ=HITS(6,i)-HITS(3,i)
       if(HITS(3,i).lt.0) DeltaZ=-DeltaZ
       LightColFact=1.+DeltaZ*LightColData(ich,igr,imdrl,itrrl)
#else
       LightColFact=1.
#endif
       enloss(ich,igr,imdrl,itrrl)=enloss(ich,igr,imdrl,itrrl)+
     $    HITS(14,I)*LightColFact

C     заполнение  RAWHITSCSI- для истории
       if(NRAWHITSCSI.le.10000) then 
         RAWHITSCSI(NRAWHITSCSI+IrawCsichan) =  HITS(10,I) ! номер канала
         RAWHITSCSI(NRAWHITSCSI+IrawCsIITRA) = ITRA(I) ! номер частицы
         RAWHITSCSI(NRAWHITSCSI+IrawCsIEnergy) = HITS(14,I)*1.E6 ! энергия в кэв
         NRAWHITSCSI=NRAWHITSCSI+NumCsIRawPar
       else
         print *,'Error csi_hitr NRAWHITSCSI gt 10000: '
         NRAWHITSCSI=10000
       endif

      ENDDO
      CALL GFHITS('CSHM','CRTR',4,14,200,0,NVENDS,ITRA,NV,HITS,NHITS)
	if(nhits .gt. 200) then
	  print *,' csi CRTR hits: ',nhits
	  nhits=200
	endif
      IHITS=IHITS+NHITS
      DO I=1,NHITS
C       itrrl=NV(1,I)
       if(HITS(6,I).gt.0) itrrl=1	! z>0, CSIL
       if(HITS(6,I).lt.0) itrrl=2	! z<0, CSIR
       imdrl=HITS(13,I)
       igr  =HITS(12,I)
       ich  =HITS(11,I)
       icr =HITS(10,I)
#if _CSILIGHTCOLLECTION_
       DeltaZ=HITS(6,i)-HITS(3,i)
       if(HITS(3,i).lt.0) DeltaZ=-DeltaZ
       LightColFact=1.+DeltaZ*LightColData(ich,igr,imdrl,itrrl)
#else
       LightColFact=1.
#endif
       enloss(ich,igr,imdrl,itrrl)=enloss(ich,igr,imdrl,itrrl)+
     $    HITS(14,I)*LightColFact

C     Сложные кристаллы - поиск второй части
       iflag=0
       DO J=0,NRAWHITSCSI-NumCsIRawPar,NumCsIRawPar
         if (RAWHITSCSI(J+IrawCsichan).eq.icr.and.
     $      RAWHITSCSI(J+IrawCsIITRA).eq.ITRA(I)) then
           iflag=1
           RAWHITSCSI(J+IrawCsIEnergy)=RAWHITSCSI(J+IrawCsIEnergy)+
     $        HITS(14,I)*1.E6    
         endif
       enddo
C     заполнение  RAWHITSCSI - для истории
       if (iflag.eq.0) then
         if(NRAWHITSCSI.le.10000) then 
           RAWHITSCSI(NRAWHITSCSI+IrawCsichan) =  HITS(10,I) ! номер канала
           RAWHITSCSI(NRAWHITSCSI+IrawCsIITRA) = ITRA(I) ! номер частицы
           RAWHITSCSI(NRAWHITSCSI+IrawCsIEnergy) = HITS(14,I)*1.E6 ! энергия в кэв
           NRAWHITSCSI=NRAWHITSCSI+NumCsIRawPar
         else
           print *,'Error csi_hitr NRAWHITSCSI gt 10000: '
           NRAWHITSCSI=10000
         endif
       endif

      ENDDO

c	print *,' CsI hits: ', ihits
C
      RETURN
      END
