      subroutine CSI_READLIGHTCOLDATA(LightColData)
      implicit none
c***********************************************************
c
c  read CsI light collection data
c
c***********************************************************

      real LightColData(14,12,4,2)
      integer ITRMC,IMDMC,IBLY,IBLX,ICR,ITRRL,IMDRL,
     &     IGR,ICH,LCN
      real Data(1536)

      open(98,file='CsIlighColData.dat',
     &     status='old',err=910)
      do LCN=1,1536
         read(98,*,err=920,end=920) Data(LCN)
         if(abs(Data(LCN)).ge.99.) Data(LCN)=0.
         Data(LCN)=Data(LCN)/100./15.
      enddo
      close(98)
      print *,'File "CsIlightColData.dat" is read in'

 50   DO ITRMC=1,2
         DO IMDMC=1,4
            DO IBLY=1,10
               DO IBLX=1,10
                  DO ICR=1,2
                     CALL CSI_MCTORL(ITRMC,IMDMC,IBLX,IBLY,ICR,
     &                    ITRRL,IMDRL,IGR,ICH)
                     if(ICH.gt.0) then
                        LCN=(imdrl-1)*16*12+(igr-1)*16+(ich-1)+
     &                       +(itrrl-1)*16*12*4+1             
                        LightColData(ICH,IGR,IMDRL,ITRRL)=Data(LCN)
                     endif
                  ENDDO
               ENDDO  
            ENDDO
         ENDDO
      ENDDO

      RETURN

 910  print *,'Failed to open "CsIlightColData.dat"',
     &     ', assuming perfect light collection'
      do LCN=1,1536
         Data(LCN)=0.
      enddo
      goto 50

 920  print *,'Error reading "CsIlightColData.dat", line',LCN
      close(98)
      stop

      END
