      subroutine csi_mcconrl
c******************************************************************
c
c  Conversion of MC coordinates to real ones
c
c  Called from gutrev
c
c******************************************************************
      include 'csiloss.inc'
C     
      include 'csi_digpar.inc'
C     
      include 'csiwrite.inc'
c      real TMPCsIAmp
c
      CALL UZERO(ICsIcr,1,384)
      CALL UZERO(ICsAmp,1,384)
c
*    calculate total energy deposition in group
cvk       do itrrl=1,2
cvk       do imdrl=1,4
cvk       do igr=1,12
cvk       do ich=1,14
cvk       enloss(15,igr,imdrl,itrrl)=enloss(15,igr,imdrl,itrrl)+
cvk     +                            enloss(ich,igr,imdrl,itrrl)
cvk       enddo
cvk       enddo
cvk       enddo
cvk       enddo

C   CREATING PARAMETR WICH WE WILL WRITE TO FILE

      do itrrl=1,2
        NCsNcr(itrrl)=0
        do imdrl=1,4
          do igr=1,12
            do ich=1,14
              enloss(15,igr,imdrl,itrrl)=
     $           enloss(15,igr,imdrl,itrrl)+
     +           enloss(ich,igr,imdrl,itrrl)
              IF (enloss(ich,igr,imdrl,itrrl).GT.CsI_Threshold) then
                N = NCsNcr(itrrl)+1
                NCsNcr(itrrl)=N
C     DAQ format
                ICsIcr(N,itrrl)=
     $             (imdrl-1)*16*12+(igr-1)*16+(ich-1)
                IAmp=(enloss(ich,igr,imdrl,itrrl)*6000.+0.5) ! 1ch=1/6Mev 
C     ICsAmp(N,itrrl)=mod(IAmp,4096)
                IF(IAmp.gt.32767) IAmp=32767
                ICsAmp(N,itrrl)=IAmp
              ENDIF
            enddo
            IF (enloss(15,igr,imdrl,itrrl).GT.CsI_Threshold) THEN 
              N = NCsNcr(itrrl)+1
              NCsNcr(itrrl)=N
C     DAQ format
              ICsIcr(N,itrrl)=(imdrl-1)*16*12+(igr-1)*16+(15-1)
              IAmp=enloss(15,igr,imdrl,itrrl)*1000. + 0.5 ! 1ch=1Mev 
C     ICsAmp(N,itrrl)=mod(IAmp,4096)
              IF(IAmp.gt.32767) IAmp=32767
              ICsAmp(N,itrrl)=IAmp
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      end


