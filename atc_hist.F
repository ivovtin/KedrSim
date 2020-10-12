      SUBROUTINE ATC_HIST
c************************************************************************
c                                                                       *
c  Calculation of amplitude distribution for integer Npe = 1,2,3,..     *
c                                                                       *
c  Used ATC_SINGLE.HIS - RZ file for one-single photoelectron           *
c  spectrum from MCP PMT                                                *
c                                                                       *
c  Last correction 17.10.2002. V.A.Tayursky:                            *
c                                                                       *
c  Instead of reading the ATC_SINGLE.HIS from disk, values are now      *
c  assigned in DATA/.../.                                               *
c                                                                       *
c************************************************************************
c
c  SPEC(500,I) - Array of amplitude distribution from MCP PMT
c                for integer I photoelectrons.
c
      COMMON /ATCPMT/ RES, SPEC(500,0:25)
      DIMENSION SINGLE_HIS(30)
      REAL MEAN
c
      DATA SPEC/13000*0./
      DATA SINGLE_HIS/
     +     0.03062569, 0.04115950, 0.05537084, 0.06077287, 0.06420110,
     +     0.06243505, 0.06316224, 0.06295447, 0.05931849, 0.06347390, 
     +     0.05620193, 0.05381257, 0.04560563, 0.04394347, 0.03822978, 
     +     0.03251609, 0.03230833, 0.02410139, 0.02337419, 0.01869935,
     +     0.01360897, 0.01173904, 0.00966133, 0.00737586, 0.00696031,
     +     0.00664866, 0.00384376, 0.00384376, 0.00145439, 0.00000000/
c
c  Read out the single-photoelectron distribution from RZ file
c-----------------------------------------------------------------
c
c      call search_free_unit(lu)
c
c      call HROPEN(lu,'hbook','ATC_SINGLE.HIS',' ',1024,istat)
c
c      icycle = 999
c
c      call hrin(0,icycle,99)
c
c      call HREND('hbook')
c
c      close(lu)
c
c
c  Transfotmation one-single photoelectron histogram
c  (ID=200) have been read out from RZ file to SPEC array
c--------------------------------------------------------
c........
c   X axis scale parameters
c
c      CALL HGIVE(200,CHTITL,NX,XMI,XMA,NY,YMI,YMA,NWT,LOC)
c
c      MEAN = HSTATI (200, 1, ' ', 0)
c      RES  = ((XMA - XMI) / NX) / MEAN   ! resolution in ph.el./channel 
c                                         ! (maximum 0.1 ph.el./channel)
      RES=0.10235972                
      NPE_MAX = 25                        ! maximum number of photoelectrons
      NCH     = NPE_MAX / RES             ! number of channels
c
c  Put one-single photoelectron histogram to SPEC array
c
c      CALL HUNPAK(200,SPEC(1,1),' ',0)
c
       DO I=1,30
         SPEC(I,1)=SINGLE_HIS(I)
       END DO
c
c  Fill SPEC array
c
      DO I=0,NPE_MAX-1
        IF(I.EQ.0) THEN
          SPEC(1,I) = 1.
        ELSE
c
          CALL ATC_SPECN(2*NCH,SPEC(1,1),SPEC(1,I),SPEC(1,I+1)) ! convolution
c
        END IF
      END DO
c
      RETURN
      END

