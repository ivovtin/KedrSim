*KEEP,VD_PAR.
c      COMMON /VD_PAR/ DRFKVD,GFRVD,TIMDVD,NCHT,IADRCH(320),NCHTON(320)
*KEND.

c     Description of VD parameters
c   rDigNS        - TDC channel price (in ns)
c   rT0           - Time pedestal in ns
c   rVDEff        - Tube efficiency
c   rVDRes        - Tube spacial resolution
c   rVDRTC        - Radius to time conversion polynom coeffs 
c   rVDEC         - Efficiency vs rad polynom coeffs
c   rVDRC         - Resolution vs rad polynom coeffs
c   Note:   rVDEC and rVDRC are multiplied by corresponding
c           rVDEff and rVDRes for a given tube, therefore, 
c           they should be 'normalized' by 1.
	integer ncht,iadrch,nchton
	real  rDigNS,rT0,rVDEff,rVDRes,rVDRTC,rVDEC,rVDRC
	
	common /vd_par/ ncht,iadrch(320),nchton(320),rDigNS,rT0, 
     &  rVDEff(320), rVDRes(320), rVDRTC(5), rVDEC(4), rVDRC(4)
	save /vd_par/
