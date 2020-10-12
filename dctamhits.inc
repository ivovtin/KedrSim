!    This common block contains 
      integer MaxNHitTAM
      parameter (MaxNHitTAM=200)
      real  TAMDrfTime(MaxNHitTAM)
      integer  NGHitDC(MaxNHitTAM),NIchTAM(MaxNHitTAM)
      integer  NnonZeroHits,NHUse
      COMMON/DCTAMHITS/ TAMDrfTime,NGHitDC,NIchTAM,
     & NnonZeroHits,NHUse     
      save /DCTAMHITS/