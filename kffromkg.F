	function kffromkg(kg)
c
c gets LUND kf code from GEANT kg code
c
       COMMON/PSPIND/NPIND,INDP(600)
c
      if(kg.lt.1.or.kg.gt.npind) then
        print *,' Error in kffromkg: K-Geant=',kg,' Npind=',npind
        stop
      end if
c
      kffromkg=indp(kg)
      return	
      end
