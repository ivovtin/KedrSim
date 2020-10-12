      SUBROUTINE TOF_PETA
c
      COMMON/SC_DATA/REFS,REFP,SS(2,2),APPA(2),A100,EPS,T0,RICH,RIKK
c
      print *,' CONSTANTS FOR SC. COUNTERS'
      print *,REFS,REFP,       ' SC., PLEX, REFRACTION'  
      print *, SS(1,1),SS(2,1),' SC., PLEX, LENGTH END CUP, CM' 
      print *, SS(1,2),SS(2,2),' SC., PLEX, LENGTH BARREL' 
      print *,APPA(1),APPA(2), ' SC., PLEX,  ABSORPTION'
      print *, A100,' GeV FOR 100 PH.E, A100'
      print *,EPS, ' THRESHOLD, % FOR A100'
      print *,T0,  ' TIME OF GAIN'
      print *,RICH,' CHANNALS FOR 1 PH.E'
      print *,RIKK,' CHANNALS FOR 1.E-9 C' 
      print *,' '
      RETURN
      END

