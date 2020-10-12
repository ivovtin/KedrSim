	function kf2kg(kf)
c
c function converts LUND kf code of particle to Geant code kg for standard 
c 38 GEANT particles + neutrinos (returns 0 for others); 
c kg for all types of neutrinos = 4 ("nu_e")
c
	dimension irecod(2,38)

c  Array for recoding of id-codes to GEANT-codes
      data irecod/11,3, -11,2, 12,4,  13,6, -13,5, 
     +     22,1, 111,7, 211,8, -211,9, 221,17, 310,16, 130,10, 
     +     321,11, -321,12, 
     +     2112,13, -2112,25, 3112,21, -3112,29, 3122,18, 
     +    -3122,26, 3212,20, -3212,28, 3222,19, -3222,27, 3312,23, 
     +    -3312,31, 3322,22, -3322,30, 3334,24, -3334,32, 1011,45,
     +     1021,46, 1022,47, 100,48, 1012,49, 2212,14, -2212,15,
     +     20022,50/ 

	kf2kg=0
	
        do j=1,38
          if(kf.eq.irecod(1,j)) kf2kg=irecod(2,j)
        end do
	
	if(kf.eq.-12.or.iabs(kf).eq.14.or.iabs(kf).eq.16) kf2kg=4 ! neutrino
	
	end
