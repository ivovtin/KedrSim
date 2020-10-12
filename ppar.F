	Subroutine ppar(kf,pname,pmass,charge,ctau)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Gives name *12, mass(GeV/c**2), charge, ctau (cm) 
c  for particle with LUND code kf (returns improbable
c  parameters for unknown kf)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      COMMON /PSPROP/ kedr_part,a4d(600),i1d(600),r1d(600),r2d(600),
     +      r3d(600),i2d(600),i3d(600),r4d(600),i4d(600),i5d(600)
      dimension Cmnt(80)
      character *1 Cmnt,a3(5),a4(12),star
      character *1 ce,cec,cn,cnc,cd,cac,cdc
      character *1 cp,cpc,ca,cr,crc,a1(12)
      character *12 a4t,a4d,a1t
      real *8 r1,r2,r3,r4
      equivalence (a4t,a4(1)),  (a1t,a1(1))

c      include 'kdpath.inc'
      character *80 file
c
      data star,ce,cn,cd,cac,cdc/'*','e','n','d','A','D'/
      data cp,cpc,ca,cr,crc/'p','P','a','r','R'/
      data cec,cnc/'E','N'/

      common/pparc/icall
      character *12 pname
	
      IF(icall.ne.1) THEN     ! read kedr_part.dat

      icall=1
c
c
      call search_free_unit(lu1)                                       
c
c      lu1=1

c      file=kdpath(:index(kdpath,' ')-1)//'kedr_part.dat'
      file='kedr_part.dat'
      OPEN (lu1,FILE = file,status='old',err=741)
      goto 742
741   file=KEDR_RELEASE//'KedrSim/kedr_part.dat'
      OPEN (lu1,FILE = file,status='old')
c      
742   Lmax=1000   ! max lines in text file
      ind=0
      iend=0
      iadd=0
      idec=0
      ipart0=50   ! initial max GEANT code
      Lmin=0
      NPIND=0     ! number of particles in /PSPIND/
c
c
15    read (lu1,4, end=2) Cmnt   ! first lines before 'Particles': comments
      Lmin=Lmin+1
      if( (Cmnt(1).eq.cpc.or.Cmnt(1).eq.cp).and.
     +    (Cmnt(2).eq.cac.or.Cmnt(2).eq.ca).and.
     +    (Cmnt(3).eq.crc.or.Cmnt(3).eq.cr) ) goto 16
      goto 15
c
c---------------  Read main part of table --------------------------------
c    
16    do 3 L=Lmin+1,Lmax
c
        if(iend.eq.0)          ! main table
     +     read (lu1,1, end=2) a3,a4,i1,r1,r2,r3,i2,i3,r4,i4
c
        if((a3(1).eq.ce.or.a3(1).eq.cec).and.
     +     (a3(2).eq.cn.or.a3(2).eq.cnc).and.
     +     (a3(3).eq.cd.or.a3(3).eq.cdc)) goto 2  ! "end"
c
        if(a3(1).eq.star) goto 3
c
        ind=ind+1  ! counter of particles in /psprop/
c
c
        i1d(ind)=i1   ! store parameters from input table
        i2d(ind)=i2
        i3d(ind)=i3
        i4d(ind)=i4
        i5d(ind)=i5
        r1d(ind)=r1   ! mass (GeV)
        r2d(ind)=r2   ! width
        r3d(ind)=r3   ! dM
        r4d(ind)=r4   ! c*tau (mm)
        a4d(ind)=a4t  ! name of particle ( char*12 )
c
        Imax=ind      ! number of particles in total table
        kedr_part=ind ! number of particles in total table for /psprop/
3     continue
c
c-------------------------------------------------------------------------
c
2     close (lu1)
c
c  Test on the same names of particles
c
      do 20 i=1,Imax-1
        a4t=a4d(i)
        j1=i+1
c
        do 21 j=j1,Imax
          if(a4d(j).ne.a4t) goto 21
          print 23,a4t,i,j
          stop
21      continue
c
20    continue

c---------------------------------------------------------------c
4     format(80a1)
1     format(5a1,12a1,i8,f13.9,f12.6,f8.5,i2,1x,i2,g13.4,1x,i3)
23    format(/,' ppar: Error in table of particles - the same ',
     +         'names ',a12,' index1=',i3,' index2=',i3,/)


       END IF    ! end of reading kedr_part.dat

c
c-------------------------------------------------------------------------
c
c Gets parameters of particle
c
      DO i=1,kedr_part
	ind=i
	if(i1d(i).eq.kf) goto 11
      END DO
c
      print 12,kf
      pname='Unknown'
      pmass=-1
      width=-1
      charge=100
      spin=-1
      ctau=-1
      return
c
11    pname=a4d(ind)
      pmass=r1d(ind)
      width=r2d(ind)
      charge=(1./3.)*i2d(ind)
      spin=(1./2.)*i3d(ind)
      ctau=0.1*r4d(ind)
c
12     format(/,' ******ppar> error: data for HEP-code=',I8,' are absent')

	end

