       SUBROUTINE VACCHAM(MOTHER,DLENG)
********************
* BY YURCHENKO   93-07
* VACUUM CHAMBER
* +25.02.98 V.K.
* +03.09.2014 Malyshev V.M. new design of VC
********************
#include "gconst.inc"
        include 'medpar.inc'
        include 'kedrif.inc'
C
       CHARACTER*4 MOTHER
       DIMENSION GEO(11)

		IF(IFVC.EQ.1) THEN ! old design

      DX=0.5
      DLFL=0.16

C Rotate on 180 deg.
      CALL FREE_ROTMAT_NUMBER(NROTV)
      CALL GSROTM( NROTV, 90., 180., 90., 270., 0., 0.)
      NROTVS=NROTV+1
      CALL GSROTM( NROTVS, 90., 0., 90., 270., 180., 0.)
      NROTVR1=NROTV+2
      CALL GSROTM( NROTVR1, 90., 90., 90., 180., 0., 0.)
      NROTVR2=NROTV+3
      CALL GSROTM( NROTVR2, 90., 270., 90., 0., 0., 0.)

C VACUUM CHAMBER :
      R=6.

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DLENG
      CALL GSVOLU('VCCH','TUBE',MEDAIF,GEO,3,IVOLU)
      CALL GSPOS('VCCH',1,MOTHER,0.,0.,0.,0,'ONLY')

C MIDDLE PART of VACUUM CHAMBER :
      R=6.
      DLI=50.+2.*DLFL

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DLI/2.
      CALL GSVOLU('VCM ','TUBE',MEDAIF,GEO,3,IVOLU)
      CALL GSPOS('VCM ',1,'VCCH',0.,0.,0.,0,'ONLY')

C Part 1 (Berili)

      R=5.7
      D=.1
      DL1=49.0
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL1/2.
      CALL GSVOLU('VMB1','TUBE',MEDBER,GEO,3,IVOLU)
      CALL GSPOS('VMB1',1,'VCM ',0.,0.,0.,0,'ONLY')

C PART 2 (Berili)
      R=5.7
      D=.25
      DL2=0.5
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL2/2.
      RZ=DL1/2.+DL2/2.
      CALL GSVOLU('VMB2','TUBE',MEDBER,GEO,3,IVOLU)
      CALL GSPOS('VMB2',1,'VCM ',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VMB2',2,'VCM ',0.,0.,-RZ,0,'ONLY')

C PART 3 (Steel)
      R=5.7
      D=.25
      DL3=DLFL
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL3/2.
      RZ=DL1/2.+DL2+DL3/2.
      CALL GSVOLU('VMS0','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VMS0',1,'VCM ',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VMS0',2,'VCM ',0.,0.,-RZ,0,'ONLY')

C Part 4 (Steel)

      R=5.95
      D=.011
      DL4=DLI
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL4/2.
      CALL GSVOLU('VMS1','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VMS1',1,'VCM ',0.,0.,0.,0,'ONLY')

C Part 5 (Tine)- olovo

      R=5.96
      D=.001
      GEO(1)=R
      GEO(2)=R+D
c      GEO(3)=DL4/2.
      CALL GSVOLU('VMT1','TUBE',MEDTIN,GEO,3,IVOLU)
      CALL GSPOS('VMT1',1,'VMS1',0.,0.,0.,0,'ONLY')

C Sides PART of VACUUM CHAMBER :

      R=6.
      DLS=(90.-DLFL-DLI)/2.

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DLS/2.
      RZ=DLI/2.+DLS/2.
      CALL GSVOLU('VCS ','TUBE',MEDAIF,GEO,3,IVOLU)
      CALL GSPOS('VCS ',1,'VCCH',0.,0.,RZ,0,'ONLY')
      CALL GSPOS('VCS ',2,'VCCH',0.,0.,-RZ,NROTVS,'ONLY')

C PART 0 (Steel)
      R=5.95
      D=.011
      DL0=(55.-50.)/2.-DLFL
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL0/2.
      RZ=-DLS/2.+DL0/2.
      CALL GSVOLU('VSS0','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSS0',1,'VCS ',0.,0., RZ,0,'ONLY')

C Part 0 (Tine)- olovo

      R=5.96
      D=.001
      GEO(1)=R
      GEO(2)=R+D
c      GEO(3)=DL0/2.
      CALL GSVOLU('VST0','TUBE',MEDTIN,GEO,3,IVOLU)
      CALL GSPOS('VST0',1,'VSS0',0.,0.,0.,0,'ONLY')

C PART 1 (Steel)
      R=5.7
      D=.25
      DL1=(63.-50.)/2.-DLFL
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL1/2.
      RZ=-DLS/2.+DL1/2.
      CALL GSVOLU('VSS1','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSS1',1,'VCS ',0.,0., RZ,0,'ONLY')

C PART 2 (Steel)
      R=5.45
      D=.25
      DL2=1.
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL2/2.
      RZ=-DLS/2.+DL1
      CALL GSVOLU('VSS2','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSS2',1,'VCS ',0.,0., RZ,0,'ONLY')

C PART 3 (Steel)
      R=5.25
      D=.25
      DL3=(87.5-1.5-63.-DL2)/2.
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL3/2.
      RZ=-DLS/2.+DL1+DL2/2.+DL3/2.
      CALL GSVOLU('VSS3','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSS3',1,'VCS ',0.,0., RZ,0,'ONLY')

C PART 4 (Alumin) - parts of VD endcup
      R=5.5
      D=.5
      DL4=2.8   
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL4/2.
      RZ=-DLS/2.+(69.8-DLI)/2.+0.9+0.6
      CALL GSVOLU('VSS4','TUBE',MEDALU,GEO,3,IVOLU)
      CALL GSPOS('VSS4',1,'VCS ',0.,0., RZ,0,'ONLY')

C PART 6 (Steel) - support outer part
      R=5.
      D=.25
      DL6=1.2    
      GEO(1)=R  
      GEO(2)=R+D
      GEO(3)=DL6/2.
      RZS=-DLS/2.+(87.5-DLI)/2.-DL6/2.       
      CALL GSVOLU('VSS6','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSS6',1,'VCS ',0.,0., RZS,0,'ONLY')

      R=5.
      DL7=0.6
      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DL7/2.
      RZ=RZS-DL7/2.
      CALL GSVOLU('VSS7','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSS7',1,'VCS ',0.,0., RZ,0,'MANY')

C Holes in support
      R=0.75
      RH=3.6
      x=RH/2.
      y=RH*SQRT(3.)/2.
      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DL7/2.    
      CALL GSVOLU('VSS8','TUBE',MEDAIF,GEO,3,IVOLU)
      CALL GSPOS('VSS8',1,'VSS7', x, y,0.,0,'ONLY')
      CALL GSPOS('VSS8',2,'VSS7',-x, y,0.,0,'ONLY') 
      CALL GSPOS('VSS8',3,'VSS7',-x,-y,0.,0,'ONLY') 
      CALL GSPOS('VSS8',4,'VSS7', x,-y,0.,0,'ONLY') 

C Outer part

      R=6.
      DLO=DLENG-DLI/2.-DLS

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DLO/2.
      RZ=DLI/2.+DLS+DLO/2.
      CALL GSVOLU('VCO ','TUBE',MEDAIF,GEO,3,IVOLU)
      CALL GSPOS('VCO ',1,'VCCH',0.,0.,RZ,0,'ONLY')
      CALL GSPOS('VCO ',2,'VCCH',0.,0.,-RZ,NROTVS,'ONLY')

C PART 1 (Steel)
      R=5.6
      DL1=DLENG-51.4
      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DL1/2.
      RZ=DLO/2.-DL1/2.
      CALL GSVOLU('VOS1','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VOS1',1,'VCO ',0.,0., RZ,0,'ONLY')

C Put vacuum
      R=5.1
      GEO(1)=0.
      GEO(2)=R  
      GEO(3)=DL1/2.
      CALL GSVOLU('VOV1','TUBE',MEDVMF,GEO,3,IVOLU)
      CALL GSPOS('VOV1',1,'VOS1',0.,0.,0.,0,'ONLY')

C PART 2 (Steel)
      R=5.6
      DL2=0.4
      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DL2/2.
      RZ=DLO/2.-DL1-DL2/2.
      CALL GSVOLU('VOS2','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VOS2',1,'VCO ',0.,0., RZ,0,'ONLY')

C put vacuum
      DR=1.2
      R=3.2

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DL2/2. ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VOV2','TUBS',MEDVMF,GEO,5,IVOLU)
      CALL GSPOS('VOV2',1,'VOS2',-DR/2.+DX,0.,0.,0,'ONLY')
      CALL GSPOS('VOV2',2,'VOS2', DR/2.+DX,0.,0.,NROTV,'ONLY')

      GEO(1)=DR/2. ! X
      GEO(2)=R     ! Y
      CALL GSVOLU('VOV3','BOX ',MEDVMF,GEO,3,IVOLU)
      CALL GSPOS('VOV3',1,'VOS2',DX, 0., 0.,0,'ONLY')

C Part 3 (Steel) FLANEZ
      DR=5.2
      R=2.1
      RZ=-DLO/2.+DLFL/2.

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=0.5*DLFL ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VOS3','TUBS',MEDIRV,GEO,5,IVOLU)
      CALL GSPOS('VOS3',1,'VCO ',-DR/2.+DX,0., RZ,0,'ONLY')
      CALL GSPOS('VOS3',2,'VCO ', DR/2.+DX,0., RZ,NROTV,'ONLY')

      GEO(1)=DR/2. ! X
      CALL GSVOLU('VOS4','BOX ',MEDIRV,GEO,3,IVOLU)

      CALL GSPOS('VOS4',1,'VCO ',DX, 0. , RZ,0,'ONLY')

C put vacuum
      DR=5.2
      R=1.2

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DLFL/2. ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VOV4','TUBS',MEDVMF,GEO,5,IVOLU)
      CALL GSPOS('VOV4',1,'VOS3',0.,0.,0.,0,'ONLY')

      GEO(1)=DR/2. ! X
      CALL GSVOLU('VOV5','BOX ',MEDVMF,GEO,3,IVOLU)
      CALL GSPOS('VOV5',1,'VOS4',0., 0., 0.,0,'ONLY')

C Part 4 (Steel) Rastrub

      DR=5.2
      R1=1.5
      R2=3.5
      DL4=DLO-DL1-DL2-DLFL
      RZ=-DLO/2.+DLFL+DL4/2.

      sc=SQRT(2.)-1.
      dt1=R1*sc
      db1=R1
      dh1=(R1-dt1)/2.
      dt2=R2*sc
      db2=R2
      dh2=(R2-dt2)/2.
      tet=atan2(dh2-dh1,DL4)*RADDEG
      drx=DR/2.+R1-(dh1+dh2)/2.
      dh1s=dh1*2.
      dh2s=dh2*2.

      GEO(1)=DL4/2. ! Z
      GEO(2)=tet
      GEO(3)=270.
      GEO(4)=dh1
      GEO(5)=db1
      GEO(6)=dt1
      GEO(7)=0.
      GEO(8)=dh2
      GEO(9)=db2
      GEO(10)=dt2
      GEO(11)=0.
      CALL GSVOLU('VOS5','TRAP',MEDIRV,GEO,11,IVOLU)
      CALL GSPOS('VOS5',1,'VCO ',-drx+DX,0., RZ,NROTVR1,'ONLY')
      CALL GSPOS('VOS5',2,'VCO ', drx+DX,0., RZ,NROTVR2,'ONLY')

      GEO(1)=DR/2.+dt1
      GEO(2)=DR/2.+R1-2.*dh2
      GEO(3)=db1
      GEO(4)=db2
      GEO(5)=DL4/2.
      CALL GSVOLU('VOS6','TRD2',MEDIRV,GEO,5,IVOLU)
      CALL GSPOS('VOS6',1,'VCO ',DX, 0. , RZ,0,'ONLY')

C put Vacuum

      DR=5.2
      R1=1.2
      R2=3.2
      D=0.3

      dt1=R1*sc
      db1=R1
      dh1=(R1-dt1)/2.
      dt2=R2*sc
      db2=R2
      dh2=(R2-dt2)/2.

C divide on 2 parts: one inside TRAP, other - in TRD2

C 1 part
      dh11=(dh1s-D)/2.
      dh21=(dh2s-D)/2.
      db11=dt1+(db1-dt1)*dh11/dh1
      db21=dt2+(db2-dt2)*dh21/dh2

      tet=atan2(dh21-dh11,DL4)*RADDEG
      dry=(dh11+dh21)/2.-(dh1s+dh2s)/4.

      GEO(1)=DL4/2. ! Z
      GEO(2)=tet
      GEO(3)=270.
      GEO(4)=dh11
      GEO(5)=db11
      GEO(6)=dt1
      GEO(7)=0.
      GEO(8)=dh21
      GEO(9)=db21
      GEO(10)=dt2
      GEO(11)=0.
      CALL GSVOLU('VOV6','TRAP',MEDVMF,GEO,11,IVOLU)
      CALL GSPOS('VOV6',1,'VOS5', 0., dry, 0., 0, 'ONLY')

c 2 part
      dh12=dh1-dh11
      dh22=dh2-dh21
      dt12=db11
      dt22=db21

      tet=atan2((dh21*2.+dh22)-(dh11*2.+dh12),DL4)*RADDEG
      drx=(DR+R1+dt1+dh12+dh22)/2.-dh2

      GEO(1)=DL4/2. ! Z
      GEO(2)=tet
      GEO(3)=270.
      GEO(4)=dh12
      GEO(5)=db1
      GEO(6)=dt12
      GEO(7)=0.
      GEO(8)=dh22
      GEO(9)=db2
      GEO(10)=dt22
      GEO(11)=0.
      CALL GSVOLU('VOV7','TRAP',MEDVMF,GEO,11,IVOLU)
      CALL GSPOS('VOV7',1,'VOS6',-drx, 0., 0., NROTVR1,'ONLY')
      CALL GSPOS('VOV7',2,'VOS6', drx, 0., 0., NROTVR2,'ONLY')

C TRD2 Vac part
      GEO(1)=DR/2.+dt1
      GEO(2)=DR/2.+R1-dh2*2.
      GEO(3)=db1
      GEO(4)=db2
      GEO(5)=DL4/2.
      CALL GSVOLU('VOV8','TRD2',MEDVMF,GEO,5,IVOLU)
      CALL GSPOS('VOV8',1,'VOS6', 0., 0., 0., 0,'ONLY')

C-----------------------------------------------------------
C Inner middle part

      DR=5.6
      R=2.11

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DLI/2.  ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VMI1','TUBS',MEDAIF,GEO,5,IVOLU)
      CALL GSPOS('VMI1',1,'VCM ',-DR/2.+DX,0.,0.,0,'ONLY')
      CALL GSPOS('VMI1',2,'VCM ', DR/2.+DX,0.,0.,NROTV,'ONLY')
      GEO(1)=DR/2. ! X
      CALL GSVOLU('VMI2','BOX ',MEDAIF,GEO,3,IVOLU)
      CALL GSPOS('VMI2',1,'VCM ',DX,0.,0.,0,'ONLY')


C Put vacuum

      DR=5.6
      R=1.2

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DLI/2.  ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VIV1','TUBS',MEDVMF,GEO,5,IVOLU)
      CALL GSPOS('VIV1',1,'VMI1',0.,0.,0.,0,'ONLY')
      GEO(1)=DR/2. ! X
      CALL GSVOLU('VIV2','BOX ',MEDVMF,GEO,3,IVOLU)
      CALL GSPOS('VIV2',1,'VMI2',0.,0.,0.,0,'ONLY')

C Part 1 (Berili)

      DR=5.6
      R=1.2
      D=0.175
      DL1=25.

      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=0.5*DL1  ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VIB1','TUBS',MEDBER,GEO,5,IVOLU)

      CALL GSPOS('VIB1',1,'VMI1',0.,0.,0.,0,'ONLY')

      GEO(1)=DR/2. ! X
      GEO(2)=D/2.  ! Y
      CALL GSVOLU('VIB2','BOX ',MEDBER,GEO,3,IVOLU)

      CALL GSPOS('VIB2',1,'VMI2',0.,  R+D/2. ,0.,0,'ONLY')
      CALL GSPOS('VIB2',2,'VMI2',0.,-(R+D/2.),0.,0,'ONLY')

C Part 2 (berili)

      DR=5.6
      R=1.2
      D=0.3
      DL2=0.8
      RZ=DL1/2.+DL2/2.

      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=0.5*DL2  ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VIB3','TUBS',MEDBER,GEO,5,IVOLU)

      CALL GSPOS('VIB3',1,'VMI1',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VIB3',2,'VMI1',0.,0.,-RZ,0,'ONLY')

      GEO(1)=DR/2. ! X
      GEO(2)=D/2.  ! Y
      CALL GSVOLU('VIB4','BOX ',MEDBER,GEO,3,IVOLU)

      CALL GSPOS('VIB4',1,'VMI2',0.,  R+D/2. , RZ,0,'ONLY')
      CALL GSPOS('VIB4',2,'VMI2',0.,-(R+D/2.), RZ,0,'ONLY')
      CALL GSPOS('VIB4',3,'VMI2',0.,  R+D/2. ,-RZ,0,'ONLY')
      CALL GSPOS('VIB4',4,'VMI2',0.,-(R+D/2.),-RZ,0,'ONLY')

C part 3 (Steel)
      DR=5.6
      R=1.2
      D=0.3
      DL3=12.5-DL2
      RZ=DL1/2.+DL2+DL3/2.

      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=0.5*DL3 ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VIS1','TUBS',MEDIRV,GEO,5,IVOLU)
      CALL GSPOS('VIS1',1,'VMI1',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VIS1',2,'VMI1',0.,0.,-RZ,0,'ONLY')

      GEO(1)=DR/2. ! X
      GEO(2)=D/2     ! Y
      CALL GSVOLU('VIS2','BOX ',MEDIRV,GEO,3,IVOLU)

      CALL GSPOS('VIS2',1,'VMI2',0.,  R+D/2. , RZ,0,'ONLY')
      CALL GSPOS('VIS2',2,'VMI2',0.,-(R+D/2.), RZ,0,'ONLY')
      CALL GSPOS('VIS2',3,'VMI2',0.,  R+D/2. ,-RZ,0,'ONLY')
      CALL GSPOS('VIS2',4,'VMI2',0.,-(R+D/2.),-RZ,0,'ONLY')

C part 4
C FLANEZ
      DR=5.6
      R=1.2
      D=0.9
      RZ=DL1/2.+DL2+DL3+DLFL/2.

      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=0.5*DLFL ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VIS3','TUBS',MEDIRV,GEO,5,IVOLU)
      CALL GSPOS('VIS3',1,'VMI1',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VIS3',2,'VMI1',0.,0.,-RZ,0,'ONLY')

      GEO(1)=DR/2. ! X
      GEO(2)=D/2.  ! Y
      CALL GSVOLU('VIS4','BOX ',MEDIRV,GEO,3,IVOLU)

      CALL GSPOS('VIS4',1,'VMI2',0.,  R+D/2. , RZ,0,'ONLY')
      CALL GSPOS('VIS4',2,'VMI2',0.,-(R+D/2.), RZ,0,'ONLY')
      CALL GSPOS('VIS4',3,'VMI2',0.,  R+D/2. ,-RZ,0,'ONLY')
      CALL GSPOS('VIS4',4,'VMI2',0.,-(R+D/2.),-RZ,0,'ONLY')

C Side inner parts

      DR=5.2
      R=1.75

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DLS/2.  ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VSI1','TUBS',MEDAIF,GEO,5,IVOLU)
      CALL GSPOS('VSI1',1,'VCS ',-DR/2.+DX,0.,0.,0,'ONLY')
      CALL GSPOS('VSI1',2,'VCS ', DR/2.+DX,0.,0.,NROTV,'ONLY')
      GEO(1)=DR/2. ! X
      CALL GSVOLU('VSI2','BOX ',MEDAIF,GEO,3,IVOLU)
      CALL GSPOS('VSI2',1,'VCS ',DX,0.,0.,0,'ONLY')

C Put vacuum

      DR=5.2
      R=1.2

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DLS/2.  ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VSV1','TUBS',MEDVMF,GEO,5,IVOLU)
      CALL GSPOS('VSV1',1,'VSI1',0.,0.,0.,0,'ONLY')
      GEO(1)=DR/2. ! X
      CALL GSVOLU('VSV2','BOX ',MEDVMF,GEO,3,IVOLU)
      CALL GSPOS('VSV2',1,'VSI2',0.,0.,0.,0,'ONLY')

C part 1 (Steel)
      DR=5.2
      R=1.2
      D=0.3

      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DLS/2. ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VSR1','TUBS',MEDIRV,GEO,5,IVOLU)
      CALL GSPOS('VSR1',1,'VSI1',0.,0.,0.,0,'ONLY')

      GEO(1)=DR/2. ! X
      GEO(2)=D/2   ! Y
      CALL GSVOLU('VSR2','BOX ',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSR2',1,'VSI2',0.,  R+D/2. ,0.,0,'ONLY')
      CALL GSPOS('VSR2',2,'VSI2',0.,-(R+D/2.),0.,0,'ONLY')

C PART 2 (Steel) - part of support outer part
      DR=5.2
      R=1.5
      D=0.25
      DL2=1.2

      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL2/2. ! Z
      GEO(4)=90.
      GEO(5)=270.
      CALL GSVOLU('VSR3','TUBS',MEDIRV,GEO,5,IVOLU)
      CALL GSPOS('VSR3',1,'VSI1',0.,0.,RZS,0,'ONLY')

      GEO(1)=DR/2. ! X
      GEO(2)=D/2   ! Y
      CALL GSVOLU('VSR4','BOX ',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSR4',1,'VSI2',0.,  R+D/2. ,RZS,0,'ONLY')
      CALL GSPOS('VSR4',2,'VSI2',0.,-(R+D/2.),RZS,0,'ONLY')

		ENDIF
                
                
C----------------------------------------

		IF(IFVC.EQ.2) THEN ! new design (2014)

      DX=0.5
      DLFL=0.16

C VACUUM CHAMBER :
      R=6.

      GEO(1)=0.
      GEO(2)=R
      GEO(3)=DLENG
      CALL GSVOLU('VCCH','TUBE',MEDAIF,GEO,3,IVOLU)
      CALL GSPOS('VCCH',1,MOTHER,0.,0.,0.,0,'ONLY')

C MIDDLE PART of VACUUM CHAMBER :

      DLI=50.+2.*DLFL

C Part 1 (Berili)

      R=5.7
      D=.1
      DL1=49.0
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL1/2.
      CALL GSVOLU('VMB1','TUBE',MEDBER,GEO,3,IVOLU)
      CALL GSPOS('VMB1',1,'VCCH ',0.,0.,0.,0,'ONLY')

C PART 2 (Berili)
      R=5.7
      D=.25
      DL2=0.5
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL2/2.
      RZ=DL1/2.+DL2/2.
      CALL GSVOLU('VMB2','TUBE',MEDBER,GEO,3,IVOLU)
      CALL GSPOS('VMB2',1,'VCCH ',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VMB2',2,'VCCH ',0.,0.,-RZ,0,'ONLY')

C PART 3 (Steel)
      R=5.7
      D=.25
      DL3=DLFL
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL3/2.
      RZ=DL1/2.+DL2+DL3/2.
      CALL GSVOLU('VMS0','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VMS0',1,'VCCH ',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VMS0',2,'VCCH ',0.,0.,-RZ,0,'ONLY')

C Part 4 (Steel)

      R=5.95
      D=.011
      DL4=DLI
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL4/2.
      CALL GSVOLU('VMS1','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VMS1',1,'VCCH ',0.,0.,0.,0,'ONLY')

C Part 5 (Tine)- olovo

      R=5.96
      D=.001
      GEO(1)=R
      GEO(2)=R+D
c      GEO(3)=DL4/2.
      CALL GSVOLU('VMT1','TUBE',MEDTIN,GEO,3,IVOLU)
      CALL GSPOS('VMT1',1,'VMS1',0.,0.,0.,0,'ONLY')

C Sides PART of VACUUM CHAMBER :

      DLS=(90.-DLFL-DLI)/2.

C PART 0 (Steel)
      ZVCS=DLI/2.+DLS/2.	! center of VCS
      R=5.95
      D=.011
      DL0=(55.-50.)/2.-DLFL
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL0/2.
      RZ=-DLS/2.+DL0/2.+ZVCS
      CALL GSVOLU('VSS0','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSS0',1,'VCCH ',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VSS0',2,'VCCH ',0.,0.,-RZ,0,'ONLY')

C Part 0 (Tine)- olovo

      R=5.96
      D=.001
      GEO(1)=R
      GEO(2)=R+D
c      GEO(3)=DL0/2.
      CALL GSVOLU('VST0','TUBE',MEDTIN,GEO,3,IVOLU)
      CALL GSPOS('VST0',1,'VSS0',0.,0.,0.,0,'ONLY')

C PART 1 (Steel)
      R=5.7
      D=.25
      DL1=(63.-50.)/2.-DLFL
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL1/2.
      RZ=-DLS/2.+DL1/2.+ZVCS
      CALL GSVOLU('VSS1','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSS1',1,'VCCH ',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VSS1',2,'VCCH ',0.,0.,-RZ,0,'ONLY')

C PART 2 (Steel)
      R=5.45
      D=.25
      DL2=1.
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL2/2.
      RZ=-DLS/2.+DL1+ZVCS
      CALL GSVOLU('VSS2','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSS2',1,'VCCH ',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VSS2',2,'VCCH ',0.,0.,-RZ,0,'ONLY')

C PART 3 (Steel)
      R=5.25
      D=.25
      DL3=(87.5-1.5-63.-DL2)/2.
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL3/2.
      RZ=-DLS/2.+DL1+DL2/2.+DL3/2.+ZVCS
      CALL GSVOLU('VSS3','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSS3',1,'VCCH ',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VSS3',2,'VCCH ',0.,0.,-RZ,0,'ONLY')

C PART 4 (Alumin) - parts of VD endcup
      R=5.5
      D=.5
      DL4=2.8   
      GEO(1)=R
      GEO(2)=R+D
      GEO(3)=DL4/2.
      RZ=-DLS/2.+(69.8-DLI)/2.+0.9+0.6+ZVCS
      CALL GSVOLU('VSS4','TUBE',MEDALU,GEO,3,IVOLU)
      CALL GSPOS('VSS4',1,'VCCH ',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VSS4',2,'VCCH ',0.,0.,-RZ,0,'ONLY')

C PART 6 (Steel) - support outer part
      R=5.
      D=.25
      DL6=1.2    
      GEO(1)=R  
      GEO(2)=R+D
      GEO(3)=DL6/2.
      RZS=-DLS/2.+(87.5-DLI)/2.-DL6/2.+ZVCS       
      CALL GSVOLU('VSS6','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VSS6',1,'VCCH ',0.,0., RZS,0,'ONLY')
      CALL GSPOS('VSS6',2,'VCCH ',0.,0.,-RZS,0,'ONLY')

C NEW (2014) inner part...

C PART 1 (Aluminium) - chamber
      dZ1=40.
      GEO(1)=8.6/2
      GEO(2)=8.8/2
      GEO(3)=dZ1/2
      CALL GSVOLU('VIA1','TUBE',MEDALU,GEO,3,IVOLU)
      CALL GSPOS('VIA1',1,'VCCH ',0.,0.,0., 0,'ONLY')

C PART 2 (Aluminium) - chamber
      GEO(1)=8.6/2
      GEO(2)=9./2
      dZ2=2.75
      GEO(3)=dZ2/2
      Zc=dZ1/2+dZ2/2
      z21=Zc-dZ2/2
      z22=Zc+dZ2/2
c      print *,'z2: ',z21,z22 
      CALL GSVOLU('VIA2','TUBE',MEDALU,GEO,3,IVOLU)
      CALL GSPOS('VIA2',1,'VCCH ',0.,0.,Zc, 0,'ONLY')
      CALL GSPOS('VIA2',2,'VCCH ',0.,0.,-Zc, 0,'ONLY')

C PART 3 (Aluminium) - flanetz promegut.
      GEO(1)=8.6/2
      GEO(2)=9.6/2
      dZ3=0.7-0.15
      GEO(3)=dZ3/2
      Zc=z22+dZ3/2
      z31=Zc-dZ3/2
      z32=Zc+dZ3/2
c      print *,'z3: ',z31,z32 
      CALL GSVOLU('VIA3','TUBE',MEDALU,GEO,3,IVOLU)
      CALL GSPOS('VIA3',1,'VCCH ',0.,0.,Zc, 0,'ONLY')
      CALL GSPOS('VIA3',2,'VCCH ',0.,0.,-Zc, 0,'ONLY')

C PART 4 (Steel) - flanetz promegut.
      GEO(1)=8.6/2
      GEO(2)=9.6/2
      dZ4=0.7-0.15
      GEO(3)=dZ4/2
      Zc=z32+dZ4/2
      z41=Zc-dZ4/2
      z42=Zc+dZ4/2
c      print *,'z4: ',z41,z42 
      CALL GSVOLU('VIS4','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VIS4',1,'VCCH ',0.,0.,Zc, 0,'ONLY')
      CALL GSPOS('VIS4',2,'VCCH ',0.,0.,-Zc, 0,'ONLY')

C PART 5 (Steel) - patrubok perekh. + flanetz
      GEO(1)=8.6/2
      GEO(2)=9.0/2
      dZ5=16.3+0.4
      GEO(3)=dZ5/2
      Zc=z42+dZ5/2
      z51=Zc-dZ5/2
      z52=Zc+dZ5/2
c      print *,'z5: ',z51,z52 
      CALL GSVOLU('VIS5','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VIS5',1,'VCCH ',0.,0.,Zc, 0,'ONLY')
      CALL GSPOS('VIS5',2,'VCCH ',0.,0.,-Zc, 0,'ONLY')

C PART 6 (Steel) - vystup dlya svarki s patrub.
      GEO(1)=8.6/2
      GEO(2)=10./2
      dZ6=0.25
      GEO(3)=dZ6/2
      Zc=z52+dZ6/2
      z61=Zc-dZ6/2
      z62=Zc+dZ6/2
c      print *,'z6: ',z61,z62 
      CALL GSVOLU('VIS6','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VIS6',1,'VCCH ',0.,0.,Zc, 0,'ONLY')
      CALL GSPOS('VIS6',2,'VCCH ',0.,0.,-Zc, 0,'ONLY')

C PART 7 (Steel) - perkhodnik+perekhodnik
      GEO(1)=8.8/2
      GEO(2)=9.4/2
      dZ7=7.5-0.5+4.0-0.1
      GEO(3)=dZ7/2
      Zc=z62+dZ7/2
      z71=Zc-dZ7/2
      z72=Zc+dZ7/2
c      print *,'z7: ',z71,z72 
      CALL GSVOLU('VIS7','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VIS7',1,'VCCH ',0.,0.,Zc, 0,'ONLY')
      CALL GSPOS('VIS7',2,'VCCH ',0.,0.,-Zc, 0,'ONLY')

C PART 8 (Steel) - vystup dlya svarki s vneshney truboi
      GEO(1)=9.4/2
      GEO(2)=10./2	! styk s vneshney chastyu
      dZ8=3.05-2.2
      GEO(3)=dZ8/2
      Zc=z62+1.9-0.15+dZ8/2
      z81=Zc-dZ8/2
      z82=Zc+dZ8/2
c      print *,'z8: ',z81,z82 
      CALL GSVOLU('VIS8','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VIS8',1,'VCCH ',0.,0.,Zc, 0,'ONLY')
      CALL GSPOS('VIS8',2,'VCCH ',0.,0.,-Zc, 0,'ONLY')

C PART 9 (Steel) - vystup dlya svarki perekhodnikov
      GEO(1)=9.4/2
      GEO(2)=10.6/2	
      dZ9=0.2
      GEO(3)=dZ9/2
      Zc=z72-4.
      z91=Zc-dZ9/2
      z92=Zc+dZ9/2
c      print *,'z9: ',z91,z92 
      CALL GSVOLU('VIS9','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VIS9',1,'VCCH ',0.,0.,Zc, 0,'ONLY')
      CALL GSPOS('VIS9',2,'VCCH ',0.,0.,-Zc, 0,'ONLY')

C PART 10 (Steel) - vystup dlya soed. s sil'phonom
      GEO(1)=9.4/2
      GEO(2)=10.2/2	
      dZ10=0.7
      GEO(3)=dZ10/2
      Zc=z72-dZ10/2
      z101=Zc-dZ10/2
      z102=Zc+dZ10/2
c      print *,'z10: ',z101,z102 
      CALL GSVOLU('VIS10','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VIS10',1,'VCCH ',0.,0.,Zc, 0,'ONLY')
      CALL GSPOS('VIS10',2,'VCCH ',0.,0.,-Zc, 0,'ONLY')


C PART 11 (Vacuum)
      GEO(1)=0
      GEO(2)=8.6/2
      dZ11=z62*2
      GEO(3)=dZ11/2
      Zc=0
      z111=Zc-dZ11/2
      z112=Zc+dZ11/2
c      print *,'z11: ',z111,z112 
      CALL GSVOLU('VIV1','TUBE',MEDVMF,GEO,3,IVOLU)
      CALL GSPOS('VIV1',1,'VCCH ',0.,0.,Zc, 0,'ONLY')


C PART 12 (Vacuum)
      GEO(1)=0
      GEO(2)=8.8/2
      dZ12=dZ7
      GEO(3)=dZ12/2
      Zc=z62+dZ12/2
      z121=Zc-dZ12/2
      z122=Zc+dZ12/2
c      print *,'z12: ',z121,z122 
      CALL GSVOLU('VIV2','TUBE',MEDVMF,GEO,3,IVOLU)
      CALL GSPOS('VIV2',1,'VCCH ',0.,0.,Zc, 0,'ONLY')
      CALL GSPOS('VIV2',2,'VCCH ',0.,0.,-Zc, 0,'ONLY')


C Outer part

c      R=6.
      DLO=DLENG-DLI/2.-DLS

C PART 1 (Steel)
      ZVCO=DLI/2.+DLS+DLO/2.	! center of VCO
      R=5.6
      DL1=DLENG-51.4
      GEO(1)=5.1
      GEO(2)=R
      GEO(3)=DL1/2.
      RZ=DLO/2.-DL1/2.+ZVCO
      Zc=RZ
      zVOS11=Zc-dL1/2
      zVOS12=Zc+dL1/2
c      print *,'zVOS1: ',zVOS11,zVOS12 
      CALL GSVOLU('VOS1','TUBE',MEDIRV,GEO,3,IVOLU)
      CALL GSPOS('VOS1',1,'VCCH ',0.,0., RZ,0,'ONLY')
      CALL GSPOS('VOS1',2,'VCCH ',0.,0.,-RZ,0,'ONLY')

C Put vacuum
      R=5.1
      GEO(1)=0.
      GEO(2)=R
      dzVOV1=DLENG-z72  
      GEO(3)=dzVOV1/2
      Zc=z72+dzVOV1/2
      zVOV11=Zc-dzVOV1/2
      zVOV12=Zc+dzVOV1/2
c      print *,'zVOV1: ',zVOV11,zVOV12 
      CALL GSVOLU('VOV1','TUBE',MEDVMF,GEO,3,IVOLU)
      CALL GSPOS('VOV1',1,'VCCH',0.,0.,Zc,0,'ONLY')
      CALL GSPOS('VOV1',2,'VCCH',0.,0.,-Zc,0,'ONLY')

		ENDIF

      RETURN
      END
