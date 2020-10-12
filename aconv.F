      SUBROUTINE ACONV(THETA,GAMMA,PHI,THETA1,PHI1,THETA2,PHI2,THETA3,PHI3)
C Converts 'avia' (or 'Shamov') angles THETA,GAMMA,PHI to GEANT angles
C angles are in degrees

      REAL *8 THETAR,GAMMAR,PHIR,CT,ST,CG,SG,CP,SP,VX,VY,VZ,VXY
      REAL *8 DEGRAD,RADDEG,PI,TWOPI

      PI=DASIN(1D0)*2D0
      TWOPI=2D0*PI
      RADDEG=180D0/PI
      DEGRAD=PI/180D0      

      THETAR=THETA*DEGRAD
      GAMMAR=GAMMA*DEGRAD
      PHIR=PHI*DEGRAD

      CT=DCOS(THETAR)
      ST=DSIN(THETAR)
      CG=DCOS(GAMMAR)
      SG=DSIN(GAMMAR)
      CP=DCOS(PHIR)
      SP=DSIN(PHIR)
c axis X'
      VX=CG*CP
      VY=SG*CP*ST+SP*CT
      VZ=-SG*CP*CT+SP*ST
      THETA1=DACOS(VZ)*RADDEG
      PHI1=0D0
      VXY=DSQRT(VX**2+VY**2)
      IF(VXY.GT.0D0) PHI1=DACOS(VX/VXY)
      IF( VY.LT.0D0) PHI1=TWOPI-PHI1
      PHI1=PHI1*RADDEG
c axis Y'
      VX=-CG*SP
      VY=-SG*SP*ST+CP*CT
      VZ=SG*SP*CT+CP*ST
      THETA2=DACOS(VZ)*RADDEG
      PHI2=0D0
      VXY=DSQRT(VX**2+VY**2)
      IF(VXY.GT.0D0) PHI2=DACOS(VX/VXY)
      IF( VY.LT.0D0) PHI2=TWOPI-PHI2
      PHI2=PHI2*RADDEG
c axis Z'
      VX=SG
      VY=-CG*ST
      VZ=CG*CT
C      print '(a,3(x,D22.16))','vx,vy,vz=',vx,vy,vz
      THETA3=DACOS(VZ)*RADDEG
      PHI3=0D0
      VXY=DSQRT(VX**2+VY**2)
      IF(VXY.GT.0D0) PHI3=DACOS(VX/VXY)
      IF( VY.LT.0D0) PHI3=TWOPI-PHI3
      PHI3=PHI3*RADDEG

C      print *,' THETA1=',THETA1,' PHI1=',PHI1,
C	1     ' THETA2=',THETA2,' PHI2=',PHI2,
C	2     ' THETA3=',THETA3,' PHI3=',PHI3
      
      END


      SUBROUTINE SCONV(THETA,GAMMA,PHI,M)
C Rotates matrix M of orts of coordinate system on 'avia' (or 'Shamov') 
C angles THETA,GAMMA,PHI. Angles are in degrees

      REAL *8 THETAR,GAMMAR,PHIR,CT,ST,CG,SG,CP,SP,DEGRAD,PI
      REAL *8 M(3,3),MOUT(3,3),MR(3,3)

      PI=DASIN(1D0)*2D0
      DEGRAD=PI/180D0      

      THETAR=THETA*DEGRAD
      GAMMAR=GAMMA*DEGRAD
      PHIR=PHI*DEGRAD

      CT=DCOS(THETAR)
      ST=DSIN(THETAR)
      CG=DCOS(GAMMAR)
      SG=DSIN(GAMMAR)
      CP=DCOS(PHIR)
      SP=DSIN(PHIR)
      
C ROTATION MATRIX...
      MR(1,1)=CG*CP
      MR(1,2)=-CG*SP
      MR(1,3)=SG
      MR(2,1)=SG*CP*ST+SP*CT
      MR(2,2)=-SG*SP*ST+CP*CT
      MR(2,3)=-CG*ST
      MR(3,1)=-SG*CP*CT+SP*ST
      MR(3,2)=SG*SP*CT+CP*ST
      MR(3,3)=CG*CT

C OUTPUT MATRIX...
      DO I=1,3
      	DO J=1,3
          MOUT(I,J)=0
      	  DO K=1,3
            MOUT(I,J)=MOUT(I,J)+MR(I,K)*M(K,J)      
	  ENDDO
	ENDDO
      ENDDO

C INPUT MATRIX...
      DO I=1,3
      	DO J=1,3
            M(I,J)=MOUT(I,J)
	ENDDO
      ENDDO

C      print *,'sconv>t,g,p, degrad=',theta,gamma,phi,degrad
C      PRINT *,' sconv>Mout='
C      DO I=1,3
C	PRINT *,Mout(I,1),Mout(I,2),Mout(I,3)
C      ENDDO
C      PRINT *,' sconv>Mr='
C      DO I=1,3
C	PRINT *,Mr(I,1),Mr(I,2),Mr(I,3)
C      ENDDO
C      PRINT *,' sconv>M='
C      DO I=1,3
C	PRINT *,M(I,1),M(I,2),M(I,3)
C      ENDDO


      END


      SUBROUTINE VCONV(M,THETA,PHI)
C Converts matrix of orts of rotated system to GEANT angles
C angles are in degrees

      REAL *8 M(3,3)
      REAL *4 THETA(3),PHI(3)
      REAL *8 VX,VY,VZ,VXY,RADDEG,PI,TWOPI

      PI=DASIN(1D0)*2D0
      TWOPI=2D0*PI
      RADDEG=180D0/PI

      DO I=1,3
	VX=M(1,I)
        VY=M(2,I)
        VZ=M(3,I)
        THETA(I)=DACOS(VZ)*RADDEG
        PHI(I)=0D0
        VXY=DSQRT(VX**2+VY**2)
        IF(VXY.GT.0D0) PHI(I)=DACOS(VX/VXY)
        IF(VY.LT.0D0) PHI(I)=TWOPI-PHI(I)
        PHI(I)=PHI(I)*RADDEG
      ENDDO

C      PRINT *,' vconv>M='
C      DO I=1,3
C	PRINT *,M(I,1),M(I,2),M(I,3)
C      ENDDO
C      PRINT *,' vconv>T,P='
C      DO I=1,3
C	PRINT *,Theta(I),Phi(I)
C      ENDDO
      
      END

      SUBROUTINE UNITM(M)
C Set matrix of orts M=1
      REAL *8 M(3,3)

      DO I=1,3
        DO J=1,3
	  M(I,J)=0D0
        ENDDO
      ENDDO
      M(1,1)=1D0
      M(2,2)=1D0
      M(3,3)=1D0

      END
