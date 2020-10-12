      SUBROUTINE KEVROT(THE,PHI,VERTEX)
c*********************************************************************
c
c  Rotates event: momenta from /PKIN/ and vertex of event in simulation  
c  KEVROT is based on LUROBO from jetset 7.4, or book of Kopylov 
c
c  Input: THE, PHI: polar angles theta, phi (rad) of rotated direction 
c                   of a momentum vector initialy along the +z axis.
c         VERTEX(3): vertex of event.
c
c  Output:  momenta of /PKIN/, 
c           VERTEX(3) after rotation  
c
c  Author: V.A.Tayursky, 30 June 2008
c
c*********************************************************************
      Implicit NONE
c
      Real *4 EPART,PXPART,PYPART,PZPART
      Integer *4 NTRACK,IPARTICLE
      COMMON /PKIN/ NTRACK,IPARTICLE(50),EPART(50),   
     &              PXPART(50),PYPART(50),PZPART(50)
      Real *4 THE,PHI,VERTEX(3)
      Real *8 DTHE,DPHI,ROT(3,3),PR(3),VR(3)
      Integer *4 I
c
      SAVE /PKIN/
c
      IF(NTRACK.LT.0.OR.NTRACK.GT.50) THEN
        print *,' '
        print *,' Error, KEVROT: NTRACK=',NTRACK
        print *,' '
        STOP
      END IF
c
      DTHE=THE
      DPHI=PHI
c
      IF(DTHE**2+DPHI**2.LT.1D-20) GOTO 500
c
c  Матрица поворота, jetset 7.4: LUROBO,  или
c  Копылов, "Основы кинематики резонансов", стр.29. 
c
      ROT(1,1)=DCOS(DTHE)*DCOS(DPHI)
      ROT(1,2)=-DSIN(DPHI)
      ROT(1,3)=DSIN(DTHE)*DCOS(DPHI)
      ROT(2,1)=DCOS(DTHE)*DSIN(DPHI)
      ROT(2,2)=DCOS(DPHI)
      ROT(2,3)=DSIN(DTHE)*DSIN(DPHI)
      ROT(3,1)=-DSIN(DTHE)
      ROT(3,2)=0.D0
      ROT(3,3)=DCOS(DTHE)
c
      DO I=1,3
        VR(I)=VERTEX(I)
      END DO
c
c  Rotation of momenta
c
      DO I=1,NTRACK  
        PR(1)=PXPART(I)  ! store initial
        PR(2)=PYPART(I)
        PR(3)=PZPART(I)
c
        PXPART(I)=ROT(1,1)*PR(1)+ROT(1,2)*PR(2)+ROT(1,3)*PR(3)
        PYPART(I)=ROT(2,1)*PR(1)+ROT(2,2)*PR(2)+ROT(2,3)*PR(3)
        PZPART(I)=ROT(3,1)*PR(1)+ROT(3,2)*PR(2)+ROT(3,3)*PR(3)
      END DO
c
c  Common vertex for all particles 
c
      DO I=1,3
        VERTEX(I)=ROT(I,1)*VR(1)+ROT(I,2)*VR(2)+ROT(I,3)*VR(3)
      END DO
c
 500  RETURN
      END
