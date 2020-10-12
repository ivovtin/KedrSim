      SUBROUTINE LUDAT2M
c**********************************************************************
c
c  Modifies data from /LUDAT2/ of JetSet7.4 according kedr_part.dat
c
c  Author: V.A.Tayursky
c
c  11.03.2005
c
c**********************************************************************
      COMMON /PSPROP/ kedr_part,a4d(600),i1d(600),r1d(600),r2d(600),
     +        r3d(600),i2d(600),i3d(600),r4d(600),i4d(600),i5d(600)
      COMMON /LUDAT2/  KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      CHARACTER *12 A4D
c
      IMOD=0
c
      DO I=1,kedr_part      
        KF=I1D(I)       ! PST
c
        KC=LUCOMP(KF)   ! JetSet
c
        IF(KC.NE.0) THEN
          IF(PMAS(KC,1).NE.R1D(I)) THEN
            PMAS(KC,1)=R1D(I) ! mass
            IMOD=1
          END IF
          IF(PMAS(KC,2).NE.R2D(I)) THEN
            PMAS(KC,2)=R2D(I) ! width
            IMOD=1
          END IF
          IF(PMAS(KC,3).NE.R3D(I)) THEN
            PMAS(KC,3)=R3D(I) ! limit
            IMOD=1
          END IF
        END IF
c
      END DO
c
      IF(IMOD.EQ.1) PRINT 1
1     FORMAT(/,' KEDRSIM modified table of particle properties of',
     &       ' Jetset 7.4')
c
      END
