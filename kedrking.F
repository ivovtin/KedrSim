*CMZ :  1.00/01 29/01/93  10.04.44  by  TANYA PURLATZ
*-- Author :    TANYA PURLATZ   27/01/93
      SUBROUTINE KEDRKING
C
C   Look after secondary tracks (adopted from CLEO simulation)
C
C  If we're in a tracking volume or this is a non-electromagnetic
C  interaction in a calorimetric volume then store the vertex and
C  tracks.  Put everything else on temp stack so they won't appear
C  in KINE bank.
C
C  Exceptions:  Don't store delta-like electrons
C               kill neutrinos
C               keep muon producing verticies.
C
C--------------------------------------------------------------------
C
#include "gctmed.inc"
C
#include "gckine.inc"
C
#include "gcking.inc"
      CHARACTER*4   KCASEC
      EQUIVALENCE   (KCASE,KCASEC)
C
#include "gctrak.inc"
C
#include "gcflag.inc"
C
C Variables
C
      INTEGER I, KPART
C
C Descriptions
C
C    IFLGK  -  A flag for each daughter.  set to
C                   1 to keep the particle
C                   0 to put it on the temporary stack
C                  -1 to Kill it completly
C    IPART  -  The particle type of the parent
C    KPART  -  The particle type of the daughter
C
C
C== Loop on secondaries, decide to keep or put on temp. stack
C
C if secondaries was born inside VD volume or in decay
        IF ( (KCASEC.EQ.'DCAY' .AND. ISTAK.EQ.0) .OR.
     +       ((VECT(1)*VECT(1)+VECT(2)*VECT(2)).LT.12.3**2
     +       .AND.ABS(VECT(3)).LT.67./2.) ) THEN
           NP1=NAPART(1)
           NP2=NAPART(2)
           if (idebug.eq.1) PRINT 110,NP1,NP2,GETOT,KCASE
           CALL VFILL (IFLGK, NGKINE, 1)
         ELSE
	   DO I=1,NGKINE
             IFLGK(I)=0
	   ENDDO
         ENDIF
C
C Handle the exceptions
C
      DO 10 I = 1, NGKINE
         KPART = GKIN(5,I)
         IF ( KPART.EQ.4 ) THEN
*
*  Kill all neutrinos
*
            IFLGK(I) = -1

         ELSEIF ( (KPART.EQ.2 .OR. KPART.EQ.3) .AND.
     1            ISVOL.EQ.1 .AND. GKIN(4,I).LT.0.010 ) THEN
*
*  Temporary stack delta-like low energy electrons
*
            IFLGK(I) = 0
         ELSEIF ( KPART.EQ.5 .OR. KPART.EQ.6 ) THEN
*
*  Always keep muons
*
            IFLGK(I) = 1
         ENDIF
           if ( idebug.eq.1 .AND. IFLGK(I).EQ.1 ) then
           pkin=sqrt(gkin(1,i)**2+gkin(2,i)**2+gkin(3,i)**2)
           PRINT 100,I,KPART,PKIN
           endif
   10 CONTINUE
C
      CALL GSKING(0)
C
      RETURN
110   FORMAT(/' Secondary Particles from ',A4,A4,' ',G12.5,' /',A4,'/:')
100   FORMAT('       ',I2,'. Particle of type ',I2,', P = ',G15.7)
      END
