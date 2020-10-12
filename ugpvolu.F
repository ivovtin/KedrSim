*
* $Id: ugpvolu.F,v 1.1 2007/12/19 04:25:48 malysh Exp $
*
* $Log: ugpvolu.F,v $
* Revision 1.1  2007/12/19 04:25:48  malysh
* Исправил ошибку в задании параметров геометрии электродной системы в LKr,
* улучшена (Масленников А) печать объемов при отладке.
*
* Revision 1.1.1.1  1995/10/24 10:20:54  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/04 13/12/94  15.26.17  by  S.Giani
*-- Author :
      SUBROUTINE UGPVOLU(NUMB)
*...  13/12/07  A. Maslennikov:
c.  1) Changed format of shape parameters from E10.3 to F10.4 (cm)
c   2) Added printing the Mother volume, position inside mother, rotation matrix etc
*...
C.
C.    ******************************************************************
C.    *                                                                *
C.    *     GPVOLU = Routine to print VOLUMES data structures JVOLUM   *
C.    *       NUMB     Volume number                                   *
C.    *                                                                *
C.    *    ==>Called by : <USER>, GPRINT                               *
C.    *         Author  R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcunit.inc"
#include "geant321/gcnum.inc"
      COMMON/FMOTH/INGLOB,IVOMGL
      CHARACTER*4 ISHAP(30),NAME,MOTHER
      CHARACTER*1 CHONLY(2)
      CHARACTER*20 CHMED
      DIMENSION PAR(50),ATT(20),UBUF(20)
      SAVE ISHAP, CHONLY
      DATA ISHAP/'BOX ','TRD1','TRD2','TRAP','TUBE','TUBS','CONE',
     +           'CONS','SPHE','PARA','PGON','PCON','ELTU','HYPE',
     +13*'    ', 'GTRA','CTUB','    '/
      DATA CHONLY/ 'M', 'O' /  !  'MANY', 'ONLY'

C.
C.    ------------------------------------------------------------------
      IF (JVOLUM.LE.0) GO TO 99
      IF (NUMB  .GT.0) GO TO 10
C
      WRITE (CHMAIL,1000)
      CALL GMAIL(0,0)
      WRITE (CHMAIL,1001)
      CALL GMAIL(0,1)  !  2nd param = N of blanc lines below
C
      N1     = 1
      N2     = NVOLUM
      GO TO 20
C
   10 WRITE (CHMAIL,1001)
      CALL GMAIL(0,1)
C
      N1     = NUMB
      N2     = NUMB
C
   20 DO 30 I=N1,N2
      JVO = LQ(JVOLUM-I)  !  pointer to current volume
      NIN=Q(JVO+3)  !  N of volumes contained inside current
      IF (JVO.LE.0)   GO TO 30
C
      CALL UHTOC(IQ(JVOLUM+I),4,NAME,4)
      CALL GFMOTH(NAME,MOTHER,KONLY)  !  KONLY=0(1) if MANY(ONLY)
      JVOMOT=LQ(JVOLUM-IVOMGL)  !  pointer to MOTHER volume
      MNIN=Q(JVOMOT+3)  !  N of volumes contained inside MOTHER
      IF(MNIN.LE.0) THEN  !  current volume was obtained by division of mother
        NMBR=1
        IROT=0
        XC=0.
        YC=0.
        ZC=0.
      ELSE
        JIN = LQ(JVOMOT-INGLOB)  !  INGLOB=index among descendants of mother
        NMBR=Q(JIN+3)  !  user copy number
        IROT=Q(JIN+4)  !  rotation matrix
        XC=Q(JIN+5)    !  position inside mother
        YC=Q(JIN+6)
        ZC=Q(JIN+7)
      ENDIF
 
      IS   = Q(JVO+2)  !  shape index of current
      NMED = Q(JVO+4)
      call gftmed(NMED, CHMED, NMAT, ISVOL, IFIELD, FIELDM, TMAXFD,
     $   STEMAX, DEEMAX, EPSIL, STMIN, UBUF, NWBUF)
C
      CALL GFPARA(NAME,NMBR,1,NPAR,NATT,PAR,ATT)
      NPAR_IN_ROW_1=3   !  def =10
      NPAR_IN_ROW_2=11  !  def =10
      IEND10=NPAR_IN_ROW_1
      IF(NPAR.LT.IEND10)IEND10=NPAR
c      WRITE(CHMAIL,1002)I,NAME,NMED,ISHAP(IS),NPAR,(PAR(J),J=1,IEND10)
      if(i.eq.1) MOTHER='XXXX'
      WRITE(CHMAIL,1002) I,NAME,INGLOB,CHONLY(KONLY+1),IROT,MOTHER,XC,YC
     $   ,ZC,NMED,CHMED,ISHAP(IS),NPAR,(PAR(J),J=1,IEND10)
      CALL GMAIL(0,0)
c      DO 25 I10=11,NPAR,10
      DO 25 I10=NPAR_IN_ROW_1+1,NPAR,NPAR_IN_ROW_2  !  
        IEND10=I10+NPAR_IN_ROW_2-1
        IF (NPAR.LT.IEND10) IEND10=NPAR
c        WRITE (CHMAIL,1003) (PAR(J),J = I10,IEND10)
        WRITE (CHMAIL,1003) I10,IEND10,(PAR(J),J = I10,IEND10)
        CALL GMAIL(0,0)
   25 CONTINUE
c      if(NAME .eq. 'ALUM') then
        if(IROT .ne. 0) call ugprotm(IROT)
c        if(IROT .ne. 0) call ugprotm(-IROT)  !  to skip header
c      endif

   30 CONTINUE
C
 1000 FORMAT ('0',51('='),5X,' VOLUMES ',6X,50('='))
c 1001 FORMAT ('0','VOLUME NAME  NUMED SHAPE NPAR  PARAMETERS')
 1001 FORMAT ('IVOL NAME Icop RotM MOTH     Xc        Yc        Zc  ',
     $ 'NUMED   CHMED',10X,' SHAPE NPAR     PARAMETERS')
c 1002 FORMAT (' ',I6,1X,A4,2X,I3,3X,A4,I5,2X,10E10.3)
 1002 FORMAT (I3,1X,A4,1X,I3,1X,A1,I4,1X,A4,3F10.4,I3,'=',A20,1X,A4,I3,10F10.4)
c 1003 FORMAT (31X,10E10.3)
c- 1003 FORMAT (31X,10(1PE10.3))
 1003 FORMAT (3X,'par(',I2,':',I2,')= ',11F10.4)
  99  END
