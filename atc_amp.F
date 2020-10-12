      SUBROUTINE ATC_AMP(IC,IAaer_CH,IAtef_CH,IAsh_CH,IAaer_SC,IAtef_SC,IAsh_SC,I_area)
c -----------------------------------------------------------------
c                                                                 |
c  Determines average number of photo-electrons in counters       |
c                                                                 |
c  Output parameters:                                             |
c                                                                 |
c    IC  - number of aerogel counter (1-160), particle in aerogel |
c                                                                 |
c    AMPL - Amplitude in number of photo-electrons                |
c                                                                 |
c                                                                 |
c   Author: I.V. Ovtin, V.A.Tayursky                              |
c                                                                 |
c   Last change 6.10.2003    V.A.Tayursky                         |
c   Last change 15.12.2015    I.V.Ovtin                           |
c------------------------------------------------------------------
c
#include "gcflag.inc"
#include "gckine.inc"
#include "gctrak.inc"
#include "gcvolu.inc"
#include "gctmed.inc"
#include "KedrSim/atccom.inc"
c
c      print 20,ICALL,IPART,GETOT                                  		е
c20    FORMAT(' ATC_AMP: ICALL=',I4,' IPART=',I2,' GETOT=',1pe12.2)
      IC=0
      DNFOT=0.
      IAaer_CH=0.
      IAsh_CH=0.
      IAtef_CH=0.
      IAaer_SC=0.
      IAsh_SC=0.
      IAtef_SC=0.
      I_area=0.
c
	NAME=NAMES(NLEVEL)    !имя геометрического объема
c 	print *,' NAME = ',NAME,' NLEVEL = ',NLEVEL
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  !считаем сцинтилляции в тефлоне, показатель преломления из 4 модели Белобородова
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	IF ( NLEVEL.EQ.3) THEN
         IC=NUMBER(NLEVEL-1)
      	 NREFTEF1=1.35
      	 NREFTEF2=1.164
      	 IAtef_SC=1000*DESTEP
c
          IF (CHARGE.NE.0 .AND. NLEVEL.EQ.3 ) THEN                              !частица попала в тефлон
           BETTA1=SQRT(1.-(AMASS*AMASS)/(GETOT*GETOT))
           IF (BETTA1.LE.0.) THEN
           IAtef_CH=0.
           DNFOT=0.
           ELSE
           DNFOT=0.2*(1.-1./(BETTA1*BETTA1*NREFTEF1*NREFTEF1))+0.8*(1.-1./(BETTA1*BETTA1*NREFTEF2*NREFTEF2))     !считаем среднее число фотоэлектронов на единицу длины в тефлоне
           IF (DNFOT.LT.0.) DNFOT=0.
  	   IAtef_CH=STEP*CHARGE*CHARGE*DNFOT                           		!среднее число чрернковских фотоэлектронов в тефлоне
c
           ENDIF
          ENDIF
c
        I_area=0.
	ENDIF
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c !считаем сцинтилляции и черенковский сигнал  в аэрогеле
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        ADEL=0
c
cc   !разбитые по аэрогелю длинные счетчики
cc
	DO i=1,68
	 	IF ( NAME.EQ.ARLSI(i) .OR. NAME.EQ.ARLDI(i) ) THEN
         		ADEL=1
                        I_area=i
         	ENDIF
c
	 	IF ( NAME.EQ.ARLSO(i) .OR. NAME.EQ.ARLDO(i) ) THEN
         		ADEL=1
                        I_area=i
         	ENDIF
	END DO
ccc
cc   !разбитые по аэрогелю короткие счетчики
ccc
	DO i=1,58                                                          	!для определения номера счетчика
		IF ( NAME.EQ.ARSSI(i) .OR. NAME.EQ.ARSDI(i) ) THEN
         		ADEL=1
                        I_area=i
         	ENDIF
c
		IF ( NAME.EQ.ARSSO(i) .OR. NAME.EQ.ARSDO(i) ) THEN
         		ADEL=1
                        I_area=i
         	ENDIF
	END DO
ccc
cc   !разбитые по аэрогелю торцевые счетчики
ccc
	DO i=1,28
		IF ( NAME.EQ.ESI(i) .OR. NAME.EQ.EDI(i) .OR. NAME.EQ.ESO(i) .OR. NAME.EQ.EDO(i) ) THEN
         		ADEL=1
                        I_area=i
         	ENDIF
	END DO
c
c Refrectiv coefficient for aerogel
c
	IF ( NLEVEL.EQ.4 .OR. ADEL.EQ.1 ) THEN
           IC=NUMBER(NLEVEL-2)
      	   IAaer_SC=DESTEP                                               !сцинтилляции в аэрогеле
           IF ( ADEL.EQ.1 ) THEN
            IC=NUMBER(NLEVEL-3)
           ENDIF
c
	   DENSFLAG = 0
	   DO K=1,MAXNDENS
	    IF (NCDENS(K).eq.IC) DENSFLAG = 1
	   ENDDO
	   IF (DENSFLAG.eq.0)  THEN
       	    NREF=1.04752                                                 !standart aerogel
	   ELSE
            NREF=1.05576                                                 !dense aerogel
	   ENDIF
        ENDIF
c
c
      IF (CHARGE.NE.0 .AND. NLEVEL.EQ.4 .OR. ADEL.EQ.1) THEN             !частица попала в аэрогель
        BETTA1=SQRT(1.-(AMASS*AMASS)/(GETOT*GETOT))
        IF (BETTA1.LE.0.) THEN
         DNFOT=0.
         ELSE
         DNFOT=(1.-1./(BETTA1*BETTA1*NREF*NREF))                  	 !считаем среднее число фотоэлектронов на единицу длины в аэрогеле
         IF (DNFOT.LT.0.) DNFOT=0.
        ENDIF
  	IAaer_CH=STEP*CHARGE*CHARGE*DNFOT                           	 !среднее число чрернковских фотоэлектронов в аэрогеле
      ENDIF
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c !считаем сцинтилляции и черенковский сигнал в шифтере
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	DO i=1,46                                                        !торцы
		IF ( NAME.EQ.SEI(i) .OR. NAME.EQ.SEI(i) ) THEN
                I_area=i
        	ENDIF
	END DO
c
	DO i=1,66                                                        !внутренний длинный баррельный счетчик
		IF ( NAME.EQ.SBIL(i) ) THEN
                I_area=i
        	ENDIF
	END DO
c
	DO i=1,58                                                        !внутренний короткий баррельный счетчик
		IF ( NAME.EQ.SBIS(i) ) THEN
                I_area=i
        	ENDIF
	END DO
c
	DO i=1,66                                                        !внешний длинный баррельный счетчик
		IF ( NAME.EQ.SBOL(i) ) THEN
                I_area=i
        	ENDIF
	END DO
c
	DO i=1,58                                                        !внешний короткий баррельный счетчик
		IF ( NAME.EQ.SBIS(i) ) THEN
                I_area=i
        	ENDIF
	END DO
c
      IF ( NLEVEL.EQ.6 .OR. (NLEVEL.EQ.5 .AND. ADEL.NE.1)) THEN          !shifter
      	  IF ( NLEVEL.EQ.5) THEN
           IC=NUMBER(NLEVEL-3)
          ELSE
           IC=NUMBER(NLEVEL-4)
 	  ENDIF
c      	  print *,' NAMEsh = ',NAME,' NLEVEL = ',NLEVEL,' IC = ',IC      !имя геометрического объема
       	  NREF=1.49                                                      !показатель преломления шифтера
c
       IF (CHARGE.NE.0 ) THEN
          BETTA1=SQRT(1.-(AMASS*AMASS)/(GETOT*GETOT))
          IF (BETTA1.LE.0.) THEN
            DNFOTsh=0.
          ELSE
            DNFOTsh=(1.-1./(BETTA1*BETTA1*NREF*NREF))                  	 !считаем среднее число фотоэлектронов на единицу длины в шифтере
           IF (DNFOTsh.LT.0.) DNFOTsh=0.
          ENDIF
          IAsh_CH=STEP*CHARGE*CHARGE*DNFOTsh                             !среднее число чрернковских фотоэлектронов в шифтере
       ENDIF
      ENDIF
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c !Results
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 	print *,'   DNFOT = ',DNFOT,'   DNFOTsh = ',DNFOTsh
c
c        print *,'   NLEVEL = ',NLEVEL,' NAME = ',NAME,' I_area = ',I_area
c      	print *,'   NSTEP = ',NSTEP,' DESTEP = ',DESTEP,' STEP = ',STEP        !номер шага, потери энергии на шаге, величина шага
c 	print *,'   VECT(7) - momentum = ',VECT(7),'GeV/c'                     !импульс частицы в ГэВ/с
c
      END
