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
c      print 20,ICALL,IPART,GETOT                                  		�
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
	NAME=NAMES(NLEVEL)    !��� ��������������� ������
c 	print *,' NAME = ',NAME,' NLEVEL = ',NLEVEL
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  !������� ������������ � �������, ���������� ����������� �� 4 ������ ������������
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	IF ( NLEVEL.EQ.3) THEN
         IC=NUMBER(NLEVEL-1)
      	 NREFTEF1=1.35
      	 NREFTEF2=1.164
      	 IAtef_SC=1000*DESTEP
c
          IF (CHARGE.NE.0 .AND. NLEVEL.EQ.3 ) THEN                              !������� ������ � ������
           BETTA1=SQRT(1.-(AMASS*AMASS)/(GETOT*GETOT))
           IF (BETTA1.LE.0.) THEN
           IAtef_CH=0.
           DNFOT=0.
           ELSE
           DNFOT=0.2*(1.-1./(BETTA1*BETTA1*NREFTEF1*NREFTEF1))+0.8*(1.-1./(BETTA1*BETTA1*NREFTEF2*NREFTEF2))     !������� ������� ����� �������������� �� ������� ����� � �������
           IF (DNFOT.LT.0.) DNFOT=0.
  	   IAtef_CH=STEP*CHARGE*CHARGE*DNFOT                           		!������� ����� ������������ �������������� � �������
c
           ENDIF
          ENDIF
c
        I_area=0.
	ENDIF
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c !������� ������������ � ������������ ������  � ��������
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        ADEL=0
c
cc   !�������� �� �������� ������� ��������
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
cc   !�������� �� �������� �������� ��������
ccc
	DO i=1,58                                                          	!��� ����������� ������ ��������
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
cc   !�������� �� �������� �������� ��������
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
      	   IAaer_SC=DESTEP                                               !������������ � ��������
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
      IF (CHARGE.NE.0 .AND. NLEVEL.EQ.4 .OR. ADEL.EQ.1) THEN             !������� ������ � ��������
        BETTA1=SQRT(1.-(AMASS*AMASS)/(GETOT*GETOT))
        IF (BETTA1.LE.0.) THEN
         DNFOT=0.
         ELSE
         DNFOT=(1.-1./(BETTA1*BETTA1*NREF*NREF))                  	 !������� ������� ����� �������������� �� ������� ����� � ��������
         IF (DNFOT.LT.0.) DNFOT=0.
        ENDIF
  	IAaer_CH=STEP*CHARGE*CHARGE*DNFOT                           	 !������� ����� ������������ �������������� � ��������
      ENDIF
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c !������� ������������ � ������������ ������ � �������
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	DO i=1,46                                                        !�����
		IF ( NAME.EQ.SEI(i) .OR. NAME.EQ.SEI(i) ) THEN
                I_area=i
        	ENDIF
	END DO
c
	DO i=1,66                                                        !���������� ������� ���������� �������
		IF ( NAME.EQ.SBIL(i) ) THEN
                I_area=i
        	ENDIF
	END DO
c
	DO i=1,58                                                        !���������� �������� ���������� �������
		IF ( NAME.EQ.SBIS(i) ) THEN
                I_area=i
        	ENDIF
	END DO
c
	DO i=1,66                                                        !������� ������� ���������� �������
		IF ( NAME.EQ.SBOL(i) ) THEN
                I_area=i
        	ENDIF
	END DO
c
	DO i=1,58                                                        !������� �������� ���������� �������
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
c      	  print *,' NAMEsh = ',NAME,' NLEVEL = ',NLEVEL,' IC = ',IC      !��� ��������������� ������
       	  NREF=1.49                                                      !���������� ����������� �������
c
       IF (CHARGE.NE.0 ) THEN
          BETTA1=SQRT(1.-(AMASS*AMASS)/(GETOT*GETOT))
          IF (BETTA1.LE.0.) THEN
            DNFOTsh=0.
          ELSE
            DNFOTsh=(1.-1./(BETTA1*BETTA1*NREF*NREF))                  	 !������� ������� ����� �������������� �� ������� ����� � �������
           IF (DNFOTsh.LT.0.) DNFOTsh=0.
          ENDIF
          IAsh_CH=STEP*CHARGE*CHARGE*DNFOTsh                             !������� ����� ������������ �������������� � �������
       ENDIF
      ENDIF
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c !Results
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 	print *,'   DNFOT = ',DNFOT,'   DNFOTsh = ',DNFOTsh
c
c        print *,'   NLEVEL = ',NLEVEL,' NAME = ',NAME,' I_area = ',I_area
c      	print *,'   NSTEP = ',NSTEP,' DESTEP = ',DESTEP,' STEP = ',STEP        !����� ����, ������ ������� �� ����, �������� ����
c 	print *,'   VECT(7) - momentum = ',VECT(7),'GeV/c'                     !������� ������� � ���/�
c
      END
