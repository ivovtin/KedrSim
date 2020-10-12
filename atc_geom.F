      SUBROUTINE ATC_GEOM
c------------------------------------------------------------------
c                                                                 |
c   Cherenkov counters geometry subroutine.                       |
c                                                                 |
c   Last change 8.11.2013 I.V.Ovtin                               |
c                                                                 |
c------------------------------------------------------------------
      include 'medpar.inc'
      include 'atccom.inc'
c
      INTEGER IRM
      REAL PAR1(3),PAR2(4),PAR3(5),PAR4(11),XP,YP
      COMMON /ATCGEOMEC/ ECACW1,ECACW2,ECACH,ECACL
      COMMON /ATCGEOMBIC/ BIR1,BIR2,BIDF,BILL,BILS
c
c     EndCap Aerogel Counter building inner layer
c     'ECSI' - 'standart' aerogel n=1.05, inner layer of ATC
c     'ECDI' - 'dense' aerogel n=1.054, inner layer of ATC
c
c  counter frame dimentions
c
c     narrow side (x1)
      PAR2(1) = 5.13*0.5
c     wide side   (x2)
      PAR2(2) = 21.52*0.5
c     height (y)
      PAR2(3) = 7.4*0.5
c     length (z)
      PAR2(4) = 51.76*0.5
c
      CALL GSVOLU('ECSI', 'TRD1' , MEDALF, PAR2 , 4 , IVOL )
      CALL GSVOLU('ECDI', 'TRD1' , MEDALF, PAR2 , 4 , IVOL )
c    Teflon reflecting film
c     narrow side (x1)
      PAR2(1) = PAR2(1)-0.1171
c     wide side   (x2)
      PAR2(2) = PAR2(2)-0.1171
c     height (y)
      PAR2(3) = PAR2(3)-0.1
c     length (z)
      PAR2(4) = PAR2(4)-0.1
      CALL GSVOLU('TESI', 'TRD1' , MEDTEF, PAR2 , 4 , IVOL )
      CALL GSVOLU('TEDI', 'TRD1' , MEDTEF, PAR2 , 4 , IVOL )
      CALL GSPOS('TESI',1,'ECSI',0.,0.,0., 0,'ONLY')
      CALL GSPOS('TEDI',1,'ECDI',0.,0.,0., 0,'ONLY')
c Aerogel
c     narrow side (x1)
      PAR2(1) = PAR2(1)-0.06323
c     wide side   (x2)
      PAR2(2) = PAR2(2)-0.06323
c     height (y)
      PAR2(3) = PAR2(3)-0.054
c     length (z)
      PAR2(4) = PAR2(4)-0.054
      ECACW1=PAR2(1)
      ECACW2=PAR2(2)
      ECACH=PAR2(3)
      ECACL=PAR2(4)
      CALL GSVOLU('AESI', 'TRD1' , MEDAERS, PAR2 , 4 , IVOL )
      CALL GSVOLU('AEDI', 'TRD1' , MEDAERD, PAR2 , 4 , IVOL )
      CALL GSPOS('AESI',2,'TESI',0.,0.,0., 0,'ONLY')
      CALL GSPOS('AEDI',2,'TEDI',0.,0.,0., 0,'ONLY')
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccc Разбиение аэрогеля на части - внутренний торцевой счетчик  (ECSI, ECDI)    cccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     (z)
      PAR4(1) = 3.5
c     THET
      PAR4(2) = 4.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 1.85*0.5
c     TL1
      PAR4(6) = 1.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(1), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(1), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(1),'COLO',2)
      CALL GSATT (EDI(1),'COLO',2)
      CALL GSPOS(ESI(1),1,'AESI',1.355,0.,-21.9759, 0,'MANY')
      CALL GSPOS(EDI(1),1,'AEDI',1.355,0.,-21.9759, 0,'MANY')
c
c     (z)
      PAR4(1) = 3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(2), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(2), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(2),'COLO',2)
      CALL GSATT (EDI(2),'COLO',2)
      CALL GSPOS(ESI(2),1,'AESI',1.5749,0.,-14.638529, 0,'MANY')
      CALL GSPOS(EDI(2),1,'AEDI',1.5749,0.,-14.638529, 0,'MANY')
ccccc
c     (z)
      PAR4(1) = 3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(3), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(3), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(3),'COLO',2)
      CALL GSATT (EDI(3),'COLO',2)
      CALL GSPOS(ESI(3),1,'AESI',1.5749,0.,-6.963589, 0,'MANY')
      CALL GSPOS(EDI(3),1,'AEDI',1.5749,0.,-6.963589, 0,'MANY')
c
c     (z)
      PAR4(1) = 3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(4), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(4), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(4),'COLO',2)
      CALL GSATT (EDI(4),'COLO',2)
      CALL GSPOS(ESI(4),1,'AESI',1.5749,0.,0.711350, 0,'MANY')
      CALL GSPOS(EDI(4),1,'AEDI',1.5749,0.,0.711350, 0,'MANY')
c
c     (z)
      PAR4(1) = 3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(5), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(5), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(5),'COLO',2)
      CALL GSATT (EDI(5),'COLO',2)
      CALL GSPOS(ESI(5),1,'AESI',1.5749,0.,8.38629, 0,'MANY')
      CALL GSPOS(EDI(5),1,'AEDI',1.5749,0.,8.38629, 0,'MANY')
cccccccccccc
c
c     (z)
      PAR4(1) = 8.5
c     THET
      PAR4(2) = 4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 0.
c     TL1
      PAR4(6) = 0.
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(6), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(6), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(6),'COLO',2)
      CALL GSATT (EDI(6),'COLO',2)
      CALL GSPOS(ESI(6),1,'AESI',3.695,0.,-9.97589, 0,'MANY')
      CALL GSPOS(EDI(6),1,'AEDI',3.695,0.,-9.97589, 0,'MANY')
c
c     (z)
      PAR4(1) = 3.5
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(7), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(7), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(7),'COLO',2)
      CALL GSATT (EDI(7),'COLO',2)
      CALL GSPOS(ESI(7),1,'AESI',4.3824,0.,2.02411, 0,'MANY')
      CALL GSPOS(EDI(7),1,'AEDI',4.3824,0.,2.02411, 0,'MANY')
c
c     (z)
      PAR4(1) = 5.0
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(8), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(8), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(8),'COLO',2)
      CALL GSATT (EDI(8),'COLO',2)
      CALL GSPOS(ESI(8),1,'AESI',4.3824,0.,10.5241, 0,'MANY')
      CALL GSPOS(EDI(8),1,'AEDI',4.3824,0.,10.5241, 0,'MANY')
c
c     (z)
      PAR4(1) = 8.5
c     THET
      PAR4(2) = 4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 0.
c     TL1
      PAR4(6) = 0.
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(9), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(9), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(9),'COLO',2)
      CALL GSATT (EDI(9),'COLO',2)
      CALL GSPOS(ESI(9),1,'AESI',6.459,0.,7.02411, 0,'MANY')
      CALL GSPOS(EDI(9),1,'AEDI',6.459,0.,7.02411, 0,'MANY')
c
c     (z)
      PAR4(1) = 5.0
c     THET
      PAR4(2) = 4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 4.1*0.5
c     TL2
      PAR4(10) = 4.1*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(10), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(10), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(10),'COLO',2)
      CALL GSATT (EDI(10),'COLO',2)
      CALL GSPOS(ESI(10),1,'AESI',7.53,0.,20.4741, 0,'MANY')
      CALL GSPOS(EDI(10),1,'AEDI',7.53,0.,20.4741, 0,'MANY')
c
c     (z)
      PAR4(1) = 1.600
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 3.35*0.5
c     TL1
      PAR4(6) = 3.35*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 3.35*0.5
c     TL2
      PAR4(10) = 3.35*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(11), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(11), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(11),'COLO',2)
      CALL GSATT (EDI(11),'COLO',2)
      CALL GSPOS(ESI(11),1,'AESI',4.1949,0.,23.82, 0,'MANY')
      CALL GSPOS(EDI(11),1,'AEDI',4.1949,0.,23.82, 0,'MANY')
c
c     (z)
      PAR4(1) = 1.4
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 3.55*0.5
c     TL1
      PAR4(6) = 3.55*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 3.55*0.5
c     TL2
      PAR4(10) = 3.55*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(12), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(12), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(12),'COLO',2)
      CALL GSATT (EDI(12),'COLO',2)
      CALL GSPOS(ESI(12),1,'AESI',4.0849,0.,20.820, 0,'MANY')
      CALL GSPOS(EDI(12),1,'AEDI',4.0849,0.,20.820, 0,'MANY')
c
c     (z)
      PAR4(1) = 1.95
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 4.95*0.5
c     TL1
      PAR4(6) = 4.95*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 4.95*0.5
c     TL2
      PAR4(10) = 4.95*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(13), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(13), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(13),'COLO',2)
      CALL GSATT (EDI(13),'COLO',2)
      CALL GSPOS(ESI(13),1,'AESI',3.35,0.,17.475, 0,'MANY')
      CALL GSPOS(EDI(13),1,'AEDI',3.35,0.,17.475, 0,'MANY')
c
ccccccccccccc
c     (z)
      PAR4(1) = 1.65
c     THET
      PAR4(2) = 6.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) =  2.15*0.5
c     TL2
      PAR4(10) = 2.15*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(14), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(14), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(14),'COLO',2)
      CALL GSATT (EDI(14),'COLO',2)
      CALL GSPOS(ESI(14),1,'AESI',1.7849,0.,13.87381, 0,'MANY')
      CALL GSPOS(EDI(14),1,'AEDI',1.7849,0.,13.87381, 0,'MANY')
c
cccccccccccccccccccccccc зеркально ccccccccccccccccccccccccccccccccccccc
c
c     (z)
      PAR4(1) = 3.5
c     THET
      PAR4(2) = -4.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 1.85*0.5
c     TL1
      PAR4(6) = 1.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(15), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(15), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(15),'COLO',2)
      CALL GSATT (EDI(15),'COLO',2)
      CALL GSPOS(ESI(15),1,'AESI',-1.355,0.,-21.9759, 0,'MANY')
      CALL GSPOS(EDI(15),1,'AEDI',-1.355,0.,-21.9759, 0,'MANY')
ccccccccccccc
c     (z)
      PAR4(1) = 3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(16), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(16), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(16),'COLO',2)
      CALL GSATT (EDI(16),'COLO',2)
      CALL GSPOS(ESI(16),1,'AESI',-1.5749,0., -14.638529, 0,'MANY')
      CALL GSPOS(EDI(16),1,'AEDI',-1.5749,0., -14.638529, 0,'MANY')
c
c     (z)
      PAR4(1) =  3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(17), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(17), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(17),'COLO',2)
      CALL GSATT (EDI(17),'COLO',2)
      CALL GSPOS(ESI(17),1,'AESI',-1.5749,0.,-6.963589, 0,'MANY')
      CALL GSPOS(EDI(17),1,'AEDI',-1.5749,0.,-6.963589, 0,'MANY')
c
c     (z)
      PAR4(1) =  3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(18), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(18), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(18),'COLO',2)
      CALL GSATT (EDI(18),'COLO',2)
      CALL GSPOS(ESI(18),1,'AESI',-1.5749,0.,0.711350, 0,'MANY')
      CALL GSPOS(EDI(18),1,'AEDI',-1.5749,0.,0.711350, 0,'MANY')
c
c     (z)
      PAR4(1) =  3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(19), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(19), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(19),'COLO',2)
      CALL GSATT (EDI(19),'COLO',2)
      CALL GSPOS(ESI(19),1,'AESI',-1.5749,0.,8.38629, 0,'MANY')
      CALL GSPOS(EDI(19),1,'AEDI',-1.5749,0.,8.38629, 0,'MANY')
c
c     (z)
      PAR4(1) = 8.5
c     THET
      PAR4(2) = -4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 0.
c     TL1
      PAR4(6) = 0.
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(20), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(20), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(20),'COLO',2)
      CALL GSATT (EDI(20),'COLO',2)
      CALL GSPOS(ESI(20),1,'AESI',-3.695,0.,-9.97589, 0,'MANY')
      CALL GSPOS(EDI(20),1,'AEDI',-3.695,0.,-9.97589, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 3.5
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(21), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(21), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(21),'COLO',2)
      CALL GSATT (EDI(21),'COLO',2)
      CALL GSPOS(ESI(21),1,'AESI',-4.3824,0.,2.02411, 0,'MANY')
      CALL GSPOS(EDI(21),1,'AEDI',-4.3824,0.,2.02411, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 5.0
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(22), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(22), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(22),'COLO',2)
      CALL GSATT (EDI(22),'COLO',2)
      CALL GSPOS(ESI(22),1,'AESI',-4.3824,0.,10.5241, 0,'MANY')
      CALL GSPOS(EDI(22),1,'AEDI',-4.3824,0.,10.5241, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 8.5
c     THET
      PAR4(2) = -4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 0.
c     TL1
      PAR4(6) = 0.
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(23), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(23), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(23),'COLO',2)
      CALL GSATT (EDI(23),'COLO',2)
      CALL GSPOS(ESI(23),1,'AESI',-6.459,0.,7.02411, 0,'MANY')
      CALL GSPOS(EDI(23),1,'AEDI',-6.459,0.,7.02411, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 5.0
c     THET
      PAR4(2) = -4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 4.1*0.5
c     TL2
      PAR4(10) = 4.1*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(24), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(24), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(24),'COLO',2)
      CALL GSATT (EDI(24),'COLO',2)
      CALL GSPOS(ESI(24),1,'AESI',-7.53,0.,20.4741, 0,'MANY')
      CALL GSPOS(EDI(24),1,'AEDI',-7.53,0.,20.4741, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 1.600
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 3.35*0.5
c     TL1
      PAR4(6) = 3.35*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 3.35*0.5
c     TL2
      PAR4(10) = 3.35*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(25), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(25), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(25),'COLO',2)
      CALL GSATT (EDI(25),'COLO',2)
      CALL GSPOS(ESI(25),1,'AESI',-4.1949,0.,23.82, 0,'MANY')
      CALL GSPOS(EDI(25),1,'AEDI',-4.1949,0.,23.82, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 1.4
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 3.55*0.5
c     TL1
      PAR4(6) = 3.55*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 3.55*0.5
c     TL2
      PAR4(10) = 3.55*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(26), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(26), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(26),'COLO',2)
      CALL GSATT (EDI(26),'COLO',2)
      CALL GSPOS(ESI(26),1,'AESI',-4.0849,0.,20.820, 0,'MANY')
      CALL GSPOS(EDI(26),1,'AEDI',-4.0849,0.,20.820, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 1.95
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 4.95*0.5
c     TL1
      PAR4(6) = 4.95*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 4.95*0.5
c     TL2
      PAR4(10) = 4.95*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(27), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(27), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(27),'COLO',2)
      CALL GSATT (EDI(27),'COLO',2)
      CALL GSPOS(ESI(27),1,'AESI',-3.35,0.,17.475, 0,'MANY')
      CALL GSPOS(EDI(27),1,'AEDI',-3.35,0.,17.475, 0,'MANY')
c
ccccccccccccc
c     (z)
      PAR4(1) = 1.65
c     THET
      PAR4(2) = -6.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) =  2.15*0.5
c     TL2
      PAR4(10) = 2.15*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESI(28), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDI(28), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESI(28),'COLO',2)
      CALL GSATT (EDI(28),'COLO',2)
      CALL GSPOS(ESI(28),1,'AESI',-1.7849,0.,13.87381, 0,'MANY')
      CALL GSPOS(EDI(28),1,'AEDI',-1.7849,0.,13.87381, 0,'MANY')
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Shifter, main plate 1  (ECSI, ECDI)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 37.3*0.5
      CALL GSVOLU('SEI1', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SDI1', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('SEI1',1,'AESI',0.,-2.75,-6.49, 0,'ONLY')
      CALL GSPOS('SDI1',1,'AEDI',0.,-2.75,-6.49, 0,'ONLY')
c
      PAR1(3) = 6.700*0.5
      CALL GSVOLU(SEI(1), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(2), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(1),1,'SEI1',0.,0.,-15.29, 0,'MANY')
      CALL GSPOS(SEI(2),1,'SDI1',0.,0.,-15.29, 0,'MANY')
c
      PAR1(3) = 7.649*0.5
      CALL GSVOLU(SEI(3), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(4), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(3),1,'SEI1',0.,0.,-8.125, 0,'MANY')
      CALL GSPOS(SEI(4),1,'SDI1',0.,0.,-8.125, 0,'MANY')
c
      CALL GSVOLU(SEI(5), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(6), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(5),1,'SEI1',0.,0.,-0.476, 0,'MANY')
      CALL GSPOS(SEI(6),1,'SDI1',0.,0.,-0.476, 0,'MANY')
c
      CALL GSVOLU(SEI(7), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(8), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(7),1,'SEI1',0.,0.,7.172, 0,'MANY')
      CALL GSPOS(SEI(8),1,'SDI1',0.,0.,7.172, 0,'MANY')
c
      CALL GSVOLU(SEI(9), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(10), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(9),1,'SEI1',0.,0.,14.821, 0,'MANY')
      CALL GSPOS(SEI(10),1,'SDI1',0.,0.,14.821, 0,'MANY')
c
c Shifter, main plate 2
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 37.3*0.5
      CALL GSVOLU('SEI2', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SDI2', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('SEI2',1,'AESI',0.,-0.98,-6.49, 0,'ONLY')
      CALL GSPOS('SDI2',1,'AEDI',0.,-0.98,-6.49, 0,'ONLY')
c
      PAR1(3) = 6.700*0.5
      CALL GSVOLU(SEI(11), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(12), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(11),1,'SEI2',0.,0.,-15.29, 0,'MANY')
      CALL GSPOS(SEI(12),1,'SDI2',0.,0.,-15.29, 0,'MANY')
c
      PAR1(3) = 7.649*0.5
      CALL GSVOLU(SEI(13), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(14), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(13),1,'SEI2',0.,0.,-8.125, 0,'MANY')
      CALL GSPOS(SEI(14),1,'SDI2',0.,0.,-8.125, 0,'MANY')
c
      CALL GSVOLU(SEI(15), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(16), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(15),1,'SEI2',0.,0.,-0.476, 0,'MANY')
      CALL GSPOS(SEI(16),1,'SDI2',0.,0.,-0.476, 0,'MANY')
c
      CALL GSVOLU(SEI(17), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(18), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(17),1,'SEI2',0.,0.,7.172, 0,'MANY')
      CALL GSPOS(SEI(18),1,'SDI2',0.,0.,7.172, 0,'MANY')
c
      CALL GSVOLU(SEI(19), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(20), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(19),1,'SEI2',0.,0.,14.821, 0,'MANY')
      CALL GSPOS(SEI(20),1,'SDI2',0.,0.,14.821, 0,'MANY')
c
c Shifter, main plate 3
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 37.3*0.5
      CALL GSVOLU('SEI3', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SDI3', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('SEI3',1,'AESI',0.,0.98,-6.49, 0,'ONLY')
      CALL GSPOS('SDI3',1,'AEDI',0.,0.98,-6.49, 0,'ONLY')
c
      PAR1(3) = 6.700*0.5
      CALL GSVOLU(SEI(21), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(22), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(21),1,'SEI3',0.,0.,-15.29, 0,'MANY')
      CALL GSPOS(SEI(22),1,'SDI3',0.,0.,-15.29, 0,'MANY')
c
      PAR1(3) = 7.649*0.5
      CALL GSVOLU(SEI(23), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(24), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(23),1,'SEI3',0.,0.,-8.125, 0,'MANY')
      CALL GSPOS(SEI(24),1,'SDI3',0.,0.,-8.125, 0,'MANY')
c
      CALL GSVOLU(SEI(25), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(26), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(25),1,'SEI3',0.,0.,-0.476, 0,'MANY')
      CALL GSPOS(SEI(26),1,'SDI3',0.,0.,-0.476, 0,'MANY')
c
      CALL GSVOLU(SEI(27), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(28), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(27),1,'SEI3',0.,0.,7.172, 0,'MANY')
      CALL GSPOS(SEI(28),1,'SDI3',0.,0.,7.172, 0,'MANY')
c
      CALL GSVOLU(SEI(29), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(30), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(29),1,'SEI3',0.,0.,14.821, 0,'MANY')
      CALL GSPOS(SEI(30),1,'SDI3',0.,0.,14.821, 0,'MANY')
c
c Shifter, main plate 4
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 37.3*0.5
      CALL GSVOLU('SEI4', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SDI4', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('SEI4',1,'AESI',0.,2.75,-6.49, 0,'ONLY')
      CALL GSPOS('SDI4',1,'AEDI',0.,2.75,-6.49, 0,'ONLY')
c
      PAR1(3) = 6.700*0.5
      CALL GSVOLU(SEI(31), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(32), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(31),1,'SEI4',0.,0.,-15.29, 0,'MANY')
      CALL GSPOS(SEI(32),1,'SDI4',0.,0.,-15.29, 0,'MANY')
c
      PAR1(3) = 7.649*0.5
      CALL GSVOLU(SEI(33), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(34), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(33),1,'SEI4',0.,0.,-8.125, 0,'MANY')
      CALL GSPOS(SEI(34),1,'SDI4',0.,0.,-8.125, 0,'MANY')
c
      CALL GSVOLU(SEI(35), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(36), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(35),1,'SEI4',0.,0.,-0.476, 0,'MANY')
      CALL GSPOS(SEI(36),1,'SDI4',0.,0.,-0.476, 0,'MANY')
c
      CALL GSVOLU(SEI(37), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(38), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(37),1,'SEI4',0.,0.,7.172, 0,'MANY')
      CALL GSPOS(SEI(38),1,'SDI4',0.,0.,7.172, 0,'MANY')
c
      CALL GSVOLU(SEI(39), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEI(40), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEI(39),1,'SEI4',0.,0.,14.821, 0,'MANY')
      CALL GSPOS(SEI(40),1,'SDI4',0.,0.,14.821, 0,'MANY')
c
c Shifter, part near PMT
c     thickness (x)
      PAR1(1) = 1.7*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 1.2*0.5
      CALL GSVOLU('S041', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('S042', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S041',1,'AESI',0.,-0.85,21.56, 0,'ONLY')
      CALL GSPOS('S042',1,'AEDI',0.,-0.85,21.56, 0,'ONLY')
c Shifter, bent part
c     RMIN
      PAR3(1) = 2.4
c     height  (y)
      PAR3(2) = 3.6
c     length (z)
      PAR3(3) = 1.7*0.5
c     phi starting
      PAR3(4) = 0.
c     phi ending
      PAR3(5) = 146.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,0.,0.,90.,90.,90.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SEI(43), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SEI(44), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SEI(43),1,'AESI',0.,-0.2,18.56, IRM,'ONLY')
      CALL GSPOS(SEI(44),1,'AEDI',0.,-0.2,18.56, IRM,'ONLY')
c Shifter, twisted part
c     wide part (x1)
      PAR3(1) = 1.7*0.5
c     narrow part  (x2)
      PAR3(2) = 0.3*0.5
c     narrow part (y1)
      PAR3(3) = 5.5*0.5
c     narrow part (y2)
      PAR3(4) = 6.8*0.5
c     length
      PAR3(5) = 3.4*0.5
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,90.,180.,90.,90.,180.,0.)
      CALL GSVOLU(SEI(45), 'TRD2' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SEI(46), 'TRD2' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SEI(45),1,'AESI',0.,0.,13.86, IRM,'ONLY')
      CALL GSPOS(SEI(46),1,'AEDI',0.,0.,13.86, IRM,'ONLY')
c MCP PMT frame
c     (x)
      PAR1(1) = 4.6*0.5
c     (y)
      PAR1(2) = 2.046*0.5
c     length (z)
      PAR1(3) = 3.95*0.5
      CALL GSVOLU('PLEI', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSVOLU('PLDI', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSPOS('PLEI',1,'AESI',0.,-2.523,21.435, 0,'ONLY')
      CALL GSPOS('PLDI',1,'AEDI',0.,-2.523,21.435, 0,'ONLY')
c Teflon reflecting film on electronic box 1
c     (x)
      PAR1(1) = 5.148*0.5
c     (y)
      PAR1(2) = 2.046*0.5
c     length (z)
      PAR1(3) = 2.316*0.5
      CALL GSVOLU('TEI1', 'BOX ' , MEDTEF, PAR1 , 3 , IVOL )
      CALL GSVOLU('TDI1', 'BOX ' , MEDTEF, PAR1 , 3 , IVOL )
      CALL GSPOS('TEI1',1,'AESI',0.,-2.523,24.568, 0,'ONLY')
      CALL GSPOS('TDI1',1,'AEDI',0.,-2.523,24.568, 0,'ONLY')
c Teflon reflecting film on electronic box 2
c     (x)
      PAR1(1) = 5.148*0.5
c     (y)
      PAR1(2) = 5.046*0.5
c     length (z)
      PAR1(3) = 3.566*0.5
      CALL GSVOLU('TEI2', 'BOX ' , MEDTEF, PAR1 , 3 , IVOL )
      CALL GSVOLU('TDI2', 'BOX ' , MEDTEF, PAR1 , 3 , IVOL )
      CALL GSPOS('TEI2',1,'AESI',0.,1.023,23.943, 0,'ONLY')
      CALL GSPOS('TDI2',1,'AEDI',0.,1.023,23.943, 0,'ONLY')
c Electronic box 1
c     (x)
      PAR1(1) = 5.148*0.5-0.054
c     (y)
      PAR1(2) = 2.046*0.5
c     length (z)
      PAR1(3) = 2.316*0.5
      CALL GSVOLU('EEI1', 'BOX ' , MEDELB, PAR1 , 3 , IVOL )
      CALL GSVOLU('EDI1', 'BOX ' , MEDELB, PAR1 , 3 , IVOL )
      CALL GSPOS('EEI1',1,'TEI1',0.,0.,0., 0,'ONLY')
      CALL GSPOS('EDI1',1,'TDI1',0.,0.,0., 0,'ONLY')
c Electronic box 2
c     (x)
      PAR1(1) = 5.148*0.5-0.054
c     (y)
      PAR1(2) = 5.046*0.5
c     length (z)
      PAR1(3) = 3.566*0.5-0.054*0.5
      CALL GSVOLU('EEI2', 'BOX ' , MEDELB, PAR1 , 3 , IVOL )
      CALL GSVOLU('EDI2', 'BOX ' , MEDELB, PAR1 , 3 , IVOL )
      CALL GSPOS('EEI2',1,'TEI2',0.,0.,0.027, 0,'ONLY')
      CALL GSPOS('EDI2',1,'TDI2',0.,0.,0.027, 0,'ONLY')
c Corner with air near shifter 1
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,155.,180.,90.,90.,115.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('AEI1', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ADI1', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('AEI1',1,'AESI',-0.39055,0.,-24.832, IRM,'ONLY')
      CALL GSPOS('ADI1',1,'AEDI',-0.39055,0.,-24.832, IRM,'ONLY')
c Corner with air near shifter 2
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 180.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,25.,180.,90.,90.,115.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('AEI2', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ADI2', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('AEI2',1,'AESI',0.39055,0.,-24.832, IRM,'ONLY')
      CALL GSPOS('ADI2',1,'AEDI',0.39055,0.,-24.832, IRM,'ONLY')
c Aluminium edge strap 1
c     DX
      PAR1(1) = 0.25
c     DY
      PAR1(2) = 0.25
c     DZ
      PAR1(3) = 50.8*0.5
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,99.,0.,90.,90.,9.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('ALEI', 'BOX ' , MEDALF, PAR1 , 3 , IVOL )
      CALL GSVOLU('AILD', 'BOX ' , MEDALF, PAR1 , 3 , IVOL )
      CALL GSPOS('ALEI',1,'AESI',6.229,3.296,0., IRM,'ONLY')
      CALL GSPOS('AILD',1,'AEDI',6.229,3.296,0., IRM,'ONLY')
c Aluminium edge strap 2
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,81.,0.,90.,90.,9.,180.)
c      CALL GPROTM(IRM)
      CALL GSPOS('ALEI',2,'AESI',-6.229,3.296,0., IRM,'ONLY')
      CALL GSPOS('AILD',2,'AEDI',-6.229,3.296,0., IRM,'ONLY')
c Aluminium edge strap on wide side (1)
c     DX
      PAR1(1) = 7.3*0.5
c     DY
      PAR1(2) = 0.25
c     DZ
      PAR1(3) = 0.25
      CALL GSVOLU('AWEI', 'BOX ' , MEDALF, PAR1 , 3 , IVOL )
      CALL GSVOLU('AWDI', 'BOX ' , MEDALF, PAR1 , 3 , IVOL )
      CALL GSPOS('AWEI',1,'AESI',6.224,3.296,25.701, 0,'ONLY')
      CALL GSPOS('AWDI',1,'AEDI',6.224,3.296,25.701, 0,'ONLY')
c Aluminium edge strap on wide side (2)
      CALL GSPOS('AWEI',2,'AESI',-6.224,3.296,25.701, 0,'ONLY')
      CALL GSPOS('AWDI',2,'AEDI',-6.224,3.296,25.701, 0,'ONLY')
c Aluminium edge strap on wide side (3)
      CALL GSPOS('AWEI',3,'AESI',6.224,-3.296,25.701, 0,'ONLY')
      CALL GSPOS('AWDI',3,'AEDI',6.224,-3.296,25.701, 0,'ONLY')
c Aluminium edge strap on wide side (4)
      CALL GSPOS('AWEI',4,'AESI',-6.224,-3.296,25.701, 0,'ONLY')
      CALL GSPOS('AWDI',4,'AEDI',-6.224,-3.296,25.701, 0,'ONLY')
c========================================================================================================================================
c     Barrel Aerogel Counter, inner layer building
c     long counter ('BILS') with 'standart' aerogel n=1.05,
c     long counter ('BILD') with 'dense' aerogel n=1.054,
c -------------------------------
c  long counter frame dimentions
c -------------------------------
c     R min
      PAR3(1) = 55.7
c     R max
      PAR3(2) = 62.5
c     length (z)
      PAR3(3) = 60.2*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.058
c     PHI2 - ending angle
      PAR3(5) = 98.942
      CALL GSVOLU( 'BILS' , 'TUBS' , MEDALF, PAR3 , 5 , IVOL )
      CALL GSVOLU( 'BILD' , 'TUBS' , MEDALF, PAR3 , 5 , IVOL )
c
c    Teflon reflecting film (long counter)
c     R min
      PAR3(1) = 55.8
c     R max
      PAR3(2) = 62.4
c     length (z)
      PAR3(3) = 60.05*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.156
c     PHI2 - ending angle
      PAR3(5) = 98.844
      CALL GSVOLU('TLSI', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TLDI', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('TLSI',1,'BILS',0.,0.,0.025, 0,'ONLY')
      CALL GSPOS('TLDI',1,'BILD',0.,0.,0.025, 0,'ONLY')
c Aerogel (long counter)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = (60.05-0.108)*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.209
c     PHI2 - ending angle
      PAR3(5) = 98.791
      BIR1=PAR3(1)
      BIR2=PAR3(2)
      BILL=PAR3(3)
      BIDF=PAR3(5)-PAR3(4)
      CALL GSVOLU('ALSI', 'TUBS' , MEDAERS, PAR3 , 5 , IVOL )
      CALL GSVOLU('ALDI', 'TUBS' , MEDAERD, PAR3 , 5 , IVOL )
      CALL GSPOS('ALSI',1,'TLSI',0.,0.,0., 0,'ONLY')
      CALL GSPOS('ALDI',1,'TLDI',0.,0.,0., 0,'ONLY')
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    Разбиение аэрогеля на 62 частей, длинный счетчик внутреннего слоя (BILS,BILD)   ccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 8.74277*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.607
c     PHI2 - ending angle
      PAR3(5) = 82.709
c
      ZM=-23.128824+0.40
      do i = 1,68
        IF ( i.GT.6 ) THEN
      	PAR3(4) = 82.709
      	PAR3(5) = 85.6045
        ENDIF
        IF ( i.GT.12 ) THEN
      	PAR3(4) = 85.6045
      	PAR3(5) = 87.800
        ENDIF
        IF ( i.GT.18 ) THEN
      	PAR3(4) = 87.800
      	PAR3(5) = 89.100
        ENDIF
        IF ( i.GT.24 ) THEN
      	PAR3(4) = 89.100
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.GT.29 ) THEN
      	PAR3(4) = 90.200
      	PAR3(5) = 90.900
        ENDIF
        IF ( i.GT.34 ) THEN
      	PAR3(4) = 90.900
      	PAR3(5) = 92.200
        ENDIF
        IF ( i.GT.40 ) THEN
      	PAR3(4) = 92.200
      	PAR3(5) = 94.39549
        ENDIF
        IF ( i.GT.46 ) THEN
      	PAR3(4) = 94.39549
      	PAR3(5) = 97.291
        ENDIF
        IF ( i.GT.52 ) THEN
      	PAR3(4) = 97.291
        PAR3(5) = 98.393
        ENDIF
c
        IF ( i.EQ.59 ) THEN
 	PAR3(3) = 4.654*0.5-0.55*0.5
	PAR3(4) = 81.607
      	PAR3(5) = 82.709
        ENDIF
        IF ( i.EQ.60 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)*2/3
	PAR3(4) = 82.709
      	PAR3(5) = 86.212
        ENDIF
        IF ( i.EQ.61 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)/3
	PAR3(4) = 82.709
      	PAR3(5) = 86.212
        ENDIF
c
        IF ( i.EQ.62 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)*2/3
	PAR3(4) = 93.788
        PAR3(5) = 97.291
        ENDIF
        IF ( i.EQ.63 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)/3
	PAR3(4) = 93.788
        PAR3(5) = 97.291
        ENDIF
        IF ( i.EQ.64 ) THEN
 	PAR3(3) = 4.654*0.5-0.55*0.5
	PAR3(4) = 97.291
        PAR3(5) = 98.393
        ENDIF
c
        IF ( i.EQ.65 ) THEN
 	PAR3(3) = 8.74277*0.5-3.0*0.5
	PAR3(4) = 89.100
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.EQ.66 ) THEN
 	PAR3(3) = 8.74277*0.5-3.0*0.5
	PAR3(4) = 90.200
      	PAR3(5) = 90.900
        ENDIF
c
        IF ( i.EQ.67 ) THEN
 	PAR3(3) = (9.1229/4+0.12)*0.5
	PAR3(4) = 81.607
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.EQ.68 ) THEN
 	PAR3(3) = (9.1229/4+0.12)*0.5
	PAR3(4) = 90.200
      	PAR3(5) = 98.393
        ENDIF
c
      CALL GSVOLU(ARLSI(i), 'TUBS' , MEDAERS, PAR3 , 5 , IVOL )
      CALL GSVOLU(ARLDI(i), 'TUBS' , MEDAERD, PAR3 , 5 , IVOL )
      CALL GSATT (ARLSI(i),'COLO',4)
       CALL GSATT (ARLDI(i),'COLO',4)
      CALL GSPOS(ARLSI(i),i,'ALSI',0.,0.,ZM, 0,'MANY')
      CALL GSPOS(ARLDI(i),i,'ALDI',0.,0.,ZM, 0,'MANY')
      ZM=ZM+8.74277
      IF ( i.EQ.6 ) THEN
      ZM=ZM-i*8.74277
      ENDIF
      IF ( i.EQ.12 ) THEN
      ZM=ZM-(i-6)*8.74277
      ENDIF
      IF ( i.EQ.18 ) THEN
      ZM=ZM-(i-12)*8.74277
      ENDIF
      IF ( i.EQ.24 ) THEN
      ZM=ZM-(i-18)*8.74277
      ENDIF
      IF ( i.EQ.29 ) THEN
      ZM=ZM-(i-24)*8.74277
      ENDIF
      IF ( i.EQ.34 ) THEN
      ZM=ZM-(i-29)*8.74277
      ENDIF
      IF ( i.EQ.40 ) THEN
      ZM=ZM-(i-34)*8.74277
      ENDIF
      IF ( i.EQ.46 ) THEN
      ZM=ZM-(i-40)*8.74277
      ENDIF
      IF ( i.EQ.52 ) THEN
      ZM=ZM-(i-46)*8.74277
      ENDIF
c
      IF ( i.EQ.58 ) THEN
      ZM=ZM-8.74277*0.5+4.654*0.5-0.55*0.5
      ENDIF
      IF ( i.EQ.59 ) THEN
      ZM=ZM-8.74277-(4.654*0.5-0.55*0.5)/3
      ENDIF
      IF ( i.EQ.60 ) THEN
      ZM=ZM-8.74277+(4.654*0.5-0.55*0.5)
      ENDIF
c
      IF ( i.EQ.61 ) THEN
      ZM=ZM-8.74277-(4.654*0.5-0.55*0.5)
      ENDIF
      IF ( i.EQ.62 ) THEN
      ZM=ZM-8.74277+(4.654*0.5-0.55*0.5)
      ENDIF
      IF ( i.EQ.63 ) THEN
      ZM=ZM-8.74277*0.5-4.654+0.28-(4.654*0.5-0.55*0.5)*2/3
      ENDIF
c
      IF ( i.EQ.64 ) THEN
      ZM=ZM-8.74277*2+4.654*0.5-3.00*0.5
      ENDIF
      IF ( i.EQ.65 ) THEN
      ZM=ZM-8.74277
      ENDIF
c
      IF ( i.EQ.66 ) THEN
      ZM=ZM-8.74277*5-8.74277-3.0-(9.1229/4)*0.5+0.15*0.5
      ENDIF
      IF ( i.EQ.67 ) THEN
      ZM=ZM-8.74277
      ENDIF
c
      end do
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Electronic box teflon (long counter)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z) !!
      PAR3(3) = 4.654*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.212
c     PHI2 - ending angle
      PAR3(5) = 93.788
      CALL GSVOLU('T1LI', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TLI1', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('T1LI',1,'ALSI',0.,0.,27.644, 0,'MANY')
      CALL GSPOS('TLI1',1,'ALDI',0.,0.,27.644, 0,'MANY')
c Electronic box G10 walls (long counter)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 4.6*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.265
c     PHI2 - ending angle
      PAR3(5) = 93.735
      CALL GSVOLU('G1LI', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSVOLU('GLI1', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSPOS('G1LI',1,'T1LI',0.,0.,0.027, 0,'MANY')
      CALL GSPOS('GLI1',1,'TLI1',0.,0.,0.027, 0,'MANY')
c Electronic box internal air (long counter)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 4.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.313
c     PHI2 - ending angle
      PAR3(5) = 93.687
      CALL GSVOLU('A1LI', 'TUBS' , MEDAIF, PAR3 , 5 , IVOL )
      CALL GSVOLU('ALI1', 'TUBS' , MEDAIF, PAR3 , 5 , IVOL )
      CALL GSPOS('A1LI',1,'G1LI',0.,0.,0.025, 0,'MANY')
      CALL GSPOS('ALI1',1,'GLI1',0.,0.,0.025, 0,'MANY')
c Preamplifier (long counter)
c     (x)
      PAR1(1) = 0.4*0.5
c     (y)
      PAR1(2) = 2.5*0.5
c     length (z)
      PAR1(3) = 4.0*0.5
      CALL GSVOLU('GALI', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSVOLU('GLIA', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSPOS('GALI',1,'A1LI',-3.2,60.4,0., 0,'ONLY')
      CALL GSPOS('GLIA',1,'ALI1',-3.2,60.4,0., 0,'ONLY')
c HV devider (long counter)
c     (x)
      PAR1(1) = 0.8*0.5
c     (y)
      PAR1(2) = 1.5*0.5
c     length (z)
      PAR1(3) = 4.0*0.5
      CALL GSVOLU('GHLI', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSVOLU('GLIH', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSPOS('GHLI',1,'A1LI',3.2,60.9,0., 0,'ONLY')
      CALL GSPOS('GLIH',1,'ALI1',3.2,60.9,0., 0,'ONLY')
c MCP PMT frame (long counter)
c     (x)
      PAR1(1) = 5.3*0.5
c     (y)
      PAR1(2) = 4.0*0.5
c     length (z)
      PAR1(3) = 2.7*0.5
      CALL GSVOLU('PFLI', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSVOLU('PLIF', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSPOS('PFLI',1,'A1LI',0.,59.1,0.925, 0,'MANY')
      CALL GSPOS('PLIF',1,'ALI1',0.,59.1,0.925, 0,'MANY')
c WLS clamp (long counter)
c     (x)
      PAR1(1) = 3.6*0.5
c     (y)
      PAR1(2) = 3.0*0.5
c     length (z)
      PAR1(3) = 1.0*0.5
      CALL GSVOLU('DCLI', 'BOX ' , MEDD16T, PAR1 , 3 , IVOL )
      CALL GSVOLU('DLIC', 'BOX ' , MEDD16T, PAR1 , 3 , IVOL )
      CALL GSPOS('DCLI',1,'A1LI',0.,58.65,-1.025, 0,'MANY')
      CALL GSPOS('DLIC',1,'ALI1',0.,58.65,-1.025, 0,'MANY')
c Shifter, part near PMT (long counter)
c     thickness (x)
      PAR1(1) = 1.7*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 2.704*0.5
      CALL GSVOLU(SBIL(1), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(2), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(1),1,'ALSI',0.,59.1,26.769, 0,'ONLY')
      CALL GSPOS(SBIL(2),1,'ALDI',0.,59.1,26.769, 0,'ONLY')
c Shifter, bent part 1 (long counter)
c     RMIN
      PAR3(1) = 2.88
c     RMAX
      PAR3(2) = 3.18
c     length (z)
      PAR3(3) = 1.3*0.5
c     phi starting
      PAR3(4) = -90.0
c     phi ending
      PAR3(5) = -19.378
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,180.,0.,90.,90.,90.,0.)
      CALL GPROTM(IRM)
      CALL GSVOLU(SBIL(3), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBIL(4), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBIL(3),1,'ALSI',0.,62.58,25.417, IRM,'ONLY')
      CALL GSPOS(SBIL(4),1,'ALDI',0.,62.58,25.417, IRM,'ONLY')
c Shifter, bent part 2 (long counter)
c     RMIN
      PAR3(1) = 5.28
c     RMAX
      PAR3(2) = 5.58
c     length (z)
      PAR3(3) = 1.7*0.5
c     phi starting
      PAR3(4) = -90.0
c     phi ending
      PAR3(5) = -57.48
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,180.,0.,90.,90.,90.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBIL(5), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBIL(6), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBIL(5),1,'ALSI',0.,64.68,25.417, IRM,'ONLY')
      CALL GSPOS(SBIL(6),1,'ALDI',0.,64.68,25.417, IRM,'ONLY')
c Shifter, bent part 3 (long counter)
c     RMIN
      PAR3(1) = 5.28
c     RMAX
      PAR3(2) = 5.58
c     length (z)
      PAR3(3) = 1.7*0.5
c     phi starting
      PAR3(4) = 57.48
c     phi ending
      PAR3(5) = 90.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,180.,0.,90.,90.,90.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBIL(7), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBIL(8), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBIL(7),1,'ALSI',0.,53.52,25.417, IRM,'ONLY')
      CALL GSPOS(SBIL(8),1,'ALDI',0.,53.52,25.417, IRM,'ONLY')
c Shifter, bent part 4 (long counter)
c     RMIN
      PAR3(1) = 2.88
c     RMAX
      PAR3(2) = 3.18
c     length (z)
      PAR3(3) = 1.3*0.5
c     phi starting
      PAR3(4) = 19.378
c     phi ending
      PAR3(5) = 90.0
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,180.,0.,90.,90.,90.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBIL(9), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBIL(10), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBIL(9),1,'ALSI',0.,55.62,25.417, IRM,'ONLY')
      CALL GSPOS(SBIL(10),1,'ALDI',0.,55.62,25.417, IRM,'ONLY')
c Shifter, straight part 1 (long counter, BILS-BILD)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 51.09*0.5          !total lenght
      CALL GSVOLU('S5LI', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SLI5', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S5LI',1,'ALSI',0.,61.525,-3.188, 0,'ONLY')
      CALL GSPOS('SLI5',1,'ALDI',0.,61.525,-3.188, 0,'ONLY')
c
      PAR1(3) = 1.63338*0.5
      CALL GSVOLU(SBIL(11), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(12), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(11),1,'S5LI',0.,0.,-24.728, 0,'MANY')
      CALL GSPOS(SBIL(12),1,'SLI5',0.,0.,-24.728, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5
      CALL GSVOLU(SBIL(13), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(14), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(13),1,'S5LI',0.,0.,-19.54, 0,'MANY')
      CALL GSPOS(SBIL(14),1,'SLI5',0.,0.,-19.54, 0,'MANY')
c
      CALL GSVOLU(SBIL(15), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(16), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(15),1,'S5LI',0.,0.,-10.797, 0,'MANY')
      CALL GSPOS(SBIL(16),1,'SLI5',0.,0.,-10.797, 0,'MANY')
c
      CALL GSVOLU(SBIL(17), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(18), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(17),1,'S5LI',0.,0.,-2.054, 0,'MANY')
      CALL GSPOS(SBIL(18),1,'SLI5',0.,0.,-2.054, 0,'MANY')
c
      CALL GSVOLU(SBIL(19), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(20), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(19),1,'S5LI',0.,0.,6.688, 0,'MANY')
      CALL GSPOS(SBIL(20),1,'SLI5',0.,0.,6.688, 0,'MANY')
c
      CALL GSVOLU(SBIL(21), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(22), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(21),1,'S5LI',0.,0.,15.43, 0,'MANY')
      CALL GSPOS(SBIL(22),1,'SLI5',0.,0.,15.43, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5-3.0*0.5
      CALL GSVOLU(SBIL(23), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(24), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(23),1,'S5LI',0.,0.,22.672, 0,'MANY')
      CALL GSPOS(SBIL(24),1,'SLI5',0.,0.,22.672, 0,'MANY')
c
c Shifter, straight part 2 (long counter)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 51.09*0.5          !total lenght
      CALL GSVOLU('S6LI', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SLI6', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S6LI',1,'ALSI',0.,59.975,-3.188, 0,'ONLY')
      CALL GSPOS('SLI6',1,'ALDI',0.,59.975,-3.188, 0,'ONLY')
c
      PAR1(3) = 1.63338*0.5
      CALL GSVOLU(SBIL(25), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(26), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(25),1,'S6LI',0.,0.,-24.728, 0,'MANY')
      CALL GSPOS(SBIL(26),1,'SLI6',0.,0.,-24.728, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5
      CALL GSVOLU(SBIL(27), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(28), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(27),1,'S6LI',0.,0.,-19.54, 0,'MANY')
      CALL GSPOS(SBIL(28),1,'SLI6',0.,0.,-19.54, 0,'MANY')
c
      CALL GSVOLU(SBIL(29), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(30), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(29),1,'S6LI',0.,0.,-10.797, 0,'MANY')
      CALL GSPOS(SBIL(30),1,'SLI6',0.,0.,-10.797, 0,'MANY')
c
      CALL GSVOLU(SBIL(31), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(32), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(31),1,'S6LI',0.,0.,-2.054, 0,'MANY')
      CALL GSPOS(SBIL(32),1,'SLI6',0.,0.,-2.054, 0,'MANY')
c
      CALL GSVOLU(SBIL(33), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(34), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(33),1,'S6LI',0.,0.,6.688, 0,'MANY')
      CALL GSPOS(SBIL(34),1,'SLI6',0.,0.,6.688, 0,'MANY')
c
      CALL GSVOLU(SBIL(35), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(36), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(35),1,'S6LI',0.,0.,15.43, 0,'MANY')
      CALL GSPOS(SBIL(36),1,'SLI6',0.,0.,15.43, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5-3.0*0.5
      CALL GSVOLU(SBIL(37), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(38), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(37),1,'S6LI',0.,0.,22.672, 0,'MANY')
      CALL GSPOS(SBIL(38),1,'SLI6',0.,0.,22.672, 0,'MANY')
c
c Shifter, straight part 3 (long counter)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 51.09*0.5          !total lenght
      CALL GSVOLU('S7LI', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SLI7', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S7LI',1,'ALSI',0.,58.225,-3.188, 0,'ONLY')
      CALL GSPOS('SLI7',1,'ALDI',0.,58.225,-3.188, 0,'ONLY')
c
      PAR1(3) = 1.63338*0.5
      CALL GSVOLU(SBIL(39), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(40), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(39),1,'S7LI',0.,0.,-24.728, 0,'MANY')
      CALL GSPOS(SBIL(40),1,'SLI7',0.,0.,-24.728, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5
      CALL GSVOLU(SBIL(41), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(42), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(41),1,'S7LI',0.,0.,-19.54, 0,'MANY')
      CALL GSPOS(SBIL(42),1,'SLI7',0.,0.,-19.54, 0,'MANY')
c
      CALL GSVOLU(SBIL(43), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(44), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(43),1,'S7LI',0.,0.,-10.797, 0,'MANY')
      CALL GSPOS(SBIL(44),1,'SLI7',0.,0.,-10.797, 0,'MANY')
c
      CALL GSVOLU(SBIL(45), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(46), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(45),1,'S7LI',0.,0.,-2.054, 0,'MANY')
      CALL GSPOS(SBIL(46),1,'SLI7',0.,0.,-2.054, 0,'MANY')
c
      CALL GSVOLU(SBIL(47), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(48), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(47),1,'S7LI',0.,0.,6.688, 0,'MANY')
      CALL GSPOS(SBIL(48),1,'SLI7',0.,0.,6.688, 0,'MANY')
c
      CALL GSVOLU(SBIL(49), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(50), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(49),1,'S7LI',0.,0.,15.43, 0,'MANY')
      CALL GSPOS(SBIL(50),1,'SLI7',0.,0.,15.43, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5-3.0*0.5
      CALL GSVOLU(SBIL(51), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(52), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(51),1,'S7LI',0.,0.,22.672, 0,'MANY')
      CALL GSPOS(SBIL(52),1,'SLI7',0.,0.,22.672, 0,'MANY')
c
c Shifter, straight part 4 (long counter)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 51.09*0.5          !total lenght
      CALL GSVOLU('S8LI', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SLI8', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S8LI',1,'ALSI',0.,56.675,-3.188, 0,'ONLY')
      CALL GSPOS('SLI8',1,'ALDI',0.,56.675,-3.188, 0,'ONLY')
c
      PAR1(3) = 1.63338*0.5
      CALL GSVOLU(SBIL(53), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(54), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(53),1,'S8LI',0.,0.,-24.728, 0,'MANY')
      CALL GSPOS(SBIL(54),1,'SLI8',0.,0.,-24.728, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5
      CALL GSVOLU(SBIL(55), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(56), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(55),1,'S8LI',0.,0.,-19.54, 0,'MANY')
      CALL GSPOS(SBIL(56),1,'SLI8',0.,0.,-19.54, 0,'MANY')
c
      CALL GSVOLU(SBIL(57), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(58), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(57),1,'S8LI',0.,0.,-10.797, 0,'MANY')
      CALL GSPOS(SBIL(58),1,'SLI8',0.,0.,-10.797, 0,'MANY')
c
      CALL GSVOLU(SBIL(59), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(60), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(59),1,'S8LI',0.,0.,-2.054, 0,'MANY')
      CALL GSPOS(SBIL(60),1,'SLI8',0.,0.,-2.054, 0,'MANY')
c
      CALL GSVOLU(SBIL(61), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(62), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(61),1,'S8LI',0.,0.,6.688, 0,'MANY')
      CALL GSPOS(SBIL(62),1,'SLI8',0.,0.,6.688, 0,'MANY')
c
      CALL GSVOLU(SBIL(63), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(64), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(63),1,'S8LI',0.,0.,15.43, 0,'MANY')
      CALL GSPOS(SBIL(64),1,'SLI8',0.,0.,15.43, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5-3.0*0.5
      CALL GSVOLU(SBIL(65), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIL(66), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIL(65),1,'S8LI',0.,0.,22.672, 0,'MANY')
      CALL GSPOS(SBIL(66),1,'SLI8',0.,0.,22.672, 0,'MANY')
c
c Corner with air near shifter 1 (long counter)
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 6.3*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 6.3*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,155.,180.,90.,90.,115.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('A2LI', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ALI2', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('A2LI',1,'ALSI',-0.39055,59.1,-29.077, IRM,'MANY')
      CALL GSPOS('ALI2',1,'ALDI',-0.39055,59.1,-29.077, IRM,'MANY')
c Corner with air near shifter 2 (long counter)
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 180.
c     H1
      PAR4(4) = 6.3*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 6.3*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,25.,180.,90.,90.,115.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('A3LI', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ALI3', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('A3LI',1,'ALSI',0.39055,59.1,-29.077, IRM,'MANY')
      CALL GSPOS('ALI3',1,'ALDI',0.39055,59.1,-29.077, IRM,'MANY')
c
c Aluminum strap 1 - right (long counter)
c     R min
      PAR3(1) = 61.796
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = (60.05-0.108)*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 81.607
      CALL GSVOLU('D1LI', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DLI1', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D1LI',1,'ALSI',0.,0.,0., 0,'ONLY')
      CALL GSPOS('DLI1',1,'ALDI',0.,0.,0., 0,'ONLY')
c
c Aluminum strap 2 - left (long counter)
c     R min
      PAR3(1) = 61.796
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = (60.05-0.108)*0.5
c     PHI1 - starting angle
      PAR3(4) = 98.393
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('D2LI', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DLI2', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D2LI',1,'ALSI',0.,0.,0., 0,'ONLY')
      CALL GSPOS('DLI2',1,'ALDI',0.,0.,0., 0,'ONLY')
c
c Aluminum strap 3 - butt-end down (long counter)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 56.404
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('D3LI', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DLI3', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D3LI',1,'ALSI',0.,0.,-29.746, 0,'ONLY')
      CALL GSPOS('DLI3',1,'ALDI',0.,0.,-29.746, 0,'ONLY')
c
c Aluminum strap 4 - middle down (long counter)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 56.404
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('D4LI', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DLI4', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D4LI',1,'ALSI',0.,0.,29.746, 0,'ONLY')
      CALL GSPOS('DLI4',1,'ALDI',0.,0.,29.746, 0,'ONLY')
c
c Aluminum strap 5 - butt-end up (long counter)
c     R min
      PAR3(1) = 61.796
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.607
c     PHI2 - ending angle
      PAR3(5) = 98.393
      CALL GSVOLU('D5LI', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DLI5', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D5LI',1,'ALSI',0.,0.,-29.746, 0,'ONLY')
      CALL GSPOS('DLI5',1,'ALDI',0.,0.,-29.746, 0,'ONLY')
c
c Aluminum strap 6 - middle up (long counter)
c     R min
      PAR3(1) = 61.796
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.607
c     PHI2 - ending angle
      PAR3(5) = 98.393
      CALL GSVOLU('D6LI', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DLI6', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D6LI',1,'ALSI',0.,0.,29.746, 0,'ONLY')
      CALL GSPOS('DLI6',1,'ALDI',0.,0.,29.746, 0,'ONLY')
c
c===========================================================================================
c-----------------------------------------------------
c     Barrel Aerogel Counter, inner layer building
c     short counter ('BISS') with 'standart' aerogel n=1.05,
c     short counter ('BISD') with 'dense' aerogel n=1.054
c --------------------------------
c  short counter frame dimentions
c --------------------------------
c     R min
      PAR3(1) = 55.7
c     R max
      PAR3(2) = 62.5
c     length (z)
      PAR3(3) = 47.8*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.058
c     PHI2 - ending angle
      PAR3(5) = 98.942
      CALL GSVOLU( 'BISS' , 'TUBS' , MEDALF, PAR3 , 5 , IVOL )
      CALL GSVOLU( 'BISD' , 'TUBS' , MEDALF, PAR3 , 5 , IVOL )
c
c    Teflon reflecting film (short counter)
c     R min
      PAR3(1) = 55.8
c     R max
      PAR3(2) = 62.4
c     length (z)
      PAR3(3) = 47.65*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.156
c     PHI2 - ending angle
      PAR3(5) = 98.844
      CALL GSVOLU('TSSI', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TSDI', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('TSSI',1,'BISS',0.,0.,-0.025, 0,'ONLY')
      CALL GSPOS('TSDI',1,'BISD',0.,0.,-0.025, 0,'ONLY')
c
c Aerogel (short counter)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = (47.65-0.108)*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.209
c     PHI2 - ending angle
      PAR3(5) = 98.791
      BILS=PAR3(3)
      CALL GSVOLU('ASSI', 'TUBS' , MEDAERS, PAR3 , 5 , IVOL )
      CALL GSVOLU('ASDI', 'TUBS' , MEDAERD, PAR3 , 5 , IVOL )
      CALL GSPOS('ASSI',1,'TSSI',0.,0.,0., 0,'ONLY')
      CALL GSPOS('ASDI',1,'TSDI',0.,0.,0., 0,'ONLY')
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    Разбиение аэрогеля на 58 частей, короткий счетчик внутреннего слоя (BISS,BISD) ccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 8.044182*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.607
c     PHI2 - ending angle
      PAR3(5) = 82.709
c
      ZM=19.53722-0.55
      do i = 1,58
        IF ( i.EQ.1 ) THEN
 	PAR3(3) = 8.044182*0.5-4.45*0.5+0.55*0.5+(8.46756/4)*0.5+0.05
	PAR3(5) = 83.567
        ZM=16.790464-0.60
	ENDIF
c
        IF ( i.GT.1 ) THEN
      	PAR3(3) = 8.044182*0.5
        PAR3(5) = 82.709
        ENDIF
        IF ( i.EQ.2 ) THEN
        ZM=ZM+0.85
        ENDIF
c
        IF ( i.GT.5 ) THEN
      	PAR3(4) = 82.709
      	PAR3(5) = 85.6045
        ENDIF
c
        IF ( i.EQ.6 ) THEN
        PAR3(4) = 83.567
        ENDIF
c
        IF ( i.GT.10 ) THEN
      	PAR3(4) = 85.6045
      	PAR3(5) = 87.800
        ENDIF
        IF ( i.GT.15 ) THEN
      	PAR3(4) = 87.800
      	PAR3(5) = 89.100
        ENDIF
        IF ( i.GT.20 ) THEN
      	PAR3(4) = 89.100
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.GT.24 ) THEN
      	PAR3(4) = 90.200
      	PAR3(5) = 90.900
        ENDIF
        IF ( i.GT.28 ) THEN
      	PAR3(4) = 90.900
      	PAR3(5) = 92.200
        ENDIF
        IF ( i.GT.33 ) THEN
      	PAR3(4) = 92.200
      	PAR3(5) = 94.39549
        ENDIF
        IF ( i.GT.38 ) THEN
      	PAR3(4) = 94.39549
      	PAR3(5) = 97.291
        ENDIF
c
        IF ( i.EQ.39 ) THEN
        PAR3(5) = 96.433
	ENDIF
c
        IF ( i.GT.43 ) THEN
      	PAR3(4) = 97.291
        PAR3(5) = 98.393
        ENDIF
c
       IF ( i.EQ.44 ) THEN
 	PAR3(3) = 8.044182*0.5-4.45*0.5+0.55*0.5+(8.46756/4)*0.5+0.05
        PAR3(4) = 96.433
        ZM=16.790464-0.60
	ENDIF
        IF ( i.EQ.45 ) THEN
        ZM=ZM+0.85
        ENDIF
c
        IF ( i.EQ.49 ) THEN
 	PAR3(3) = 4.654*0.5-0.55*0.5
	PAR3(4) = 81.607
      	PAR3(5) = 82.709
        ENDIF
        IF ( i.EQ.50 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)*2/3
	PAR3(4) = 82.709
      	PAR3(5) = 86.212
        ENDIF
        IF ( i.EQ.51 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)/3
	PAR3(4) = 82.709
      	PAR3(5) = 86.212
        ENDIF
c
        IF ( i.EQ.52 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)*2/3
	PAR3(4) = 93.788
        PAR3(5) = 97.291
        ENDIF
        IF ( i.EQ.53 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)/3
	PAR3(4) = 93.788
        PAR3(5) = 97.291
        ENDIF
        IF ( i.EQ.54 ) THEN
 	PAR3(3) = 4.654*0.5-0.55*0.5
	PAR3(4) = 97.291
        PAR3(5) = 98.393
        ENDIF
c
        IF ( i.EQ.55 ) THEN
 	PAR3(3) = 8.044182*0.5-3.0*0.5
	PAR3(4) = 89.100
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.EQ.56 ) THEN
 	PAR3(3) = 8.044182*0.5-3.0*0.5
	PAR3(4) = 90.200
      	PAR3(5) = 90.900
        ENDIF
c
        IF ( i.EQ.57 ) THEN
 	PAR3(3) = (8.46756/4+0.12)*0.5
	PAR3(4) = 83.567
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.EQ.58 ) THEN
 	PAR3(3) = (8.46756/4+0.12)*0.5
	PAR3(4) = 90.200
      	PAR3(5) = 96.433
        ENDIF
c
      CALL GSVOLU(ARSSI(i), 'TUBS' , MEDAERS, PAR3 , 5 , IVOL )
      CALL GSVOLU(ARSDI(i), 'TUBS' , MEDAERD, PAR3 , 5 , IVOL )
      CALL GSATT (ARSSI(i),'COLO',4)
      CALL GSATT (ARSDI(i),'COLO',4)
      CALL GSPOS(ARSSI(i),i,'ASSI',0.,0.,ZM, 0,'MANY')
      CALL GSPOS(ARSDI(i),i,'ASDI',0.,0.,ZM, 0,'MANY')
      ZM=ZM-8.044182
      IF ( i.EQ.5 ) THEN
      ZM=ZM+i*8.044182
      ENDIF
      IF ( i.EQ.10 ) THEN
      ZM=ZM+(i-5)*8.044182
      ENDIF
      IF ( i.EQ.15 ) THEN
      ZM=ZM+(i-10)*8.044182
      ENDIF
      IF ( i.EQ.20 ) THEN
      ZM=ZM+(i-15)*8.044182
      ENDIF
      IF ( i.EQ.24 ) THEN
      ZM=ZM+(i-20)*8.044182
      ENDIF
      IF ( i.EQ.28 ) THEN
      ZM=ZM+(i-24)*8.044182
      ENDIF
      IF ( i.EQ.33 ) THEN
      ZM=ZM+(i-28)*8.044182
      ENDIF
      IF ( i.EQ.38 ) THEN
      ZM=ZM+(i-33)*8.044182
      ENDIF
      IF ( i.EQ.43 ) THEN
      ZM=ZM+(i-38)*8.044182
      ENDIF
c
      IF ( i.EQ.48 ) THEN
      ZM=ZM+8.044182*0.5-4.654*0.5+0.55*0.5
      ENDIF
      IF ( i.EQ.49 ) THEN
      ZM=ZM+8.044182+(4.654*0.5-0.55*0.5)/3
      ENDIF
      IF ( i.EQ.50 ) THEN
      ZM=ZM+8.044182-(4.654*0.5-0.55*0.5)
      ENDIF
c
      IF ( i.EQ.51 ) THEN
      ZM=ZM+8.044182+4.654*0.5-0.55*0.5
      ENDIF
      IF ( i.EQ.52 ) THEN
      ZM=ZM+8.044182-(4.654*0.5-0.55*0.5)
      ENDIF
      IF ( i.EQ.53 ) THEN
      ZM=ZM+8.044182*0.5+4.654-0.65+(4.654*0.5-0.55*0.5)*2/3
      ENDIF
c
      IF ( i.EQ.54 ) THEN
      ZM=ZM+8.044182+4.654*0.5+8.044182*0.5+2.5*0.5
      ENDIF
      IF ( i.EQ.55 ) THEN
      ZM=ZM+8.044182
      ENDIF
c
      IF ( i.EQ.56 ) THEN
      ZM=ZM+8.044182*4+8.044182+2.9+(8.46756/4)*0.5-0.55
      ENDIF
      IF ( i.EQ.57 ) THEN
      ZM=ZM+8.044182
      ENDIF
c
      end do
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Electronic box teflon (short counter)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 4.654*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.212
c     PHI2 - ending angle
      PAR3(5) = 93.788
      CALL GSVOLU('T1SI', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TSI1', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('T1SI',1,'ASSI',0.,0.,-21.444, 0,'MANY')
      CALL GSPOS('TSI1',1,'ASDI',0.,0.,-21.444, 0,'MANY')
c Electronic box G10 walls (short counter)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 4.6*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.265
c     PHI2 - ending angle
      PAR3(5) = 93.735
      CALL GSVOLU('G1SI', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSVOLU('GSI1', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSPOS('G1SI',1,'T1SI',0.,0.,-0.027, 0,'MANY')
      CALL GSPOS('GSI1',1,'TSI1',0.,0.,-0.027, 0,'MANY')
c Electronic box internal air (short counter)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 4.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.313
c     PHI2 - ending angle
      PAR3(5) = 93.687
      CALL GSVOLU('A1SI', 'TUBS' , MEDAIF, PAR3 , 5 , IVOL )
      CALL GSVOLU('ASI1', 'TUBS' , MEDAIF, PAR3 , 5 , IVOL )
      CALL GSPOS('A1SI',1,'G1SI',0.,0.,-0.025, 0,'MANY')
      CALL GSPOS('ASI1',1,'GSI1',0.,0.,-0.025, 0,'MANY')
c Preamplifier (short counter)
c     (x)
      PAR1(1) = 0.4*0.5
c     (y)
      PAR1(2) = 2.5*0.5
c     length (z)
      PAR1(3) = 4.0*0.5
      CALL GSVOLU('GASI', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSVOLU('GSIA', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSPOS('GASI',1,'A1SI',3.2,60.4,0., 0,'ONLY')
      CALL GSPOS('GSIA',1,'ASI1',3.2,60.4,0., 0,'ONLY')
c HV devider (short counter)
c     (x)
      PAR1(1) = 0.8*0.5
c     (y)
      PAR1(2) = 1.5*0.5
c     length (z)
      PAR1(3) = 4.0*0.5
      CALL GSVOLU('GHSI', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSVOLU('GSIH', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSPOS('GHSI',1,'A1SI',-3.2,60.9,0., 0,'ONLY')
      CALL GSPOS('GSIH',1,'ASI1',-3.2,60.9,0., 0,'ONLY')
c MCP PMT frame (short counter)
c     (x)
      PAR1(1) = 5.3*0.5
c     (y)
      PAR1(2) = 4.0*0.5
c     length (z)
      PAR1(3) = 2.7*0.5
      CALL GSVOLU('PFSI', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSVOLU('PSIF', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSPOS('PFSI',1,'A1SI',0.,59.1,-0.925, 0,'MANY')
      CALL GSPOS('PSIF',1,'ASI1',0.,59.1,-0.925, 0,'MANY')
c WLS clamp (short counter)
c     (x)
      PAR1(1) = 3.6*0.5
c     (y)
      PAR1(2) = 3.0*0.5
c     length (z)
      PAR1(3) = 1.0*0.5
      CALL GSVOLU('DCSI', 'BOX ' , MEDD16T, PAR1 , 3 , IVOL )
      CALL GSVOLU('DSIC', 'BOX ' , MEDD16T, PAR1 , 3 , IVOL )
      CALL GSPOS('DCSI',1,'A1SI',0.,58.65,1.025, 0,'MANY')
      CALL GSPOS('DSIC',1,'ASI1',0.,58.65,1.025, 0,'MANY')
c Shifter, part near PMT (short counter)
c     thickness (x)
      PAR1(1) = 1.7*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 2.704*0.5
      CALL GSVOLU(SBIS(1), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(2), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(1),1,'ASSI',0.,59.1,-20.569, 0,'ONLY')
      CALL GSPOS(SBIS(2),1,'ASDI',0.,59.1,-20.569, 0,'ONLY')
c Shifter, bent part 1 (short counter)
c     RMIN
      PAR3(1) = 2.88
c     RMAX
      PAR3(2) = 3.18
c     length (z)
      PAR3(3) = 1.3*0.5
c     phi starting
      PAR3(4) = -90.0
c     phi ending
      PAR3(5) = -19.378
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,0.,0.,90.,90.,90.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBIS(3), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBIS(4), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBIS(3),1,'ASSI',0.,62.58,-19.217, IRM,'ONLY')
      CALL GSPOS(SBIS(4),1,'ASDI',0.,62.58,-19.217, IRM,'ONLY')
c Shifter, bent part 2 (short counter)
c     RMIN
      PAR3(1) = 5.28
c     RMAX
      PAR3(2) = 5.58
c     length (z)
      PAR3(3) = 1.7*0.5
c     phi starting
      PAR3(4) = -90.0
c     phi ending
      PAR3(5) = -57.48
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,0.,0.,90.,90.,90.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBIS(5), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBIS(6), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBIS(5),1,'ASSI',0.,64.68,-19.217, IRM,'ONLY')
      CALL GSPOS(SBIS(6),1,'ASDI',0.,64.68,-19.217, IRM,'ONLY')
c Shifter, bent part 3 (short counter)
c     RMIN
      PAR3(1) = 5.28
c     RMAX
      PAR3(2) = 5.58
c     length (z)
      PAR3(3) = 1.7*0.5
c     phi starting
      PAR3(4) = 57.48
c     phi ending
      PAR3(5) = 90.0
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,0.,0.,90.,90.,90.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBIS(7), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBIS(8), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBIS(7),1,'ASSI',0.,53.52,-19.217, IRM,'ONLY')
      CALL GSPOS(SBIS(8),1,'ASDI',0.,53.52,-19.217, IRM,'ONLY')
c Shifter, bent part 4 (short counter)
c     RMIN
      PAR3(1) = 2.88
c     RMAX
      PAR3(2) = 3.18
c     length (z)
      PAR3(3) = 1.3*0.5
c     phi starting
      PAR3(4) = 19.378
c     phi ending
      PAR3(5) = 90.0
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,0.,0.,90.,90.,90.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBIS(9), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBIS(10), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBIS(9),1,'ASSI',0.,55.62,-19.217, IRM,'ONLY')
      CALL GSPOS(SBIS(10),1,'ASDI',0.,55.62,-19.217, IRM,'ONLY')
c Shifter, straight part 1 (short counter, BISS-BISD)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 38.69*0.5      !total lenght straight part of shifter
      CALL GSVOLU('S5SI', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SSI5', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S5SI',1,'ASSI',0.,61.525,3.188, 0,'ONLY')
      CALL GSPOS('SSI5',1,'ASDI',0.,61.525,3.188, 0,'ONLY')
c
      PAR1(3) = 1.46909*0.5
      CALL GSVOLU(SBIS(11), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(12), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(11),1,'S5SI',0.,0.,18.61, 0,'MANY')
      CALL GSPOS(SBIS(12),1,'SSI5',0.,0.,18.61, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5
      CALL GSVOLU(SBIS(13), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(14), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(13),1,'S5SI',0.,0.,13.853, 0,'MANY')
      CALL GSPOS(SBIS(14),1,'SSI5',0.,0.,13.853, 0,'MANY')
c
      CALL GSVOLU(SBIS(15), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(16), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(15),1,'S5SI',0.,0.,5.809, 0,'MANY')
      CALL GSPOS(SBIS(16),1,'SSI5',0.,0.,5.809, 0,'MANY')
c
      CALL GSVOLU(SBIS(17), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(18), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(17),1,'S5SI',0.,0.,-2.234, 0,'MANY')
      CALL GSPOS(SBIS(18),1,'SSI5',0.,0.,-2.234, 0,'MANY')
c
      CALL GSVOLU(SBIS(19), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(20), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(19),1,'S5SI',0.,0.,-10.278, 0,'MANY')
      CALL GSPOS(SBIS(20),1,'SSI5',0.,0.,-10.278, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5-3.0*0.5
      CALL GSVOLU(SBIS(21), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(22), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(21),1,'S5SI',0.,0.,-16.822, 0,'MANY')
      CALL GSPOS(SBIS(22),1,'SSI5',0.,0.,-16.822, 0,'MANY')
c
c Shifter, straight part 2 (short counter)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 38.69*0.5      !total lenght straight part of shifter
      CALL GSVOLU('S6SI', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SSI6', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S6SI',1,'ASSI',0.,59.975,3.188, 0,'ONLY')
      CALL GSPOS('SSI6',1,'ASDI',0.,59.975,3.188, 0,'ONLY')
c
      PAR1(3) = 1.46909*0.5
      CALL GSVOLU(SBIS(23), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(24), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(23),1,'S6SI',0.,0.,18.61, 0,'MANY')
      CALL GSPOS(SBIS(24),1,'SSI6',0.,0.,18.61, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5
      CALL GSVOLU(SBIS(25), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(26), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(25),1,'S6SI',0.,0.,13.853, 0,'MANY')
      CALL GSPOS(SBIS(26),1,'SSI6',0.,0.,13.853, 0,'MANY')
c
      CALL GSVOLU(SBIS(27), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(28), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(27),1,'S6SI',0.,0.,5.809, 0,'MANY')
      CALL GSPOS(SBIS(28),1,'SSI6',0.,0.,5.809, 0,'MANY')
c
      CALL GSVOLU(SBIS(29), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(30), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(29),1,'S6SI',0.,0.,-2.234, 0,'MANY')
      CALL GSPOS(SBIS(30),1,'SSI6',0.,0.,-2.234, 0,'MANY')
c
      CALL GSVOLU(SBIS(31), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(32), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(31),1,'S6SI',0.,0.,-10.278, 0,'MANY')
      CALL GSPOS(SBIS(32),1,'SSI6',0.,0.,-10.278, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5-3.0*0.5
      CALL GSVOLU(SBIS(33), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(34), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(33),1,'S6SI',0.,0.,-16.822, 0,'MANY')
      CALL GSPOS(SBIS(34),1,'SSI6',0.,0.,-16.822, 0,'MANY')
c
c Shifter, straight part 3 (short counter)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 38.69*0.5      !total lenght straight part of shifter
      CALL GSVOLU('S7SI', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SSI7', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S7SI',1,'ASSI',0.,58.225,3.188, 0,'ONLY')
      CALL GSPOS('SSI7',1,'ASDI',0.,58.225,3.188, 0,'ONLY')
c
      PAR1(3) = 1.46909*0.5
      CALL GSVOLU(SBIS(35), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(36), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(35),1,'S7SI',0.,0.,18.61, 0,'MANY')
      CALL GSPOS(SBIS(36),1,'SSI7',0.,0.,18.61, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5
      CALL GSVOLU(SBIS(37), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(38), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(37),1,'S7SI',0.,0.,13.853, 0,'MANY')
      CALL GSPOS(SBIS(38),1,'SSI7',0.,0.,13.853, 0,'MANY')
c
      CALL GSVOLU(SBIS(39), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(40), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(39),1,'S7SI',0.,0.,5.809, 0,'MANY')
      CALL GSPOS(SBIS(40),1,'SSI7',0.,0.,5.809, 0,'MANY')
c
      CALL GSVOLU(SBIS(41), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(42), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(41),1,'S7SI',0.,0.,-2.234, 0,'MANY')
      CALL GSPOS(SBIS(42),1,'SSI7',0.,0.,-2.234, 0,'MANY')
c
      CALL GSVOLU(SBIS(43), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(44), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(43),1,'S7SI',0.,0.,-10.278, 0,'MANY')
      CALL GSPOS(SBIS(44),1,'SSI7',0.,0.,-10.278, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5-3.0*0.5
      CALL GSVOLU(SBIS(45), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(46), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(45),1,'S7SI',0.,0.,-16.822, 0,'MANY')
      CALL GSPOS(SBIS(46),1,'SSI7',0.,0.,-16.822, 0,'MANY')
c
c Shifter, straight part 4 (short counter)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
c      PAR1(3) = 38.69*0.5
      PAR1(3) = 38.69*0.5      !total lenght straight part of shifter
      CALL GSVOLU('S8SI', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SSI8', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S8SI',1,'ASSI',0.,56.675,3.188, 0,'ONLY')
      CALL GSPOS('SSI8',1,'ASDI',0.,56.675,3.188, 0,'ONLY')
c
      PAR1(3) = 1.46909*0.5
      CALL GSVOLU(SBIS(47), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(48), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(47),1,'S8SI',0.,0.,18.61, 0,'MANY')
      CALL GSPOS(SBIS(48),1,'SSI8',0.,0.,18.61, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5
      CALL GSVOLU(SBIS(49), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(50), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(49),1,'S8SI',0.,0.,13.853, 0,'MANY')
      CALL GSPOS(SBIS(50),1,'SSI8',0.,0.,13.853, 0,'MANY')
c
      CALL GSVOLU(SBIS(51), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(52), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(51),1,'S8SI',0.,0.,5.809, 0,'MANY')
      CALL GSPOS(SBIS(52),1,'SSI8',0.,0.,5.809, 0,'MANY')
c
      CALL GSVOLU(SBIS(53), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(54), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(53),1,'S8SI',0.,0.,-2.234, 0,'MANY')
      CALL GSPOS(SBIS(54),1,'SSI8',0.,0.,-2.234, 0,'MANY')
c
      CALL GSVOLU(SBIS(55), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(56), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(55),1,'S8SI',0.,0.,-10.278, 0,'MANY')
      CALL GSPOS(SBIS(56),1,'SSI8',0.,0.,-10.278, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5-3.0*0.5
      CALL GSVOLU(SBIS(57), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBIS(58), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBIS(57),1,'S8SI',0.,0.,-16.822, 0,'MANY')
      CALL GSPOS(SBIS(58),1,'SSI8',0.,0.,-16.822, 0,'MANY')
c
c Connector box teflon (short counter,left)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 4.45*0.5
c     PHI1 - starting angle
      PAR3(4) = 96.433
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('TCSI', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TSIC', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('TCSI',1,'ASSI',0.,0.,21.546, 0,'ONLY')
      CALL GSPOS('TSIC',1,'ASDI',0.,0.,21.546, 0,'ONLY')
c
c Connector box G10 (short counter,left)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 4.396*0.5
c     PHI1 - starting angle
      PAR3(4) = 96.485
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('GLSI', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSVOLU('GSIL', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSPOS('GLSI',1,'TCSI',0.,0.,0.027, 0,'ONLY')
      CALL GSPOS('GSIL',1,'TSIC',0.,0.,0.027, 0,'ONLY')
c
c Connector box volume (short counter,left)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 4.296*0.5
c     PHI1 - starting angle
      PAR3(4) = 96.582
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('VCSI', 'TUBS' , MEDELB, PAR3 , 5 , IVOL )
      CALL GSVOLU('VSIC', 'TUBS' , MEDELB, PAR3 , 5 , IVOL )
      CALL GSPOS('VCSI',1,'GLSI',0.,0.,0.05, 0,'ONLY')
      CALL GSPOS('VSIC',1,'GSIL',0.,0.,0.05, 0,'ONLY')
c
c Connector box teflon (short counter,right)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 4.45*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 83.567
      CALL GSVOLU('TRSI', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TSIR', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('TRSI',1,'ASSI',0.,0.,21.546, 0,'ONLY')
      CALL GSPOS('TSIR',1,'ASDI',0.,0.,21.546, 0,'ONLY')
c Connector box G10 (short counter,right)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 4.396*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 83.515
      CALL GSVOLU('GRSI', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSVOLU('GSIR', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSPOS('GRSI',1,'TRSI',0.,0.,0.027, 0,'ONLY')
      CALL GSPOS('GSIR',1,'TSIR',0.,0.,0.027, 0,'ONLY')
c
c Connector box volume (short counter,right)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 4.296*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 83.418
      CALL GSVOLU('VRSI', 'TUBS' , MEDELB, PAR3 , 5 , IVOL )
      CALL GSVOLU('VSIR', 'TUBS' , MEDELB, PAR3 , 5 , IVOL )
      CALL GSPOS('VRSI',1,'GRSI',0.,0.,0.05, 0,'ONLY')
      CALL GSPOS('VSIR',1,'GSIR',0.,0.,0.05, 0,'ONLY')
c Corner with air near shifter 1 (short counter)
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 180.
c     H1
      PAR4(4) = 6.3*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 6.3*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,155.,0.,90.,90.,65.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('A2SI', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ASI2', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('A2SI',1,'ASSI',-0.39055,59.1,22.877, IRM,'MANY')
      CALL GSPOS('ASI2',1,'ASDI',-0.39055,59.1,22.877, IRM,'MANY')
c Corner with air near shifter 2 (short counter)
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 6.3*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 6.3*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,25.,0.,90.,90.,65.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('A3SI', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ASI3', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('A3SI',1,'ASSI',0.39055,59.1,22.877, IRM,'MANY')
      CALL GSPOS('ASI3',1,'ASDI',0.39055,59.1,22.877, IRM,'MANY')
c
c Aluminum strap 1 - right (short counter)
c     R min
      PAR3(1) = 61.796
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 43.092*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 81.607
      CALL GSVOLU('D1SI', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DSI1', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D1SI',1,'ASSI',0.,0.,-2.225, 0,'ONLY')
      CALL GSPOS('DSI1',1,'ASDI',0.,0.,-2.225, 0,'ONLY')
c
c Aluminum strap 2 - left (short counter)
c     R min
      PAR3(1) = 61.796
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 43.092*0.5
c     PHI1 - starting angle
      PAR3(4) = 98.393
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('D2SI', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DSI2', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D2SI',1,'ASSI',0.,0.,-2.225, 0,'ONLY')
      CALL GSPOS('DSI2',1,'ASDI',0.,0.,-2.225, 0,'ONLY')
c
c Aluminum strap 3 - middle up (short counter)
c     R min
      PAR3(1) = 61.796
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.607
c     PHI2 - ending angle
      PAR3(5) = 98.393
      CALL GSVOLU('D3SI', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DSI3', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D3SI',1,'ASSI',0.,0.,-23.546, 0,'ONLY')
      CALL GSPOS('DSI3',1,'ASDI',0.,0.,-23.546, 0,'ONLY')
c
c Aluminum strap 4 - butt-end down (short counter)
c     R min
      PAR3(1) = 55.854
c     R max
      PAR3(2) = 56.404
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 83.567
c     PHI2 - ending angle
      PAR3(5) = 96.433
      CALL GSVOLU('D4SI', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DSI4', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D4SI',1,'ASSI',0.,0.,23.546, 0,'ONLY')
      CALL GSPOS('DSI4',1,'ASDI',0.,0.,23.546, 0,'ONLY')
c
c Aluminum strap 5 - butt-end up (short counter)
c     R min
      PAR3(1) = 61.796
c     R max
      PAR3(2) = 62.346
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 83.567
c     PHI2 - ending angle
      PAR3(5) = 96.433
      CALL GSVOLU('D5SI', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DSI5', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D5SI',1,'ASSI',0.,0.,23.546, 0,'ONLY')
      CALL GSPOS('DSI5',1,'ASDI',0.,0.,23.546, 0,'ONLY')
c=============================================================================
c     EndCap Aerogel Counter building outer layer
c     'ECSO' - 'standart' aerogel n=1.05, outer layer of ATC
c     'ECDO' - 'dense' aerogel n=1.054, outer layer of ATC
c
c  counter frame dimentions
c
c     narrow side (x1)
      PAR2(1) = 5.13*0.5
c     wide side   (x2)
      PAR2(2) = 21.52*0.5
c     height (y)
      PAR2(3) = 7.4*0.5
c     length (z)
      PAR2(4) = 51.76*0.5
      CALL GSVOLU('ECSO', 'TRD1' , MEDALF, PAR2 , 4 , IVOL )
      CALL GSVOLU('ECDO', 'TRD1' , MEDALF, PAR2 , 4 , IVOL )
c    Teflon reflecting film
c     narrow side (x1)
      PAR2(1) = PAR2(1)-0.1171
c     wide side   (x2)
      PAR2(2) = PAR2(2)-0.1171
c     height (y)
      PAR2(3) = PAR2(3)-0.1
c     length (z)
      PAR2(4) = PAR2(4)-0.1
      CALL GSVOLU('TESO', 'TRD1' , MEDTEF, PAR2 , 4 , IVOL )
      CALL GSVOLU('TEDO', 'TRD1' , MEDTEF, PAR2 , 4 , IVOL )
      CALL GSPOS('TESO',1,'ECSO',0.,0.,0., 0,'ONLY')
      CALL GSPOS('TEDO',1,'ECDO',0.,0.,0., 0,'ONLY')
c Aerogel
c     narrow side (x1)
      PAR2(1) = PAR2(1)-0.06323
c     wide side   (x2)
      PAR2(2) = PAR2(2)-0.06323
c     height (y)
      PAR2(3) = PAR2(3)-0.054
c     length (z)
      PAR2(4) = PAR2(4)-0.054
      ECACW1=PAR2(1)
      ECACW2=PAR2(2)
      ECACH=PAR2(3)
      ECACL=PAR2(4)
      CALL GSVOLU('AESO', 'TRD1' , MEDAERS, PAR2 , 4 , IVOL )
      CALL GSVOLU('AEDO', 'TRD1' , MEDAERD, PAR2 , 4 , IVOL )
      CALL GSPOS('AESO',1,'TESO',0.,0.,0., 0,'ONLY')
      CALL GSPOS('AEDO',1,'TEDO',0.,0.,0., 0,'ONLY')
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccc Разбиение аэрогеля на части - внешний торцевой счетчик  (ECSO, ECDO)    cccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     (z)
      PAR4(1) = 3.5
c     THET
      PAR4(2) = 4.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 1.85*0.5
c     TL1
      PAR4(6) = 1.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(1), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(1), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(1),'COLO',2)
      CALL GSATT (EDO(1),'COLO',2)
      CALL GSPOS(ESO(1),1,'AESO',1.355,0.,-21.9759, 0,'MANY')
      CALL GSPOS(EDO(1),1,'AEDO',1.355,0.,-21.9759, 0,'MANY')
c
c     (z)
      PAR4(1) = 3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(2), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(2), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(2),'COLO',2)
      CALL GSATT (EDO(2),'COLO',2)
      CALL GSPOS(ESO(2),1,'AESO',1.5749,0.,-14.638529, 0,'MANY')
      CALL GSPOS(EDO(2),1,'AEDO',1.5749,0.,-14.638529, 0,'MANY')
ccccc
c     (z)
      PAR4(1) = 3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(3), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(3), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(3),'COLO',2)
      CALL GSATT (EDO(3),'COLO',2)
      CALL GSPOS(ESO(3),1,'AESO',1.5749,0.,-6.963589, 0,'MANY')
      CALL GSPOS(EDO(3),1,'AEDO',1.5749,0.,-6.963589, 0,'MANY')
c
c     (z)
      PAR4(1) = 3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(4), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(4), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(4),'COLO',2)
      CALL GSATT (EDO(4),'COLO',2)
      CALL GSPOS(ESO(4),1,'AESO',1.5749,0.,0.711350, 0,'MANY')
      CALL GSPOS(EDO(4),1,'AEDO',1.5749,0.,0.711350, 0,'MANY')
c
c     (z)
      PAR4(1) = 3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(5), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(5), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(5),'COLO',2)
      CALL GSATT (EDO(5),'COLO',2)
      CALL GSPOS(ESO(5),1,'AESO',1.5749,0.,8.38629, 0,'MANY')
      CALL GSPOS(EDO(5),1,'AEDO',1.5749,0.,8.38629, 0,'MANY')
cccccccccccc
c
c     (z)
      PAR4(1) = 8.5
c     THET
      PAR4(2) = 4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 0.
c     TL1
      PAR4(6) = 0.
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(6), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(6), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(6),'COLO',2)
      CALL GSATT (EDO(6),'COLO',2)
      CALL GSPOS(ESO(6),1,'AESO',3.695,0.,-9.97589, 0,'MANY')
      CALL GSPOS(EDO(6),1,'AEDO',3.695,0.,-9.97589, 0,'MANY')
c
c     (z)
      PAR4(1) = 3.5
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(7), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(7), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(7),'COLO',2)
      CALL GSATT (EDO(7),'COLO',2)
      CALL GSPOS(ESO(7),1,'AESO',4.3824,0.,2.02411, 0,'MANY')
      CALL GSPOS(EDO(7),1,'AEDO',4.3824,0.,2.02411, 0,'MANY')
c
c     (z)
      PAR4(1) = 5.0
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(8), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(8), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(8),'COLO',2)
      CALL GSATT (EDO(8),'COLO',2)
      CALL GSPOS(ESO(8),1,'AESO',4.3824,0.,10.5241, 0,'MANY')
      CALL GSPOS(EDO(8),1,'AEDO',4.3824,0.,10.5241, 0,'MANY')
c
c     (z)
      PAR4(1) = 8.5
c     THET
      PAR4(2) = 4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 0.
c     TL1
      PAR4(6) = 0.
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(9), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(9), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(9),'COLO',2)
      CALL GSATT (EDO(9),'COLO',2)
      CALL GSPOS(ESO(9),1,'AESO',6.459,0.,7.02411, 0,'MANY')
      CALL GSPOS(EDO(9),1,'AEDO',6.459,0.,7.02411, 0,'MANY')
c
c     (z)
      PAR4(1) = 5.0
c     THET
      PAR4(2) = 4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 4.1*0.5
c     TL2
      PAR4(10) = 4.1*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(10), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(10), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(10),'COLO',2)
      CALL GSATT (EDO(10),'COLO',2)
      CALL GSPOS(ESO(10),1,'AESO',7.53,0.,20.4741, 0,'MANY')
      CALL GSPOS(EDO(10),1,'AEDO',7.53,0.,20.4741, 0,'MANY')
c
c     (z)
      PAR4(1) = 1.600
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 3.35*0.5
c     TL1
      PAR4(6) = 3.35*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 3.35*0.5
c     TL2
      PAR4(10) = 3.35*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(11), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(11), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(11),'COLO',2)
      CALL GSATT (EDO(11),'COLO',2)
      CALL GSPOS(ESO(11),1,'AESO',4.1949,0.,23.82, 0,'MANY')
      CALL GSPOS(EDO(11),1,'AEDO',4.1949,0.,23.82, 0,'MANY')
c
c     (z)
      PAR4(1) = 1.4
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 3.55*0.5
c     TL1
      PAR4(6) = 3.55*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 3.55*0.5
c     TL2
      PAR4(10) = 3.55*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(12), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(12), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(12),'COLO',2)
      CALL GSATT (EDO(12),'COLO',2)
      CALL GSPOS(ESO(12),1,'AESO',4.0849,0.,20.820, 0,'MANY')
      CALL GSPOS(EDO(12),1,'AEDO',4.0849,0.,20.820, 0,'MANY')
c
c     (z)
      PAR4(1) = 1.95
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 4.95*0.5
c     TL1
      PAR4(6) = 4.95*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 4.95*0.5
c     TL2
      PAR4(10) = 4.95*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(13), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(13), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(13),'COLO',2)
      CALL GSATT (EDO(13),'COLO',2)
      CALL GSPOS(ESO(13),1,'AESO',3.35,0.,17.475, 0,'MANY')
      CALL GSPOS(EDO(13),1,'AEDO',3.35,0.,17.475, 0,'MANY')
c
ccccccccccccc
c     (z)
      PAR4(1) = 1.65
c     THET
      PAR4(2) = 6.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) =  2.15*0.5
c     TL2
      PAR4(10) = 2.15*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(14), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(14), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(14),'COLO',2)
      CALL GSATT (EDO(14),'COLO',2)
      CALL GSPOS(ESO(14),1,'AESO',1.7849,0.,13.87381, 0,'MANY')
      CALL GSPOS(EDO(14),1,'AEDO',1.7849,0.,13.87381, 0,'MANY')
c
cccccccccccccccccccccccc зеркально ccccccccccccccccccccccccccccccccccccc
c
c     (z)
      PAR4(1) = 3.5
c     THET
      PAR4(2) = -4.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 1.85*0.5
c     TL1
      PAR4(6) = 1.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(15), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(15), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(15),'COLO',2)
      CALL GSATT (EDO(15),'COLO',2)
      CALL GSPOS(ESO(15),1,'AESO',-1.355,0.,-21.9759, 0,'MANY')
      CALL GSPOS(EDO(15),1,'AEDO',-1.355,0.,-21.9759, 0,'MANY')
ccccccccccccc
c     (z)
      PAR4(1) = 3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(16), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(16), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(16),'COLO',2)
      CALL GSATT (EDO(16),'COLO',2)
      CALL GSPOS(ESO(16),1,'AESO',-1.5749,0., -14.638529, 0,'MANY')
      CALL GSPOS(EDO(16),1,'AEDO',-1.5749,0., -14.638529, 0,'MANY')
c
c     (z)
      PAR4(1) =  3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(17), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(17), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(17),'COLO',2)
      CALL GSATT (EDO(17),'COLO',2)
      CALL GSPOS(ESO(17),1,'AESO',-1.5749,0.,-6.963589, 0,'MANY')
      CALL GSPOS(EDO(17),1,'AEDO',-1.5749,0.,-6.963589, 0,'MANY')
c
c     (z)
      PAR4(1) =  3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(18), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(18), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(18),'COLO',2)
      CALL GSATT (EDO(18),'COLO',2)
      CALL GSPOS(ESO(18),1,'AESO',-1.5749,0.,0.711350, 0,'MANY')
      CALL GSPOS(EDO(18),1,'AEDO',-1.5749,0.,0.711350, 0,'MANY')
c
c     (z)
      PAR4(1) =  3.83747
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.85*0.5
c     TL2
      PAR4(10) = 2.85*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(19), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(19), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(19),'COLO',2)
      CALL GSATT (EDO(19),'COLO',2)
      CALL GSPOS(ESO(19),1,'AESO',-1.5749,0.,8.38629, 0,'MANY')
      CALL GSPOS(EDO(19),1,'AEDO',-1.5749,0.,8.38629, 0,'MANY')
c
c     (z)
      PAR4(1) = 8.5
c     THET
      PAR4(2) = -4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 0.
c     TL1
      PAR4(6) = 0.
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(20), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(20), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(20),'COLO',2)
      CALL GSATT (EDO(20),'COLO',2)
      CALL GSPOS(ESO(20),1,'AESO',-3.695,0.,-9.97589, 0,'MANY')
      CALL GSPOS(EDO(20),1,'AEDO',-3.695,0.,-9.97589, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 3.5
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(21), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(21), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(21),'COLO',2)
      CALL GSATT (EDO(21),'COLO',2)
      CALL GSPOS(ESO(21),1,'AESO',-4.3824,0.,2.02411, 0,'MANY')
      CALL GSPOS(EDO(21),1,'AEDO',-4.3824,0.,2.02411, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 5.0
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(22), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(22), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(22),'COLO',2)
      CALL GSATT (EDO(22),'COLO',2)
      CALL GSPOS(ESO(22),1,'AESO',-4.3824,0.,10.5241, 0,'MANY')
      CALL GSPOS(EDO(22),1,'AEDO',-4.3824,0.,10.5241, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 8.5
c     THET
      PAR4(2) = -4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 0.
c     TL1
      PAR4(6) = 0.
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 2.765*0.5
c     TL2
      PAR4(10) = 2.765*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(23), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(23), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(23),'COLO',2)
      CALL GSATT (EDO(23),'COLO',2)
      CALL GSPOS(ESO(23),1,'AESO',-6.459,0.,7.02411, 0,'MANY')
      CALL GSPOS(EDO(23),1,'AEDO',-6.459,0.,7.02411, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 5.0
c     THET
      PAR4(2) = -4.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.765*0.5
c     TL1
      PAR4(6) = 2.765*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 4.1*0.5
c     TL2
      PAR4(10) = 4.1*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(24), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(24), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(24),'COLO',2)
      CALL GSATT (EDO(24),'COLO',2)
      CALL GSPOS(ESO(24),1,'AESO',-7.53,0.,20.4741, 0,'MANY')
      CALL GSPOS(EDO(24),1,'AEDO',-7.53,0.,20.4741, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 1.600
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 3.35*0.5
c     TL1
      PAR4(6) = 3.35*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 3.35*0.5
c     TL2
      PAR4(10) = 3.35*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(25), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(25), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(25),'COLO',2)
      CALL GSATT (EDO(25),'COLO',2)
      CALL GSPOS(ESO(25),1,'AESO',-4.1949,0.,23.82, 0,'MANY')
      CALL GSPOS(EDO(25),1,'AEDO',-4.1949,0.,23.82, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 1.4
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 3.55*0.5
c     TL1
      PAR4(6) = 3.55*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 3.55*0.5
c     TL2
      PAR4(10) = 3.55*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(26), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(26), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(26),'COLO',2)
      CALL GSATT (EDO(26),'COLO',2)
      CALL GSPOS(ESO(26),1,'AESO',-4.0849,0.,20.820, 0,'MANY')
      CALL GSPOS(EDO(26),1,'AEDO',-4.0849,0.,20.820, 0,'MANY')
c
cccccccccccc
c     (z)
      PAR4(1) = 1.95
c     THET
      PAR4(2) = 0.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 4.95*0.5
c     TL1
      PAR4(6) = 4.95*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 4.95*0.5
c     TL2
      PAR4(10) = 4.95*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(27), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(27), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(27),'COLO',2)
      CALL GSATT (EDO(27),'COLO',2)
      CALL GSPOS(ESO(27),1,'AESO',-3.35,0.,17.475, 0,'MANY')
      CALL GSPOS(EDO(27),1,'AEDO',-3.35,0.,17.475, 0,'MANY')
c
ccccccccccccc
c     (z)
      PAR4(1) = 1.65
c     THET
      PAR4(2) = -6.5
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.85*0.5
c     TL1
      PAR4(6) = 2.85*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) =  2.15*0.5
c     TL2
      PAR4(10) = 2.15*0.5
c     ALP2
      PAR4(11) = 0.
      CALL GSVOLU(ESO(28), 'TRAP' , MEDAERS, PAR4 , 11, IVOL )
      CALL GSVOLU(EDO(28), 'TRAP' , MEDAERD, PAR4 , 11, IVOL )
      CALL GSATT (ESO(28),'COLO',2)
      CALL GSATT (EDO(28),'COLO',2)
      CALL GSPOS(ESO(28),1,'AESO',-1.7849,0.,13.87381, 0,'MANY')
      CALL GSPOS(EDO(28),1,'AEDO',-1.7849,0.,13.87381, 0,'MANY')
c
c Shifter, main plate 1  (ECSO,ECDO)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 37.3*0.5
      CALL GSVOLU('SEO1', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SDO1', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('SEO1',1,'AESO',0.,-2.75,-6.49, 0,'ONLY')
      CALL GSPOS('SDO1',1,'AEDO',0.,-2.75,-6.49, 0,'ONLY')
c
      PAR1(3) = 6.700*0.5
      CALL GSVOLU(SEO(1), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(2), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(1),1,'SEO1',0.,0.,-15.29, 0,'MANY')
      CALL GSPOS(SEO(2),1,'SDO1',0.,0.,-15.29, 0,'MANY')
c
      PAR1(3) = 7.649*0.5
      CALL GSVOLU(SEO(3), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(4), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(3),1,'SEO1',0.,0.,-8.125, 0,'MANY')
      CALL GSPOS(SEO(4),1,'SDO1',0.,0.,-8.125, 0,'MANY')
c
      CALL GSVOLU(SEO(5), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(6), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(5),1,'SEO1',0.,0.,-0.476, 0,'MANY')
      CALL GSPOS(SEO(6),1,'SDO1',0.,0.,-0.476, 0,'MANY')
c
      CALL GSVOLU(SEO(7), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(8), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(7),1,'SEO1',0.,0.,7.172, 0,'MANY')
      CALL GSPOS(SEO(8),1,'SDO1',0.,0.,7.172, 0,'MANY')
c
      CALL GSVOLU(SEO(9), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(10), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(9),1,'SEO1',0.,0.,14.821, 0,'MANY')
      CALL GSPOS(SEO(10),1,'SDO1',0.,0.,14.821, 0,'MANY')
c
c Shifter, main plate 2
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 37.3*0.5
      CALL GSVOLU('SEO2', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SDO2', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('SEO2',1,'AESO',0.,-0.98,-6.49, 0,'ONLY')
      CALL GSPOS('SDO2',1,'AEDO',0.,-0.98,-6.49, 0,'ONLY')
c
      PAR1(3) = 6.700*0.5
      CALL GSVOLU(SEO(11), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(12), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(11),1,'SEO2',0.,0.,-15.29, 0,'MANY')
      CALL GSPOS(SEO(12),1,'SDO2',0.,0.,-15.29, 0,'MANY')
c
      PAR1(3) = 7.649*0.5
      CALL GSVOLU(SEO(13), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(14), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(13),1,'SEO2',0.,0.,-8.125, 0,'MANY')
      CALL GSPOS(SEO(14),1,'SDO2',0.,0.,-8.125, 0,'MANY')
c
      CALL GSVOLU(SEO(15), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(16), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(15),1,'SEO2',0.,0.,-0.476, 0,'MANY')
      CALL GSPOS(SEO(16),1,'SDO2',0.,0.,-0.476, 0,'MANY')
c
      CALL GSVOLU(SEO(17), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(18), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(17),1,'SEO2',0.,0.,7.172, 0,'MANY')
      CALL GSPOS(SEO(18),1,'SDO2',0.,0.,7.172, 0,'MANY')
c
      CALL GSVOLU(SEO(19), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(20), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(19),1,'SEO2',0.,0.,14.821, 0,'MANY')
      CALL GSPOS(SEO(20),1,'SDO2',0.,0.,14.821, 0,'MANY')
c
c Shifter, main plate 3
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 37.3*0.5
      CALL GSVOLU('SEO3', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SDO3', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('SEO3',1,'AESO',0.,0.98,-6.49, 0,'ONLY')
      CALL GSPOS('SDO3',1,'AEDO',0.,0.98,-6.49, 0,'ONLY')
c
      PAR1(3) = 6.700*0.5
      CALL GSVOLU(SEO(21), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(22), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(21),1,'SEO3',0.,0.,-15.29, 0,'MANY')
      CALL GSPOS(SEO(22),1,'SDO3',0.,0.,-15.29, 0,'MANY')
c
      PAR1(3) = 7.649*0.5
      CALL GSVOLU(SEO(23), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(24), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(23),1,'SEO3',0.,0.,-8.125, 0,'MANY')
      CALL GSPOS(SEO(24),1,'SDO3',0.,0.,-8.125, 0,'MANY')
c
      CALL GSVOLU(SEO(25), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(26), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(25),1,'SEO3',0.,0.,-0.476, 0,'MANY')
      CALL GSPOS(SEO(26),1,'SDO3',0.,0.,-0.476, 0,'MANY')
c
      CALL GSVOLU(SEO(27), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(28), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(27),1,'SEO3',0.,0.,7.172, 0,'MANY')
      CALL GSPOS(SEO(28),1,'SDO3',0.,0.,7.172, 0,'MANY')
c
      CALL GSVOLU(SEO(29), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(30), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(29),1,'SEO3',0.,0.,14.821, 0,'MANY')
      CALL GSPOS(SEO(30),1,'SDO3',0.,0.,14.821, 0,'MANY')
c
c Shifter, main plate 4
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 37.3*0.5
      CALL GSVOLU('SEO4', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SDO4', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('SEO4',1,'AESO',0.,2.75,-6.49, 0,'ONLY')
      CALL GSPOS('SDO4',1,'AEDO',0.,2.75,-6.49, 0,'ONLY')
c
      PAR1(3) = 6.700*0.5
      CALL GSVOLU(SEO(31), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(32), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(31),1,'SEO4',0.,0.,-15.29, 0,'MANY')
      CALL GSPOS(SEO(32),1,'SDO4',0.,0.,-15.29, 0,'MANY')
c
      PAR1(3) = 7.649*0.5
      CALL GSVOLU(SEO(33), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(34), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(33),1,'SEO4',0.,0.,-8.125, 0,'MANY')
      CALL GSPOS(SEO(34),1,'SDO4',0.,0.,-8.125, 0,'MANY')
c
      CALL GSVOLU(SEO(35), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(36), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(35),1,'SEO4',0.,0.,-0.476, 0,'MANY')
      CALL GSPOS(SEO(36),1,'SDO4',0.,0.,-0.476, 0,'MANY')
c
      CALL GSVOLU(SEO(37), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(38), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(37),1,'SEO4',0.,0.,7.172, 0,'MANY')
      CALL GSPOS(SEO(38),1,'SDO4',0.,0.,7.172, 0,'MANY')
c
      CALL GSVOLU(SEO(39), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(40), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(39),1,'SEO4',0.,0.,14.821, 0,'MANY')
      CALL GSPOS(SEO(40),1,'SDO4',0.,0.,14.821, 0,'MANY')
c
c Shifter, part near PMT
c     thickness (x)
      PAR1(1) = 1.7*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 1.2*0.5
      CALL GSVOLU(SEO(41), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SEO(42), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SEO(41),1,'AESO',0.,-0.85,21.56, 0,'ONLY')
      CALL GSPOS(SEO(42),1,'AEDO',0.,-0.85,21.56, 0,'ONLY')
c Shifter, bent part
c     RMIN
      PAR3(1) = 2.4
c     height  (y)
      PAR3(2) = 3.6
c     length (z)
      PAR3(3) = 1.7*0.5
c     phi starting
      PAR3(4) = 0.
c     phi ending
      PAR3(5) = 146.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,0.,0.,90.,90.,90.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SEO(43), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SEO(44), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SEO(43),1,'AESO',0.,-0.2,18.56, IRM,'ONLY')
      CALL GSPOS(SEO(44),1,'AEDO',0.,-0.2,18.56, IRM,'ONLY')
c Shifter, twisted part
c     wide part (x1)
      PAR3(1) = 1.7*0.5
c     narrow part  (x2)
      PAR3(2) = 0.3*0.5
c     narrow part (y1)
      PAR3(3) = 5.5*0.5
c     narrow part (y2)
      PAR3(4) = 6.8*0.5
c     length
      PAR3(5) = 3.4*0.5
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,90.,180.,90.,90.,180.,0.)
      CALL GSVOLU(SEO(45), 'TRD2' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SEO(46), 'TRD2' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SEO(45),1,'AESO',0.,0.,13.86, IRM,'ONLY')
      CALL GSPOS(SEO(46),1,'AEDO',0.,0.,13.86, IRM,'ONLY')
c MCP PMT frame
c     (x)
      PAR1(1) = 4.6*0.5
c     (y)
      PAR1(2) = 2.046*0.5
c     length (z)
      PAR1(3) = 3.95*0.5
      CALL GSVOLU('PLEO', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSVOLU('PLDO', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSPOS('PLEO',1,'AESO',0.,-2.523,21.435, 0,'ONLY')
      CALL GSPOS('PLDO',1,'AEDO',0.,-2.523,21.435, 0,'ONLY')
c Teflon reflecting film on electronic box 1
c     (x)
      PAR1(1) = 5.148*0.5
c     (y)
      PAR1(2) = 2.046*0.5
c     length (z)
      PAR1(3) = 2.316*0.5
      CALL GSVOLU('TEO1', 'BOX ' , MEDTEF, PAR1 , 3 , IVOL )
      CALL GSVOLU('TDO1', 'BOX ' , MEDTEF, PAR1 , 3 , IVOL )
      CALL GSPOS('TEO1',1,'AESO',0.,-2.523,24.568, 0,'ONLY')
      CALL GSPOS('TDO1',1,'AEDO',0.,-2.523,24.568, 0,'ONLY')
c Teflon reflecting film on electronic box 2
c     (x)
      PAR1(1) = 5.148*0.5
c     (y)
      PAR1(2) = 5.046*0.5
c     length (z)
      PAR1(3) = 3.566*0.5
      CALL GSVOLU('TEO2', 'BOX ' , MEDTEF, PAR1 , 3 , IVOL )
      CALL GSVOLU('TDO2', 'BOX ' , MEDTEF, PAR1 , 3 , IVOL )
      CALL GSPOS('TEO2',1,'AESO',0.,1.023,23.943, 0,'ONLY')
      CALL GSPOS('TDO2',1,'AEDO',0.,1.023,23.943, 0,'ONLY')
c Electronic box 1
c     (x)
      PAR1(1) = 5.148*0.5-0.054
c     (y)
      PAR1(2) = 2.046*0.5
c     length (z)
      PAR1(3) = 2.316*0.5
      CALL GSVOLU('EEO1', 'BOX ' , MEDELB, PAR1 , 3 , IVOL )
      CALL GSVOLU('EDO1', 'BOX ' , MEDELB, PAR1 , 3 , IVOL )
      CALL GSPOS('EEO1',1,'TEO1',0.,0.,0., 0,'ONLY')
      CALL GSPOS('EDO1',1,'TDO1',0.,0.,0., 0,'ONLY')
c Electronic box 2
c     (x)
      PAR1(1) = 5.148*0.5-0.054
c     (y)
      PAR1(2) = 5.046*0.5
c     length (z)
      PAR1(3) = 3.566*0.5-0.054*0.5
      CALL GSVOLU('EEO2', 'BOX ' , MEDELB, PAR1 , 3 , IVOL )
      CALL GSVOLU('EDO2', 'BOX ' , MEDELB, PAR1 , 3 , IVOL )
      CALL GSPOS('EEO2',1,'TEO2',0.,0.,0.027, 0,'ONLY')
      CALL GSPOS('EDO2',1,'TDO2',0.,0.,0.027, 0,'ONLY')
c Corner with air near shifter 1
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,155.,180.,90.,90.,115.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('AEO1', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ADO1', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('AEO1',1,'AESO',-0.39055,0.,-24.832, IRM,'ONLY')
      CALL GSPOS('ADO1',1,'AEDO',-0.39055,0.,-24.832, IRM,'ONLY')
c Corner with air near shifter 2
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 180.
c     H1
      PAR4(4) = 7.092*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 7.092*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,25.,180.,90.,90.,115.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('AEO2', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ADO2', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('AEO2',1,'AESO',0.39055,0.,-24.832, IRM,'ONLY')
      CALL GSPOS('ADO2',1,'AEDO',0.39055,0.,-24.832, IRM,'ONLY')
c Aluminium edge strap 1
c     DX
      PAR1(1) = 0.25
c     DY
      PAR1(2) = 0.25
c     DZ
      PAR1(3) = 50.8*0.5
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,99.,0.,90.,90.,9.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('ALEO', 'BOX ' , MEDALF, PAR1 , 3 , IVOL )
      CALL GSVOLU('AOLD', 'BOX ' , MEDALF, PAR1 , 3 , IVOL )
      CALL GSPOS('ALEO',1,'AESO',6.229,3.296,0., IRM,'ONLY')
      CALL GSPOS('AOLD',1,'AEDO',6.229,3.296,0., IRM,'ONLY')
c Aluminium edge strap 2
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,81.,0.,90.,90.,9.,180.)
c      CALL GPROTM(IRM)
      CALL GSPOS('ALEO',2,'AESO',-6.229,3.296,0., IRM,'ONLY')
      CALL GSPOS('AOLD',2,'AEDO',-6.229,3.296,0., IRM,'ONLY')
c Aluminium edge strap on wide side (1)
c     DX
      PAR1(1) = 7.3*0.5
c     DY
      PAR1(2) = 0.25
c     DZ
      PAR1(3) = 0.25
      CALL GSVOLU('AWEO', 'BOX ' , MEDALF, PAR1 , 3 , IVOL )
      CALL GSVOLU('AWDO', 'BOX ' , MEDALF, PAR1 , 3 , IVOL )
      CALL GSPOS('AWEO',1,'AESO',6.224,3.296,25.701, 0,'ONLY')
      CALL GSPOS('AWDO',1,'AEDO',6.224,3.296,25.701, 0,'ONLY')
c Aluminium edge strap on wide side (2)
      CALL GSPOS('AWEO',2,'AESO',-6.224,3.296,25.701, 0,'ONLY')
      CALL GSPOS('AWDO',2,'AEDO',-6.224,3.296,25.701, 0,'ONLY')
c Aluminium edge strap on wide side (3)
      CALL GSPOS('AWEO',3,'AESO',6.224,-3.296,25.701, 0,'ONLY')
      CALL GSPOS('AWDO',3,'AEDO',6.224,-3.296,25.701, 0,'ONLY')
c Aluminium edge strap on wide side (4)
      CALL GSPOS('AWEO',4,'AESO',-6.224,-3.296,25.701, 0,'ONLY')
      CALL GSPOS('AWDO',4,'AEDO',-6.224,-3.296,25.701, 0,'ONLY')

c===============================================================================
c     Barrel Aerogel Counter, outer layer building
c     long counter ('BOLS') with 'standart' aerogel n=1.05,
c     long counter ('BOLD') with 'dense' aerogel n=1.054,
c -------------------------------
c  long counter frame dimentions
c -------------------------------
c     R min
      PAR3(1) = 62.5
c     R max
      PAR3(2) = 69.8
c     length (z)
      PAR3(3) = 60.2*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.020
c     PHI2 - ending angle
      PAR3(5) = 98.980
      CALL GSVOLU( 'BOLS' , 'TUBS' , MEDALF, PAR3 , 5 , IVOL )
      CALL GSVOLU( 'BOLD' , 'TUBS' , MEDALF, PAR3 , 5 , IVOL )
c
c    Teflon reflecting film (long counter)
c     R min
      PAR3(1) = 62.6
c     R max
      PAR3(2) = 69.7
c     length (z)
      PAR3(3) = 60.05*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.108
c     PHI2 - ending angle
      PAR3(5) = 98.892
      CALL GSVOLU('TLSO', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TLDO', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('TLSO',1,'BOLS',0.,0.,0.025, 0,'ONLY')
      CALL GSPOS('TLDO',1,'BOLD',0.,0.,0.025, 0,'ONLY')
c
c Aerogel (long counter)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = (60.05-0.108)*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.155
c     PHI2 - ending angle
      PAR3(5) = 98.845
      BIR1=PAR3(1)
      BIR2=PAR3(2)
      BILL=PAR3(3)
      BIDF=PAR3(5)-PAR3(4)
      CALL GSVOLU('ALSO', 'TUBS' , MEDAERS, PAR3 , 5 , IVOL )
      CALL GSVOLU('ALDO', 'TUBS' , MEDAERD, PAR3 , 5 , IVOL )
      CALL GSPOS('ALSO',1,'TLSO',0.,0.,0., 0,'ONLY')
      CALL GSPOS('ALDO',1,'TLDO',0.,0.,0., 0,'ONLY')
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    Разбиение аэрогеля на 68 частей, длинный счетчик внешнего слоя (BOLS,BOLD)     ccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 8.742779*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.607
c     PHI2 - ending angle
      PAR3(5) = 82.709
c
      ZM=-25.5996105+0.55
      do i = 1,68
        IF ( i.EQ.1 ) THEN
 	PAR3(3) = 8.742779*0.5-4.45*0.5+0.55*0.5+(9.1229/4)*0.5+0.05
	PAR3(5) = 83.567
        ZM=-21.6842479-0.295
	ENDIF
        IF ( i.GT.1 ) THEN
      	PAR3(3) = 8.742779*0.5
        PAR3(5) = 82.709
        ENDIF
        IF ( i.EQ.2 ) THEN
        ZM=ZM-0.800
        ENDIF
c
        IF ( i.GT.6 ) THEN
      	PAR3(4) = 82.709
      	PAR3(5) = 85.6045
        ENDIF
c
        IF ( i.EQ.7 ) THEN
        PAR3(4) = 83.567
        ENDIF
c
        IF ( i.GT.12 ) THEN
      	PAR3(4) = 85.6045
      	PAR3(5) = 87.800
        ENDIF
        IF ( i.GT.18 ) THEN
      	PAR3(4) = 87.800
      	PAR3(5) = 89.100
        ENDIF
        IF ( i.GT.24 ) THEN
      	PAR3(4) = 89.100
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.GT.29 ) THEN
      	PAR3(4) = 90.200
      	PAR3(5) = 90.900
        ENDIF
        IF ( i.GT.34 ) THEN
      	PAR3(4) = 90.900
      	PAR3(5) = 92.200
        ENDIF
        IF ( i.GT.40 ) THEN
      	PAR3(4) = 92.200
      	PAR3(5) = 94.39549
        ENDIF
        IF ( i.GT.46 ) THEN
      	PAR3(4) = 94.39549
      	PAR3(5) = 97.291
        ENDIF
c
        IF ( i.EQ.47 ) THEN
        PAR3(5) = 96.433
	ENDIF
c
        IF ( i.GT.52 ) THEN
      	PAR3(4) = 97.291
        PAR3(5) = 98.393
        ENDIF
c
        IF ( i.EQ.53 ) THEN
 	PAR3(3) = 8.742779*0.5-4.45*0.5+0.55*0.5+(9.1229/4)*0.5+0.05
        PAR3(4) = 96.433
        ZM=-21.6842479-0.295
	ENDIF
        IF ( i.EQ.54 ) THEN
        ZM=ZM-0.800
        ENDIF
c
        IF ( i.EQ.59 ) THEN
 	PAR3(3) = 4.654*0.5-0.55*0.5
	PAR3(4) = 81.607
      	PAR3(5) = 82.709
        ENDIF
        IF ( i.EQ.60 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)*2/3
	PAR3(4) = 82.709
      	PAR3(5) = 86.212
        ENDIF
        IF ( i.EQ.61 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)/3
	PAR3(4) = 82.709
      	PAR3(5) = 86.212
        ENDIF
c
        IF ( i.EQ.62 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)*2/3
	PAR3(4) = 93.788
        PAR3(5) = 97.291
        ENDIF
        IF ( i.EQ.63 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)/3
	PAR3(4) = 93.788
        PAR3(5) = 97.291
        ENDIF
        IF ( i.EQ.64 ) THEN
 	PAR3(3) = 4.654*0.5-0.55*0.5
	PAR3(4) = 97.291
        PAR3(5) = 98.393
        ENDIF
c
        IF ( i.EQ.65 ) THEN
 	PAR3(3) = 8.742779*0.5-3.0*0.5
	PAR3(4) = 89.100
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.EQ.66 ) THEN
 	PAR3(3) = 8.742779*0.5-3.0*0.5
	PAR3(4) = 90.200
      	PAR3(5) = 90.900
        ENDIF
c
        IF ( i.EQ.67 ) THEN
 	PAR3(3) = (9.1229/4+0.12)*0.5
	PAR3(4) = 83.567
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.EQ.68 ) THEN
 	PAR3(3) = (9.1229/4+0.12)*0.5
	PAR3(4) = 90.200
      	PAR3(5) = 96.433
        ENDIF
c
      CALL GSVOLU(ARLSO(i), 'TUBS' , MEDAERS, PAR3 , 5 , IVOL )
      CALL GSVOLU(ARLDO(i), 'TUBS' , MEDAERD, PAR3 , 5 , IVOL )
      CALL GSATT (ARLSO(i),'COLO',4)
      CALL GSATT (ARLDO(i),'COLO',4)
      CALL GSPOS(ARLSO(i),i,'ALSO',0.,0.,ZM, 0,'MANY')
      CALL GSPOS(ARLDO(i),i,'ALDO',0.,0.,ZM, 0,'MANY')
      ZM=ZM+8.742779
      IF ( i.EQ.6 ) THEN
      ZM=ZM-i*8.742779
      ENDIF
      IF ( i.EQ.12 ) THEN
      ZM=ZM-(i-6)*8.742779
      ENDIF
      IF ( i.EQ.18 ) THEN
      ZM=ZM-(i-12)*8.742779
      ENDIF
      IF ( i.EQ.24 ) THEN
      ZM=ZM-(i-18)*8.742779
      ENDIF
      IF ( i.EQ.29 ) THEN
      ZM=ZM-(i-24)*8.742779
      ENDIF
      IF ( i.EQ.34 ) THEN
      ZM=ZM-(i-29)*8.742779
      ENDIF
      IF ( i.EQ.40 ) THEN
      ZM=ZM-(i-34)*8.742779
      ENDIF
      IF ( i.EQ.46 ) THEN
      ZM=ZM-(i-40)*8.742779
      ENDIF
      IF ( i.EQ.52 ) THEN
      ZM=ZM-(i-46)*8.742779
      ENDIF
c
      IF ( i.EQ.58 ) THEN
      ZM=ZM-8.742779*0.5+4.654*0.5-0.55*0.5
      ENDIF
      IF ( i.EQ.59 ) THEN
      ZM=ZM-8.742779-(4.654*0.5-0.55*0.5)/3
      ENDIF
      IF ( i.EQ.60 ) THEN
      ZM=ZM-8.742779+(4.654*0.5-0.55*0.5)
      ENDIF
c
      IF ( i.EQ.61 ) THEN
      ZM=ZM-8.742779-(4.654*0.5-0.55*0.5)
      ENDIF
      IF ( i.EQ.62 ) THEN
      ZM=ZM-8.742779+(4.654*0.5-0.55*0.5)
      ENDIF
      IF ( i.EQ.63 ) THEN
      ZM=ZM-8.742779*0.5-4.654+0.255-(4.654*0.5-0.55*0.5)*2/3
      ENDIF
c
      IF ( i.EQ.64 ) THEN
      ZM=ZM-8.742779-4.654*0.5-8.742779*0.5-2.5*0.5
      ENDIF
      IF ( i.EQ.65 ) THEN
      ZM=ZM-8.742779
      ENDIF
c
      IF ( i.EQ.66 ) THEN
      ZM=ZM-8.742779*5-8.742779-3.0-(9.1229/4)*0.5+0.15*0.5+0.08
      ENDIF
      IF ( i.EQ.67 ) THEN
      ZM=ZM-8.74277
      ENDIF
c
      end do
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Electronic box teflon (long counter)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.654*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.212
c     PHI2 - ending angle
      PAR3(5) = 93.788
      CALL GSVOLU('T1LO', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TLO1', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('T1LO',1,'ALSO',0.,0.,27.644, 0,'MANY')
      CALL GSPOS('TLO1',1,'ALDO',0.,0.,27.644, 0,'MANY')
c Electronic box G10 walls (long counter)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.6*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.265
c     PHI2 - ending angle
      PAR3(5) = 93.735
      CALL GSVOLU('G1LO', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSVOLU('GLO1', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSPOS('G1LO',1,'T1LO',0.,0.,0.027, 0,'MANY')
      CALL GSPOS('GLO1',1,'TLO1',0.,0.,0.027, 0,'MANY')
c Electronic box internal air (long counter)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.313
c     PHI2 - ending angle
      PAR3(5) = 93.687
      CALL GSVOLU('A1LO', 'TUBS' , MEDAIF, PAR3 , 5 , IVOL )
      CALL GSVOLU('ALO1', 'TUBS' , MEDAIF, PAR3 , 5 , IVOL )
      CALL GSPOS('A1LO',1,'G1LO',0.,0.,0.025, 0,'MANY')
      CALL GSPOS('ALO1',1,'GLO1',0.,0.,0.025, 0,'MANY')
c Preamplifier (long counter)
c     (x)
      PAR1(1) = 0.4*0.5
c     (y)
      PAR1(2) = 2.5*0.5
c     length (z)
      PAR1(3) = 4.0*0.5
      CALL GSVOLU('GALO', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSVOLU('GLOA', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSPOS('GALO',1,'A1LO',-3.2,67.7,0., 0,'ONLY')
      CALL GSPOS('GLOA',1,'ALO1',-3.2,67.7,0., 0,'ONLY')
c HV devider (long counter)
c     (x)
      PAR1(1) = 0.8*0.5
c     (y)
      PAR1(2) = 1.5*0.5
c     length (z)
      PAR1(3) = 4.0*0.5
      CALL GSVOLU('GHLO', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSVOLU('GLOH', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSPOS('GHLO',1,'A1LO',3.2,68.2,0., 0,'ONLY')
      CALL GSPOS('GLOH',1,'ALO1',3.2,68.2,0., 0,'ONLY')
c MCP PMT frame (long counter)
c     (x)
      PAR1(1) = 5.3*0.5
c     (y)
      PAR1(2) = 4.0*0.5
c     length (z)
      PAR1(3) = 2.7*0.5
      CALL GSVOLU('PFLO', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSVOLU('PLOF', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSPOS('PFLO',1,'A1LO',0.,66.4,0.925, 0,'MANY')
      CALL GSPOS('PLOF',1,'ALO1',0.,66.4,0.925, 0,'MANY')
c WLS clamp (long counter)
c     (x)
      PAR1(1) = 3.6*0.5
c     (y)
      PAR1(2) = 3.0*0.5
c     length (z)
      PAR1(3) = 1.0*0.5
      CALL GSVOLU('DCLO', 'BOX ' , MEDD16T, PAR1 , 3 , IVOL )
      CALL GSVOLU('DLOC', 'BOX ' , MEDD16T, PAR1 , 3 , IVOL )
      CALL GSPOS('DCLO',1,'A1LO',0.,65.95,-1.025, 0,'MANY')
      CALL GSPOS('DLOC',1,'ALO1',0.,65.95,-1.025, 0,'MANY')
c Shifter, part near PMT (long counter, BOLS-BOLD)
c     thickness (x)
      PAR1(1) = 1.7*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 2.704*0.5
      CALL GSVOLU(SBOL(1), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(2), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(1),1,'ALSO',0.,66.4,26.769, 0,'ONLY')
      CALL GSPOS(SBOL(2),1,'ALDO',0.,66.4,26.769, 0,'ONLY')
c Shifter, bent part 1 (long counter)
c     RMIN
      PAR3(1) = 2.88
c     RMAX
      PAR3(2) = 3.18
c     length (z)
      PAR3(3) = 1.3*0.5
c     phi starting
      PAR3(4) = -90.0
c     phi ending
      PAR3(5) = -19.378
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,180.,0.,90.,90.,90.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBOL(3), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBOL(4), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBOL(3),1,'ALSO',0.,69.88,25.417, IRM,'ONLY')
      CALL GSPOS(SBOL(4),1,'ALDO',0.,69.88,25.417, IRM,'ONLY')
c Shifter, bent part 2 (long counter)
c     RMIN
      PAR3(1) = 5.28
c     RMAX
      PAR3(2) = 5.58
c     length (z)
      PAR3(3) = 1.7*0.5
c     phi starting
      PAR3(4) = -90.0
c     phi ending
      PAR3(5) = -57.48
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,180.,0.,90.,90.,90.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBOL(5), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBOL(6), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBOL(5),1,'ALSO',0.,71.98,25.417, IRM,'ONLY')
      CALL GSPOS(SBOL(6),1,'ALDO',0.,71.98,25.417, IRM,'ONLY')
c Shifter, bent part 3 (long counter)
c     RMIN
      PAR3(1) = 5.28
c     RMAX
      PAR3(2) = 5.58
c     length (z)
      PAR3(3) = 1.7*0.5
c     phi starting
      PAR3(4) = 57.48
c     phi ending
      PAR3(5) = 90.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,180.,0.,90.,90.,90.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBOL(7), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBOL(8), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBOL(7),1,'ALSO',0.,60.82,25.417, IRM,'ONLY')
      CALL GSPOS(SBOL(8),1,'ALDO',0.,60.82,25.417, IRM,'ONLY')
c Shifter, bent part 4 (long counter)
c     RMIN
      PAR3(1) = 2.88
c     RMAX
      PAR3(2) = 3.18
c     length (z)
      PAR3(3) = 1.3*0.5
c     phi starting
      PAR3(4) = 19.378
c     phi ending
      PAR3(5) = 90.0
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,180.,0.,90.,90.,90.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBOL(9), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBOL(10), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBOL(9),1,'ALSO',0.,62.92,25.417, IRM,'ONLY')
      CALL GSPOS(SBOL(10),1,'ALDO',0.,62.92,25.417, IRM,'ONLY')
c Shifter, straight part 1 (long counter,BOLS-BOLD)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 51.09*0.5          !total lenght
      CALL GSVOLU('S5LO', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SLO5', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S5LO',1,'ALSO',0.,68.825,-3.188, 0,'ONLY')
      CALL GSPOS('SLO5',1,'ALDO',0.,68.825,-3.188, 0,'ONLY')
c
      PAR1(3) = 1.63338*0.5
      CALL GSVOLU(SBOL(11), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(12), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(11),1,'S5LO',0.,0.,-24.728, 0,'MANY')
      CALL GSPOS(SBOL(12),1,'SLO5',0.,0.,-24.728, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5
      CALL GSVOLU(SBOL(13), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(14), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(13),1,'S5LO',0.,0.,-19.54, 0,'MANY')
      CALL GSPOS(SBOL(14),1,'SLO5',0.,0.,-19.54, 0,'MANY')
c
      CALL GSVOLU(SBOL(15), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(16), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(15),1,'S5LO',0.,0.,-10.797, 0,'MANY')
      CALL GSPOS(SBOL(16),1,'SLO5',0.,0.,-10.797, 0,'MANY')
c
      CALL GSVOLU(SBOL(17), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(18), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(17),1,'S5LO',0.,0.,-2.054, 0,'MANY')
      CALL GSPOS(SBOL(18),1,'SLO5',0.,0.,-2.054, 0,'MANY')
c
      CALL GSVOLU(SBOL(19), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(20), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(19),1,'S5LO',0.,0.,6.688, 0,'MANY')
      CALL GSPOS(SBOL(20),1,'SLO5',0.,0.,6.688, 0,'MANY')
c
      CALL GSVOLU(SBOL(21), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(22), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(21),1,'S5LO',0.,0.,15.43, 0,'MANY')
      CALL GSPOS(SBOL(22),1,'SLO5',0.,0.,15.43, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5-3.0*0.5
      CALL GSVOLU(SBOL(23), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(24), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(23),1,'S5LO',0.,0.,22.673, 0,'MANY')
      CALL GSPOS(SBOL(24),1,'SLO5',0.,0.,22.673, 0,'MANY')
c
c Shifter, straight part 2 (long counter)      !Шифтер - прямая часть 2
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 51.09*0.5          !total lenght
      CALL GSVOLU('S6LO', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SLO6', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S6LO',1,'ALSO',0.,67.275,-3.188, 0,'ONLY')
      CALL GSPOS('SLO6',1,'ALDO',0.,67.275,-3.188, 0,'ONLY')
c
      PAR1(3) = 1.63338*0.5
      CALL GSVOLU(SBOL(25), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(26), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(25),1,'S6LO',0.,0.,-24.728, 0,'MANY')
      CALL GSPOS(SBOL(26),1,'SLO6',0.,0.,-24.728, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5
      CALL GSVOLU(SBOL(27), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(28), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(27),1,'S6LO',0.,0.,-19.54, 0,'MANY')
      CALL GSPOS(SBOL(28),1,'SLO6',0.,0.,-19.54, 0,'MANY')
c
      CALL GSVOLU(SBOL(29), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(30), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(29),1,'S6LO',0.,0.,-10.797, 0,'MANY')
      CALL GSPOS(SBOL(30),1,'SLO6',0.,0.,-10.797, 0,'MANY')
c
      CALL GSVOLU(SBOL(31), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(32), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(31),1,'S6LO',0.,0.,-2.054, 0,'MANY')
      CALL GSPOS(SBOL(32),1,'SLO6',0.,0.,-2.054, 0,'MANY')
c
      CALL GSVOLU(SBOL(33), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(34), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(33),1,'S6LO',0.,0.,6.688, 0,'MANY')
      CALL GSPOS(SBOL(34),1,'SLO6',0.,0.,6.688, 0,'MANY')
c
      CALL GSVOLU(SBOL(35), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(36), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(35),1,'S6LO',0.,0.,15.43, 0,'MANY')
      CALL GSPOS(SBOL(36),1,'SLO6',0.,0.,15.43, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5-3.0*0.5
      CALL GSVOLU(SBOL(37), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(38), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(37),1,'S6LO',0.,0.,22.673, 0,'MANY')
      CALL GSPOS(SBOL(38),1,'SLO6',0.,0.,22.673, 0,'MANY')
c
c Shifter, straight part 3 (long counter)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 51.09*0.5          !total lenght
      CALL GSVOLU('S7LO', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SLO7', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S7LO',1,'ALSO',0.,65.525,-3.188, 0,'ONLY')
      CALL GSPOS('SLO7',1,'ALDO',0.,65.525,-3.188, 0,'ONLY')
c
      PAR1(3) = 1.63338*0.5
      CALL GSVOLU(SBOL(39), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(40), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(39),1,'S7LO',0.,0.,-24.728, 0,'MANY')
      CALL GSPOS(SBOL(40),1,'SLO7',0.,0.,-24.728, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5
      CALL GSVOLU(SBOL(41), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(42), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(41),1,'S7LO',0.,0.,-19.54, 0,'MANY')
      CALL GSPOS(SBOL(42),1,'SLO7',0.,0.,-19.54, 0,'MANY')
c
      CALL GSVOLU(SBOL(43), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(44), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(43),1,'S7LO',0.,0.,-10.797, 0,'MANY')
      CALL GSPOS(SBOL(44),1,'SLO7',0.,0.,-10.797, 0,'MANY')
c
      CALL GSVOLU(SBOL(45), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(46), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(45),1,'S7LO',0.,0.,-2.054, 0,'MANY')
      CALL GSPOS(SBOL(46),1,'SLO7',0.,0.,-2.054, 0,'MANY')
c
      CALL GSVOLU(SBOL(47), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(48), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(47),1,'S7LO',0.,0.,6.688, 0,'MANY')
      CALL GSPOS(SBOL(48),1,'SLO7',0.,0.,6.688, 0,'MANY')
c
      CALL GSVOLU(SBOL(49), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(50), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(49),1,'S7LO',0.,0.,15.43, 0,'MANY')
      CALL GSPOS(SBOL(50),1,'SLO7',0.,0.,15.43, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5-3.0*0.5
      CALL GSVOLU(SBOL(51), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(52), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(51),1,'S7LO',0.,0.,22.673, 0,'MANY')
      CALL GSPOS(SBOL(52),1,'SLO7',0.,0.,22.673, 0,'MANY')
c
c Shifter, straight part 4 (long counter)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 51.09*0.5          !total lenght
      CALL GSVOLU('S8LO', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SLO8', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S8LO',1,'ALSO',0.,63.975,-3.188, 0,'ONLY')
      CALL GSPOS('SLO8',1,'ALDO',0.,63.975,-3.188, 0,'ONLY')
c
      PAR1(3) = 1.63338*0.5
      CALL GSVOLU(SBOL(53), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(54), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(53),1,'S8LO',0.,0.,-24.728, 0,'MANY')
      CALL GSPOS(SBOL(54),1,'SLO8',0.,0.,-24.728, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5
      CALL GSVOLU(SBOL(55), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(56), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(55),1,'S8LO',0.,0.,-19.54, 0,'MANY')
      CALL GSPOS(SBOL(56),1,'SLO8',0.,0.,-19.54, 0,'MANY')
c
      CALL GSVOLU(SBOL(57), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(58), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(57),1,'S8LO',0.,0.,-10.797, 0,'MANY')
      CALL GSPOS(SBOL(58),1,'SLO8',0.,0.,-10.797, 0,'MANY')
c
      CALL GSVOLU(SBOL(59), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(60), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(59),1,'S8LO',0.,0.,-2.054, 0,'MANY')
      CALL GSPOS(SBOL(60),1,'SLO8',0.,0.,-2.054, 0,'MANY')
c
      CALL GSVOLU(SBOL(61), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(62), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(61),1,'S8LO',0.,0.,6.688, 0,'MANY')
      CALL GSPOS(SBOL(62),1,'SLO8',0.,0.,6.688, 0,'MANY')
c
      CALL GSVOLU(SBOL(63), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(64), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(63),1,'S8LO',0.,0.,15.43, 0,'MANY')
      CALL GSPOS(SBOL(64),1,'SLO8',0.,0.,15.43, 0,'MANY')
c
      PAR1(3) = 8.74277*0.5-3.0*0.5
      CALL GSVOLU(SBOL(65), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOL(66), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOL(65),1,'S8LO',0.,0.,22.673, 0,'MANY')
      CALL GSPOS(SBOL(66),1,'SLO8',0.,0.,22.673, 0,'MANY')
c
c Connector box teflon (short counter,left)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.45*0.5
c     PHI1 - starting angle
      PAR3(4) = 96.433
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('TCLO', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TLOC', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('TCLO',1,'ALSO',0.,0.,-27.875, 0,'ONLY')
      CALL GSPOS('TLOC',1,'ALDO',0.,0.,-27.875, 0,'ONLY')
c
c Connector box G10 (short counter,left)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.396*0.5
c     PHI1 - starting angle
      PAR3(4) = 96.485
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('GLLO', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSVOLU('GLOL', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSPOS('GLLO',1,'TCLO',0.,0.,0.027, 0,'ONLY')
      CALL GSPOS('GLOL',1,'TLOC',0.,0.,0.027, 0,'ONLY')
c
c Connector box volume (short counter,left)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.296*0.5
c     PHI1 - starting angle
      PAR3(4) = 96.582
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('VCLO', 'TUBS' , MEDELB, PAR3 , 5 , IVOL )
      CALL GSVOLU('VLOC', 'TUBS' , MEDELB, PAR3 , 5 , IVOL )
      CALL GSPOS('VCLO',1,'GLLO',0.,0.,0.05, 0,'ONLY')
      CALL GSPOS('VLOC',1,'GLOL',0.,0.,0.05, 0,'ONLY')
c
c Connector box teflon (short counter,right)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.45*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 83.567
      CALL GSVOLU('TRLO', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TLOR', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('TRLO',1,'ALSO',0.,0.,-27.875, 0,'ONLY')
      CALL GSPOS('TLOR',1,'ALDO',0.,0.,-27.875, 0,'ONLY')
c Connector box G10 (short counter,right)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.396*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 83.515
      CALL GSVOLU('GRLO', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSVOLU('GLOR', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSPOS('GRLO',1,'TRLO',0.,0.,0.027, 0,'ONLY')
      CALL GSPOS('GLOR',1,'TLOR',0.,0.,0.027, 0,'ONLY')
c
c Connector box volume (short counter,right)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.296*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 83.418
      CALL GSVOLU('VRLO', 'TUBS' , MEDELB, PAR3 , 5 , IVOL )
      CALL GSVOLU('VLOR', 'TUBS' , MEDELB, PAR3 , 5 , IVOL )
      CALL GSPOS('VRLO',1,'GRLO',0.,0.,0.05, 0,'ONLY')
      CALL GSPOS('VLOR',1,'GLOR',0.,0.,0.05, 0,'ONLY')

c Corner with air near shifter 1 (long counter)
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 6.3*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 6.3*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,155.,180.,90.,90.,115.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('A2LO', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ALO2', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('A2LO',1,'ALSO',-0.39055,66.4,-29.077, IRM,'MANY')
      CALL GSPOS('ALO2',1,'ALDO',-0.39055,66.4,-29.077, IRM,'MANY')
c Corner with air near shifter 2 (long counter)
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 180.
c     H1
      PAR4(4) = 6.3*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 6.3*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,25.,180.,90.,90.,115.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('A3LO', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ALO3', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('A3LO',1,'ALSO',0.39055,66.4,-29.077, IRM,'MANY')
      CALL GSPOS('ALO3',1,'ALDO',0.39055,66.4,-29.077, IRM,'MANY')
c
c Aluminum strap 1 - right (long counter)
c     R min
      PAR3(1) = 69.096
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 55.492*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 81.607
      CALL GSVOLU('D1LO', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DLO1', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D1LO',1,'ALSO',0.,0.,2.225, 0,'ONLY')
      CALL GSPOS('DLO1',1,'ALDO',0.,0.,2.225, 0,'ONLY')
c
c Aluminum strap 2 - left (long counter)
c     R min
      PAR3(1) = 69.096
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 55.492*0.5
c     PHI1 - starting angle
      PAR3(4) = 98.393
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('D2LO', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DLO2', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D2LO',1,'ALSO',0.,0.,2.225, 0,'ONLY')
      CALL GSPOS('DLO2',1,'ALDO',0.,0.,2.225, 0,'ONLY')
c
c Aluminum strap 3 - butt-end down (long counter)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 63.204
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 83.567
c     PHI2 - ending angle
      PAR3(5) = 96.433
      CALL GSVOLU('D3LO', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DLO3', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D3LO',1,'ALSO',0.,0.,-29.746, 0,'ONLY')
      CALL GSPOS('DLO3',1,'ALDO',0.,0.,-29.746, 0,'ONLY')
c
c Aluminum strap 4 - middle down (long counter)
c     R min
c      PAR3(1) = 62.654
c     R max
c      PAR3(2) = 63.204
c     length (z)
c      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
c      PAR3(4) = 81.192
c     PHI2 - ending angle
c      PAR3(5) = 98.808
c      CALL GSVOLU('D4LO', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
c      CALL GSVOLU('DLO4', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
c      CALL GSPOS('D4LO',1,'ALSO',0.,0.,29.746, 0,'ONLY')
c      CALL GSPOS('DLO4',1,'ALDO',0.,0.,29.746, 0,'ONLY')
c
c Aluminum strap 5 - butt-end up (long counter)
c     R min
      PAR3(1) = 69.096
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 83.567
c     PHI2 - ending angle
      PAR3(5) = 96.433
      CALL GSVOLU('D5LO', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DLO5', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D5LO',1,'ALSO',0.,0.,-29.746, 0,'ONLY')
      CALL GSPOS('DLO5',1,'ALDO',0.,0.,-29.746, 0,'ONLY')
c
c Aluminum strap 6 - middle up (long counter)
c     R min
      PAR3(1) = 69.096
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.607
c     PHI2 - ending angle
      PAR3(5) = 98.393
      CALL GSVOLU('D6LO', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DLO6', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D6LO',1,'ALSO',0.,0.,29.746, 0,'ONLY')
      CALL GSPOS('DLO6',1,'ALDO',0.,0.,29.746, 0,'ONLY')
c==============================================================================
c-----------------------------------------------------
c     Barrel Aerogel Counter, outer layer building
c     short counter ('BOSS') with 'standart' aerogel n=1.05,
c     short counter ('BOSD') with 'dense' aerogel n=1.054
c --------------------------------
c  short counter frame dimentions
c --------------------------------
c     R min
      PAR3(1) = 62.5
c     R max
      PAR3(2) = 69.8
c     length (z)
      PAR3(3) = 47.8*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.020
c     PHI2 - ending angle
      PAR3(5) = 98.980
      CALL GSVOLU( 'BOSS' , 'TUBS' , MEDALF, PAR3 , 5 , IVOL )
      CALL GSVOLU( 'BOSD' , 'TUBS' , MEDALF, PAR3 , 5 , IVOL )
c
c    Teflon reflecting film (short counter)
c     R min
      PAR3(1) = 62.6
c     R max
      PAR3(2) = 69.7
c     length (z)
      PAR3(3) = 47.65*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.108
c     PHI2 - ending angle
      PAR3(5) = 98.892
      CALL GSVOLU('TSSO', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TSDO', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('TSSO',1,'BOSS',0.,0.,-0.025, 0,'ONLY')
      CALL GSPOS('TSDO',1,'BOSD',0.,0.,-0.025, 0,'ONLY')
c
c Aerogel (short counter)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = (47.65-0.108)*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.155
c     PHI2 - ending angle
      PAR3(5) = 98.845
      BILS=PAR3(3)
      CALL GSVOLU('ASSO', 'TUBS' , MEDAERS, PAR3 , 5 , IVOL )
      CALL GSVOLU('ASDO', 'TUBS' , MEDAERD, PAR3 , 5 , IVOL )
      CALL GSPOS('ASSO',1,'TSSO',0.,0.,0., 0,'ONLY')
      CALL GSPOS('ASDO',1,'TSDO',0.,0.,0., 0,'ONLY')
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    Разбиение аэрогеля на 58 частей, длинный счетчик внутреннего слоя  (BOSS,BOSD)  ccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 8.044182*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.607
c     PHI2 - ending angle
      PAR3(5) = 82.709
c
      ZM=17.42032-0.38
      do i = 1,58
        IF ( i.GT.5 ) THEN
      	PAR3(4) = 82.709
      	PAR3(5) = 85.6045
        ENDIF
        IF ( i.GT.10 ) THEN
      	PAR3(4) = 85.6045
      	PAR3(5) = 87.800
        ENDIF
        IF ( i.GT.15 ) THEN
      	PAR3(4) = 87.800
      	PAR3(5) = 89.100
        ENDIF
        IF ( i.GT.20 ) THEN
      	PAR3(4) = 89.100
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.GT.24 ) THEN
      	PAR3(4) = 90.200
      	PAR3(5) = 90.900
        ENDIF
        IF ( i.GT.28 ) THEN
      	PAR3(4) = 90.900
      	PAR3(5) = 92.200
        ENDIF
        IF ( i.GT.33 ) THEN
      	PAR3(4) = 92.200
      	PAR3(5) = 94.39549
        ENDIF
        IF ( i.GT.38 ) THEN
      	PAR3(4) = 94.39549
      	PAR3(5) = 97.291
        ENDIF
        IF ( i.GT.43 ) THEN
      	PAR3(4) = 97.291
        PAR3(5) = 98.393
        ENDIF
c
        IF ( i.EQ.49 ) THEN
 	PAR3(3) = 4.654*0.5-0.55*0.5
	PAR3(4) = 81.607
      	PAR3(5) = 82.709
        ENDIF
        IF ( i.EQ.50 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)*2/3
	PAR3(4) = 82.709
      	PAR3(5) = 86.212
        ENDIF
        IF ( i.EQ.51 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)/3
	PAR3(4) = 82.709
      	PAR3(5) = 86.212
        ENDIF
c
        IF ( i.EQ.52 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)*2/3
	PAR3(4) = 93.788
        PAR3(5) = 97.291
        ENDIF
        IF ( i.EQ.53 ) THEN
 	PAR3(3) = (4.654*0.5-0.55*0.5)/3
	PAR3(4) = 93.788
        PAR3(5) = 97.291
        ENDIF
        IF ( i.EQ.54 ) THEN
 	PAR3(3) = 4.654*0.5-0.55*0.5
	PAR3(4) = 97.291
        PAR3(5) = 98.393
        ENDIF
c
        IF ( i.EQ.55 ) THEN
 	PAR3(3) = 8.044182*0.5-3.0*0.5
	PAR3(4) = 89.100
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.EQ.56 ) THEN
 	PAR3(3) = 8.044182*0.5-3.0*0.5
	PAR3(4) = 90.200
      	PAR3(5) = 90.900
        ENDIF
c
        IF ( i.EQ.57 ) THEN
 	PAR3(3) = (8.46756/4+0.12)*0.5
	PAR3(4) = 81.607
      	PAR3(5) = 89.800
        ENDIF
        IF ( i.EQ.58 ) THEN
 	PAR3(3) = (8.46756/4+0.12)*0.5
	PAR3(4) = 90.200
      	PAR3(5) = 98.393
        ENDIF
c

      CALL GSVOLU(ARSSO(i), 'TUBS' , MEDAERS, PAR3 , 5 , IVOL )
      CALL GSVOLU(ARSDO(i), 'TUBS' , MEDAERD, PAR3 , 5 , IVOL )
      CALL GSATT (ARSSO(i),'COLO',4)
      CALL GSATT (ARSDO(i),'COLO',4)
      CALL GSPOS(ARSSO(i),i,'ASSO',0.,0.,ZM, 0,'MANY')
      CALL GSPOS(ARSDO(i),i,'ASDO',0.,0.,ZM, 0,'MANY')
c
      ZM=ZM-8.044182
      IF ( i.EQ.5 ) THEN
      ZM=ZM+i*8.044182
      ENDIF
      IF ( i.EQ.10 ) THEN
      ZM=ZM+(i-5)*8.044182
      ENDIF
      IF ( i.EQ.15 ) THEN
      ZM=ZM+(i-10)*8.044182
      ENDIF
      IF ( i.EQ.20 ) THEN
      ZM=ZM+(i-15)*8.044182
      ENDIF
      IF ( i.EQ.24 ) THEN
      ZM=ZM+(i-20)*8.044182
      ENDIF
      IF ( i.EQ.28 ) THEN
      ZM=ZM+(i-24)*8.044182
      ENDIF
      IF ( i.EQ.33 ) THEN
      ZM=ZM+(i-28)*8.044182
      ENDIF
      IF ( i.EQ.38 ) THEN
      ZM=ZM+(i-33)*8.044182
      ENDIF
      IF ( i.EQ.43 ) THEN
      ZM=ZM+(i-38)*8.044182
      ENDIF
c
      IF ( i.EQ.48 ) THEN
      ZM=ZM+8.044182*0.5-4.654*0.5+0.55*0.5
      ENDIF
      IF ( i.EQ.49 ) THEN
      ZM=ZM+8.044182+(4.654*0.5-0.55*0.5)/3
      ENDIF
      IF ( i.EQ.50 ) THEN
      ZM=ZM+8.044182-(4.654*0.5-0.55*0.5)
      ENDIF
c
      IF ( i.EQ.51 ) THEN
      ZM=ZM+8.044182+4.654*0.5-0.55*0.5
      ENDIF
      IF ( i.EQ.52 ) THEN
      ZM=ZM+8.044182-(4.654*0.5-0.55*0.5)
      ENDIF
      IF ( i.EQ.53 ) THEN
      ZM=ZM+8.044182*0.5+4.654-0.57+(4.654*0.5-0.55*0.5)*2/3
      ENDIF
c
      IF ( i.EQ.54 ) THEN
      ZM=ZM+8.044182+4.654*0.5+8.044182*0.5+2.5*0.5
      ENDIF
      IF ( i.EQ.55 ) THEN
      ZM=ZM+8.044182
      ENDIF
c
      IF ( i.EQ.56 ) THEN
      ZM=ZM+8.044182*4+8.044182+3.0+(8.46756/4)*0.5-0.55
      ENDIF
      IF ( i.EQ.57 ) THEN
      ZM=ZM+8.044182
      ENDIF
      end do
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Electronic box teflon (short counter)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.654*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.212
c     PHI2 - ending angle
      PAR3(5) = 93.788
      CALL GSVOLU('T1SO', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSVOLU('TSO1', 'TUBS' , MEDTEF, PAR3 , 5 , IVOL )
      CALL GSPOS('T1SO',1,'ASSO',0.,0.,-21.444, 0,'MANY')
      CALL GSPOS('TSO1',1,'ASDO',0.,0.,-21.444, 0,'MANY')
c Electronic box G10 walls (short counter)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.6*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.265
c     PHI2 - ending angle
      PAR3(5) = 93.735
      CALL GSVOLU('G1SO', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSVOLU('GSO1', 'TUBS' , MEDG10, PAR3 , 5 , IVOL )
      CALL GSPOS('G1SO',1,'T1SO',0.,0.,-0.027, 0,'MANY')
      CALL GSPOS('GSO1',1,'TSO1',0.,0.,-0.027, 0,'MANY')
c Electronic box internal air (short counter)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 4.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 86.313
c     PHI2 - ending angle
      PAR3(5) = 93.687
      CALL GSVOLU('A1SO', 'TUBS' , MEDAIF, PAR3 , 5 , IVOL )
      CALL GSVOLU('ASO1', 'TUBS' , MEDAIF, PAR3 , 5 , IVOL )
      CALL GSPOS('A1SO',1,'G1SO',0.,0.,-0.025, 0,'MANY')
      CALL GSPOS('ASO1',1,'GSO1',0.,0.,-0.025, 0,'MANY')
c Preamplifier (short counter)
c     (x)
      PAR1(1) = 0.4*0.5
c     (y)
      PAR1(2) = 2.5*0.5
c     length (z)
      PAR1(3) = 4.0*0.5
      CALL GSVOLU('GASO', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSVOLU('GSOA', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSPOS('GASO',1,'A1SO',3.2,67.7,0., 0,'ONLY')
      CALL GSPOS('GSOA',1,'ASO1',3.2,67.7,0., 0,'ONLY')
c HV devider (short counter)
c     (x)
      PAR1(1) = 0.8*0.5
c     (y)
      PAR1(2) = 1.5*0.5
c     length (z)
      PAR1(3) = 4.0*0.5
      CALL GSVOLU('GHSO', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSVOLU('GSOH', 'BOX ' , MEDG10, PAR1 , 3 , IVOL )
      CALL GSPOS('GHSO',1,'A1SO',-3.2,68.2,0., 0,'ONLY')
      CALL GSPOS('GSOH',1,'ASO1',-3.2,68.2,0., 0,'ONLY')
c MCP PMT frame (short counter)
c     (x)
      PAR1(1) = 5.3*0.5
c     (y)
      PAR1(2) = 4.0*0.5
c     length (z)
      PAR1(3) = 2.7*0.5
      CALL GSVOLU('PFSO', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSVOLU('PSOF', 'BOX ' , MEDPLXF, PAR1 , 3 , IVOL )
      CALL GSPOS('PFSO',1,'A1SO',0.,66.4,-0.925, 0,'MANY')
      CALL GSPOS('PSOF',1,'ASO1',0.,66.4,-0.925, 0,'MANY')
c WLS clamp (short counter)
c     (x)
      PAR1(1) = 3.6*0.5
c     (y)
      PAR1(2) = 3.0*0.5
c     length (z)
      PAR1(3) = 1.0*0.5
      CALL GSVOLU('DCSO', 'BOX ' , MEDD16T, PAR1 , 3 , IVOL )
      CALL GSVOLU('DSOC', 'BOX ' , MEDD16T, PAR1 , 3 , IVOL )
      CALL GSPOS('DCSO',1,'A1SO',0.,65.95,1.025, 0,'MANY')
      CALL GSPOS('DSOC',1,'ASO1',0.,65.95,1.025, 0,'MANY')
c Shifter, part near PMT (short counter, BOSS-BOSD)
c     thickness (x)
      PAR1(1) = 1.7*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 2.704*0.5
      CALL GSVOLU(SBOS(1), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(2), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(1),1,'ASSO',0.,66.4,-20.569, 0,'ONLY')
      CALL GSPOS(SBOS(2),1,'ASDO',0.,66.4,-20.569, 0,'ONLY')
c Shifter, bent part 1 (short counter)
c     RMIN
      PAR3(1) = 2.88
c     RMAX
      PAR3(2) = 3.18
c     length (z)
      PAR3(3) = 1.3*0.5
c     phi starting
      PAR3(4) = -90.0
c     phi ending
      PAR3(5) = -19.378
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,0.,0.,90.,90.,90.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBOS(3), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBOS(4), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBOS(3),1,'ASSO',0.,69.88,-19.217, IRM,'ONLY')
      CALL GSPOS(SBOS(4),1,'ASDO',0.,69.88,-19.217, IRM,'ONLY')
c Shifter, bent part 2 (short counter)
c     RMIN
      PAR3(1) = 5.28
c     RMAX
      PAR3(2) = 5.58
c     length (z)
      PAR3(3) = 1.7*0.5
c     phi starting
      PAR3(4) = -90.0
c     phi ending
      PAR3(5) = -57.48
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,0.,0.,90.,90.,90.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBOS(5), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBOS(6), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBOS(5),1,'ASSO',0.,71.98,-19.217, IRM,'ONLY')
      CALL GSPOS(SBOS(6),1,'ASDO',0.,71.98,-19.217, IRM,'ONLY')
c Shifter, bent part 3 (short counter)
c     RMIN
      PAR3(1) = 5.28
c     RMAX
      PAR3(2) = 5.58
c     length (z)
      PAR3(3) = 1.7*0.5
c     phi starting
      PAR3(4) = 57.48
c     phi ending
      PAR3(5) = 90.0
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,0.,0.,90.,90.,90.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBOS(7), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBOS(8), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBOS(7),1,'ASSO',0.,60.82,-19.217, IRM,'ONLY')
      CALL GSPOS(SBOS(8),1,'ASDO',0.,60.82,-19.217, IRM,'ONLY')
c Shifter, bent part 4 (short counter)
c     RMIN
      PAR3(1) = 2.88
c     RMAX
      PAR3(2) = 3.18
c     length (z)
      PAR3(3) = 1.3*0.5
c     phi starting
      PAR3(4) = 19.378
c     phi ending
      PAR3(5) = 90.0
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,0.,0.,90.,90.,90.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU(SBOS(9), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSVOLU(SBOS(10), 'TUBS' , MEDSHF, PAR3 , 5 , IVOL )
      CALL GSPOS(SBOS(9),1,'ASSO',0.,62.92,-19.217, IRM,'ONLY')
      CALL GSPOS(SBOS(10),1,'ASDO',0.,62.92,-19.217, IRM,'ONLY')
c Shifter, straight part 1 (short counter, BOSS-BOSD)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 38.69*0.5
      CALL GSVOLU('S5SO', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SSO5', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S5SO',1,'ASSO',0.,68.825,3.188, 0,'ONLY')
      CALL GSPOS('SSO5',1,'ASDO',0.,68.825,3.188, 0,'ONLY')
c
      PAR1(3) = 1.46909*0.5
      CALL GSVOLU(SBOS(11), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(12), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(11),1,'S5SO',0.,0.,18.61, 0,'MANY')
      CALL GSPOS(SBOS(12),1,'SSO5',0.,0.,18.61, 0,'MANY')
c
      PAR1(3) = 8.044*0.5
      CALL GSVOLU(SBOS(13), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(14), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(13),1,'S5SO',0.,0.,13.853, 0,'MANY')
      CALL GSPOS(SBOS(14),1,'SSO5',0.,0.,13.853, 0,'MANY')
c
      CALL GSVOLU(SBOS(15), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(16), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(15),1,'S5SO',0.,0.,5.809, 0,'MANY')
      CALL GSPOS(SBOS(16),1,'SSO5',0.,0.,5.809, 0,'MANY')
c
      CALL GSVOLU(SBOS(17), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(18), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(17),1,'S5SO',0.,0.,-2.234, 0,'MANY')
      CALL GSPOS(SBOS(18),1,'SSO5',0.,0.,-2.234, 0,'MANY')
c
      CALL GSVOLU(SBOS(19), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(20), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(19),1,'S5SO',0.,0.,-10.278, 0,'MANY')
      CALL GSPOS(SBOS(20),1,'SSO5',0.,0.,-10.278, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5-3.0*0.5
      CALL GSVOLU(SBOS(21), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(22), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(21),1,'S5SO',0.,0.,-16.822, 0,'MANY')
      CALL GSPOS(SBOS(22),1,'SSO5',0.,0.,-16.822, 0,'MANY')
c
c Shifter, straight part 2 (short counter)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 38.69*0.5
      CALL GSVOLU('S6SO', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SSO6', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S6SO',1,'ASSO',0.,67.275,3.188, 0,'ONLY')
      CALL GSPOS('SSO6',1,'ASDO',0.,67.275,3.188, 0,'ONLY')
c
      PAR1(3) = 1.46909*0.5
      CALL GSVOLU(SBOS(23), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(24), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(23),1,'S6SO',0.,0.,18.61, 0,'MANY')
      CALL GSPOS(SBOS(24),1,'SSO6',0.,0.,18.61, 0,'MANY')
c
      PAR1(3) = 8.044*0.5
      CALL GSVOLU(SBOS(25), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(26), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(25),1,'S6SO',0.,0.,13.853, 0,'MANY')
      CALL GSPOS(SBOS(26),1,'SSO6',0.,0.,13.853, 0,'MANY')
c
      CALL GSVOLU(SBOS(27), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(28), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(27),1,'S6SO',0.,0.,5.809, 0,'MANY')
      CALL GSPOS(SBOS(28),1,'SSO6',0.,0.,5.809, 0,'MANY')
c
      CALL GSVOLU(SBOS(29), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(30), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(29),1,'S6SO',0.,0.,-2.234, 0,'MANY')
      CALL GSPOS(SBOS(30),1,'SSO6',0.,0.,-2.234, 0,'MANY')
c
      CALL GSVOLU(SBOS(31), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(32), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(31),1,'S6SO',0.,0.,-10.278, 0,'MANY')
      CALL GSPOS(SBOS(32),1,'SSO6',0.,0.,-10.278, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5-3.0*0.5
      CALL GSVOLU(SBOS(33), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(34), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(33),1,'S6SO',0.,0.,-16.822, 0,'MANY')
      CALL GSPOS(SBOS(34),1,'SSO6',0.,0.,-16.822, 0,'MANY')
c
c Shifter, straight part 3 (short counter)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.7*0.5
c     length (z)
      PAR1(3) = 38.69*0.5
      CALL GSVOLU('S7SO', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SSO7', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S7SO',1,'ASSO',0.,65.525,3.188, 0,'ONLY')
      CALL GSPOS('SSO7',1,'ASDO',0.,65.525,3.188, 0,'ONLY')
c
      PAR1(3) = 1.46909*0.5
      CALL GSVOLU(SBOS(35), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(36), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(35),1,'S7SO',0.,0.,18.61, 0,'MANY')
      CALL GSPOS(SBOS(36),1,'SSO7',0.,0.,18.61, 0,'MANY')
c
      PAR1(3) = 8.044*0.5
      CALL GSVOLU(SBOS(37), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(38), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(37),1,'S7SO',0.,0.,13.853, 0,'MANY')
      CALL GSPOS(SBOS(38),1,'SSO7',0.,0.,13.853, 0,'MANY')
c
      CALL GSVOLU(SBOS(39), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(40), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(39),1,'S7SO',0.,0.,5.809, 0,'MANY')
      CALL GSPOS(SBOS(40),1,'SSO7',0.,0.,5.809, 0,'MANY')
c
      CALL GSVOLU(SBOS(41), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(42), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(41),1,'S7SO',0.,0.,-2.234, 0,'MANY')
      CALL GSPOS(SBOS(42),1,'SSO7',0.,0.,-2.234, 0,'MANY')
c
      CALL GSVOLU(SBOS(43), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(44), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(43),1,'S7SO',0.,0.,-10.278, 0,'MANY')
      CALL GSPOS(SBOS(44),1,'SSO7',0.,0.,-10.278, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5-3.0*0.5
      CALL GSVOLU(SBOS(45), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(46), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(45),1,'S7SO',0.,0.,-16.822, 0,'MANY')
      CALL GSPOS(SBOS(46),1,'SSO7',0.,0.,-16.822, 0,'MANY')
c
c Shifter, straight part 4 (short counter)
c     thickness (x)
      PAR1(1) = 0.3*0.5
c     height  (y)
      PAR1(2) = 1.3*0.5
c     length (z)
      PAR1(3) = 38.69*0.5
      CALL GSVOLU('S8SO', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU('SSO8', 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS('S8SO',1,'ASSO',0.,63.975,3.188, 0,'ONLY')
      CALL GSPOS('SSO8',1,'ASDO',0.,63.975,3.188, 0,'ONLY')
c
      PAR1(3) = 1.46909*0.5
      CALL GSVOLU(SBOS(47), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(48), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(47),1,'S8SO',0.,0.,18.61, 0,'MANY')
      CALL GSPOS(SBOS(48),1,'SSO8',0.,0.,18.61, 0,'MANY')
c
      PAR1(3) = 8.044*0.5
      CALL GSVOLU(SBOS(49), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(50), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(49),1,'S8SO',0.,0.,13.853, 0,'MANY')
      CALL GSPOS(SBOS(50),1,'SSO8',0.,0.,13.853, 0,'MANY')
c
      CALL GSVOLU(SBOS(51), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(52), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(51),1,'S8SO',0.,0.,5.809, 0,'MANY')
      CALL GSPOS(SBOS(52),1,'SSO8',0.,0.,5.809, 0,'MANY')
c
      CALL GSVOLU(SBOS(53), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(54), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(53),1,'S8SO',0.,0.,-2.234, 0,'MANY')
      CALL GSPOS(SBOS(54),1,'SSO8',0.,0.,-2.234, 0,'MANY')
c
      CALL GSVOLU(SBOS(55), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(56), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(55),1,'S8SO',0.,0.,-10.278, 0,'MANY')
      CALL GSPOS(SBOS(56),1,'SSO8',0.,0.,-10.278, 0,'MANY')
c
      PAR1(3) = 8.044182*0.5-3.0*0.5
      CALL GSVOLU(SBOS(57), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSVOLU(SBOS(58), 'BOX ' , MEDSHF, PAR1 , 3 , IVOL )
      CALL GSPOS(SBOS(57),1,'S8SO',0.,0.,-16.822, 0,'MANY')
      CALL GSPOS(SBOS(58),1,'SSO8',0.,0.,-16.822, 0,'MANY')
c
c Corner with air near shifter 1 (short counter)
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 180.
c     H1
      PAR4(4) = 6.3*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 6.3*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,155.,0.,90.,90.,65.,0.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('A2SO', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ASO2', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('A2SO',1,'ASSO',-0.39055,66.4,22.877, IRM,'MANY')
      CALL GSPOS('ASO2',1,'ASDO',-0.39055,66.4,22.877, IRM,'MANY')
c Corner with air near shifter 2 (short counter)
c     (z)
      PAR4(1) = 0.459626666
c     THET
      PAR4(2) = 40.
c     PHI
      PAR4(3) = 0.
c     H1
      PAR4(4) = 6.3*0.5
c     BL1
      PAR4(5) = 2.4*0.5
c     TL1
      PAR4(6) = 2.4*0.5
c     ALP1
      PAR4(7) = 0.
c     H2
      PAR4(8) = 6.3*0.5
c     BL2
      PAR4(9) = 0.
c     TL2
      PAR4(10) = 0.
c     ALP2
      PAR4(11) = 0.
      CALL FREE_ROTMAT_NUMBER(IRM)
      CALL GSROTM(IRM,25.,0.,90.,90.,65.,180.)
c      CALL GPROTM(IRM)
      CALL GSVOLU('A3SO', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSVOLU('ASO3', 'TRAP' , MEDAIF, PAR4 , 11 , IVOL )
      CALL GSPOS('A3SO',1,'ASSO',0.39055,66.4,22.877, IRM,'MANY')
      CALL GSPOS('ASO3',1,'ASDO',0.39055,66.4,22.877, IRM,'MANY')
c
c Aluminum strap 1 - right (short counter)
c     R min
      PAR3(1) = 69.096
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = (47.65-0.108)*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 81.607
      CALL GSVOLU('D1SO', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DSO1', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
c      CALL GSPOS('D1SO',1,'ASSO',0.,0.,-2.225, 0,'ONLY')
c      CALL GSPOS('D1SO',1,'ASDO',0.,0.,-2.225, 0,'ONLY')
      CALL GSPOS('D1SO',1,'ASSO',0.,0.,0., 0,'ONLY')
      CALL GSPOS('DSO1',1,'ASDO',0.,0.,0., 0,'ONLY')
c
c Aluminum strap 2 - left (short counter)
c     R min
      PAR3(1) = 69.096
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = (47.65-0.108)*0.5
c     PHI1 - starting angle
      PAR3(4) = 98.393
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('D2SO', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DSO2', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D2SO',1,'ASSO',0.,0.,0., 0,'ONLY')
      CALL GSPOS('DSO2',1,'ASDO',0.,0.,0., 0,'ONLY')
c
c Aluminum strap 3 - middle up (short counter)
c     R min
      PAR3(1) = 69.096
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.607
c     PHI2 - ending angle
      PAR3(5) = 98.393
      CALL GSVOLU('D3SO', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DSO3', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D3SO',1,'ASSO',0.,0.,-23.546, 0,'ONLY')
      CALL GSPOS('DSO3',1,'ASDO',0.,0.,-23.546, 0,'ONLY')
c
c Aluminum strap 4 - butt-end down (short counter)
c     R min
      PAR3(1) = 62.654
c     R max
      PAR3(2) = 63.204
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.192
c     PHI2 - ending angle
      PAR3(5) = 98.808
      CALL GSVOLU('D4SO', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DSO4', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D4SO',1,'ASSO',0.,0.,23.546, 0,'ONLY')
      CALL GSPOS('DSO4',1,'ASDO',0.,0.,23.546, 0,'ONLY')
c
c Aluminum strap 5 - butt-end up (long counter)
c     R min
      PAR3(1) = 69.096
c     R max
      PAR3(2) = 69.646
c     length (z)
      PAR3(3) = 0.55*0.5
c     PHI1 - starting angle
      PAR3(4) = 81.607
c     PHI2 - ending angle
      PAR3(5) = 98.393
      CALL GSVOLU('D5SO', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSVOLU('DSO5', 'TUBS' , MEDD16T, PAR3 , 5 , IVOL )
      CALL GSPOS('D5SO',1,'ASSO',0.,0.,23.546, 0,'ONLY')
      CALL GSPOS('DSO5',1,'ASDO',0.,0.,23.546, 0,'ONLY')
c====================================================================================================================================
	IF (NBL(1).NE.0) THEN
c
c Positioning of the left endcap, counters 1-20
c
	DO I=1,20
          CALL FREE_ROTMAT_NUMBER(IRM)
          CALL GSROTM(IRM,90.,18.-18.*I-2.0343,0.,0.,90.,108.-18.*I-2.0343)
          DENSFLAG = 0
	  DO K=1,MAXNDENS
	     IF (NCDENS(K).eq.I) DENSFLAG=1
	  ENDDO
	  XP=42.12*COS(3.141592*(108.-18.*I-2.0343)/180.)
	  YP=42.12*SIN(3.141592*(108.-18.*I-2.0343)/180.)
	  IF (DENSFLAG.eq.0)  THEN
		CALL GSPOS('ECSI',I,'KEDR',XP,YP,61.346-0.5, IRM,'ONLY')
	  ELSE
                CALL GSPOS('ECDI',I,'KEDR',XP,YP,61.346-0.5, IRM,'ONLY')
	  ENDIF
	ENDDO
        ENDIF
c
	IF (NBL(2).NE.0) THEN
c
c Positioning of the barrel 'short' counters 21-40
c
	DO I=21,40
          CALL FREE_ROTMAT_N UMBER(IRM)
          CALL GSROTM(IRM,90.,-18.*(I-21)-2.5024173,90.,90.-18.*(I-21)-2.5024173,0.,0.)
          DENSFLAG = 0
	  DO K=1,MAXNDENS
	     IF (NCDENS(K).eq.I) DENSFLAG=1
	  ENDDO
	  IF (DENSFLAG.eq.0)  THEN
                CALL GSPOS('BISS',I,'KEDR',0.,0.,+30.1+0.3, IRM,'ONLY')
	  ELSE
                CALL GSPOS('BISD',I,'KEDR',0.,0.,+30.1+0.3, IRM,'ONLY')
	  ENDIF
	ENDDO
        ENDIF
c
	IF (NBL(3).NE.0) THEN    !41-60-подключение
c
c Positioning of the barrel 'long' counters 41-60
c
	DO I=41,60
          CALL FREE_ROTMAT_NUMBER(IRM)
          CALL GSROTM(IRM,90.,-18.*(I-41)-2.6183547,90.,90.-18.*(I-41)-2.6183547,0.,0.)
          DENSFLAG = 0
	  DO K=1,MAXNDENS
	     IF (NCDENS(K).eq.I) DENSFLAG=1
	  ENDDO
	  IF (DENSFLAG.eq.0)  THEN
                CALL GSPOS('BILS',I,'KEDR',0.,0.,-24.9+0.3, IRM,'ONLY')
	  ELSE
                CALL GSPOS('BILD',I,'KEDR',0.,0.,-24.9+0.3, IRM,'ONLY')
	  ENDIF
	ENDDO
        ENDIF
c
	IF (NBL(4).NE.0) THEN
c
c Positioning of the rigth endcap, counters 61-80
c
	DO I=61,80
          CALL FREE_ROTMAT_NUMBER(IRM)
          CALL GSROTM(IRM,90.,18.-18.*(I-60)-1.9753,0.,0.,90.,108.-18.*(I-60)-1.9753)
          DENSFLAG = 0
	  DO K=1,MAXNDENS
	     IF (NCDENS(K).eq.I) DENSFLAG=1
	  ENDDO
	  XP=42.12*COS(3.141592*(108.-18.*(I-60)-1.9753)/180.)
	  YP=42.12*SIN(3.141592*(108.-18.*(I-60)-1.9753)/180.)
	  IF (DENSFLAG.eq.0)  THEN
                CALL GSPOS('ECSI',I,'KEDR',XP,YP,-60.846, IRM,'ONLY')
	  ELSE
                CALL GSPOS('ECDI',I,'KEDR',XP,YP,-60.846, IRM,'ONLY')
	  ENDIF
	ENDDO
        ENDIF
c
	IF (NBL(5).NE.0) THEN
c
ccccccccccccccccccccccccc
c Positioning of the left endcap, counters 81-100
c
	DO I=81,100
          CALL FREE_ROTMAT_NUMBER(IRM)
          CALL GSROTM(IRM,90.,189.-18.*I-1.9753,180.,0.,90.,99.-18.*I-1.9753)
          DENSFLAG = 0
	  DO K=1,MAXNDENS
	     IF (NCDENS(K).eq.I) DENSFLAG=1
	  ENDDO
	  XP=42.12*COS(3.141592*(99.-18.*I-1.9753)/180.)
	  YP=42.12*SIN(3.141592*(99.-18.*I-1.9753)/180.)
	  IF (DENSFLAG.eq.0)  THEN
              CALL GSPOS('ECSO',I,'KEDR',XP,YP,69.246-0.5, IRM,'ONLY')
	  ELSE
              CALL GSPOS('ECDO',I,'KEDR',XP,YP,69.246-0.5, IRM,'ONLY')
	  ENDIF
	ENDDO
        ENDIF
c
	IF (NBL(6).NE.0) THEN
c
c Positioning of the barrel 'long' counters 101-120
c
        DO I=101,120
          CALL FREE_ROTMAT_NUMBER(IRM)
          CALL GSROTM(IRM,90.,-9-18.*(I-101)+180-2.48883822,90.+180,99.-18.*(I-101)+180-2.48883822,0.,0.)
       	  DENSFLAG = 0
	  DO K=1,MAXNDENS
	     IF (NCDENS(K).eq.I) DENSFLAG=1
	  ENDDO
	  IF (DENSFLAG.eq.0)  THEN
                CALL GSPOS('BOLS',I,'KEDR',0.,0.,+23.9+0.3, IRM,'ONLY')
	  ELSE
                CALL GSPOS('BOLD',I,'KEDR',0.,0.,+23.9+0.3, IRM,'ONLY')
	  ENDIF
	ENDDO
        ENDIF
c
	IF (NBL(7).NE.0) THEN
c
c Positioning of the barrel 'short' counters 121-140
c
        DO I=121,140
          CALL FREE_ROTMAT_NUMBER(IRM)
          CALL GSROTM(IRM,90.,-9-18.*(I-121)+180-2.5495018,90.+180,99.-18.*(I-121)+180-2.5495018,0.,0.)
	  DENSFLAG = 0
	  DO K=1,MAXNDENS
	     IF (NCDENS(K).eq.I) DENSFLAG=1
	  ENDDO
	  IF (DENSFLAG.eq.0)  THEN
	CALL GSPOS('BOSS',I,'KEDR',0.,0.,-31.1+0.3, IRM,'ONLY')
	  ELSE
	CALL GSPOS('BOSD',I,'KEDR',0.,0.,-31.1+0.3, IRM,'ONLY')
	  ENDIF
	ENDDO
        ENDIF
c
	IF (NBL(8).NE.0) THEN
c
c Positioning of the rigth endcap, counters 141-160
c
	DO I=141,160
          CALL FREE_ROTMAT_NUMBER(IRM)
          CALL GSROTM(IRM,90.,9.-18.*(I-60)-2.0313,0.,0.,90.,99.-18.*(I-60)-2.0313)
          DENSFLAG = 0
	  DO K=1,MAXNDENS
	     IF (NCDENS(K).eq.I) DENSFLAG=1
	  ENDDO
	  XP=42.12*COS(3.141592*(99.-18.*(I-60)-2.0313)/180.)
	  YP=42.12*SIN(3.141592*(99.-18.*(I-60)-2.0313)/180.)
	  IF (DENSFLAG.eq.0)  THEN
               CALL GSPOS('ECSO',I,'KEDR',XP,YP,-68.746, IRM,'ONLY')
	  ELSE
               CALL GSPOS('ECDO',I,'KEDR',XP,YP,-68.746, IRM,'ONLY')
	  ENDIF
	ENDDO
	ENDIF
c
c
      RETURN
      END
