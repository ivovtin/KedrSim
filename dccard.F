*CMZU:          21/05/97  15.06.50  by  YURI SKOVPEN
*CMZU:  1.00/03 26/11/96  15.56.19  by  VLADIMIR KOZLOV
*-- Author :    VLADIMIR KOZLOV   29/10/96
      SUBROUTINE DCCARD
C
	include 'celpar.inc'
C
!       скорость дрейфа, угол поворота ДК в rho-phi, угол Лоренца, ширина области собирания ионизации, ?
      DATA DRFVEL/0.465/,ANGDC/0.0/,ANGLOR/3.0/,BOUND/0.4/,RELPS/0.05/
!     частота расш. ЦСУ(МГц), время срабатыв. ПТ (мкс), мертвое время дискр. платы ТАМ (600нс),?
      DATA GFREQ/143.2/,TIMD/6.0/, TIMLCK/0.6/, SIGLCK/0.06/
      DATA IWGEOM/0/,LAYSON/1,1,1,1,1,1,1/
C
      CALL FFKEY ('DCPR', DRFVEL, 10, 'MIXED')
      CALL FFKEY ('DCLS', LAYSON, 7, 'INTEGER')
C
      RETURN
      END
