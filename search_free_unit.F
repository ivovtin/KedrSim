*CMZ :  2.01/00 06/05/92  16.53.08  by  Tanya Purlatz
*CMZ :  1.01/00 30/04/92  18.10.36  by  Tanya Purlatz
*-- Author : A.Maksimov
C
C*****************************************************************
C
      SUBROUTINE search_free_unit(Iunit)
      LOGICAL Lopen, Lexist
      Iunit = 0
    1 Iunit = Iunit + 1
      INQUIRE( UNIT = Iunit , OPENED = Lopen, EXIST = Lexist )
      IF( .NOT. Lexist ) GOTO 1
      IF( Lopen ) GOTO 1
C
      END
