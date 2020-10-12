*CMZ :  1.01/00 13/11/91  16.59.32  by  Master Librarian (CMD)
*-- Author :
      SUBROUTINE COSI(C,S)
      PI=3.1415926535
      CALL GRNDM(RD,1)
      ANG=2*PI*RD
      C=COS(ANG)
      S=SIN(ANG)
      RETURN
      END
