      SUBROUTINE TOF_ZERO
c
      COMMON /SC_PHOT/ PHO(3,128)
c
      DO I=1,128
c
      DO K=1,3
      PHO(K,I)=0
      END DO
c
      END DO
c
      RETURN
      END      