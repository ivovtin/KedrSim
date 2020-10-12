*KEEP,CSIWRITE.

C What we have at the end
C CsNcr - Number of cristalls wich have amplitude grater then Threads
C CsIcr - number of touched cristall
c CsIcr=(torec-1)*2^10+(quad-1)*2^8+(group-1)*2^4+(chanel-1)
C CsAmp - amplitude at the same cristall

      integer NCsNcr
      integer *2 ICsIcr(384,2),ICsAmp(384,2)
      COMMON/CSIWRITE/NCsNcr(2),ICsIcr,ICsAmp
