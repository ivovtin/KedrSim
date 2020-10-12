      SUBROUTINE ATC_CARD
c--------------------------------------------------------
c
c   Subr. reads card for aerogel counters simulation
c
c   Changed  20.04.2000   V.Tayursky
c            14.10.2003   V.Tayursky
c            15.11.2013   I.Ovtin
c--------------------------------------------------------
	include 'atccom.inc'
	DATA NBL/1,1,1,1,1,1,1,1/
c   NBL(1)=1-20 - include 1-20 of ATC in simulation
c   NBL(2)=21-40 - include 21-40 of ATC in simulation
c   NBL(3)=41-60 - include 41-60 of ATC in simulation
c   NBL(4)=61-80 - include 61-80 of ATC in simulation
c   NBL(5)=81-100 - include 81-100 of ATC in simulation
c   NBL(6)=101-120 - include 101-120 of ATC in simulation
c   NBL(7)=121-140 - include 121-140 of ATC in simulation
c   NBL(8)=141-160 - include 141-160 of ATC in simulation
	CALL FFKEY('ATHC',NBL,8,'INTEGER')  !подключение из mc.cards
      RETURN
      END
