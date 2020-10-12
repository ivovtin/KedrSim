      subroutine csi_mctorl(itrmc,imdmc,iblx,ibly,icr,itrrl,imdrl,igr,
     +                      ich)
c*************************************************************************
c
c     CsI coordinate converter
c     Monte-Carlo coordinates       ->      REAL coordinates
c (torc,  mod,   blockx, blocky, cryst)     -> (torc,  mod,   group,  chan)
c  [1:2], [1:4], [1:10], [1:10], [1:2]          [1:2], [1:4], [1:12], [1:14]
c
c*************************************************************************
c
       integer mctogr(10,10),mctoch(10,10)
*                  1 2 3 4 5 6 7 8 9 10 
       data mctogr/0,0,6,6,4,4,4,1,1,1,      ! 1st row (Y)
     +             0,6,6,6,4,4,4,1,1,1,      ! 2
     +             6,6,6,6,4,4,4,1,1,1,      ! 3
     +             6,6,6,4,4,4,4,1,1,1,      ! 4
     +             5,5,5,5,5,2,2,1,1,0,      ! 5
     +             5,5,5,5,2,2,2,2,2,0,      ! 6
     +             5,5,5,5,2,2,2,2,0,0,      ! 7
     +             3,3,3,3,3,2,2,0,0,0,      ! 8
     +             3,3,3,3,3,2,0,0,0,0,      ! 9
     +             3,3,3,3,0,0,0,0,0,0/      ! 10th row (Y=10)
*
       data mctoch/ 0, 0, 7, 1, 9, 5, 1,10, 5, 1,      ! 1st row (Y=1)
     +              0,12, 8, 2,10, 6, 2,11, 6, 2,      ! 2
     +             11,10, 9, 3,11, 7, 3,12, 7, 3,      ! 3
     +              6, 5, 4,13,12, 8, 4,13, 8, 4,      ! 4
     +              9,10,11,12,13,12, 7,14, 9, 0,      ! 5
     +              5, 6, 7, 8,14,13, 8, 2, 1, 0,      ! 6
     +              1, 2, 3, 4,11,10, 9, 3, 0, 0,      ! 7
     +             10,11,12,13,14, 5, 4, 0, 0, 0,      ! 8
     +              5, 6, 7, 8, 9, 6, 0, 0, 0, 0,      ! 9
     +              1, 2, 3, 4, 0, 0, 0, 0, 0, 0/      ! 10th row (Y=10)
*
* torc number conversion
       itrrl=itrmc
*
* module number conversion
       imdrl=imdmc
       if(itrmc.eq.2) imdrl=5-imdmc
*
* find out corresponding group number
       igr=mctogr(iblx,ibly)
       if(icr.eq.2) igr=igr+6
*
* and channel number
       ich=mctoch(iblx,ibly)
C     отбрасываем длинные кристаллы из 12ой группы
       if (igr.eq.12.and.ich.gt.6) ich=0

       end
