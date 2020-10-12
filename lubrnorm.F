        subroutine lubrnorm
        external lunewdata
        include 'lubrnew.inc'
        COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
        COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
        save  /LUDAT3/
        real BRAT
        integer mdcy,mdme,kfdp
        real sum
        sum=0
        DO  KC=1,MSTU(6)
        sum=0.
        DO IDC=MDCY(KC,2),MDCY(KC,2)+MDCY(KC,3)-1
        sum=sum+BRN(IDC)
        ENDDO
        if(sum.ne.1.and.sum.gt.0.90)  then
        DO IDC=MDCY(KC,2),MDCY(KC,2)+MDCY(KC,3)-1
        BRN(IDC)=BRN(IDC)/sum
        BRNERR(IDC)=BRNERR(IDC)/sum
        ENDDO
        endif
        ENDDO
        do idc=1,2000
        BRAT(idc)=BRN(IDC)
        enddo

        end