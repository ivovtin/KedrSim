		subroutine gufld_simple(vect,outf)
		implicit none
		
		real fiel,fiean,fieldmap
		include 'fiel.inc'

      INTEGER*4 NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,
     +          NLEVMX,NLDEV,LINMX
      REAL*4    GTRAN,GRMAT,GONLY,GLX
#include "gcvolu.inc"
		character name(4,15)
		equivalence(name,names)
				
		real vect(3),outf(3)
		real b1,b2,b3,b4,s2
		real r,B0
		data b1/-0.712/,b2/-0.568/,b3/-0.424/,b4/-0.288/
		data s2/1.4142136/
		
		if(vect(3).gt.0) then
			B0=fiel
		else
			B0=-fiel
		endif
		outf(3)=0
		outf(1)=0
		outf(2)=0
		r=sqrt(vect(1)**2+vect(2)**2)
		if(name(4,nlevel).eq.'6'.or.name(4,nlevel).eq.'8') then ! YOK6,8
			outf(3)=B0/s2
			outf(1)=outf(3)/r
			outf(2)=outf(1)*vect(2)
			outf(1)=outf(1)*vect(1)
		elseif(name(4,nlevel).eq.'5'.or.name(4,nlevel).eq.'7') then 
			outf(1)=B0*vect(1)/r
			outf(2)=B0*vect(2)/r
		elseif(name(4,nlevel).eq.'1') then
			outf(3)=b1*fiel
		elseif(name(4,nlevel).eq.'2') then
			outf(3)=b2*fiel
		elseif(name(4,nlevel).eq.'3') then
			outf(3)=b3*fiel
		elseif(name(4,nlevel).eq.'4') then
			outf(3)=b4*fiel
		endif

c		print *,'"',name(4,nlevel),'"',outf
		return

		end
