      subroutine mu_ev
	  
	  IMPLICIT NONE

      include 'mu_data.inc'
	  include 'mu_par.inc'

      integer event_number
	  data event_number/1/

      integer i

      if(mu_debug.gt.0) print *,'MuSystem hits:', mu_n_hits
      if(mu_write.gt.0.and.mu_n_hits.gt.0) then
         write (70, *) event_number,mu_n_hits
		 do i = 1, mu_n_hits
		    write (70, *) mu_ch_hit(i), mu_ch_time(i)
		 enddo
	  endif
      event_number = event_number + 1
      mu_hits = mu_n_hits
      mu_n_hits = 0
c		read (*,*) i
      return
	 
      end
