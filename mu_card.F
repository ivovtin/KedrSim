      subroutine mucard

      include 'mu_par.inc'

      data mu_resolution/0.0/, mu_efficiency/1.0/
      data mu_debug/0./, mu_write/0./

      call ffkey('MUPR', mu_resolution, 4, 'REAL')

      return

      end
