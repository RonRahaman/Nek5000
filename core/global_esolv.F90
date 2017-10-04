      module GLOBAL_ESOLV
      use GLOBAL_SIZE
      implicit none
c
c     Variables for E-solver
c
      integer         iesolv
      common /econst/ iesolv

      logical         ifalgn(lelv), ifrsxy(lelv)
      common /efastm/ ifalgn      , ifrsxy

      real            volel(lelv)
      common /eouter/ volel       
      end module GLOBAL_ESOLV
