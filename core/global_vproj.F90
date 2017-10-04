      module GLOBAL_VPROJ
      use GLOBAL_SIZE
      implicit none
c
c     Vproj variables
c
      integer    ktop
      parameter (ktop = lelv*lx1*ly1*lz1)

      integer         ivproj(2,6)
      common /wrthoi/ ivproj

      real             vproj(ktop*mxprev,2*ldim)
      common /wrthov/  vproj
      end module GLOBAL_VPROJ
