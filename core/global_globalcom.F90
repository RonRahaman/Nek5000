      module GLOBAL_GLOBALCOM
      use GLOBAL_SIZE
      implicit none
      integer nid_global, idsess, idsess_neighbor, intercomm
     $      , iglobalcomm, npsess(0:nsessmax-1), np_neighbor, np_global
      common /nekmpi_global/ nid_global, idsess, idsess_neighbor
     $                     , intercomm, iglobalcomm
     $                     , npsess,np_neighbor,np_global

      integer               nsessions
      common /session_info/ nsessions
      end module GLOBAL_GLOBALCOM
