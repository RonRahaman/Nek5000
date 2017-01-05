#!/usr/bin/env bash

# Takes a path to a .usr file (/path/to/userfile.usr) and outputs a configured .f file in the same
# directory.  The result .f file has the necessary subroutines appended.  Example usage:
# $ Nek5000/bin/config_usr.sh NekExamples/eddy/eddy_uv.usr
# TODO: handle CVODE and CMT builds

# Strips extension off of .usr file
CASENAME=${1%.*}

rm -f $CASENAME.f
cp -pv $CASENAME.usr $CASENAME.f

cat $CASENAME.f | grep -i "subroutine.*usrsetvert" >/dev/null
if [ $? -ne 0 ]; then
cat >> $CASENAME.f << _ACEOF

c automatically added by makenek
      subroutine usrsetvert(glo_num,nel,nx,ny,nz) ! to modify glo_num
      integer*8 glo_num(1)

      return
      end
_ACEOF
fi

cat $CASENAME.f | grep -i "subroutine.*userqtl" >/dev/null
if [ $? -ne 0 ]; then
cat >> $CASENAME.f << _ACEOF

c automatically added by makenek
      subroutine userqtl(flag)
      logical flag

      call qthermal_ig(flag)

      return
      end
_ACEOF
fi

# TODO: Implement CVODE and CMT options
# if [ "$IFCVODE" == "true" -o "$IFCVODE" == "yes" ]; then
# 
#   cat $CASENAME.f | grep -i "^#include.*cvode_aux.*\.h" >/dev/null
#   if [ $? -ne 0 ]; then
#   cat >> $CASENAME.f << _ACEOF
# 
# c automatically added by makenek
# #include "cvode_aux.h"
# _ACEOF
#   fi
# 
#   cat $CASENAME.f | grep -i "^#include.*cvode_preco.*\.h" >/dev/null
#   if [ $? -ne 0 ]; then
#     cat >> $CASENAME.f << _ACEOF
# 
# c automatically added by makenek
# #include "cvode_preco_dummy.h"
# _ACEOF
#   fi
# 
# fi
# 
# if [ "$IFCMT" == "true" -o "$IFCMT" == "yes" ]; then
#   cat $CASENAME.f | grep -i "subroutine.*cmt_usrflt" >/dev/null
#   if [ $? -ne 0 ]; then
# cat >> $CASENAME.f << _ACEOF
# 
# c automatically added by makenek
#       subroutine cmt_usrflt(rmult) ! user defined filter
#       include 'SIZE'
#       real rmult(lx1)
#       call rone(rmult,lx1)
#       return
#       end
# _ACEOF
#   fi
# 
#   cat $CASENAME.f | grep -i "subroutine.*cmt_userflux" >/dev/null
#   if [ $? -ne 0 ]; then
# cat >> $CASENAME.f << _ACEOF
# 
# c automatically added by makenek
#       subroutine cmt_userflux ! user defined flux
#       include 'SIZE'
#       include 'TOTAL'
#       include 'NEKUSE'
#       include 'CMTDATA'
#       real fluxout(lx1*lz1)
#       return
#       end
# _ACEOF
#   fi
# 
#   cat $CASENAME.f | grep -i "subroutine.*cmt_userEOS" >/dev/null
#   if [ $? -ne 0 ]; then
#   cat >> $CASENAME.f << _ACEOF
# 
# c automatically added by makenek
#       subroutine cmt_userEOS ! user defined EOS 
#       include 'SIZE'
#       include 'TOTAL'
#       include 'NEKUSE'
#       include 'CMTDATA'
# 
#       return
#       end
# _ACEOF
#   fi
#   if [ -e "cmtparticles.usrp" ]; then
#     echo 'Particles found CMT'
#   else
# cat >> $CASENAME.f << _ACEOF
# c
# c automatically added by makenek
#       subroutine usr_particles_init ! used for particles
#       return
#       end
# c
# c automatically added by makenek
#       subroutine usr_particles_solver ! used for particles
#       return
#       end
# c
# c automatically added by makenek
#       subroutine usr_particles_io(istep) ! used for particles
#       integer istep
#       return
#       end
# 
# _ACEOF
#   fi
# fi
