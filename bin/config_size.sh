#!/usr/bin/env bash

# Configures a SIZE file by appending necessary subroutines. Example usage:
# $ Nek5000/config_size.sh NekExamples/eddy/SIZE
# TODO: handle CMT builds

# The first arg is a path to the SIZE file
SIZE=$1

cat $SIZE | grep 'SIZE.inc' >/dev/null
if [ $? -ne 0 ];  then
  cat $SIZE | grep -i 'optlevel' >/dev/null
  if [ $? -ne 0 ]; then
     echo >>$SIZE
     echo 'c automatically added by makenek' >>$SIZE
     echo '      integer optlevel,loglevel' >>$SIZE
     echo '      common /lolevels/ optlevel,loglevel' >>$SIZE
  fi
  cat $SIZE | grep -i 'lxo' >/dev/null
  if [ $? -ne 0 ]; then
     echo >>$SIZE
     echo 'c automatically added by makenek' >>$SIZE
     echo '      parameter(lxo   = lx1) ! max output grid size (lxo>=lx1)' >>$SIZE
  fi
  cat $SIZE | grep -i 'lpart' >/dev/null
  if [ $? -ne 0 ]; then
     echo >>$SIZE
     echo 'c automatically added by makenek' >>$SIZE
     echo '      parameter(lpart = 1  ) ! max number of particles/proc' >>$SIZE
  fi
  cat $SIZE | grep -i 'ax1' >/dev/null
  if [ $? -ne 0 ]; then
     echo >>$SIZE
     echo 'c automatically added by makenek' >>$SIZE
     echo '      integer ax1,ay1,az1,ax2,ay2,az2' >> $SIZE
     echo '      parameter (ax1=lx1,ay1=ly1,az1=lz1,ax2=lx2,ay2=ly2,az2=lz2) ! running averages' >> $SIZE
  fi
  cat $SIZE | grep -i 'lys=lxs' >/dev/null
  if [ $? -ne 0 ]; then
     cat $SIZE | grep -iv lxs > $SIZE.x; mv $SIZE.x $SIZE  # Clean existing SIZE file of old version
     echo >>$SIZE
     echo 'c automatically added by makenek' >>$SIZE
     echo '      parameter (lxs=1,lys=lxs,lzs=(lxs-1)*(ldim-2)+1) !New Pressure Preconditioner' >> $SIZE

  fi
  cat $SIZE | grep -i 'lcvx1' >/dev/null
  if [ $? -ne 0 ]; then
     echo >>$SIZE
     echo 'c automatically added by makenek' >>$SIZE
     echo '      integer lcvx1,lcvy1,lcvz1,lcvelt' >> $SIZE
     echo '      parameter (lcvx1=1,lcvy1=1,lcvz1=1,lcvelt=1) ! cvode arrays' >> $SIZE
  fi
  cat $SIZE | grep -i 'lfdm' >/dev/null
  if [ $? -ne 0 ]; then
     echo >>$SIZE
     echo 'c automatically added by makenek' >>$SIZE
     echo '      parameter (lfdm=0)  ! == 1 for fast diagonalization method' >> $SIZE
  fi
  cat $SIZE | grep -i 'nio' >/dev/null
  if [ $? -ne 0 ]; then
     echo >>$SIZE
     echo 'c automatically added by makenek' >>$SIZE
     echo '      common/IOFLAG/nio  ! for logfile verbosity control' >> $SIZE
  fi

  cat $SIZE | grep -i 'toteq' >/dev/null
  if [ $? -ne 0 ]; then
  if [ "$IFCMT" == "true" ]; then
      echo >>$SIZE
      echo 'c automatically added by makenek' >>$SIZE
      echo '      integer toteq' >> $SIZE
      echo '      parameter(toteq = 5  ) ! Number of conserved variables '  >>$SIZE
      echo 'c IFCMT=TRUE  then toteq=5'  >>$SIZE
  else
      echo >>$SIZE
      echo 'c automatically added by makenek' >>$SIZE
      echo '      integer toteq' >> $SIZE
      echo '      parameter(toteq = 1  ) ! Number of conserved variables '  >>$SIZE
      echo 'c IFCMT=FALSE  then toteq=1'  >>$SIZE
  fi
  fi
fi
