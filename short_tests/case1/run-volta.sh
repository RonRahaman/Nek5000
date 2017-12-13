#!/bin/bash
#COBALT -q dgx
#COBALT -n 1
#COBALT -t 01:00:00
#COBALT -o t3.head_01.np8.stdout
#COBALT -e t3.head_01.np8.stderr

echo "t3" > SESSION.NAME
echo `pwd` >> SESSION.NAME

export MPI_DIR=/home/rahaman/install/pgilinux-2017-1710-x86_64/linux86-64/17.10/mpi/openmpi/

#nvprof ./nek5000
#nvprof --kernels axhelm_acc_589_gpu --metrics flop_count_dp,flop_dp_efficiency ./nek5000
#nvprof --kernels axhelm_acc_597_gpu --metrics flop_count_dp,flop_dp_efficiency ./nek5000
#nvprof --kernels global_div3_451_gpu --metrics flop_count_dp,flop_dp_efficiency ./nek5000
#nvprof --kernels global_grad3_401_gpu --metrics flop_count_dp,flop_dp_efficiency ./nek5000

$MPI_DIR/bin/mpirun --report-bindings -np 8 ./nek5000

# $MPI_DIR/bin/mpirun -np 2 ./nek5000
# $MPI_DIR/bin/mpirun -np 4 ./nek5000
# $MPI_DIR/bin/mpirun -np 6 ./nek5000
# $MPI_DIR/bin/mpirun -np 8 ./nek5000
# $MPI_DIR/bin/mpirun -np 16 ./nek5000
