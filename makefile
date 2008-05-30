############################################################################### 
S         = $(SOURCE_ROOT)
J         = $(SOURCE_ROOT)/jl
J2        = $J/../jl2
# FLAGS ARE SET BY MAKENEK
#IFMPI     = true
#F77       = mpif77
#CC        = mpicc
#P         = -r8
#G         = -g
#UNDERSCORE= -DUNDERSCORE
#USR_LIB   = -lblas
################################################################################
#
# core nekton files ...
#
CORE = drive.o plan4.o bdry.o coef.o conduct.o connect1.o connect2.o \
dssum.o edgec.o eigsolv.o gauss.o genxyz.o navier1.o \
navier0.o navier2.o navier3.o navier4_std.o prepost.o speclib.o \
map2.o turb.o mvmesh.o ic.o ssolv.o planx.o mat1.o mat2.o \
hmholtz.o gfdm_par.o  gfdm_op.o gfdm_solve.o subs1.o subs2.o \
genbox.o uzawa_gmres.o hsmg.o convect.o induct.o perturb.o \
navier5.o navier6.o navier7.o navier8.o fast3d.o fasts.o calcz.o \
mxm44f2.o \
subuser.o \
isosurf.o byte.o \
cvode_driver.o setprop.o qthermal.o makeq.o \
ssygv.o dsygv.o
#
# new gather-scatter (Lottes)
JO2 = jl2_
GS = $(JO2)gs.o $(JO2)gs_local.o $(JO2)sarray_sort.o $(JO2)sarray_transfer.o \
$(JO2)crystal.o $(JO2)comm.o $(JO2)sort.o $(JO2)errmem.o
#
XXT = $(JO2)sparse_cholesky.o $(JO2)xxt.o $(JO2)fcrs.o
#
JO = jl_
AMG = $(JO)errmem.o $(JO)sort.o $(JO)crystal.o $(JO)fcrystal.o \
$(JO)tuple_list.o $(JO)transfer.o $(JO)gs.o $(JO)amg.o
#
COMM_MPI = comm_seq.o
ifeq ($(IFMPI),true) 
COMM_MPI = comm_mpi.o
MPI  = -DMPI
endif

NOBJS = $(CORE) $(OS) $(CRYSTAL) $(GS) $(XXT) $(USR) $(COMM_MPI) blas.o

L0 = $(G) -O0 
L2 = $(G) -O2
L3 = $(G) -O2
L4 = $(G) -O3
JL    = $(MPI) $(UNDERSCORE)
JL2   = -DPREFIX=jl_ $(MPI) $(UNDERSCORE)

FL0i4 = $(L0)
FL0   = $(L0) $(P)
FL2   = $(L2) $(P)
FL3   = $(L3) $(P)
FL4   = $(L4) $(P)
FLd   = $(L4) $(P)

###
DRL0    = $(FL0) 
DFL0    = $(FL0) 
DFL2    = $(FL2)
DFL3    = $(FL3)
DFL4    = $(FL4)

RFL0    = $(FL0)
RFL2    = $(FL2)
RFL3    = $(FL3)
RFL4    = $(FL4)
RFLd    = $(FLd)

DRFL0   = $(FL0)
DRFL2   = $(FL3)
DRFL3   = $(FL3)
DRFL4   = $(FL4)

cFL0   = $(L0) 
cFL2   = $(L2) 
cFL3   = $(L3) 
cFL4   = $(L4) 

###

lFLAGS = $(USR_LIB)


################################################################################

all : nek5000  print

nek5000:	 $(NOBJS) 
	 $(F77) -o nek5000 $G $(NOBJS) $(lFLAGS)

print:
	size nek5000
	echo "I am done" 

clean:
	rm -rf ./obj nek5000

################################################################################
# fortran source files
################################################################################
drive.o		:$S/drive.f;		$(F77) -c $(FL2) $S/drive.f
################################################################################
dsygv.o         :$S/dsygv.f;            $(F77) -c $(FL0i4) $S/dsygv.f
ssygv.o         :$S/ssygv.f;            $(F77) -c $(FL0i4) $S/ssygv.f
blas.o	        :$S/blas.f;		$(F77) -c $(FL0i4) $S/blas.f
################################################################################
prepost.o	:$S/prepost.f;		$(F77) -c $(FL2) $S/prepost.f
connect1.o	:$S/connect1.f;		$(F77) -c $(FL2) $S/connect1.f
connect2.o	:$S/connect2.f;		$(F77) -c $(FL2) $S/connect2.f
edgec.o		:$S/edgec.f;		$(F77) -c $(FL2) $S/edgec.f
genxyz.o	:$S/genxyz.f;		$(F77) -c $(FL2) $S/genxyz.f
subs1.o		:$S/subs1.f;		$(F77) -c $(FL2) $S/subs1.f
subs2.o		:$S/subs2.f;		$(F77) -c $(FL2) $S/subs2.f
turb.o		:$S/turb.f;		$(F77) -c $(FL2) $S/turb.f
map2.o		:$S/map2.f;		$(F77) -c $(FL2) $S/map2.f
mvmesh.o	:$S/mvmesh.f;		$(F77) -c $(FL2) $S/mvmesh.f
induct.o	:$S/induct.f;	        $(F77) -c $(FL2) $S/induct.f
convect.o	:$S/convect.f;	        $(F77) -c $(FL2) $S/convect.f
perturb.o	:$S/perturb.f;	        $(F77) -c $(FL2) $S/perturb.f
genbox.o	:$S/genbox.f;		$(F77) -c $(FL2) $S/genbox.f
hsmg.o		:$S/hsmg.f;		$(F77) -c $(FL2) $S/hsmg.f
uzawa_gmres.o	:$S/uzawa_gmres.f;	$(F77) -c $(FL2) $S/uzawa_gmres.f
################################################################################
bdry.o		:$S/bdry.f;		$(F77) -c $(FL2) $S/bdry.f
comm_seq.o	:$S/comm_seq.f;		$(F77) -c $(FL2) $S/comm_seq.f
comm_mpi.o	:$S/comm_mpi.f;		$(F77) -c $(FL2) $S/comm_mpi.f
ic.o		:$S/ic.f;		$(F77) -c $(FL2) $S/ic.f
conduct.o	:$S/conduct.f;		$(F77) -c $(FL2) $S/conduct.f
navier0.o	:$S/navier0.f;		$(F77) -c $(FL2) $S/navier0.f
navier2.o	:$S/navier2.f;		$(F77) -c $(FL2) $S/navier2.f
navier3.o	:$S/navier3.f;		$(F77) -c $(FL2) $S/navier3.f
navier4_std.o	:$S/navier4_std.f;	$(F77) -c $(FL2) $S/navier4_std.f
navier4_div.o	:$S/navier4_div.f;	$(F77) -c $(FL2) $S/navier4_div.f
navier5.o	:$S/navier5.f;		$(F77) -c $(FL2) $S/navier5.f
navier6.o	:$S/navier6.f;		$(F77) -c $(FL2) $S/navier6.f
navier7.o	:$S/navier7.f;		$(F77) -c $(FL2) $S/navier7.f
navier8.o	:$S/navier8.f;		$(F77) -c $(FL2) $S/navier8.f
speclib.o	:$S/speclib.f;		$(F77) -c $(FL2) $S/speclib.f
calcz.o		:$S/calcz.f;		$(F77) -c $(FL2) $S/calcz.f
fast3d.o	:$S/fast3d.f;		$(F77) -c $(FL2) $S/fast3d.f
################################################################################
dssum.o		:$S/dssum.f;		$(F77) -c $(FL3) $S/dssum.f
eigsolv.o	:$S/eigsolv.f;		$(F77) -c $(FL3) $S/eigsolv.f
gauss.o		:$S/gauss.f;		$(F77) -c $(FL3) $S/gauss.f
planx.o		:$S/planx.f;		$(F77) -c $(FL3) $S/planx.f
ssolv.o		:$S/ssolv.f;		$(F77) -c $(FL3) $S/ssolv.f
gfdm_par.o	:$S/gfdm_par.f;		$(F77) -c $(FL3) $S/gfdm_par.f
gfdm_solve.o	:$S/gfdm_solve.f;	$(F77) -c $(FL3) $S/gfdm_solve.f
gfdm_op.o	:$S/gfdm_op.f;		$(F77) -c $(FL3) $S/gfdm_op.f
################################################################################
coef.o		:$S/coef.f;		$(F77) -c $(FL4) $S/coef.f
mat1.o		:$S/mat1.f;		$(F77) -c $(FL4) $S/mat1.f
mat2std.o	:$S/mat2std.f;		$(F77) -c $(FL4) $S/mat2std.f
################################################################################
plan4.o		:$S/plan4.f;		$(F77) -c $(FL2) $S/plan4.f
qthermal.o	:$S/qthermal.f;	        $(F77) -c $(FL2) $S/qthermal.f
setprop.o	:$S/setprop.f;	        $(F77) -c $(FL2) $S/setprop.f
cvode_driver.o	:$S/cvode_driver.f;	$(F77) -c $(FL2) $S/cvode_driver.f
makeq.o	        :$S/makeq.f;		$(F77) -c $(FL2) $S/makeq.f


################################################################################
#reentrant modules
################################################################################
mat2.o		:$S/mat2.f;		$(F77) -c $(RFLd) $S/mat2.f
mxm.o		:$S/mxm.f;		$(F77) -c $(RFLd) $S/mxm.f
mxm44f2.o	:$S/mxm44f2.f;		$(F77) -c $(RFLd) $S/mxm44f2.f
fastsa.o	:$S/fastsa.f;		$(F77) -c $(RFL4) $S/fastsa.f
################################################################################


################################################################################
#dual mode modules
################################################################################
hmholtz.o	:$S/hmholtz.f;		$(F77) -c $(DFL4) $S/hmholtz.f
navier1.o	:$S/navier1.f;		$(F77) -c $(DFL4) $S/navier1.f
fasts.o		:$S/fasts.f;		$(F77) -c $(DFL4) $S/fasts.f
subuser.o	:$S/subuser.f;		$(F77) -c $(RFL2) $S/subuser.f
################################################################################
################################################################################
# c source files
################################################################################
################################################################################
isosurf.o       :$S/isosurf.c;          $(CC)  -c $(cFL3) $(JL) $S/isosurf.c
byte.o          :$S/byte.c;             $(CC)  -c $(cFL3) $(JL) $S/byte.c
################################################################################
$(JO)errmem.o          :$J/errmem.c;          $(CC) -c $(L4) $(JL) $< -o $@
$(JO)sort.o            :$J/sort.c;            $(CC) -c $(L4) $(JL) $< -o $@
$(JO)crystal.o         :$J/crystal.c;         $(CC) -c $(L4) $(JL) $< -o $@
$(JO)tuple_list.o      :$J/tuple_list.c;      $(CC) -c $(L4) $(JL) $< -o $@
$(JO)transfer.o        :$J/transfer.c;        $(CC) -c $(L4) $(JL) $< -o $@
$(JO)fcrystal.o        :$J/fcrystal.c;        $(CC) -c $(L4) $(JL) $< -o $@
$(JO)gs.o              :$J/gs.c;              $(CC) -c $(L4) $(JL) $< -o $@
$(JO)sparse_cholesky.o :$J/sparse_cholesky.c; $(CC) -c $(L4) $(JL) $< -o $@
$(JO)xxt.o             :$J/xxt.c;             $(CC) -c $(L4) $(JL) $< -o $@
$(JO)poly.o            :$J/poly.c;            $(CC) -c $(L4) $(JL) $< -o $@
$(JO)tensor.o          :$J/tensor.c;          $(CC) -c $(L4) $(JL) $< -o $@
$(JO)findpt.o          :$J/findpt.c;          $(CC) -c $(L4) $(JL) $< -o $@
$(JO)pfindpt.o         :$J/pfindpt.c;         $(CC) -c $(L4) $(JL) $< -o $@
$(JO)findpts.o         :$J/findpts.c;         $(CC) -c $(L4) $(JL) $< -o $@
$(JO)amg.o             :$J/amg.c;             $(CC) -c $(L4) $(JL) $< -o $@
################################################################################
$(JO2)errmem.o          :$(J2)/errmem.c;          $(CC) -c $(L4) $(JL2) $< -o $@
$(JO2)sort.o            :$(J2)/sort.c;            $(CC) -c $(L4) $(JL2) $< -o $@
$(JO2)sarray_sort.o     :$(J2)/sarray_sort.c;     $(CC) -c $(L4) $(JL2) $< -o $@
$(JO2)comm.o            :$(J2)/comm.c;            $(CC) -c $(L4) $(JL2) $< -o $@
$(JO2)crystal.o         :$(J2)/crystal.c;         $(CC) -c $(L4) $(JL2) $< -o $@
$(JO2)sarray_transfer.o :$(J2)/sarray_transfer.c; $(CC) -c $(L4) $(JL2) $< -o $@
$(JO2)gs.o              :$(J2)/gs.c;              $(CC) -c $(L4) $(JL2) $< -o $@
$(JO2)gs_local.o        :$(J2)/gs_local.c;        $(CC) -c $(L4) $(JL2) $< -o $@
$(JO2)fcrs.o            :$(J2)/fcrs.c;            $(CC) -c $(L4) $(JL2) $< -o $@
$(JO2)xxt.o             :$(J2)/xxt.c;             $(CC) -c $(L4) $(JL2) $< -o $@
$(JO2)sparse_cholesky.o :$(J2)/sparse_cholesky.c; $(CC) -c $(L4) $(JL2) $< -o $@
