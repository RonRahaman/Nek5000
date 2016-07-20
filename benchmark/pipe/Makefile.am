AUTOMAKE_OPTIONS = foreign subdir-objects

include ../Makefile.problem

PROBLEM         = pipe
DRIVER          = single_physics_pipe

single_physics_pipe_SOURCES = ../../../../tests/drivers/single_physics.cxx $(DEFAULT_DRIVER_SRCS)
single_physics_pipe_LDADD = $(LDADD)
single_physics_pipe_CPPFLAGS = -DENABLE_NEK

