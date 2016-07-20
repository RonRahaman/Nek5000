AUTOMAKE_OPTIONS = foreign subdir-objects

include ../Makefile.problem

PROBLEM         = vortex
DRIVER          = single_physics_vortex

single_physics_vortex_SOURCES = ../../../../tests/drivers/single_physics.cxx $(DEFAULT_DRIVER_SRCS)
single_physics_vortex_LDADD = $(LDADD)
single_physics_vortex_CPPFLAGS = -DENABLE_NEK

