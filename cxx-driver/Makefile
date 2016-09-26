SOURCE_ROOT = ${HOME}/repos/Nek5000
CASEDIR     = eddy
CASENAME    = eddy_uv
CXXFLAGS    = -Wall

all: $(CASEDIR)/cxx-driver $(CASEDIR)/SESSION.NAME 

$(CASEDIR)/cxx-driver: main.cpp $(CASEDIR)/libnek5000.a $(CASEDIR)/obj/$(CASENAME).o
	g++ $(CXXFLAGS) $< -o $@ $(CASEDIR)/obj/$(CASENAME).o -L$(CASEDIR) -lnek5000 -lgfortran

$(CASEDIR)/libnek5000.a: run_makenek
	make -C $(CASEDIR) lib

$(CASEDIR)/obj/$(CASENAME).o: run_makenek

run_makenek: 
	cd $(CASEDIR) && ./makenek $(CASENAME) $(SOURCE_ROOT)

$(CASEDIR)/SESSION.NAME:
	cd $(CASEDIR) && \
		echo $(CASENAME) > SESSION.NAME && \
		pwd >> SESSION.NAME

clean: 
	make -C $(CASEDIR) clean
	cd $(CASEDIR) && rm -f cxx-driver libnek5000.a SESSION.NAME *.sch 

.PHONY: run_makenek clean
