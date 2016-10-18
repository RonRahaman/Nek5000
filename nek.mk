# Assume that NEK_DIR and NEK_CASENAME are defined by parent Makefile
$(NEK_OBJ): run_makenek

#all:: $(NEK_OBJ) $(NEK_LIB) SESSION.NAME

run_makenek:
	$(NEK_SOURCE_DIR)/core/makenek $(NEK_CASENAME) $(NEK_SOURCE_DIR)

SESSION.NAME:
	echo $(NEK_CASENAME) > SESSION.NAME && echo $(NEK_CASE_DIR) >> SESSION.NAME

.PHONY: run_makenek

$(NEK_LIB): run_makenek
	make lib
