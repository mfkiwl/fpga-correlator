SRC	:= $(wildcard *.v)
RTL	:= $(filter-out %_tb.v, $(SRC))
BENCH	:= $(filter %_tb.v, $(SRC))

BUILD	:= ../../build
EXECS	:= $(BENCH:%_tb.v=$(BUILD)/%.out)

.PHONY:	dir all run clean
all:	$(EXECS) dir

dir:
	@mkdir -p $(BUILD)

run:	all
	echo "TODO"

clean:
	@rm $(EXECS)

#
#  Implicit rules
##
# OPT	:= -I$(TART)/include/
../../build/%.out: %_tb.v $(RTL)
	+iverilog $< $(RTL) $(OPT) -s $*_tb -o $@
