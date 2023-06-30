TART	:= ../tart_spi

RTL	:= $(wildcard rtl/*.v)
EXAMPLE	:= $(wildcard example/*.v)
XILINX	:= $(wildcard $(TART)/verilog/xilinx/*.v) $(wildcard $(TART)/bench/xilinx/*.v)
LEGACY	:= $(wildcard legacy/*.v)

SRC	:= $(RTL) $(EXAMPLE) $(XILINX) $(LEGACY)

BENCH	?= bench
BENCHES	:= $(wildcard $(BENCH)/*_tb.v)
EXECS	:= $(BENCHES:bench/%_tb.v=build/%.out)
# BENCHES	:= $(filter %_tb.v, $(SRC))

.PHONY:	dir all run clean
all:	$(EXECS) dir
	@echo $(SRC)
	@echo $(EXECS)

dir:
	@mkdir -p build

run:	all
	echo "TODO"

build/%.out: bench/%_tb.v $(SRC)
	+iverilog $< $(SRC) -I$(TART)/include/ -o $@
