#
# NOTE: currently, the only working Makefile targets are 'all' and 'dsp'.
.PHONY:	all dsp cor fak ctl blk fch arb

IVC	?= iverilog
INC	= ../include/
OPT	= -D__icarus -Wall -I$(INC)

OUT	?= ../build


#
#  Library cores
# -------------------------------------------------------------------------- #
# Source directories:
RTL	= ../verilog
MISC	= $(RTL)/misc

# Some Wishbone-like cores:
BUS	= ../../wishbone/rtl
BUSRTL	= $(wildcard $(BUS)/*.v)

# Common library cores:
FIFO	= $(wildcard $(RTL)/fifo/*.v)
XILINX	= $(wildcard $(RTL)/xilinx/*.v)
MISCRTL	= $(MISC)/shift_reg.v $(RTL)/mfsr/mfsr32.v $(RTL)/tart_control.v
LIB	= $(FIFO) $(BUSRTL) $(XILINX) $(MISCRTL)

# SPI core:
SPI	= $(RTL)/spi
SPIRTL	= $(wildcard $(SPI)/*.v)

# Data acquisition logic:
ACQ	= $(RTL)/acquire
ACQRTL	= $(wildcard $(ACQ)/*.v)

# Correlator files:
CORD	= $(RTL)/correlator
CORRTL	= $(wildcard $(CORD)/*.v)
CORTOP	= $(CORD)/tart_dsp.v $(CORD)/tart_fake_dsp.v
COR	= $(filter-out $(CORTOP), $(CORRTL))

# DSP files:
DSP	= $(COR) $(CORD)/tart_dsp.v
FAK	= $(COR) $(CORD)/tart_fake_dsp.v

# Common dependencies:
DEP	= $(LIB) $(SPIRTL) $(ACQ)/dram_prefetch.v

# TODO:
TOP	= $(DEP) $(COR) $(ACQRTL) $(RTL)/tart.v

#
# Simulation-only files:
XIL	= xilinx
SIM	= $(XIL)/DSP48A1.v $(XIL)/LUT6.v $(XIL)/MUXF7.v $(XIL)/IDDR2.v


#
#  TART DSP simulation
# -------------------------------------------------------------------------- #
#  Override this from the command-line to do shorter/longer calculations,
#  where the number of correlations performed is '2^^COUNTER_BITS' (for each
#  of the two visibilities blocks).
COUNTER_BITS	?= 12

all:	dsp

DSP_ARG	= $(OPT) -DCOUNTER_BITS=$(COUNTER_BITS) -s tart_dsp_tb
dsp:	${DEP} ${DSP} tart_dsp_tb.v
	$(IVC) $(DSP_ARG) -o $(OUT)/tart.out $(DEP) $(DSP) $(SIM) tart_dsp_tb.v


#
#  BROKEN !?
# -------------------------------------------------------------------------- #
LEGACY	= $(wildcard ../../wishbone/legacy/*.v)
COR_TB	= $(wildcard $(CORD)/*.v) $(LIB) $(ACQRTL) $(SPIRTL) $(LEGACY)
cor:	$(COR_TB) $(SIM) correlator/tart_correlator_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(COR_TB) $(SIM) correlator/tart_correlator_tb.v

fak: ${DEP} ${FAK} tart_dsp_tb.v
	$(IVC) $(OPT) -D__USE_FAKE_DSP -o $(OUT)/tart.out $(DEP) $(FAK) $(SIM) tart_dsp_tb.v

BLK	= $(CORD)/bank_switch.v $(RTL)/correlator_block_SDP.v $(RTL)/correlator_block_DSP.v $(RTL)/correlator_SDP.v $(RTL)/correlator_DSP.v $(RTL)/rmw_address_unit.v $(RTL)/correlate_cos_sin.v $(RTL)/correlate_cos_sin_DSP.v $(RTL)/fake_hilbert.v $(RTL)/xilinx/RAM32X6_SDP.v $(RTL)/xilinx/RAMB8X32_SDP.v $(RTL)/xilinx/MUX8.v $(BUS)/wb_get_block.v $(RTL)/mfsr/mfsr32.v

blk:	${BLK} tart_block_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(BLK) $(SIM) tart_block_tb.v

ctl:	${COR} correlator/control_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(COR) $(SIM) control_tb.v

#
#  Simulate Wishbone (B4 spec) pipelined BURST READS & WRITES.
#
FCH =$(BUS)/wb_fetch.v $(BUS)/wb_store.v $(BUS)/wb_sram_port.v $(RTL)/xilinx/RAMB16X32X8_TDP.v
fch:	${FCH} bus/wb_fetch_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(FCH) bus/wb_fetch_tb.v

#  Simulate the TART Wishbone arbitration and control logic.
ARB	= ${RTL}/tart_wishbone.v
arb:	${ARB} tart_wishbone_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(ARB) tart_wishbone_tb.v


.PHONY: run
run: all
	@$(OUT)/tart.out

dir:
	@mkdir $(OUT)

.PHONY: clean
clean:
	rm -f *~ $(OUT)/tart.out
