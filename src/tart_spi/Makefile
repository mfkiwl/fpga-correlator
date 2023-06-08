##--------------------------------------------------------------------------##
##
##  Makefile for command line synthesis, using Gowin tools.
##
##  Copyright:
##    Tim Molteno, 2023
##
##  NOTE:
##   + early  ~/gowin/IDE/bin/GowinSynthesis -i verilog/tart.v -top tart -pn GW2A-LV18PG256C8/I7 -dir include/
##
##  TODO:
##   + needs a different floorplan when correlators are not used;
##   + improve the `upload`, `images`, and `clean` Makefile targets;
##
##--------------------------------------------------------------------------##

GOWIN_SYNTH=~/gowin/IDE/bin/GowinSynthesis
SHELL=/bin/bash

standard: build upload

# TODO: use `papilio-prog`
upload: program

# Build the various prebuilt images
# TODO: build for several configurations
images: build

all: pairs permute floor build upload

.PHONY: pairs
pairs:
	runhaskell script/pairs.hs --antennae=24 --blocksize=6 --multiplex=12 --outfile include/tart_pairs.v

.PHONY: permute
permute:
	runhaskell script/pairs.hs --antennae=24 --blocksize=6 --multiplex=12 --permute --outfile data/permute.txt

.PHONY: hex
hex:
	runhaskell script/pairs.hs --antennae=24 --blocksize=6 --multiplex=12 --hex --verbose --outfile data/permute.hex

.PHONY: floor
floor:
	runhaskell script/floor.hs --numDSP=4 --prefix="DSP/COR/CXB" --outfile papilio/floor.ucf



##--------------------------------------------------------------------------##
##
##  GOWIN SYNTHESIS STUFF.
##
##--------------------------------------------------------------------------##

# Provide some defaults for the user settings
TOP_MODULE=$(shell echo ${GFILES} | cut -d\. -f1)
PINOUT_STEM=${TOP_MODULE}
PINOUT=${PINOUT_STEM}-$(shell echo ${TARGET} | cut -d- -f1,2).ucf
#########################################################
# User settings start here
#########################################################

# Target as part_num-package-speed_grade. USE LOWER CASE
#  Papilio
TARGET=GW2A-LV18PG256C8/I7


##--------------------------------------------------------------------------##
##  Synthesis files.
##--------------------------------------------------------------------------##
DIR=$(shell pwd)
RTL=${DIR}/verilog
UCF=${DIR}/papilio
INC=${DIR}/include

LIB_FILES=${RTL}/fifo/afifo_gray.v ${RTL}/mfsr/mfsr32.v ${RTL}/misc/shift_reg.v

WB4=${DIR}/../wishbone/rtl
WB4_FILES=${WB4}/wb_transfer.v ${WB4}/wb_cycle.v ${WB4}/wb_sram_port.v ${WB4}/wb_sram_prefetch.v ${WB4}/wb_fetch.v ${WB4}/wb_sram_stream.v ${WB4}/wb_reset.v

XIL=${RTL}/xilinx
XIL_FILES=${XIL}/MUX8.v ${XIL}/RAM32X6_SDP.v ${XIL}/RAMB8X32_SDP.v ${XIL}/RAMB16X16X4_TDP.v

SPI=${RTL}/spi
SPI_FILES=${SPI}/spi_slave_wb.v ${SPI}/spi_layer.v

CAP=${RTL}/capture
CAP_FILES=${CAP}/tart_capture.v ${CAP}/signal_phase_DDR.v ${CAP}/signal_source.v ${CAP}/fake_telescope.v

ACQ=${RTL}/acquire
ACQ_FILES=${ACQ}/tart_acquire.v ${ACQ}/raw_acquire.v ${ACQ}/dram_prefetch.v ${ACQ}/fifo_control.v ${ACQ}/block_buffer.v

DSP=${RTL}/correlator
DSP_FILES=${DSP}/tart_dsp.v ${DSP}/tart_correlator.v ${DSP}/correlator_block.v ${DSP}/tart_visibilities.v ${DSP}/control.v ${DSP}/block_DSP.v ${DSP}/block_SDP.v ${DSP}/correlate_cos_sin.v ${DSP}/correlate_cos_sin_DSP.v ${DSP}/correlator_DSP.v ${DSP}/correlator_SDP.v ${DSP}/fake_hilbert.v ${DSP}/rmw_address_unit.v ${DSP}/bank_switch.v


##--------------------------------------------------------------------------##
##  Simulation files.
##--------------------------------------------------------------------------##
SIM=${DIR}/bench


##--------------------------------------------------------------------------##
##  File & top-level groups.
##--------------------------------------------------------------------------##
COMMON_FILES=${RTL}/tart.v ${RTL}/tart_dual_dcm.v ${RTL}/tart_wishbone.v ${RTL}/tart_control.v ${RTL}/SDRAM_Controller_v.v ${LIB_FILES} ${XIL_FILES} ${WB4_FILES} ${SPI_FILES} ${CAP_FILES} ${ACQ_FILES} ${DSP_FILES}

GFILES=${COMMON_FILES}
IFILES=${COMMON_FILES} ${SIM}/tart_dsp_tb.v ${SIM}/xilinx/IDDR2.v ${SIM}/xilinx/DSP48A1.v 

# (optional) Name of top module. Defaults to name of first file with suffix removed
TOP_MODULE=tart
ICARUS_TOP_MODULE=tart_dsp_tb


# (optional) List of modules for a show resources report (FPGA only)
RSRC_REPORT=

# (optional) Name of Contraints file. Defaults to PINOUT_STEM-PART-PACKAGE.ucf
PINOUT=${UCF}/constraints.ucf
FLOOR =${UCF}/floor.ucf

# Place to stick all Xilinx-generated files
BUILD =${DIR}/build

# (optional) Name of PINOUT_STEM. Defaults to TOP_MODULE
#PINOUT_STEM=testing

# whether to require matching of all the paramaters in the .ucf file true/false
# WARNING: if this is false, be careful of spelling errors ... they won't appear as warnings
#STRICT_UCF=false

# opt level is 1,2,3 or 4. 4 will take some time (maybe hours). 5 is just insane. Default is 1
OPT_LEVEL=4

# opt mode is one of speed or area
# OPT_MODE=area
OPT_MODE=speed

# quiet mode - true or false.
QUIET_MODE=false


ICARUS_OPTIONS=-I ./include/

#########################################################
# User settings end here
#########################################################

# Internal settings. Probably no need to tamper with these


prom:
	${XILINX}/promgen -w -u 0 ${X_PREFIX}.bit -p mcs


# .PHONY: clean
clean:
	@rm -rf demo.s
	@rm -rf ${X_LABEL}_dir ${X_LABEL}.* ${X_PREFIX}.* ${BUILD}/_* *.srp *.lso tmperr.err icarus.out
	@md5sum Makefile > make.md5

clean_test:
ifneq '$(shell cat make.md5)' '$(shell md5sum Makefile)'
	@echo 'Makefile changed: making clean'
	make clean
else
endif

test:
	echo ${TEST}

#########################################################

icarus: clean_test ${IFILES}
	iverilog -D__icarus -Wall -o icarus.out -s ${ICARUS_TOP_MODULE} ${ICARUS_OPTIONS} ${IFILES}

#########################################################
# Synthesis Follows


help:
	${GOWIN_SYNTH} -h

build: clean_test
	${GOWIN_SYNTH} -i ${GFILES} -top tart -pn ${TARGET} -inc include/

program: clean_test clean_test build
	. ${X_SETTINGS} ; impact -batch impact_${PART_TYPE}.cmd

