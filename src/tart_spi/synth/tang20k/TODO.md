# Tasks to Complete for the Tang Primer 20k FPGA Dev Board

Tasks:

+ top-level testbench;

+ correlators for the Tang 20k;

## Top Module

Hardware-specific modules required for:

+ PLL's for the master & correlator clocks;

+ source-synchronous capture for IQ, from each of the radio modules;

+ SDRAM controller instance and IOBs;

+ I/O for the visibilities (and acquired raw-data) -- perhaps via USB 2.0?

## Correlators

Need to determine:

+ $f_{max}$ of the correlators, for the `GW2A-LV18` FPGA;
