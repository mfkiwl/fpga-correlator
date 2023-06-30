`timescale 1ns/100ps

//--------------------------------------------------------------------------//
//--------------------------------------------------------------------------//
//                                                                          //
 //                    _____      _      ____    _____                     //
 //                   |_   _|    / \    |  _ \  |_   _|                    //
 //                     | |     / _ \   | |_) |   | |                      //
 //                     | |    / ___ \  |  _ <    | |                      //
 //                     |_|   /_/   \_\ |_| \_\   |_|                      //
 //                                                                        //
//                                                                          //
//--------------------------------------------------------------------------//
//--------------------------------------------------------------------------//

`include "tang20k.v"

module top
  #(// Memory controller parameters:
    // TODO

    //  Antenna/signal parameters:
    parameter ANTENNAE = `NUM_ANTENNA,
    parameter NSB      = ANTENNAE-1,
    parameter ALIGN    = `USE_ALIGN,

    //  Simulation-only parameters:
    parameter DELAY    = `DELAY)     // Simulation gate-delay setting
   (
    // SDRAM
    output wire        SDRAM_CLK,
    output wire        SDRAM_CKE,
    output wire        SDRAM_CS,
    output wire        SDRAM_RAS,
    output wire        SDRAM_CAS,
    output wire        SDRAM_WE,
    output wire [1:0]  SDRAM_DQM,
    output wire [12:0] SDRAM_ADDR,
    output wire [1:0]  SDRAM_BA,
    inout wire [15:0]  SDRAM_DQ,
   
    // SPI
    input              SPI_SCK,
    input              SPI_SSEL,
    input              SPI_MOSI,
    output wire        SPI_MISO,
   
    // TELESCOPE
    input              rx_clk_16, // 16.368 MHz receiver master clock
    input [NSB:0]      antenna, // Radio Data Interface

    // MISCELLANEOUS
    output wire        led              // Papilio LED
    );



   //-------------------------------------------------------------------------
   //
   //     ADDITIONAL TART SETTINGS
   //
   //-------------------------------------------------------------------------
   //  Visibilities/correlator settings.
   parameter ACCUM = `ACCUM_BITS; // Bit-width of the accumulators
   parameter BLOCK = ACCUM;       // Maximum #bits of the block-size
   parameter MSB   = BLOCK-1;     // Data transfer MSB
   parameter TRATE = `TMUX_RATE;  // Time-multiplexing rate
   parameter TBITS = `TMUX_BITS;   // TMUX bits
   parameter COUNT = `VISB_LOG2;  // (1 << 3) - 1;
   parameter NREAD = `READ_COUNT; // Number of visibilities to read back
   parameter RBITS = `READ_BITS;  // = ceiling{log2(NREAD)};
   parameter RSB   = RBITS-1;     // MSB of read-back address

   //  Wishbone settings.
   parameter BBITS = `WBBUS_BITS; // Bit-width of the SoC Wishbone bus
   parameter BSB   = BBITS-1;     // Bus data MSB
   parameter WSB   = BBITS-2;     // SPI -> WB bus address-width
   parameter ABITS = `WBADR_BITS; // Correlator bus address bit-width
   parameter ASB   = ABITS-1;     // Address MSB

   //  Settings for the visibilities data banks:
   parameter XBITS = `BANK_BITS;  // Bit-width of the block-counter
   parameter XSB   = XBITS-1;     // MSB of the block-counter

   //  Internal correlator data-bus settings:
   parameter CBITS = XBITS+RBITS; // Correlator-bus address bit-width
   parameter CSB   = CBITS-1;



endmodule // top
