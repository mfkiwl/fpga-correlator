`timescale 1ns/100ps
module IDES4_tb;

reg reset = 1'b0;
reg fclk = 1'b0;
reg pclk = 1'b1;

always #10 fclk <= ~fclk;
always #20 pclk <= ~pclk;


reg [1:0] d = 2'b00;
reg calib = 1'b0;
// wire [7:0] q;
wire [1:0] q0, q1, q2, q3;

initial begin : SIM_BLOCK
    $dumpfile ("vcd/IDES4_tb.vcd");
    $dumpvars;

    #5  reset <= 1;
    #40 reset <= 0;

    #400
        calib <= 1;
    #40 calib <= 0;

    #400
        calib <= 1;
    #80 calib <= 0;

    #400
        calib <= 1;
    #80 calib <= 0;

    #400
        calib <= 1;
    #40 calib <= 0;

    #400
    $display ("\n%8t: Simulation finished", $time);
    $finish;
end // SIM_BLOCK


// DDR source signals
reg dclk = 1'b1;
always #5 dclk <= ~dclk;

always @(posedge dclk) begin : DDR_CLOCK_DATA
    d <= #5 d+1;
end // DDR_CLOCK_DATA


//
//  Device under test
//
IDES4 IDES40[1:0] (
    .D(d),
    .FCLK(fclk),
    .PCLK(pclk),
    .CALIB(calib),
    .RESET(reset),
    .Q0(q0),
    .Q1(q1),
    .Q2(q2),
    .Q3(q3)
);

endmodule // IDES4_tb
