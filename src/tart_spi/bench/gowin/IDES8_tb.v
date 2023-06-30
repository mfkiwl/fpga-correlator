`timescale 1ns/100ps
module IDES8_tb;

reg reset = 1'b0;
reg fclk = 1'b0;
reg pclk = 1'b1;

always #10 fclk <= ~fclk;
always #40 pclk <= ~pclk;


reg [2:0] d = 3'b000;
reg calib = 1'b0;
wire [2:0] q0, q1, q2, q3, q4, q5, q6, q7;

initial begin : SIM_BLOCK
    $dumpfile ("vcd/IDES8_tb.vcd");
    $dumpvars;

    #5  reset <= 1;
    #80 reset <= 0;

    #800
        calib <= 1;
    #80 calib <= 0;

    #800
        calib <= 1;
    #80 calib <= 0;

    #800
        calib <= 1;
    #80 calib <= 0;

    #800
        calib <= 1;
    #80 calib <= 0;

    #800
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
IDES8 IDES80[2:0] (
    .D(d),
    .FCLK(fclk),
    .PCLK(pclk),
    .CALIB(calib),
    .RESET(reset),
    .Q0(q0),
    .Q1(q1),
    .Q2(q2),
    .Q3(q3),
    .Q4(q4),
    .Q5(q5),
    .Q6(q6),
    .Q7(q7)
);

endmodule // IDES8_tb
