`timescale 1ns/100ps
module IDES4 (
    input D,
    input FCLK,
    input PCLK,
    input CALIB,
    input RESET,
    output reg Q0,
    output reg Q1,
    output reg Q2,
    output reg Q3
    );

// DDR registers
reg [3:0] shreg0;

// Primary clock-domain registers
reg [3:0] shreg1;
reg [1:0] ptr;


// Primary (slow) clock
always @(posedge PCLK or posedge RESET) begin
    if (RESET)
        ptr <= 2'b00;
    else if (CALIB)
        ptr <= ptr + 1;
    else
        ptr <= ptr;
end

always @(posedge PCLK or posedge RESET) begin
    if (RESET)
        {Q3, Q2, Q1, Q0} <= 4'b0000;
    else
        {Q3, Q2, Q1, Q0} <= shreg1;
end


// Capture (fast) clock
reg [1:0] p_shreg, n_shreg;

always @(posedge FCLK) begin
    if (RESET)
        p_shreg <= 2'b00;
    else
        p_shreg <= {D, p_shreg[1]};
end

always @(negedge FCLK) begin
    if (RESET)
        n_shreg <= 2'b00;
    else
        n_shreg <= {D, n_shreg[1]};
end

wire p_hi = ptr[1];
wire p_lo = ptr[0];

wire bit0 = p_lo ? n_shreg[p_hi] : p_shreg[p_hi];
wire bit1 = p_lo ? p_shreg[~p_hi] : n_shreg[p_hi];

always @(posedge FCLK) begin
    if (RESET) begin
        shreg0 <= 4'b0000;
        shreg1 <= 4'b0000;
    end
    else begin
        shreg0 <= {bit1, bit0, shreg0[3:2]};
        shreg1 <= shreg0;
    end
end


endmodule // IDES4
