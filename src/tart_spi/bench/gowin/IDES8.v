module IDES8 (
    input D,
    input FCLK,
    input PCLK,
    input CALIB,
    input RESET,
    output reg Q0,
    output reg Q1,
    output reg Q2,
    output reg Q3,
    output reg Q4,
    output reg Q5,
    output reg Q6,
    output reg Q7
    );

// DDR registers
reg [7:0] shreg0;

// Primary clock-domain registers
reg [7:0] shreg1, shreg2;
reg [2:0] ptr;


// Primary (slow) clock
always @(posedge PCLK) begin
    if (RESET)
        ptr <= 3'b000;
    else if (CALIB)
        ptr <= ptr + 1;
    else
        ptr <= ptr;
end

always @(posedge PCLK) begin
    shreg2 <= shreg1;
    {Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7} <= shreg2;
end


// Capture (fast) clock
always @(posedge FCLK) begin
    shreg0 <= {shreg0[6:0], D};
end

always @(negedge FCLK) begin
    shreg0 <= {shreg0[6:0], D};
end

// TODO: finish and test the bit-selections and delays
wire [7:0] shift0 = {shreg0[5:0], shreg0[7:6]};
wire [7:0] shift1 = {shreg0[6:0], shreg0[7]};

wire bit0 = shift0[ptr];
wire bit1 = shift1[ptr];

always @(posedge FCLK) begin
    shreg1 <= {shreg1[5:0], bit1, bit0};
end


endmodule // IDES8
