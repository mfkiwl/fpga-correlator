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
reg [3:0] shreg1, shreg2;
reg [1:0] ptr;


// Primary (slow) clock
always @(posedge PCLK) begin
    if (RESET)
        ptr <= 2'b00;
    else if (CALIB)
        ptr <= ptr + 1;
    else
        ptr <= ptr;
end

always @(posedge PCLK) begin
    shreg2 <= shreg1;
    {Q0, Q1, Q2, Q3} <= shreg2;
    // {Q3, Q2, Q1, Q0} <= shreg2;
end


// Capture (fast) clock
always @(posedge FCLK) begin
    shreg0 <= {shreg0[2:0], D};
end

always @(negedge FCLK) begin
    shreg0 <= {shreg0[2:0], D};
end

wire [3:0] shift0 = {shreg0[1:0], shreg0[3:2]};
wire [3:0] shift1 = {shreg0[2:0], shreg0[3]};
// wire [3:0] shift0 = {shreg0[0], shreg0[3:1]};
// wire [3:0] shift1 = {shreg0[1:0], shreg0[3:2]};

wire bit0 = shift0[ptr];
wire bit1 = shift1[ptr];

always @(posedge FCLK) begin
    shreg1 <= {shreg1[1:0], bit1, bit0};
end

/*
always @(posedge FCLK) begin
    case (ptr)
        2'b00: shreg1 <= {shreg1[1:0], shreg0[3:2]};
        2'b01: shreg1 <= {shreg1[1:0], shreg0[2:1]};
        2'b10: shreg1 <= {shreg1[1:0], shreg0[1:0]};
        2'b11: shreg1 <= {shreg1[1:0], shreg0[0], shreg0[3]};
    endcase
    // shreg1 <= {shreg1[2:0], shreg0[ptr]};
end
*/


endmodule // IDES4
