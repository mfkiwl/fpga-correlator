`timescale 1ns/100ps

module inner_correlator #(
    parameter       ACCUM   = 24,
    parameter       TRATE   = 12,
    parameter       DELAY   = 3
) (
    input           clock_x,
    input           reset_x,

    input           valid_i,
    output          ready_o,
    input           tlast_i,
    input  [MSB:0]  dat_q_i,
    input  [MSB:0]  dat_i_i,

    output          valid_o,
    input           ready_i,
    output          tlast_o,
    output [MSB:0]  dat_q_o,
    output [MSB:0]  dat_i_o
);


reg [ASB:0] ssram_cos   [0:TRATE-1];
reg [ASB:0] ssram_sin   [0:TRATE-1];

reg ready = 0;
reg valid = 0;
reg [ASB:0] dat_q;
reg [ASB:0] dat_i;

wire [ASB:0] new_cos = #DELAY ssram_cos[ptr] + data_q_i[i]*dat_q_i[j];
wire [ASB:0] new_sin = #DELAY ssram_sin[ptr] + data_q_i[i]*dat_i_i[j];

always @(posedge clock_x) begin
    if (reset_x) begin
        ready <= 0;
        valid <= 0;
    end
    else begin
        if (tlast_i && valid_i && ready_o)
            valid <= 1'b1;
        else if (ready_i && valid)
            valid <= 1'b0;
        else
            valid <= valid;
    end
end

always @(posedge clock_x) begin
    if (tlast_i && valid_i && ready_o) begin
        ssram_cos[ptr] <= #DELAY 0;
        ssram_sin[ptr] <= #DELAY 0;

        dat_q <= new_cos;
        dat_i <= new_sin;
    end
    else begin
        ssram_cos[ptr] <= new_cos;
        ssram_sin[ptr] <= new_sin;
    end

    ptr <= #DELAY ptr + 1;
end


endmodule // inner_correlator
