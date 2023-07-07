`timescale 1ns/100ps
module accum
  #( parameter SRC_WIDTH = 3,
     parameter SSB = SRC_WIDTH - 1,
     parameter DST_WIDTH = 6,
     parameter DSB = DST_WIDTH - 1,
     parameter RATE_BITS = 3, // DST_WIDTH - SRC_WIDTH
     parameter TRATE = 1 << 3
) (  input          clock_i,
     input          reset_i,

     input          enable_i,
     input          start_i,

     input          valid_i,
     output         ready_o,
     input [SSB:0]  real_i,
     input [SSB:0]  imag_i,

     output         valid_o,
     input          ready_i,
     output [DSB:0] real_o,
     output [DSB:0] imag_o
     );

localparam WSB = TRATE - 1;

    reg [DSB:0]     re_sram [WSB:0];
    reg [DSB:0]     im_sram [WSB:0];

    wire [DSB:0]    rd_ptr_next = rd_ptr + 1;

    reg [DSB:0]     re_dat, im_dat;
    reg             eval = 0;


always @(posedge clock_i) begin
    if (reset_i) begin
        rd_ptr <= 0;
        wr_ptr <= 0;
        eval <= 0;
        write <= 0;
    end
    else begin
        if (ready_o && valid_i) begin
            rd_ptr <= rd_ptr_next;
            wr_ptr <= rd_ptr;

            re_dat <= re_sram[rd_ptr];
            im_dat <= im_sram[rd_ptr];

            eval <= 1;
            write <= eval;
        end
        else begin
            eval <= 0;
            write	<= eval;
        end
    end
end


always @(posedge clock_i) begin
    if (eval) begin
        re_sum <= re_sum + real_i;
        im_sum <= im_sum + imag_i;
    end
end


always @(posedge clock_i) begin
    if (write) begin
        re_sram[wr_ptr] <= re_sum;
        im_sram[wr_ptr] <= im_sum;
    end
end


endmodule // accum
