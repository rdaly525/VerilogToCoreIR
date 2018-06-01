module top4(
  input clk,
  input [15:0] in,
  output [15:0] out
);

  wire [15:0] tmp;
  assign tmp = in;

  (* namespace="coreir" *)
  \reg #(.width(16)) reg_inst(
    .in(tmp),
    .out(out),
    .clk(clk)
  );

endmodule
