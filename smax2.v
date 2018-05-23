module smax #(parameter width=1) (
  input [width-1:0] in0,
  input [width-1:0] in1,
  output [width-1:0] out
);
  
  assign out = in0 > in1;

endmodule

module top2(
  input [15:0] a,
  input [15:0] b,
  output [15:0] max
);

  wire [15:0] tmp;
  assign tmp = a;

  smax #(.width(16)) smax_inst(
    .in0(tmp),
    .in1(b),
    .out(max)
  );

endmodule
