module smax (
  input [16-1:0] in0,
  input [16-1:0] in1,
  output [16-1:0] out
);
  
  assign out = in0 > in1;

endmodule

module top1(
  input [15:0] a,
  input [15:0] b,
  output [15:0] max
);

  wire [15:0] tmp;
  assign tmp = a;
 
  smax smax_inst(
    .in0(tmp),
    .in1(b),
    .out(max)
  );

endmodule
