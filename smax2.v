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

  wire [11:0] tmp2;
  smax #(.width(12)) smax_inst1(
    .in0(a[11:0]),
    .in1(b[11:0]),
    .out(tmp2)
  );
  smax #(.width(16)) smax_inst(
    .in0(tmp),
    .in1({tmp2,4'h3}),
    .out(max)
  );

endmodule
