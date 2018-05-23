module top(
  input [15:0] a,
  input [15:0] b,
  output [15:0] max
);

  wire [15:0] tmp;
  assign tmp = a;


  (* namespace="commonlib" *)
  smax #(.width(16)) smax_inst(
    .in0(tmp),
    .in1(b),
    .out(max)
  );


endmodule
