module uut(in1, out1, out2);

   input [1:0] in1;
   
   output [1:0] out1, out2;

   assign out1 = !in1;

   assign out2 = !in1;
   
   

endmodule
