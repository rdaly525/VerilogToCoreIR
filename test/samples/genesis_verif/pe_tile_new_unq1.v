//
//--------------------------------------------------------------------------------
//          THIS FILE WAS AUTOMATICALLY GENERATED BY THE GENESIS2 ENGINE        
//  FOR MORE INFORMATION: OFER SHACHAM (CHIP GENESIS INC / STANFORD VLSI GROUP)
//    !! THIS VERSION OF GENESIS2 IS NOT FOR ANY COMMERCIAL USE !!
//     FOR COMMERCIAL LICENSE CONTACT SHACHAM@ALUMNI.STANFORD.EDU
//--------------------------------------------------------------------------------
//
//  
//	-----------------------------------------------
//	|            Genesis Release Info             |
//	|  $Change: 11904 $ --- $Date: 2013/08/03 $   |
//	-----------------------------------------------
//	
//
//  Source file: /Users/dillon/VerilogWorkspace/CGRAGenerator/hardware/generator_z/pe_tile_new/pe_tile_new.vp
//  Source template: pe_tile_new
//
// --------------- Begin Pre-Generation Parameters Status Report ---------------
//
//	From 'generate' statement (priority=5):
// Parameter bus_config 	= BUS16:16b#1_4:1_4:1_4:1_4:1_4 BUS1:1b#1_4:1_4:1_4:1_4:1_4
// Parameter is_bidi 	= 0
// Parameter has_constant 	= 1
// Parameter mult_mode 	= 1
// Parameter cb_connections 	= 1111111111
// Parameter is_msb 	= 0
// Parameter en_double 	= 0
// Parameter use_shift 	= 1
// Parameter reg_inputs 	= 1
// Parameter sb_fs 	= 10000#10000#10000
// Parameter use_cntr 	= 1
// Parameter lut_inps 	= 3
// Parameter use_add 	= 1
// Parameter use_bool 	= 1
// Parameter sides 	= 4
// Parameter use_div 	= 0
// Parameter reg_out 	= 0
// Parameter all_segments_for_all_tiles 	= 1
// Parameter registered_outputs 	= BUS16:11111 BUS1:11111
// Parameter feedthrough_outputs 	= BUS16:00000 BUS1:00000
//
//		---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
//
//	From Command Line input (priority=4):
//
//		---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
//
//	From XML input (priority=3):
//
//		---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
//
//	From Config File input (priority=2):
//
// ---------------- End Pre-Generation Pramameters Status Report ----------------

///////////////////////////////////////////////////////////////////
// CGRA PE generator 
//
// (C) Stanford University
// Please do not remove this header
//////////////////////////////////////////////////////////////////
// bus_config (_GENESIS2_INHERITANCE_PRIORITY_) = BUS16:16b#1_4:1_4:1_4:1_4:1_4 BUS1:1b#1_4:1_4:1_4:1_4:1_4
//
// all_segments_for_all_tiles (_GENESIS2_INHERITANCE_PRIORITY_) = 1
//
// sides (_GENESIS2_INHERITANCE_PRIORITY_) = 4
//
// feedthrough_outputs (_GENESIS2_INHERITANCE_PRIORITY_) = BUS16:00000 BUS1:00000
//
// registered_outputs (_GENESIS2_INHERITANCE_PRIORITY_) = BUS16:11111 BUS1:11111
//
// is_bidi (_GENESIS2_INHERITANCE_PRIORITY_) = 0
//
// sb_fs (_GENESIS2_INHERITANCE_PRIORITY_) = 10000#10000#10000
//

// cb_connections (_GENESIS2_INHERITANCE_PRIORITY_) = 0x423a35c7
//
// has_constant (_GENESIS2_INHERITANCE_PRIORITY_) = 1
//

// reg_inputs (_GENESIS2_INHERITANCE_PRIORITY_) = 1
//
// reg_out (_GENESIS2_INHERITANCE_PRIORITY_) = 0
//
// use_add (_GENESIS2_INHERITANCE_PRIORITY_) = 1
//
// use_cntr (_GENESIS2_INHERITANCE_PRIORITY_) = 1
//
// use_bool (_GENESIS2_INHERITANCE_PRIORITY_) = 1
//
// use_shift (_GENESIS2_INHERITANCE_PRIORITY_) = 1
//
// mult_mode (_GENESIS2_INHERITANCE_PRIORITY_) = 1
//
// use_div (_GENESIS2_INHERITANCE_PRIORITY_) = 0
//
// is_msb (_GENESIS2_INHERITANCE_PRIORITY_) = 0
//
// en_double (_GENESIS2_INHERITANCE_PRIORITY_) = 0
//
// lut_inps (_GENESIS2_INHERITANCE_PRIORITY_) = 3
//



module pe_tile_new_unq1 (
clk,
config_addr,
config_data,
out_BUS1_S0_T0,
in_BUS1_S0_T0,
out_BUS1_S0_T1,
in_BUS1_S0_T1,
out_BUS1_S0_T2,
in_BUS1_S0_T2,
out_BUS1_S0_T3,
in_BUS1_S0_T3,
out_BUS1_S0_T4,
in_BUS1_S0_T4,
out_BUS1_S1_T0,
in_BUS1_S1_T0,
out_BUS1_S1_T1,
in_BUS1_S1_T1,
out_BUS1_S1_T2,
in_BUS1_S1_T2,
out_BUS1_S1_T3,
in_BUS1_S1_T3,
out_BUS1_S1_T4,
in_BUS1_S1_T4,
out_BUS1_S2_T0,
in_BUS1_S2_T0,
out_BUS1_S2_T1,
in_BUS1_S2_T1,
out_BUS1_S2_T2,
in_BUS1_S2_T2,
out_BUS1_S2_T3,
in_BUS1_S2_T3,
out_BUS1_S2_T4,
in_BUS1_S2_T4,
out_BUS1_S3_T0,
in_BUS1_S3_T0,
out_BUS1_S3_T1,
in_BUS1_S3_T1,
out_BUS1_S3_T2,
in_BUS1_S3_T2,
out_BUS1_S3_T3,
in_BUS1_S3_T3,
out_BUS1_S3_T4,
in_BUS1_S3_T4,
out_BUS16_S0_T0,
in_BUS16_S0_T0,
out_BUS16_S0_T1,
in_BUS16_S0_T1,
out_BUS16_S0_T2,
in_BUS16_S0_T2,
out_BUS16_S0_T3,
in_BUS16_S0_T3,
out_BUS16_S0_T4,
in_BUS16_S0_T4,
out_BUS16_S1_T0,
in_BUS16_S1_T0,
out_BUS16_S1_T1,
in_BUS16_S1_T1,
out_BUS16_S1_T2,
in_BUS16_S1_T2,
out_BUS16_S1_T3,
in_BUS16_S1_T3,
out_BUS16_S1_T4,
in_BUS16_S1_T4,
out_BUS16_S2_T0,
in_BUS16_S2_T0,
out_BUS16_S2_T1,
in_BUS16_S2_T1,
out_BUS16_S2_T2,
in_BUS16_S2_T2,
out_BUS16_S2_T3,
in_BUS16_S2_T3,
out_BUS16_S2_T4,
in_BUS16_S2_T4,
out_BUS16_S3_T0,
in_BUS16_S3_T0,
out_BUS16_S3_T1,
in_BUS16_S3_T1,
out_BUS16_S3_T2,
in_BUS16_S3_T2,
out_BUS16_S3_T3,
in_BUS16_S3_T3,
out_BUS16_S3_T4,
in_BUS16_S3_T4,
reset,
tile_id
);
  input clk;
  input [31:0] config_addr;
  input [31:0] config_data;

  output [0:0] out_BUS1_S0_T0;
  input [0:0] in_BUS1_S0_T0;
  output [0:0] out_BUS1_S0_T1;
  input [0:0] in_BUS1_S0_T1;
  output [0:0] out_BUS1_S0_T2;
  input [0:0] in_BUS1_S0_T2;
  output [0:0] out_BUS1_S0_T3;
  input [0:0] in_BUS1_S0_T3;
  output [0:0] out_BUS1_S0_T4;
  input [0:0] in_BUS1_S0_T4;
  output [0:0] out_BUS1_S1_T0;
  input [0:0] in_BUS1_S1_T0;
  output [0:0] out_BUS1_S1_T1;
  input [0:0] in_BUS1_S1_T1;
  output [0:0] out_BUS1_S1_T2;
  input [0:0] in_BUS1_S1_T2;
  output [0:0] out_BUS1_S1_T3;
  input [0:0] in_BUS1_S1_T3;
  output [0:0] out_BUS1_S1_T4;
  input [0:0] in_BUS1_S1_T4;
  output [0:0] out_BUS1_S2_T0;
  input [0:0] in_BUS1_S2_T0;
  output [0:0] out_BUS1_S2_T1;
  input [0:0] in_BUS1_S2_T1;
  output [0:0] out_BUS1_S2_T2;
  input [0:0] in_BUS1_S2_T2;
  output [0:0] out_BUS1_S2_T3;
  input [0:0] in_BUS1_S2_T3;
  output [0:0] out_BUS1_S2_T4;
  input [0:0] in_BUS1_S2_T4;
  output [0:0] out_BUS1_S3_T0;
  input [0:0] in_BUS1_S3_T0;
  output [0:0] out_BUS1_S3_T1;
  input [0:0] in_BUS1_S3_T1;
  output [0:0] out_BUS1_S3_T2;
  input [0:0] in_BUS1_S3_T2;
  output [0:0] out_BUS1_S3_T3;
  input [0:0] in_BUS1_S3_T3;
  output [0:0] out_BUS1_S3_T4;
  input [0:0] in_BUS1_S3_T4;
  output [15:0] out_BUS16_S0_T0;
  input [15:0] in_BUS16_S0_T0;
  output [15:0] out_BUS16_S0_T1;
  input [15:0] in_BUS16_S0_T1;
  output [15:0] out_BUS16_S0_T2;
  input [15:0] in_BUS16_S0_T2;
  output [15:0] out_BUS16_S0_T3;
  input [15:0] in_BUS16_S0_T3;
  output [15:0] out_BUS16_S0_T4;
  input [15:0] in_BUS16_S0_T4;
  output [15:0] out_BUS16_S1_T0;
  input [15:0] in_BUS16_S1_T0;
  output [15:0] out_BUS16_S1_T1;
  input [15:0] in_BUS16_S1_T1;
  output [15:0] out_BUS16_S1_T2;
  input [15:0] in_BUS16_S1_T2;
  output [15:0] out_BUS16_S1_T3;
  input [15:0] in_BUS16_S1_T3;
  output [15:0] out_BUS16_S1_T4;
  input [15:0] in_BUS16_S1_T4;
  output [15:0] out_BUS16_S2_T0;
  input [15:0] in_BUS16_S2_T0;
  output [15:0] out_BUS16_S2_T1;
  input [15:0] in_BUS16_S2_T1;
  output [15:0] out_BUS16_S2_T2;
  input [15:0] in_BUS16_S2_T2;
  output [15:0] out_BUS16_S2_T3;
  input [15:0] in_BUS16_S2_T3;
  output [15:0] out_BUS16_S2_T4;
  input [15:0] in_BUS16_S2_T4;
  output [15:0] out_BUS16_S3_T0;
  input [15:0] in_BUS16_S3_T0;
  output [15:0] out_BUS16_S3_T1;
  input [15:0] in_BUS16_S3_T1;
  output [15:0] out_BUS16_S3_T2;
  input [15:0] in_BUS16_S3_T2;
  output [15:0] out_BUS16_S3_T3;
  input [15:0] in_BUS16_S3_T3;
  output [15:0] out_BUS16_S3_T4;
  input [15:0] in_BUS16_S3_T4;

  input [15:0] tile_id;
  input reset;


   reg config_en_pe;
   always @(*) begin
     if (reset) begin
        config_en_pe = 1'b0;
     end else begin
        if ((config_addr[15:0]==tile_id)&&(config_addr[23:16]==8'd0)) begin
          config_en_pe = 1'b1;
        end else begin
          config_en_pe = 1'b0;
        end
     end
   end


  // Note: verilator complains because some bits (opcode[31:16]) go unused...
  // For now, I will fix it with a verilator directive
  // FIXME owner please verify that unused bits are correct behavior and
  // FIXME maybe add a comment to that effect...?

  /* verilator lint_off UNUSED */
  reg [31:0] opcode;
  /* verilator lint_on UNUSED */

  always @(posedge clk) begin
    if (reset) begin
       opcode <= 32'd0;
    end else begin
       if ((config_addr[15:0]==tile_id)&&(config_addr[23:16]==8'd1)) begin
         opcode <= config_data;
       end
    end
  end
  reg config_en_cb_op_a_in;
  always @(*) begin
    if (reset) begin
       config_en_cb_op_a_in = 1'b0;
    end else begin
       if ((config_addr[15:0]==tile_id)&&(config_addr[23:16]==8'd2)) begin
         config_en_cb_op_a_in = 1'b1;
       end else begin
         config_en_cb_op_a_in = 1'b0;
       end
    end
  end
  wire [15:0] op_a_in;
  cb_unq1  cb_op_a_in
  (
    .clk(clk),
    .reset(reset),
    .out(op_a_in),
    .in_0(in_BUS16_S2_T0),
    .in_1(in_BUS16_S2_T1),
    .in_2(in_BUS16_S2_T2),
    .in_3(in_BUS16_S2_T3),
    .in_4(in_BUS16_S2_T4),
    .in_5(out_BUS16_S2_T0),
    .in_6(out_BUS16_S2_T1),
    .in_7(out_BUS16_S2_T2),
    .in_8(out_BUS16_S2_T3),
    .in_9(out_BUS16_S2_T4),
    .config_addr(config_addr),
    .config_data(config_data),
    .config_en(config_en_cb_op_a_in)
  );
  reg config_en_cb_op_b_in;
  always @(*) begin
    if (reset) begin
       config_en_cb_op_b_in = 1'b0;
    end else begin
       if ((config_addr[15:0]==tile_id)&&(config_addr[23:16]==8'd3)) begin
         config_en_cb_op_b_in = 1'b1;
       end else begin
         config_en_cb_op_b_in = 1'b0;
       end
    end
  end
  wire [15:0] op_b_in;
  cb_unq1  cb_op_b_in
  (
    .clk(clk),
    .reset(reset),
    .out(op_b_in),
    .in_0(in_BUS16_S1_T0),
    .in_1(in_BUS16_S1_T1),
    .in_2(in_BUS16_S1_T2),
    .in_3(in_BUS16_S1_T3),
    .in_4(in_BUS16_S1_T4),
    .in_5(out_BUS16_S1_T0),
    .in_6(out_BUS16_S1_T1),
    .in_7(out_BUS16_S1_T2),
    .in_8(out_BUS16_S1_T3),
    .in_9(out_BUS16_S1_T4),
    .config_addr(config_addr),
    .config_data(config_data),
    .config_en(config_en_cb_op_b_in)
  );
  reg config_en_cb_op_d_p_in;
  always @(*) begin
    if (reset) begin
       config_en_cb_op_d_p_in = 1'b0;
    end else begin
       if ((config_addr[15:0]==tile_id)&&(config_addr[23:16]==8'd4)) begin
         config_en_cb_op_d_p_in = 1'b1;
       end else begin
         config_en_cb_op_d_p_in = 1'b0;
       end
    end
  end
  wire op_d_p_in;
  cb_unq2  cb_op_d_p_in
  (
    .clk(clk),
    .reset(reset),
    .out(op_d_p_in),
    .in_0(in_BUS1_S2_T0),
    .in_1(in_BUS1_S2_T1),
    .in_2(in_BUS1_S2_T2),
    .in_3(in_BUS1_S2_T3),
    .in_4(in_BUS1_S2_T4),
    .in_5(out_BUS1_S2_T0),
    .in_6(out_BUS1_S2_T1),
    .in_7(out_BUS1_S2_T2),
    .in_8(out_BUS1_S2_T3),
    .in_9(out_BUS1_S2_T4),
    .config_addr(config_addr),
    .config_data(config_data),
    .config_en(config_en_cb_op_d_p_in)
  );
  reg config_en_cb_op_e_p_in;
  always @(*) begin
    if (reset) begin
       config_en_cb_op_e_p_in = 1'b0;
    end else begin
       if ((config_addr[15:0]==tile_id)&&(config_addr[23:16]==8'd5)) begin
         config_en_cb_op_e_p_in = 1'b1;
       end else begin
         config_en_cb_op_e_p_in = 1'b0;
       end
    end
  end
  wire op_e_p_in;
  cb_unq2  cb_op_e_p_in
  (
    .clk(clk),
    .reset(reset),
    .out(op_e_p_in),
    .in_0(in_BUS1_S1_T0),
    .in_1(in_BUS1_S1_T1),
    .in_2(in_BUS1_S1_T2),
    .in_3(in_BUS1_S1_T3),
    .in_4(in_BUS1_S1_T4),
    .in_5(out_BUS1_S1_T0),
    .in_6(out_BUS1_S1_T1),
    .in_7(out_BUS1_S1_T2),
    .in_8(out_BUS1_S1_T3),
    .in_9(out_BUS1_S1_T4),
    .config_addr(config_addr),
    .config_data(config_data),
    .config_en(config_en_cb_op_e_p_in)
  );
  reg config_en_cb_op_f_p_in;
  always @(*) begin
    if (reset) begin
       config_en_cb_op_f_p_in = 1'b0;
    end else begin
       if ((config_addr[15:0]==tile_id)&&(config_addr[23:16]==8'd6)) begin
         config_en_cb_op_f_p_in = 1'b1;
       end else begin
         config_en_cb_op_f_p_in = 1'b0;
       end
    end
  end
  wire op_f_p_in;
  cb_unq2  cb_op_f_p_in
  (
    .clk(clk),
    .reset(reset),
    .out(op_f_p_in),
    .in_0(in_BUS1_S2_T0),
    .in_1(in_BUS1_S2_T1),
    .in_2(in_BUS1_S2_T2),
    .in_3(in_BUS1_S2_T3),
    .in_4(in_BUS1_S2_T4),
    .in_5(out_BUS1_S2_T0),
    .in_6(out_BUS1_S2_T1),
    .in_7(out_BUS1_S2_T2),
    .in_8(out_BUS1_S2_T3),
    .in_9(out_BUS1_S2_T4),
    .config_addr(config_addr),
    .config_data(config_data),
    .config_en(config_en_cb_op_f_p_in)
  );

  reg config_en_sb_wide;
  always @(*) begin
    if (reset) begin
       config_en_sb_wide = 1'b0;
    end else begin
       if ((config_addr[15:0]==tile_id)&&(config_addr[23:16]==8'd7)) begin
         config_en_sb_wide = 1'b1;
       end else begin
         config_en_sb_wide = 1'b0;
       end
    end
  end
  wire [15:0] pe_out_res;
  sb_unq1  sb_wide
  (
    .clk(clk),
    .reset(reset),
    .pe_output_0(pe_out_res),
    .out_0_0(out_BUS16_S0_T0),
    .in_0_0(in_BUS16_S0_T0),
    .out_0_1(out_BUS16_S0_T1),
    .in_0_1(in_BUS16_S0_T1),
    .out_0_2(out_BUS16_S0_T2),
    .in_0_2(in_BUS16_S0_T2),
    .out_0_3(out_BUS16_S0_T3),
    .in_0_3(in_BUS16_S0_T3),
    .out_0_4(out_BUS16_S0_T4),
    .in_0_4(in_BUS16_S0_T4),
    .out_1_0(out_BUS16_S1_T0),
    .in_1_0(in_BUS16_S1_T0),
    .out_1_1(out_BUS16_S1_T1),
    .in_1_1(in_BUS16_S1_T1),
    .out_1_2(out_BUS16_S1_T2),
    .in_1_2(in_BUS16_S1_T2),
    .out_1_3(out_BUS16_S1_T3),
    .in_1_3(in_BUS16_S1_T3),
    .out_1_4(out_BUS16_S1_T4),
    .in_1_4(in_BUS16_S1_T4),
    .out_2_0(out_BUS16_S2_T0),
    .in_2_0(in_BUS16_S2_T0),
    .out_2_1(out_BUS16_S2_T1),
    .in_2_1(in_BUS16_S2_T1),
    .out_2_2(out_BUS16_S2_T2),
    .in_2_2(in_BUS16_S2_T2),
    .out_2_3(out_BUS16_S2_T3),
    .in_2_3(in_BUS16_S2_T3),
    .out_2_4(out_BUS16_S2_T4),
    .in_2_4(in_BUS16_S2_T4),
    .out_3_0(out_BUS16_S3_T0),
    .in_3_0(in_BUS16_S3_T0),
    .out_3_1(out_BUS16_S3_T1),
    .in_3_1(in_BUS16_S3_T1),
    .out_3_2(out_BUS16_S3_T2),
    .in_3_2(in_BUS16_S3_T2),
    .out_3_3(out_BUS16_S3_T3),
    .in_3_3(in_BUS16_S3_T3),
    .out_3_4(out_BUS16_S3_T4),
    .in_3_4(in_BUS16_S3_T4),
    .config_addr(config_addr),
    .config_data(config_data),
    .config_en(config_en_sb_wide)
  );


  reg config_en_sb_1bit;
  always @(*) begin
    if (reset) begin
       config_en_sb_1bit = 1'b0;
    end else begin
       if ((config_addr[15:0]==tile_id)&&(config_addr[23:16]==8'd8)) begin
         config_en_sb_1bit = 1'b1;
       end else begin
         config_en_sb_1bit = 1'b0;
       end
    end
  end
  wire pe_out_res_p;
  sb_unq2  sb_1b
  (
    .clk(clk),
    .reset(reset),
    .pe_output_0(pe_out_res_p),
    .out_0_0(out_BUS1_S0_T0),
    .in_0_0(in_BUS1_S0_T0),
    .out_0_1(out_BUS1_S0_T1),
    .in_0_1(in_BUS1_S0_T1),
    .out_0_2(out_BUS1_S0_T2),
    .in_0_2(in_BUS1_S0_T2),
    .out_0_3(out_BUS1_S0_T3),
    .in_0_3(in_BUS1_S0_T3),
    .out_0_4(out_BUS1_S0_T4),
    .in_0_4(in_BUS1_S0_T4),
    .out_1_0(out_BUS1_S1_T0),
    .in_1_0(in_BUS1_S1_T0),
    .out_1_1(out_BUS1_S1_T1),
    .in_1_1(in_BUS1_S1_T1),
    .out_1_2(out_BUS1_S1_T2),
    .in_1_2(in_BUS1_S1_T2),
    .out_1_3(out_BUS1_S1_T3),
    .in_1_3(in_BUS1_S1_T3),
    .out_1_4(out_BUS1_S1_T4),
    .in_1_4(in_BUS1_S1_T4),
    .out_2_0(out_BUS1_S2_T0),
    .in_2_0(in_BUS1_S2_T0),
    .out_2_1(out_BUS1_S2_T1),
    .in_2_1(in_BUS1_S2_T1),
    .out_2_2(out_BUS1_S2_T2),
    .in_2_2(in_BUS1_S2_T2),
    .out_2_3(out_BUS1_S2_T3),
    .in_2_3(in_BUS1_S2_T3),
    .out_2_4(out_BUS1_S2_T4),
    .in_2_4(in_BUS1_S2_T4),
    .out_3_0(out_BUS1_S3_T0),
    .in_3_0(in_BUS1_S3_T0),
    .out_3_1(out_BUS1_S3_T1),
    .in_3_1(in_BUS1_S3_T1),
    .out_3_2(out_BUS1_S3_T2),
    .in_3_2(in_BUS1_S3_T2),
    .out_3_3(out_BUS1_S3_T3),
    .in_3_3(in_BUS1_S3_T3),
    .out_3_4(out_BUS1_S3_T4),
    .in_3_4(in_BUS1_S3_T4),
    .config_addr(config_addr),
    .config_data(config_data),
    .config_en(config_en_sb_1bit)
  );



// test_pe_unq1  test_pe
//  (
//   .clk(clk),
//   .rst_n(~reset),
//   .clk_en(1'b1),
//   .cfg_d(config_data[15:0]),
//   .cfg_a(config_addr[31:24]),
//   .cfg_en(config_en_pe),
//   .op_a_in(op_a_in),
//   .op_b_in(op_b_in),
//   .op_d_p_in(op_d_p_in),
//   .op_e_p_in(op_e_p_in),
//   .op_f_p_in(op_f_p_in),
//   .res(pe_out_res),
//   .res_p(pe_out_res_p)
// );
endmodule
