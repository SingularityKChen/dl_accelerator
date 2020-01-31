module ProcessingElementControl(
  input        clock,
  input        reset,
  output       io_ctrlPad_doMACEn,
  input        io_ctrlPad_fromTopIO_pSumEnqOrProduct_ready,
  output       io_ctrlPad_fromTopIO_pSumEnqOrProduct_bits,
  output       io_ctrlPad_fromTopIO_doLoadEn,
  input        io_ctrlPad_fromTopIO_calFinish,
  output       io_ctrlTop_pSumEnqOrProduct_ready,
  input        io_ctrlTop_pSumEnqOrProduct_bits,
  input        io_ctrlTop_doLoadEn,
  output       io_ctrlTop_calFinish,
  input        io_ctrlTop_writeFinish,
  output [1:0] io_debugIO_peState,
  output       io_debugIO_doMACEnDebug
);
  reg [1:0] stateMac; // @[ProcessingElement.scala 72:31]
  reg [31:0] _RAND_0;
  wire  _T; // @[ProcessingElement.scala 73:53]
  wire  _T_5; // @[Conditional.scala 37:30]
  wire  _T_6; // @[Conditional.scala 37:30]
  wire  _T_7; // @[Conditional.scala 37:30]
  assign _T = stateMac == 2'h2; // @[ProcessingElement.scala 73:53]
  assign _T_5 = 2'h0 == stateMac; // @[Conditional.scala 37:30]
  assign _T_6 = 2'h1 == stateMac; // @[Conditional.scala 37:30]
  assign _T_7 = 2'h2 == stateMac; // @[Conditional.scala 37:30]
  assign io_ctrlPad_doMACEn = stateMac == 2'h2; // @[ProcessingElement.scala 77:22]
  assign io_ctrlPad_fromTopIO_pSumEnqOrProduct_bits = io_ctrlTop_pSumEnqOrProduct_bits; // @[ProcessingElement.scala 75:46]
  assign io_ctrlPad_fromTopIO_doLoadEn = io_ctrlTop_doLoadEn; // @[ProcessingElement.scala 76:33]
  assign io_ctrlTop_pSumEnqOrProduct_ready = _T & io_ctrlPad_fromTopIO_pSumEnqOrProduct_ready; // @[ProcessingElement.scala 73:37]
  assign io_ctrlTop_calFinish = io_ctrlPad_fromTopIO_calFinish; // @[ProcessingElement.scala 64:24]
  assign io_debugIO_peState = stateMac; // @[ProcessingElement.scala 96:24]
  assign io_debugIO_doMACEnDebug = io_ctrlPad_doMACEn; // @[ProcessingElement.scala 97:29]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  stateMac = _RAND_0[1:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      stateMac <= 2'h0;
    end else if (_T_5) begin
      if (io_ctrlTop_doLoadEn) begin
        stateMac <= 2'h1;
      end
    end else if (_T_6) begin
      if (io_ctrlTop_writeFinish) begin
        stateMac <= 2'h2;
      end
    end else if (_T_7) begin
      if (io_ctrlPad_fromTopIO_calFinish) begin
        stateMac <= 2'h0;
      end
    end
  end
endmodule
module SPadAddrModule(
  input        clock,
  input        reset,
  output [3:0] io_commonIO_columnNum,
  output [3:0] io_commonIO_readOutData,
  input        io_commonIO_writeEn,
  output       io_commonIO_dataLenFinIO_writeInDataIO_ready,
  input        io_commonIO_dataLenFinIO_writeInDataIO_valid,
  input  [3:0] io_commonIO_dataLenFinIO_writeInDataIO_bits_data,
  input  [3:0] io_commonIO_dataLenFinIO_streamLen,
  output       io_commonIO_dataLenFinIO_writeFin,
  input        io_addrIO_indexInc
);
  reg [3:0] dataLenReg; // @[SPadModule.scala 79:33]
  reg [31:0] _RAND_0;
  reg [3:0] padWriteIndexReg; // @[SPadModule.scala 82:39]
  reg [31:0] _RAND_1;
  reg [3:0] padReadIndexReg; // @[SPadModule.scala 83:38]
  reg [31:0] _RAND_2;
  wire [3:0] _T_1; // @[SPadModule.scala 88:53]
  wire  writeWrapWire; // @[SPadModule.scala 88:37]
  wire  readWrapWire; // @[SPadModule.scala 89:35]
  wire  _T_6; // @[SPadModule.scala 91:31]
  wire [3:0] _T_8; // @[SPadModule.scala 93:42]
  reg [3:0] addrSPad_0; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_3;
  reg [3:0] addrSPad_1; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_4;
  reg [3:0] addrSPad_2; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_5;
  reg [3:0] addrSPad_3; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_6;
  reg [3:0] addrSPad_4; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_7;
  reg [3:0] addrSPad_5; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_8;
  reg [3:0] addrSPad_6; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_9;
  reg [3:0] addrSPad_7; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_10;
  reg [3:0] addrSPad_8; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_11;
  wire [3:0] _T_13; // @[SPadModule.scala 21:40]
  wire [3:0] _GEN_24; // @[SPadModule.scala 27:12]
  wire [3:0] _GEN_25; // @[SPadModule.scala 27:12]
  wire [3:0] _GEN_26; // @[SPadModule.scala 27:12]
  wire [3:0] _GEN_27; // @[SPadModule.scala 27:12]
  wire [3:0] _GEN_28; // @[SPadModule.scala 27:12]
  wire [3:0] _GEN_29; // @[SPadModule.scala 27:12]
  wire [3:0] _GEN_30; // @[SPadModule.scala 27:12]
  assign _T_1 = dataLenReg - 4'h1; // @[SPadModule.scala 88:53]
  assign writeWrapWire = padWriteIndexReg == _T_1; // @[SPadModule.scala 88:37]
  assign readWrapWire = padReadIndexReg == _T_1; // @[SPadModule.scala 89:35]
  assign _T_6 = io_commonIO_dataLenFinIO_writeInDataIO_valid & io_commonIO_writeEn; // @[SPadModule.scala 91:31]
  assign _T_8 = padWriteIndexReg + 4'h1; // @[SPadModule.scala 93:42]
  assign _T_13 = padReadIndexReg + 4'h1; // @[SPadModule.scala 21:40]
  assign _GEN_24 = 4'h1 == padReadIndexReg ? addrSPad_1 : addrSPad_0; // @[SPadModule.scala 27:12]
  assign _GEN_25 = 4'h2 == padReadIndexReg ? addrSPad_2 : _GEN_24; // @[SPadModule.scala 27:12]
  assign _GEN_26 = 4'h3 == padReadIndexReg ? addrSPad_3 : _GEN_25; // @[SPadModule.scala 27:12]
  assign _GEN_27 = 4'h4 == padReadIndexReg ? addrSPad_4 : _GEN_26; // @[SPadModule.scala 27:12]
  assign _GEN_28 = 4'h5 == padReadIndexReg ? addrSPad_5 : _GEN_27; // @[SPadModule.scala 27:12]
  assign _GEN_29 = 4'h6 == padReadIndexReg ? addrSPad_6 : _GEN_28; // @[SPadModule.scala 27:12]
  assign _GEN_30 = 4'h7 == padReadIndexReg ? addrSPad_7 : _GEN_29; // @[SPadModule.scala 27:12]
  assign io_commonIO_columnNum = padReadIndexReg; // @[SPadModule.scala 103:25]
  assign io_commonIO_readOutData = 4'h8 == padReadIndexReg ? addrSPad_8 : _GEN_30; // @[SPadModule.scala 28:27]
  assign io_commonIO_dataLenFinIO_writeInDataIO_ready = io_commonIO_dataLenFinIO_writeInDataIO_valid & io_commonIO_writeEn; // @[SPadModule.scala 92:27 SPadModule.scala 98:27]
  assign io_commonIO_dataLenFinIO_writeFin = io_commonIO_dataLenFinIO_writeInDataIO_valid & writeWrapWire; // @[SPadModule.scala 102:37]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  dataLenReg = _RAND_0[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  padWriteIndexReg = _RAND_1[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  padReadIndexReg = _RAND_2[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  addrSPad_0 = _RAND_3[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  addrSPad_1 = _RAND_4[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  addrSPad_2 = _RAND_5[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  addrSPad_3 = _RAND_6[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  addrSPad_4 = _RAND_7[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  addrSPad_5 = _RAND_8[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  addrSPad_6 = _RAND_9[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  addrSPad_7 = _RAND_10[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  addrSPad_8 = _RAND_11[3:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      dataLenReg <= 4'h9;
    end else begin
      dataLenReg <= io_commonIO_dataLenFinIO_streamLen;
    end
    if (reset) begin
      padWriteIndexReg <= 4'h0;
    end else if (_T_6) begin
      if (writeWrapWire) begin
        padWriteIndexReg <= 4'h0;
      end else begin
        padWriteIndexReg <= _T_8;
      end
    end
    if (reset) begin
      padReadIndexReg <= 4'h0;
    end else if (io_addrIO_indexInc) begin
      if (readWrapWire) begin
        padReadIndexReg <= 4'h0;
      end else begin
        padReadIndexReg <= _T_13;
      end
    end
    if (reset) begin
      addrSPad_0 <= 4'h0;
    end else if (_T_6) begin
      if (4'h0 == padWriteIndexReg) begin
        addrSPad_0 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_1 <= 4'h0;
    end else if (_T_6) begin
      if (4'h1 == padWriteIndexReg) begin
        addrSPad_1 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_2 <= 4'h0;
    end else if (_T_6) begin
      if (4'h2 == padWriteIndexReg) begin
        addrSPad_2 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_3 <= 4'h0;
    end else if (_T_6) begin
      if (4'h3 == padWriteIndexReg) begin
        addrSPad_3 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_4 <= 4'h0;
    end else if (_T_6) begin
      if (4'h4 == padWriteIndexReg) begin
        addrSPad_4 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_5 <= 4'h0;
    end else if (_T_6) begin
      if (4'h5 == padWriteIndexReg) begin
        addrSPad_5 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_6 <= 4'h0;
    end else if (_T_6) begin
      if (4'h6 == padWriteIndexReg) begin
        addrSPad_6 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_7 <= 4'h0;
    end else if (_T_6) begin
      if (4'h7 == padWriteIndexReg) begin
        addrSPad_7 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_8 <= 4'h0;
    end else if (_T_6) begin
      if (4'h8 == padWriteIndexReg) begin
        addrSPad_8 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
  end
endmodule
module SPadDataModule(
  input         clock,
  input         reset,
  output [3:0]  io_commonIO_columnNum,
  output [11:0] io_commonIO_readOutData,
  input         io_commonIO_writeEn,
  output        io_commonIO_dataLenFinIO_writeInDataIO_ready,
  input         io_commonIO_dataLenFinIO_writeInDataIO_valid,
  input  [11:0] io_commonIO_dataLenFinIO_writeInDataIO_bits_data,
  input  [3:0]  io_commonIO_dataLenFinIO_streamLen,
  output        io_commonIO_dataLenFinIO_writeFin,
  input         io_dataIO_indexInc
);
  reg [3:0] dataLenReg; // @[SPadModule.scala 79:33]
  reg [31:0] _RAND_0;
  reg [3:0] padWriteIndexReg; // @[SPadModule.scala 82:39]
  reg [31:0] _RAND_1;
  reg [3:0] padReadIndexReg; // @[SPadModule.scala 83:38]
  reg [31:0] _RAND_2;
  wire [3:0] _T_1; // @[SPadModule.scala 88:53]
  wire  writeWrapWire; // @[SPadModule.scala 88:37]
  wire  readWrapWire; // @[SPadModule.scala 89:35]
  wire  _T_6; // @[SPadModule.scala 91:31]
  wire [3:0] _T_8; // @[SPadModule.scala 93:42]
  reg [11:0] _T_11_0; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_3;
  reg [11:0] _T_11_1; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_4;
  reg [11:0] _T_11_2; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_5;
  reg [11:0] _T_11_3; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_6;
  reg [11:0] _T_11_4; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_7;
  reg [11:0] _T_11_5; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_8;
  reg [11:0] _T_11_6; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_9;
  reg [11:0] _T_11_7; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_10;
  reg [11:0] _T_11_8; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_11;
  reg [11:0] _T_11_9; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_12;
  reg [11:0] _T_11_10; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_13;
  reg [11:0] _T_11_11; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_14;
  reg [11:0] _T_11_12; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_15;
  reg [11:0] _T_11_13; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_16;
  reg [11:0] _T_11_14; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_17;
  reg [11:0] _T_11_15; // @[SPadModule.scala 54:38]
  reg [31:0] _RAND_18;
  wire [3:0] _T_14; // @[SPadModule.scala 60:42]
  wire [11:0] _GEN_38; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_39; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_40; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_41; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_42; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_43; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_44; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_45; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_46; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_47; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_48; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_49; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_50; // @[SPadModule.scala 66:14]
  wire [11:0] _GEN_51; // @[SPadModule.scala 66:14]
  assign _T_1 = dataLenReg - 4'h1; // @[SPadModule.scala 88:53]
  assign writeWrapWire = padWriteIndexReg == _T_1; // @[SPadModule.scala 88:37]
  assign readWrapWire = padReadIndexReg == _T_1; // @[SPadModule.scala 89:35]
  assign _T_6 = io_commonIO_dataLenFinIO_writeInDataIO_valid & io_commonIO_writeEn; // @[SPadModule.scala 91:31]
  assign _T_8 = padWriteIndexReg + 4'h1; // @[SPadModule.scala 93:42]
  assign _T_14 = padReadIndexReg + 4'h1; // @[SPadModule.scala 60:42]
  assign _GEN_38 = 4'h1 == padReadIndexReg ? _T_11_1 : _T_11_0; // @[SPadModule.scala 66:14]
  assign _GEN_39 = 4'h2 == padReadIndexReg ? _T_11_2 : _GEN_38; // @[SPadModule.scala 66:14]
  assign _GEN_40 = 4'h3 == padReadIndexReg ? _T_11_3 : _GEN_39; // @[SPadModule.scala 66:14]
  assign _GEN_41 = 4'h4 == padReadIndexReg ? _T_11_4 : _GEN_40; // @[SPadModule.scala 66:14]
  assign _GEN_42 = 4'h5 == padReadIndexReg ? _T_11_5 : _GEN_41; // @[SPadModule.scala 66:14]
  assign _GEN_43 = 4'h6 == padReadIndexReg ? _T_11_6 : _GEN_42; // @[SPadModule.scala 66:14]
  assign _GEN_44 = 4'h7 == padReadIndexReg ? _T_11_7 : _GEN_43; // @[SPadModule.scala 66:14]
  assign _GEN_45 = 4'h8 == padReadIndexReg ? _T_11_8 : _GEN_44; // @[SPadModule.scala 66:14]
  assign _GEN_46 = 4'h9 == padReadIndexReg ? _T_11_9 : _GEN_45; // @[SPadModule.scala 66:14]
  assign _GEN_47 = 4'ha == padReadIndexReg ? _T_11_10 : _GEN_46; // @[SPadModule.scala 66:14]
  assign _GEN_48 = 4'hb == padReadIndexReg ? _T_11_11 : _GEN_47; // @[SPadModule.scala 66:14]
  assign _GEN_49 = 4'hc == padReadIndexReg ? _T_11_12 : _GEN_48; // @[SPadModule.scala 66:14]
  assign _GEN_50 = 4'hd == padReadIndexReg ? _T_11_13 : _GEN_49; // @[SPadModule.scala 66:14]
  assign _GEN_51 = 4'he == padReadIndexReg ? _T_11_14 : _GEN_50; // @[SPadModule.scala 66:14]
  assign io_commonIO_columnNum = padReadIndexReg; // @[SPadModule.scala 103:25]
  assign io_commonIO_readOutData = 4'hf == padReadIndexReg ? _T_11_15 : _GEN_51; // @[SPadModule.scala 69:27]
  assign io_commonIO_dataLenFinIO_writeInDataIO_ready = io_commonIO_dataLenFinIO_writeInDataIO_valid & io_commonIO_writeEn; // @[SPadModule.scala 92:27 SPadModule.scala 98:27]
  assign io_commonIO_dataLenFinIO_writeFin = io_commonIO_dataLenFinIO_writeInDataIO_valid & writeWrapWire; // @[SPadModule.scala 102:37]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  dataLenReg = _RAND_0[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  padWriteIndexReg = _RAND_1[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  padReadIndexReg = _RAND_2[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_11_0 = _RAND_3[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  _T_11_1 = _RAND_4[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  _T_11_2 = _RAND_5[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  _T_11_3 = _RAND_6[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  _T_11_4 = _RAND_7[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  _T_11_5 = _RAND_8[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  _T_11_6 = _RAND_9[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  _T_11_7 = _RAND_10[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  _T_11_8 = _RAND_11[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  _T_11_9 = _RAND_12[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  _T_11_10 = _RAND_13[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {1{`RANDOM}};
  _T_11_11 = _RAND_14[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  _T_11_12 = _RAND_15[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_16 = {1{`RANDOM}};
  _T_11_13 = _RAND_16[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_17 = {1{`RANDOM}};
  _T_11_14 = _RAND_17[11:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_18 = {1{`RANDOM}};
  _T_11_15 = _RAND_18[11:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      dataLenReg <= 4'h9;
    end else begin
      dataLenReg <= io_commonIO_dataLenFinIO_streamLen;
    end
    if (reset) begin
      padWriteIndexReg <= 4'h0;
    end else if (_T_6) begin
      if (writeWrapWire) begin
        padWriteIndexReg <= 4'h0;
      end else begin
        padWriteIndexReg <= _T_8;
      end
    end
    if (reset) begin
      padReadIndexReg <= 4'h0;
    end else if (io_dataIO_indexInc) begin
      if (readWrapWire) begin
        padReadIndexReg <= 4'h0;
      end else begin
        padReadIndexReg <= _T_14;
      end
    end
    if (reset) begin
      _T_11_0 <= 12'h0;
    end else if (_T_6) begin
      if (4'h0 == padWriteIndexReg) begin
        _T_11_0 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_1 <= 12'h0;
    end else if (_T_6) begin
      if (4'h1 == padWriteIndexReg) begin
        _T_11_1 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_2 <= 12'h0;
    end else if (_T_6) begin
      if (4'h2 == padWriteIndexReg) begin
        _T_11_2 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_3 <= 12'h0;
    end else if (_T_6) begin
      if (4'h3 == padWriteIndexReg) begin
        _T_11_3 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_4 <= 12'h0;
    end else if (_T_6) begin
      if (4'h4 == padWriteIndexReg) begin
        _T_11_4 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_5 <= 12'h0;
    end else if (_T_6) begin
      if (4'h5 == padWriteIndexReg) begin
        _T_11_5 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_6 <= 12'h0;
    end else if (_T_6) begin
      if (4'h6 == padWriteIndexReg) begin
        _T_11_6 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_7 <= 12'h0;
    end else if (_T_6) begin
      if (4'h7 == padWriteIndexReg) begin
        _T_11_7 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_8 <= 12'h0;
    end else if (_T_6) begin
      if (4'h8 == padWriteIndexReg) begin
        _T_11_8 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_9 <= 12'h0;
    end else if (_T_6) begin
      if (4'h9 == padWriteIndexReg) begin
        _T_11_9 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_10 <= 12'h0;
    end else if (_T_6) begin
      if (4'ha == padWriteIndexReg) begin
        _T_11_10 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_11 <= 12'h0;
    end else if (_T_6) begin
      if (4'hb == padWriteIndexReg) begin
        _T_11_11 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_12 <= 12'h0;
    end else if (_T_6) begin
      if (4'hc == padWriteIndexReg) begin
        _T_11_12 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_13 <= 12'h0;
    end else if (_T_6) begin
      if (4'hd == padWriteIndexReg) begin
        _T_11_13 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_14 <= 12'h0;
    end else if (_T_6) begin
      if (4'he == padWriteIndexReg) begin
        _T_11_14 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      _T_11_15 <= 12'h0;
    end else if (_T_6) begin
      if (4'hf == padWriteIndexReg) begin
        _T_11_15 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
  end
endmodule
module WeightSPadAddrModule(
  input        clock,
  input        reset,
  output [6:0] io_commonIO_readOutData,
  input        io_commonIO_writeEn,
  output       io_commonIO_dataLenFinIO_writeInDataIO_ready,
  input        io_commonIO_dataLenFinIO_writeInDataIO_valid,
  input  [6:0] io_commonIO_dataLenFinIO_writeInDataIO_bits_data,
  input  [3:0] io_commonIO_dataLenFinIO_streamLen,
  output       io_commonIO_dataLenFinIO_writeFin,
  input  [3:0] io_addrIO_readInIdx,
  input        io_addrIO_indexInc,
  input        io_addrIO_readInIdxEn
);
  reg [3:0] dataLenReg; // @[SPadModule.scala 79:33]
  reg [31:0] _RAND_0;
  reg [3:0] padWriteIndexReg; // @[SPadModule.scala 82:39]
  reg [31:0] _RAND_1;
  reg [3:0] padReadIndexReg; // @[SPadModule.scala 83:38]
  reg [31:0] _RAND_2;
  wire [3:0] _T_1; // @[SPadModule.scala 88:53]
  wire  writeWrapWire; // @[SPadModule.scala 88:37]
  wire  readWrapWire; // @[SPadModule.scala 89:35]
  wire  _T_6; // @[SPadModule.scala 91:31]
  wire [3:0] _T_8; // @[SPadModule.scala 93:42]
  reg [6:0] addrSPad_0; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_3;
  reg [6:0] addrSPad_1; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_4;
  reg [6:0] addrSPad_2; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_5;
  reg [6:0] addrSPad_3; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_6;
  reg [6:0] addrSPad_4; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_7;
  reg [6:0] addrSPad_5; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_8;
  reg [6:0] addrSPad_6; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_9;
  reg [6:0] addrSPad_7; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_10;
  reg [6:0] addrSPad_8; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_11;
  reg [6:0] addrSPad_9; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_12;
  reg [6:0] addrSPad_10; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_13;
  reg [6:0] addrSPad_11; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_14;
  reg [6:0] addrSPad_12; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_15;
  reg [6:0] addrSPad_13; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_16;
  reg [6:0] addrSPad_14; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_17;
  reg [6:0] addrSPad_15; // @[SPadModule.scala 14:36]
  reg [31:0] _RAND_18;
  wire [3:0] _T_13; // @[SPadModule.scala 21:40]
  wire [6:0] _GEN_38; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_39; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_40; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_41; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_42; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_43; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_44; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_45; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_46; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_47; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_48; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_49; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_50; // @[SPadModule.scala 27:12]
  wire [6:0] _GEN_51; // @[SPadModule.scala 27:12]
  assign _T_1 = dataLenReg - 4'h1; // @[SPadModule.scala 88:53]
  assign writeWrapWire = padWriteIndexReg == _T_1; // @[SPadModule.scala 88:37]
  assign readWrapWire = padReadIndexReg == _T_1; // @[SPadModule.scala 89:35]
  assign _T_6 = io_commonIO_dataLenFinIO_writeInDataIO_valid & io_commonIO_writeEn; // @[SPadModule.scala 91:31]
  assign _T_8 = padWriteIndexReg + 4'h1; // @[SPadModule.scala 93:42]
  assign _T_13 = padReadIndexReg + 4'h1; // @[SPadModule.scala 21:40]
  assign _GEN_38 = 4'h1 == padReadIndexReg ? addrSPad_1 : addrSPad_0; // @[SPadModule.scala 27:12]
  assign _GEN_39 = 4'h2 == padReadIndexReg ? addrSPad_2 : _GEN_38; // @[SPadModule.scala 27:12]
  assign _GEN_40 = 4'h3 == padReadIndexReg ? addrSPad_3 : _GEN_39; // @[SPadModule.scala 27:12]
  assign _GEN_41 = 4'h4 == padReadIndexReg ? addrSPad_4 : _GEN_40; // @[SPadModule.scala 27:12]
  assign _GEN_42 = 4'h5 == padReadIndexReg ? addrSPad_5 : _GEN_41; // @[SPadModule.scala 27:12]
  assign _GEN_43 = 4'h6 == padReadIndexReg ? addrSPad_6 : _GEN_42; // @[SPadModule.scala 27:12]
  assign _GEN_44 = 4'h7 == padReadIndexReg ? addrSPad_7 : _GEN_43; // @[SPadModule.scala 27:12]
  assign _GEN_45 = 4'h8 == padReadIndexReg ? addrSPad_8 : _GEN_44; // @[SPadModule.scala 27:12]
  assign _GEN_46 = 4'h9 == padReadIndexReg ? addrSPad_9 : _GEN_45; // @[SPadModule.scala 27:12]
  assign _GEN_47 = 4'ha == padReadIndexReg ? addrSPad_10 : _GEN_46; // @[SPadModule.scala 27:12]
  assign _GEN_48 = 4'hb == padReadIndexReg ? addrSPad_11 : _GEN_47; // @[SPadModule.scala 27:12]
  assign _GEN_49 = 4'hc == padReadIndexReg ? addrSPad_12 : _GEN_48; // @[SPadModule.scala 27:12]
  assign _GEN_50 = 4'hd == padReadIndexReg ? addrSPad_13 : _GEN_49; // @[SPadModule.scala 27:12]
  assign _GEN_51 = 4'he == padReadIndexReg ? addrSPad_14 : _GEN_50; // @[SPadModule.scala 27:12]
  assign io_commonIO_readOutData = 4'hf == padReadIndexReg ? addrSPad_15 : _GEN_51; // @[SPadModule.scala 28:27]
  assign io_commonIO_dataLenFinIO_writeInDataIO_ready = io_commonIO_dataLenFinIO_writeInDataIO_valid & io_commonIO_writeEn; // @[SPadModule.scala 92:27 SPadModule.scala 98:27]
  assign io_commonIO_dataLenFinIO_writeFin = io_commonIO_dataLenFinIO_writeInDataIO_valid & writeWrapWire; // @[SPadModule.scala 102:37]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  dataLenReg = _RAND_0[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  padWriteIndexReg = _RAND_1[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  padReadIndexReg = _RAND_2[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  addrSPad_0 = _RAND_3[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  addrSPad_1 = _RAND_4[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  addrSPad_2 = _RAND_5[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  addrSPad_3 = _RAND_6[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  addrSPad_4 = _RAND_7[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  addrSPad_5 = _RAND_8[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  addrSPad_6 = _RAND_9[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  addrSPad_7 = _RAND_10[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  addrSPad_8 = _RAND_11[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  addrSPad_9 = _RAND_12[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  addrSPad_10 = _RAND_13[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {1{`RANDOM}};
  addrSPad_11 = _RAND_14[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  addrSPad_12 = _RAND_15[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_16 = {1{`RANDOM}};
  addrSPad_13 = _RAND_16[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_17 = {1{`RANDOM}};
  addrSPad_14 = _RAND_17[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_18 = {1{`RANDOM}};
  addrSPad_15 = _RAND_18[6:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      dataLenReg <= 4'h9;
    end else begin
      dataLenReg <= io_commonIO_dataLenFinIO_streamLen;
    end
    if (reset) begin
      padWriteIndexReg <= 4'h0;
    end else if (_T_6) begin
      if (writeWrapWire) begin
        padWriteIndexReg <= 4'h0;
      end else begin
        padWriteIndexReg <= _T_8;
      end
    end
    if (reset) begin
      padReadIndexReg <= 4'h0;
    end else if (io_addrIO_readInIdxEn) begin
      padReadIndexReg <= io_addrIO_readInIdx;
    end else if (io_addrIO_indexInc) begin
      if (readWrapWire) begin
        padReadIndexReg <= 4'h0;
      end else begin
        padReadIndexReg <= _T_13;
      end
    end
    if (reset) begin
      addrSPad_0 <= 7'h0;
    end else if (_T_6) begin
      if (4'h0 == padWriteIndexReg) begin
        addrSPad_0 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_1 <= 7'h0;
    end else if (_T_6) begin
      if (4'h1 == padWriteIndexReg) begin
        addrSPad_1 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_2 <= 7'h0;
    end else if (_T_6) begin
      if (4'h2 == padWriteIndexReg) begin
        addrSPad_2 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_3 <= 7'h0;
    end else if (_T_6) begin
      if (4'h3 == padWriteIndexReg) begin
        addrSPad_3 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_4 <= 7'h0;
    end else if (_T_6) begin
      if (4'h4 == padWriteIndexReg) begin
        addrSPad_4 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_5 <= 7'h0;
    end else if (_T_6) begin
      if (4'h5 == padWriteIndexReg) begin
        addrSPad_5 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_6 <= 7'h0;
    end else if (_T_6) begin
      if (4'h6 == padWriteIndexReg) begin
        addrSPad_6 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_7 <= 7'h0;
    end else if (_T_6) begin
      if (4'h7 == padWriteIndexReg) begin
        addrSPad_7 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_8 <= 7'h0;
    end else if (_T_6) begin
      if (4'h8 == padWriteIndexReg) begin
        addrSPad_8 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_9 <= 7'h0;
    end else if (_T_6) begin
      if (4'h9 == padWriteIndexReg) begin
        addrSPad_9 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_10 <= 7'h0;
    end else if (_T_6) begin
      if (4'ha == padWriteIndexReg) begin
        addrSPad_10 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_11 <= 7'h0;
    end else if (_T_6) begin
      if (4'hb == padWriteIndexReg) begin
        addrSPad_11 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_12 <= 7'h0;
    end else if (_T_6) begin
      if (4'hc == padWriteIndexReg) begin
        addrSPad_12 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_13 <= 7'h0;
    end else if (_T_6) begin
      if (4'hd == padWriteIndexReg) begin
        addrSPad_13 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_14 <= 7'h0;
    end else if (_T_6) begin
      if (4'he == padWriteIndexReg) begin
        addrSPad_14 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
    if (reset) begin
      addrSPad_15 <= 7'h0;
    end else if (_T_6) begin
      if (4'hf == padWriteIndexReg) begin
        addrSPad_15 <= io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
      end
    end
  end
endmodule
module SPadDataModule_1(
  input         clock,
  input         reset,
  output [7:0]  io_commonIO_columnNum,
  output [11:0] io_commonIO_readOutData,
  input         io_commonIO_readEn,
  input         io_commonIO_writeEn,
  output        io_commonIO_dataLenFinIO_writeInDataIO_ready,
  input         io_commonIO_dataLenFinIO_writeInDataIO_valid,
  input  [11:0] io_commonIO_dataLenFinIO_writeInDataIO_bits_data,
  input  [7:0]  io_commonIO_dataLenFinIO_streamLen,
  output        io_commonIO_dataLenFinIO_writeFin,
  input  [7:0]  io_dataIO_readInIdx,
  input         io_dataIO_indexInc,
  input         io_dataIO_readInIdxEn
);
  reg [11:0] _T_10 [0:191]; // @[SPadModule.scala 35:50]
  reg [31:0] _RAND_0;
  wire [11:0] _T_10__T_18_data; // @[SPadModule.scala 35:50]
  wire [7:0] _T_10__T_18_addr; // @[SPadModule.scala 35:50]
  reg [31:0] _RAND_1;
  wire [11:0] _T_10__T_12_data; // @[SPadModule.scala 35:50]
  wire [7:0] _T_10__T_12_addr; // @[SPadModule.scala 35:50]
  wire  _T_10__T_12_mask; // @[SPadModule.scala 35:50]
  wire  _T_10__T_12_en; // @[SPadModule.scala 35:50]
  reg  _T_10__T_18_en_pipe_0;
  reg [31:0] _RAND_2;
  reg [7:0] _T_10__T_18_addr_pipe_0;
  reg [31:0] _RAND_3;
  reg [7:0] dataLenReg; // @[SPadModule.scala 79:33]
  reg [31:0] _RAND_4;
  reg [7:0] padWriteIndexReg; // @[SPadModule.scala 82:39]
  reg [31:0] _RAND_5;
  reg [7:0] padReadIndexReg; // @[SPadModule.scala 83:38]
  reg [31:0] _RAND_6;
  wire [7:0] _T_1; // @[SPadModule.scala 88:53]
  wire  writeWrapWire; // @[SPadModule.scala 88:37]
  wire  readWrapWire; // @[SPadModule.scala 89:35]
  wire  _T_6; // @[SPadModule.scala 91:31]
  wire [7:0] _T_8; // @[SPadModule.scala 93:42]
  wire [7:0] _T_14; // @[SPadModule.scala 45:44]
  assign _T_10__T_18_addr = _T_10__T_18_addr_pipe_0;
  `ifndef RANDOMIZE_GARBAGE_ASSIGN
  assign _T_10__T_18_data = _T_10[_T_10__T_18_addr]; // @[SPadModule.scala 35:50]
  `else
  assign _T_10__T_18_data = _T_10__T_18_addr >= 8'hc0 ? _RAND_1[11:0] : _T_10[_T_10__T_18_addr]; // @[SPadModule.scala 35:50]
  `endif // RANDOMIZE_GARBAGE_ASSIGN
  assign _T_10__T_12_data = io_commonIO_dataLenFinIO_writeInDataIO_bits_data;
  assign _T_10__T_12_addr = padWriteIndexReg;
  assign _T_10__T_12_mask = 1'h1;
  assign _T_10__T_12_en = io_commonIO_dataLenFinIO_writeInDataIO_valid & io_commonIO_writeEn;
  assign _T_1 = dataLenReg - 8'h1; // @[SPadModule.scala 88:53]
  assign writeWrapWire = padWriteIndexReg == _T_1; // @[SPadModule.scala 88:37]
  assign readWrapWire = padReadIndexReg == _T_1; // @[SPadModule.scala 89:35]
  assign _T_6 = io_commonIO_dataLenFinIO_writeInDataIO_valid & io_commonIO_writeEn; // @[SPadModule.scala 91:31]
  assign _T_8 = padWriteIndexReg + 8'h1; // @[SPadModule.scala 93:42]
  assign _T_14 = padReadIndexReg + 8'h1; // @[SPadModule.scala 45:44]
  assign io_commonIO_columnNum = padReadIndexReg; // @[SPadModule.scala 103:25]
  assign io_commonIO_readOutData = _T_10__T_18_data; // @[SPadModule.scala 69:27]
  assign io_commonIO_dataLenFinIO_writeInDataIO_ready = io_commonIO_dataLenFinIO_writeInDataIO_valid & io_commonIO_writeEn; // @[SPadModule.scala 92:27 SPadModule.scala 98:27]
  assign io_commonIO_dataLenFinIO_writeFin = io_commonIO_dataLenFinIO_writeInDataIO_valid & writeWrapWire; // @[SPadModule.scala 102:37]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  _RAND_0 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 192; initvar = initvar+1)
    _T_10[initvar] = _RAND_0[11:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_1 = {1{`RANDOM}};
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_10__T_18_en_pipe_0 = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_10__T_18_addr_pipe_0 = _RAND_3[7:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  dataLenReg = _RAND_4[7:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  padWriteIndexReg = _RAND_5[7:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  padReadIndexReg = _RAND_6[7:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if(_T_10__T_12_en & _T_10__T_12_mask) begin
      _T_10[_T_10__T_12_addr] <= _T_10__T_12_data; // @[SPadModule.scala 35:50]
    end
    _T_10__T_18_en_pipe_0 <= io_commonIO_readEn;
    if (io_commonIO_readEn) begin
      _T_10__T_18_addr_pipe_0 <= padReadIndexReg;
    end
    if (reset) begin
      dataLenReg <= 8'h9;
    end else begin
      dataLenReg <= io_commonIO_dataLenFinIO_streamLen;
    end
    if (reset) begin
      padWriteIndexReg <= 8'h0;
    end else if (_T_6) begin
      if (writeWrapWire) begin
        padWriteIndexReg <= 8'h0;
      end else begin
        padWriteIndexReg <= _T_8;
      end
    end
    if (reset) begin
      padReadIndexReg <= 8'h0;
    end else if (io_dataIO_readInIdxEn) begin
      padReadIndexReg <= io_dataIO_readInIdx;
    end else if (io_dataIO_indexInc) begin
      if (readWrapWire) begin
        padReadIndexReg <= 8'h0;
      end else begin
        padReadIndexReg <= _T_14;
      end
    end
  end
endmodule
module ProcessingElementPad(
  input         clock,
  input         reset,
  input         io_padCtrl_doMACEn,
  output        io_padCtrl_fromTopIO_pSumEnqOrProduct_ready,
  input         io_padCtrl_fromTopIO_pSumEnqOrProduct_bits,
  input         io_padCtrl_fromTopIO_doLoadEn,
  output        io_padCtrl_fromTopIO_calFinish,
  output        io_dataStream_ipsIO_ready,
  input         io_dataStream_ipsIO_valid,
  input  [19:0] io_dataStream_ipsIO_bits,
  input         io_dataStream_opsIO_ready,
  output        io_dataStream_opsIO_valid,
  output [19:0] io_dataStream_opsIO_bits,
  output        io_dataStream_iactIOs_dataIOs_writeInDataIO_ready,
  input         io_dataStream_iactIOs_dataIOs_writeInDataIO_valid,
  input  [11:0] io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data,
  input  [3:0]  io_dataStream_iactIOs_dataIOs_streamLen,
  output        io_dataStream_iactIOs_dataIOs_writeFin,
  output        io_dataStream_iactIOs_addrIOs_writeInDataIO_ready,
  input         io_dataStream_iactIOs_addrIOs_writeInDataIO_valid,
  input  [3:0]  io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data,
  input  [3:0]  io_dataStream_iactIOs_addrIOs_streamLen,
  output        io_dataStream_iactIOs_addrIOs_writeFin,
  output        io_dataStream_weightIOs_dataIOs_writeInDataIO_ready,
  input         io_dataStream_weightIOs_dataIOs_writeInDataIO_valid,
  input  [11:0] io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data,
  input  [7:0]  io_dataStream_weightIOs_dataIOs_streamLen,
  output        io_dataStream_weightIOs_dataIOs_writeFin,
  output        io_dataStream_weightIOs_addrIOs_writeInDataIO_ready,
  input         io_dataStream_weightIOs_addrIOs_writeInDataIO_valid,
  input  [6:0]  io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data,
  input  [3:0]  io_dataStream_weightIOs_addrIOs_streamLen,
  output        io_dataStream_weightIOs_addrIOs_writeFin,
  output [7:0]  io_debugIO_iactMatrixData,
  output [3:0]  io_debugIO_iactMatrixRow,
  output [3:0]  io_debugIO_iactMatrixColumn,
  output        io_debugIO_iactAddrInc,
  output        io_debugIO_iactDataInc,
  output [3:0]  io_debugIO_iactAddrIdx,
  output [6:0]  io_debugIO_weightAddrSPadReadOut,
  output [7:0]  io_debugIO_weightMatrixData,
  output [3:0]  io_debugIO_weightMatrixRow,
  output [19:0] io_debugIO_productResult,
  output [19:0] io_debugIO_pSumResult,
  output [19:0] io_debugIO_pSumLoad,
  output [3:0]  io_debugIO_weightAddrInIdx,
  output [2:0]  io_debugIO_sPadState
);
  wire  SPadSeq_0_clock; // @[ProcessingElement.scala 152:44]
  wire  SPadSeq_0_reset; // @[ProcessingElement.scala 152:44]
  wire [3:0] SPadSeq_0_io_commonIO_columnNum; // @[ProcessingElement.scala 152:44]
  wire [3:0] SPadSeq_0_io_commonIO_readOutData; // @[ProcessingElement.scala 152:44]
  wire  SPadSeq_0_io_commonIO_writeEn; // @[ProcessingElement.scala 152:44]
  wire  SPadSeq_0_io_commonIO_dataLenFinIO_writeInDataIO_ready; // @[ProcessingElement.scala 152:44]
  wire  SPadSeq_0_io_commonIO_dataLenFinIO_writeInDataIO_valid; // @[ProcessingElement.scala 152:44]
  wire [3:0] SPadSeq_0_io_commonIO_dataLenFinIO_writeInDataIO_bits_data; // @[ProcessingElement.scala 152:44]
  wire [3:0] SPadSeq_0_io_commonIO_dataLenFinIO_streamLen; // @[ProcessingElement.scala 152:44]
  wire  SPadSeq_0_io_commonIO_dataLenFinIO_writeFin; // @[ProcessingElement.scala 152:44]
  wire  SPadSeq_0_io_addrIO_indexInc; // @[ProcessingElement.scala 152:44]
  wire  SPadSeq_1_clock; // @[ProcessingElement.scala 153:44]
  wire  SPadSeq_1_reset; // @[ProcessingElement.scala 153:44]
  wire [3:0] SPadSeq_1_io_commonIO_columnNum; // @[ProcessingElement.scala 153:44]
  wire [11:0] SPadSeq_1_io_commonIO_readOutData; // @[ProcessingElement.scala 153:44]
  wire  SPadSeq_1_io_commonIO_writeEn; // @[ProcessingElement.scala 153:44]
  wire  SPadSeq_1_io_commonIO_dataLenFinIO_writeInDataIO_ready; // @[ProcessingElement.scala 153:44]
  wire  SPadSeq_1_io_commonIO_dataLenFinIO_writeInDataIO_valid; // @[ProcessingElement.scala 153:44]
  wire [11:0] SPadSeq_1_io_commonIO_dataLenFinIO_writeInDataIO_bits_data; // @[ProcessingElement.scala 153:44]
  wire [3:0] SPadSeq_1_io_commonIO_dataLenFinIO_streamLen; // @[ProcessingElement.scala 153:44]
  wire  SPadSeq_1_io_commonIO_dataLenFinIO_writeFin; // @[ProcessingElement.scala 153:44]
  wire  SPadSeq_1_io_dataIO_indexInc; // @[ProcessingElement.scala 153:44]
  wire  SPadSeq_2_clock; // @[ProcessingElement.scala 154:46]
  wire  SPadSeq_2_reset; // @[ProcessingElement.scala 154:46]
  wire [6:0] SPadSeq_2_io_commonIO_readOutData; // @[ProcessingElement.scala 154:46]
  wire  SPadSeq_2_io_commonIO_writeEn; // @[ProcessingElement.scala 154:46]
  wire  SPadSeq_2_io_commonIO_dataLenFinIO_writeInDataIO_ready; // @[ProcessingElement.scala 154:46]
  wire  SPadSeq_2_io_commonIO_dataLenFinIO_writeInDataIO_valid; // @[ProcessingElement.scala 154:46]
  wire [6:0] SPadSeq_2_io_commonIO_dataLenFinIO_writeInDataIO_bits_data; // @[ProcessingElement.scala 154:46]
  wire [3:0] SPadSeq_2_io_commonIO_dataLenFinIO_streamLen; // @[ProcessingElement.scala 154:46]
  wire  SPadSeq_2_io_commonIO_dataLenFinIO_writeFin; // @[ProcessingElement.scala 154:46]
  wire [3:0] SPadSeq_2_io_addrIO_readInIdx; // @[ProcessingElement.scala 154:46]
  wire  SPadSeq_2_io_addrIO_indexInc; // @[ProcessingElement.scala 154:46]
  wire  SPadSeq_2_io_addrIO_readInIdxEn; // @[ProcessingElement.scala 154:46]
  wire  SPadSeq_3_clock; // @[ProcessingElement.scala 155:46]
  wire  SPadSeq_3_reset; // @[ProcessingElement.scala 155:46]
  wire [7:0] SPadSeq_3_io_commonIO_columnNum; // @[ProcessingElement.scala 155:46]
  wire [11:0] SPadSeq_3_io_commonIO_readOutData; // @[ProcessingElement.scala 155:46]
  wire  SPadSeq_3_io_commonIO_readEn; // @[ProcessingElement.scala 155:46]
  wire  SPadSeq_3_io_commonIO_writeEn; // @[ProcessingElement.scala 155:46]
  wire  SPadSeq_3_io_commonIO_dataLenFinIO_writeInDataIO_ready; // @[ProcessingElement.scala 155:46]
  wire  SPadSeq_3_io_commonIO_dataLenFinIO_writeInDataIO_valid; // @[ProcessingElement.scala 155:46]
  wire [11:0] SPadSeq_3_io_commonIO_dataLenFinIO_writeInDataIO_bits_data; // @[ProcessingElement.scala 155:46]
  wire [7:0] SPadSeq_3_io_commonIO_dataLenFinIO_streamLen; // @[ProcessingElement.scala 155:46]
  wire  SPadSeq_3_io_commonIO_dataLenFinIO_writeFin; // @[ProcessingElement.scala 155:46]
  wire [7:0] SPadSeq_3_io_dataIO_readInIdx; // @[ProcessingElement.scala 155:46]
  wire  SPadSeq_3_io_dataIO_indexInc; // @[ProcessingElement.scala 155:46]
  wire  SPadSeq_3_io_dataIO_readInIdxEn; // @[ProcessingElement.scala 155:46]
  reg [19:0] psDataSPad_0; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_0;
  reg [19:0] psDataSPad_1; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_1;
  reg [19:0] psDataSPad_2; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_2;
  reg [19:0] psDataSPad_3; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_3;
  reg [19:0] psDataSPad_4; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_4;
  reg [19:0] psDataSPad_5; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_5;
  reg [19:0] psDataSPad_6; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_6;
  reg [19:0] psDataSPad_7; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_7;
  reg [19:0] psDataSPad_8; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_8;
  reg [19:0] psDataSPad_9; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_9;
  reg [19:0] psDataSPad_10; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_10;
  reg [19:0] psDataSPad_11; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_11;
  reg [19:0] psDataSPad_12; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_12;
  reg [19:0] psDataSPad_13; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_13;
  reg [19:0] psDataSPad_14; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_14;
  reg [19:0] psDataSPad_15; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_15;
  reg [19:0] psDataSPad_16; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_16;
  reg [19:0] psDataSPad_17; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_17;
  reg [19:0] psDataSPad_18; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_18;
  reg [19:0] psDataSPad_19; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_19;
  reg [19:0] psDataSPad_20; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_20;
  reg [19:0] psDataSPad_21; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_21;
  reg [19:0] psDataSPad_22; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_22;
  reg [19:0] psDataSPad_23; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_23;
  reg [19:0] psDataSPad_24; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_24;
  reg [19:0] psDataSPad_25; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_25;
  reg [19:0] psDataSPad_26; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_26;
  reg [19:0] psDataSPad_27; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_27;
  reg [19:0] psDataSPad_28; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_28;
  reg [19:0] psDataSPad_29; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_29;
  reg [19:0] psDataSPad_30; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_30;
  reg [19:0] psDataSPad_31; // @[ProcessingElement.scala 151:38]
  reg [31:0] _RAND_31;
  reg  iactDataSPadReadEnReg; // @[ProcessingElement.scala 161:44]
  reg [31:0] _RAND_32;
  reg [3:0] iactMatrixColumnReg; // @[ProcessingElement.scala 164:42]
  reg [31:0] _RAND_33;
  reg [3:0] iactZeroColumnNumber; // @[ProcessingElement.scala 165:43]
  reg [31:0] _RAND_34;
  reg  iactDataSPadFirstReadReg; // @[ProcessingElement.scala 166:47]
  reg [31:0] _RAND_35;
  reg  weightDataSPadFirstRead; // @[ProcessingElement.scala 179:46]
  reg [31:0] _RAND_36;
  reg [19:0] productReg; // @[ProcessingElement.scala 186:33]
  reg [31:0] _RAND_37;
  reg [19:0] pSumSPadLoadReg; // @[ProcessingElement.scala 187:38]
  reg [31:0] _RAND_38;
  reg [2:0] sPad; // @[ProcessingElement.scala 199:27]
  reg [31:0] _RAND_39;
  wire  padEqIA; // @[ProcessingElement.scala 205:19]
  wire  padEqMpy; // @[ProcessingElement.scala 206:20]
  wire  padEqWB; // @[ProcessingElement.scala 207:19]
  wire  padEqWA; // @[ProcessingElement.scala 208:19]
  wire  padEqID; // @[ProcessingElement.scala 209:19]
  wire  iactDataCountVec_3; // @[ProcessingElement.scala 228:74]
  wire  iactDataCountVec_2; // @[ProcessingElement.scala 228:74]
  wire [1:0] _T_15; // @[Cat.scala 29:58]
  wire  iactDataCountVec_1; // @[ProcessingElement.scala 228:74]
  wire  iactDataCountVec_0; // @[ProcessingElement.scala 228:74]
  wire [1:0] _T_14; // @[Cat.scala 29:58]
  wire [3:0] iactMatrixRowWire; // @[Cat.scala 29:58]
  wire  weightMatrixReadFirstColumn; // @[ProcessingElement.scala 210:52]
  wire  iactDataCountVec_4; // @[ProcessingElement.scala 228:74]
  wire  iactDataCountVec_5; // @[ProcessingElement.scala 228:74]
  wire  iactDataCountVec_6; // @[ProcessingElement.scala 228:74]
  wire  iactDataCountVec_7; // @[ProcessingElement.scala 228:74]
  wire  iactDataCountVec_8; // @[ProcessingElement.scala 228:74]
  wire  iactDataCountVec_9; // @[ProcessingElement.scala 228:74]
  wire  iactDataCountVec_10; // @[ProcessingElement.scala 228:74]
  wire  iactDataCountVec_11; // @[ProcessingElement.scala 228:74]
  wire [3:0] _T_9; // @[Cat.scala 29:58]
  wire [3:0] _T_12; // @[Cat.scala 29:58]
  wire [7:0] iactMatrixDataWire; // @[Cat.scala 29:58]
  wire  weightDataCountVec_0; // @[ProcessingElement.scala 250:78]
  wire  weightDataCountVec_1; // @[ProcessingElement.scala 250:78]
  wire  weightDataCountVec_2; // @[ProcessingElement.scala 250:78]
  wire  weightDataCountVec_3; // @[ProcessingElement.scala 250:78]
  wire  weightDataCountVec_4; // @[ProcessingElement.scala 250:78]
  wire  weightDataCountVec_5; // @[ProcessingElement.scala 250:78]
  wire  weightDataCountVec_6; // @[ProcessingElement.scala 250:78]
  wire  weightDataCountVec_7; // @[ProcessingElement.scala 250:78]
  wire  weightDataCountVec_8; // @[ProcessingElement.scala 250:78]
  wire  weightDataCountVec_9; // @[ProcessingElement.scala 250:78]
  wire  weightDataCountVec_10; // @[ProcessingElement.scala 250:78]
  wire  weightDataCountVec_11; // @[ProcessingElement.scala 250:78]
  wire [3:0] _T_19; // @[Cat.scala 29:58]
  wire [3:0] _T_22; // @[Cat.scala 29:58]
  wire [7:0] weightMatrixDataReg; // @[Cat.scala 29:58]
  wire [1:0] _T_24; // @[Cat.scala 29:58]
  wire [1:0] _T_25; // @[Cat.scala 29:58]
  wire [3:0] weightMatrixRowReg; // @[Cat.scala 29:58]
  wire [6:0] weightAddrDataWire; // @[ProcessingElement.scala 171:38 ProcessingElement.scala 239:22]
  wire [6:0] _T_27; // @[ProcessingElement.scala 254:44]
  wire  _T_28; // @[ProcessingElement.scala 259:41]
  reg [4:0] value; // @[Counter.scala 29:33]
  reg [31:0] _RAND_40;
  wire  _T_29; // @[ProcessingElement.scala 261:39]
  wire [19:0] _GEN_1; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_2; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_3; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_4; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_5; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_6; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_7; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_8; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_9; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_10; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_11; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_12; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_13; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_14; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_15; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_16; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_17; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_18; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_19; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_20; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_21; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_22; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_23; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_24; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_25; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_26; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_27; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_28; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_29; // @[ProcessingElement.scala 262:30]
  wire [19:0] _GEN_30; // @[ProcessingElement.scala 262:30]
  wire  _T_30; // @[Counter.scala 37:24]
  wire [4:0] _T_32; // @[Counter.scala 38:22]
  wire [4:0] _GEN_32; // @[Counter.scala 40:21]
  wire [4:0] _GEN_35; // @[ProcessingElement.scala 261:69]
  wire [19:0] _T_34; // @[ProcessingElement.scala 271:50]
  wire [19:0] pSumResultWire; // @[ProcessingElement.scala 271:24]
  reg  iactSPadZeroColumnReg; // @[ProcessingElement.scala 280:44]
  reg [31:0] _RAND_41;
  wire [3:0] iactAddrDataWire; // @[ProcessingElement.scala 158:36 ProcessingElement.scala 219:20]
  wire  mightIactZeroColumnWire; // @[ProcessingElement.scala 287:47]
  wire  mightWeightZeroColumnWire; // @[ProcessingElement.scala 288:51]
  wire [3:0] iactDataIndexWire; // @[ProcessingElement.scala 159:37 ProcessingElement.scala 227:21]
  wire [3:0] _T_39; // @[ProcessingElement.scala 289:66]
  wire  mightIactIdxIncWire; // @[ProcessingElement.scala 289:43]
  wire [3:0] weightDataIndexWire; // @[ProcessingElement.scala 172:39 ProcessingElement.scala 249:23]
  wire [3:0] _T_42; // @[ProcessingElement.scala 290:72]
  wire [6:0] _GEN_491; // @[ProcessingElement.scala 290:47]
  wire  mightWeightIdxIncWire; // @[ProcessingElement.scala 290:47]
  wire  mightIactReadFinish; // @[ProcessingElement.scala 291:52]
  wire [7:0] _GEN_492; // @[ProcessingElement.scala 292:56]
  wire  mightWeightReadFinish; // @[ProcessingElement.scala 292:56]
  wire  _T_50; // @[ProcessingElement.scala 293:38]
  wire  _T_51; // @[ProcessingElement.scala 293:80]
  wire  _T_52; // @[ProcessingElement.scala 293:122]
  wire  _T_53; // @[ProcessingElement.scala 293:110]
  wire  _T_54; // @[ProcessingElement.scala 293:149]
  wire  _T_56; // @[ProcessingElement.scala 294:49]
  wire  _T_57; // @[ProcessingElement.scala 294:41]
  wire  _T_60; // @[ProcessingElement.scala 295:38]
  wire  _T_62; // @[ProcessingElement.scala 295:66]
  wire  _T_67; // @[ProcessingElement.scala 295:179]
  wire  _T_70; // @[ProcessingElement.scala 296:40]
  wire  _T_72; // @[ProcessingElement.scala 296:70]
  wire  _T_74; // @[ProcessingElement.scala 296:111]
  wire  _T_76; // @[ProcessingElement.scala 297:35]
  wire  _T_78; // @[ProcessingElement.scala 298:35]
  wire  weightDataIdxMuxWire; // @[ProcessingElement.scala 298:62]
  wire [3:0] _T_82; // @[ProcessingElement.scala 299:76]
  wire  _T_84; // @[ProcessingElement.scala 300:34]
  wire  _T_87; // @[ProcessingElement.scala 301:45]
  wire [6:0] _T_89; // @[ProcessingElement.scala 302:64]
  wire [6:0] _GEN_493; // @[ProcessingElement.scala 302:43]
  wire [6:0] _T_91; // @[ProcessingElement.scala 302:43]
  wire  _T_92; // @[Conditional.scala 37:30]
  wire  _GEN_41; // @[ProcessingElement.scala 305:32]
  wire  _GEN_42; // @[ProcessingElement.scala 305:32]
  wire  _T_93; // @[Conditional.scala 37:30]
  wire [3:0] _T_95; // @[ProcessingElement.scala 314:54]
  wire  _GEN_48; // @[ProcessingElement.scala 311:38]
  wire  _T_96; // @[Conditional.scala 37:30]
  wire  _T_97; // @[Conditional.scala 37:30]
  wire [3:0] _T_99; // @[ProcessingElement.scala 329:56]
  wire [3:0] _T_101; // @[ProcessingElement.scala 329:62]
  wire  _GEN_55; // @[ProcessingElement.scala 325:36]
  wire  _GEN_63; // @[ProcessingElement.scala 324:40]
  wire  _GEN_66; // @[ProcessingElement.scala 324:40]
  wire  _T_104; // @[Conditional.scala 37:30]
  wire  _T_105; // @[Conditional.scala 37:30]
  wire  _T_106; // @[Conditional.scala 37:30]
  wire  _GEN_72; // @[ProcessingElement.scala 351:42]
  wire [15:0] _T_107; // @[ProcessingElement.scala 359:44]
  wire  _GEN_76; // @[ProcessingElement.scala 350:57]
  wire  _T_108; // @[Conditional.scala 37:30]
  wire [4:0] psDataSpadIdxWire; // @[ProcessingElement.scala 286:37 ProcessingElement.scala 302:21]
  wire  _T_109; // @[ProcessingElement.scala 365:33]
  wire  _GEN_114; // @[ProcessingElement.scala 370:38]
  wire  _GEN_128; // @[ProcessingElement.scala 365:59]
  wire  _GEN_170; // @[Conditional.scala 39:67]
  wire  _GEN_175; // @[Conditional.scala 39:67]
  wire  _GEN_215; // @[Conditional.scala 39:67]
  wire  _GEN_225; // @[Conditional.scala 39:67]
  wire  _GEN_260; // @[Conditional.scala 39:67]
  wire  _GEN_270; // @[Conditional.scala 39:67]
  wire  _GEN_305; // @[Conditional.scala 39:67]
  wire  _GEN_313; // @[Conditional.scala 39:67]
  wire  _GEN_319; // @[Conditional.scala 39:67]
  wire  _GEN_358; // @[Conditional.scala 39:67]
  wire  _GEN_359; // @[Conditional.scala 39:67]
  wire  _GEN_365; // @[Conditional.scala 39:67]
  wire  _GEN_404; // @[Conditional.scala 39:67]
  wire  _GEN_407; // @[Conditional.scala 39:67]
  wire  _GEN_411; // @[Conditional.scala 39:67]
  wire  _GEN_450; // @[Conditional.scala 40:58]
  wire  _GEN_451; // @[Conditional.scala 40:58]
  SPadAddrModule SPadSeq_0 ( // @[ProcessingElement.scala 152:44]
    .clock(SPadSeq_0_clock),
    .reset(SPadSeq_0_reset),
    .io_commonIO_columnNum(SPadSeq_0_io_commonIO_columnNum),
    .io_commonIO_readOutData(SPadSeq_0_io_commonIO_readOutData),
    .io_commonIO_writeEn(SPadSeq_0_io_commonIO_writeEn),
    .io_commonIO_dataLenFinIO_writeInDataIO_ready(SPadSeq_0_io_commonIO_dataLenFinIO_writeInDataIO_ready),
    .io_commonIO_dataLenFinIO_writeInDataIO_valid(SPadSeq_0_io_commonIO_dataLenFinIO_writeInDataIO_valid),
    .io_commonIO_dataLenFinIO_writeInDataIO_bits_data(SPadSeq_0_io_commonIO_dataLenFinIO_writeInDataIO_bits_data),
    .io_commonIO_dataLenFinIO_streamLen(SPadSeq_0_io_commonIO_dataLenFinIO_streamLen),
    .io_commonIO_dataLenFinIO_writeFin(SPadSeq_0_io_commonIO_dataLenFinIO_writeFin),
    .io_addrIO_indexInc(SPadSeq_0_io_addrIO_indexInc)
  );
  SPadDataModule SPadSeq_1 ( // @[ProcessingElement.scala 153:44]
    .clock(SPadSeq_1_clock),
    .reset(SPadSeq_1_reset),
    .io_commonIO_columnNum(SPadSeq_1_io_commonIO_columnNum),
    .io_commonIO_readOutData(SPadSeq_1_io_commonIO_readOutData),
    .io_commonIO_writeEn(SPadSeq_1_io_commonIO_writeEn),
    .io_commonIO_dataLenFinIO_writeInDataIO_ready(SPadSeq_1_io_commonIO_dataLenFinIO_writeInDataIO_ready),
    .io_commonIO_dataLenFinIO_writeInDataIO_valid(SPadSeq_1_io_commonIO_dataLenFinIO_writeInDataIO_valid),
    .io_commonIO_dataLenFinIO_writeInDataIO_bits_data(SPadSeq_1_io_commonIO_dataLenFinIO_writeInDataIO_bits_data),
    .io_commonIO_dataLenFinIO_streamLen(SPadSeq_1_io_commonIO_dataLenFinIO_streamLen),
    .io_commonIO_dataLenFinIO_writeFin(SPadSeq_1_io_commonIO_dataLenFinIO_writeFin),
    .io_dataIO_indexInc(SPadSeq_1_io_dataIO_indexInc)
  );
  WeightSPadAddrModule SPadSeq_2 ( // @[ProcessingElement.scala 154:46]
    .clock(SPadSeq_2_clock),
    .reset(SPadSeq_2_reset),
    .io_commonIO_readOutData(SPadSeq_2_io_commonIO_readOutData),
    .io_commonIO_writeEn(SPadSeq_2_io_commonIO_writeEn),
    .io_commonIO_dataLenFinIO_writeInDataIO_ready(SPadSeq_2_io_commonIO_dataLenFinIO_writeInDataIO_ready),
    .io_commonIO_dataLenFinIO_writeInDataIO_valid(SPadSeq_2_io_commonIO_dataLenFinIO_writeInDataIO_valid),
    .io_commonIO_dataLenFinIO_writeInDataIO_bits_data(SPadSeq_2_io_commonIO_dataLenFinIO_writeInDataIO_bits_data),
    .io_commonIO_dataLenFinIO_streamLen(SPadSeq_2_io_commonIO_dataLenFinIO_streamLen),
    .io_commonIO_dataLenFinIO_writeFin(SPadSeq_2_io_commonIO_dataLenFinIO_writeFin),
    .io_addrIO_readInIdx(SPadSeq_2_io_addrIO_readInIdx),
    .io_addrIO_indexInc(SPadSeq_2_io_addrIO_indexInc),
    .io_addrIO_readInIdxEn(SPadSeq_2_io_addrIO_readInIdxEn)
  );
  SPadDataModule_1 SPadSeq_3 ( // @[ProcessingElement.scala 155:46]
    .clock(SPadSeq_3_clock),
    .reset(SPadSeq_3_reset),
    .io_commonIO_columnNum(SPadSeq_3_io_commonIO_columnNum),
    .io_commonIO_readOutData(SPadSeq_3_io_commonIO_readOutData),
    .io_commonIO_readEn(SPadSeq_3_io_commonIO_readEn),
    .io_commonIO_writeEn(SPadSeq_3_io_commonIO_writeEn),
    .io_commonIO_dataLenFinIO_writeInDataIO_ready(SPadSeq_3_io_commonIO_dataLenFinIO_writeInDataIO_ready),
    .io_commonIO_dataLenFinIO_writeInDataIO_valid(SPadSeq_3_io_commonIO_dataLenFinIO_writeInDataIO_valid),
    .io_commonIO_dataLenFinIO_writeInDataIO_bits_data(SPadSeq_3_io_commonIO_dataLenFinIO_writeInDataIO_bits_data),
    .io_commonIO_dataLenFinIO_streamLen(SPadSeq_3_io_commonIO_dataLenFinIO_streamLen),
    .io_commonIO_dataLenFinIO_writeFin(SPadSeq_3_io_commonIO_dataLenFinIO_writeFin),
    .io_dataIO_readInIdx(SPadSeq_3_io_dataIO_readInIdx),
    .io_dataIO_indexInc(SPadSeq_3_io_dataIO_indexInc),
    .io_dataIO_readInIdxEn(SPadSeq_3_io_dataIO_readInIdxEn)
  );
  assign padEqIA = sPad == 3'h1; // @[ProcessingElement.scala 205:19]
  assign padEqMpy = sPad == 3'h6; // @[ProcessingElement.scala 206:20]
  assign padEqWB = sPad == 3'h7; // @[ProcessingElement.scala 207:19]
  assign padEqWA = sPad == 3'h3; // @[ProcessingElement.scala 208:19]
  assign padEqID = sPad == 3'h2; // @[ProcessingElement.scala 209:19]
  assign iactDataCountVec_3 = SPadSeq_1_io_commonIO_readOutData[3]; // @[ProcessingElement.scala 228:74]
  assign iactDataCountVec_2 = SPadSeq_1_io_commonIO_readOutData[2]; // @[ProcessingElement.scala 228:74]
  assign _T_15 = {iactDataCountVec_3,iactDataCountVec_2}; // @[Cat.scala 29:58]
  assign iactDataCountVec_1 = SPadSeq_1_io_commonIO_readOutData[1]; // @[ProcessingElement.scala 228:74]
  assign iactDataCountVec_0 = SPadSeq_1_io_commonIO_readOutData[0]; // @[ProcessingElement.scala 228:74]
  assign _T_14 = {iactDataCountVec_1,iactDataCountVec_0}; // @[Cat.scala 29:58]
  assign iactMatrixRowWire = {iactDataCountVec_3,iactDataCountVec_2,iactDataCountVec_1,iactDataCountVec_0}; // @[Cat.scala 29:58]
  assign weightMatrixReadFirstColumn = iactMatrixRowWire == 4'h0; // @[ProcessingElement.scala 210:52]
  assign iactDataCountVec_4 = SPadSeq_1_io_commonIO_readOutData[4]; // @[ProcessingElement.scala 228:74]
  assign iactDataCountVec_5 = SPadSeq_1_io_commonIO_readOutData[5]; // @[ProcessingElement.scala 228:74]
  assign iactDataCountVec_6 = SPadSeq_1_io_commonIO_readOutData[6]; // @[ProcessingElement.scala 228:74]
  assign iactDataCountVec_7 = SPadSeq_1_io_commonIO_readOutData[7]; // @[ProcessingElement.scala 228:74]
  assign iactDataCountVec_8 = SPadSeq_1_io_commonIO_readOutData[8]; // @[ProcessingElement.scala 228:74]
  assign iactDataCountVec_9 = SPadSeq_1_io_commonIO_readOutData[9]; // @[ProcessingElement.scala 228:74]
  assign iactDataCountVec_10 = SPadSeq_1_io_commonIO_readOutData[10]; // @[ProcessingElement.scala 228:74]
  assign iactDataCountVec_11 = SPadSeq_1_io_commonIO_readOutData[11]; // @[ProcessingElement.scala 228:74]
  assign _T_9 = {iactDataCountVec_7,iactDataCountVec_6,iactDataCountVec_5,iactDataCountVec_4}; // @[Cat.scala 29:58]
  assign _T_12 = {iactDataCountVec_11,iactDataCountVec_10,iactDataCountVec_9,iactDataCountVec_8}; // @[Cat.scala 29:58]
  assign iactMatrixDataWire = {iactDataCountVec_11,iactDataCountVec_10,iactDataCountVec_9,iactDataCountVec_8,iactDataCountVec_7,iactDataCountVec_6,iactDataCountVec_5,iactDataCountVec_4}; // @[Cat.scala 29:58]
  assign weightDataCountVec_0 = SPadSeq_3_io_commonIO_readOutData[0]; // @[ProcessingElement.scala 250:78]
  assign weightDataCountVec_1 = SPadSeq_3_io_commonIO_readOutData[1]; // @[ProcessingElement.scala 250:78]
  assign weightDataCountVec_2 = SPadSeq_3_io_commonIO_readOutData[2]; // @[ProcessingElement.scala 250:78]
  assign weightDataCountVec_3 = SPadSeq_3_io_commonIO_readOutData[3]; // @[ProcessingElement.scala 250:78]
  assign weightDataCountVec_4 = SPadSeq_3_io_commonIO_readOutData[4]; // @[ProcessingElement.scala 250:78]
  assign weightDataCountVec_5 = SPadSeq_3_io_commonIO_readOutData[5]; // @[ProcessingElement.scala 250:78]
  assign weightDataCountVec_6 = SPadSeq_3_io_commonIO_readOutData[6]; // @[ProcessingElement.scala 250:78]
  assign weightDataCountVec_7 = SPadSeq_3_io_commonIO_readOutData[7]; // @[ProcessingElement.scala 250:78]
  assign weightDataCountVec_8 = SPadSeq_3_io_commonIO_readOutData[8]; // @[ProcessingElement.scala 250:78]
  assign weightDataCountVec_9 = SPadSeq_3_io_commonIO_readOutData[9]; // @[ProcessingElement.scala 250:78]
  assign weightDataCountVec_10 = SPadSeq_3_io_commonIO_readOutData[10]; // @[ProcessingElement.scala 250:78]
  assign weightDataCountVec_11 = SPadSeq_3_io_commonIO_readOutData[11]; // @[ProcessingElement.scala 250:78]
  assign _T_19 = {weightDataCountVec_7,weightDataCountVec_6,weightDataCountVec_5,weightDataCountVec_4}; // @[Cat.scala 29:58]
  assign _T_22 = {weightDataCountVec_11,weightDataCountVec_10,weightDataCountVec_9,weightDataCountVec_8}; // @[Cat.scala 29:58]
  assign weightMatrixDataReg = {weightDataCountVec_11,weightDataCountVec_10,weightDataCountVec_9,weightDataCountVec_8,weightDataCountVec_7,weightDataCountVec_6,weightDataCountVec_5,weightDataCountVec_4}; // @[Cat.scala 29:58]
  assign _T_24 = {weightDataCountVec_1,weightDataCountVec_0}; // @[Cat.scala 29:58]
  assign _T_25 = {weightDataCountVec_3,weightDataCountVec_2}; // @[Cat.scala 29:58]
  assign weightMatrixRowReg = {weightDataCountVec_3,weightDataCountVec_2,weightDataCountVec_1,weightDataCountVec_0}; // @[Cat.scala 29:58]
  assign weightAddrDataWire = SPadSeq_2_io_commonIO_readOutData; // @[ProcessingElement.scala 171:38 ProcessingElement.scala 239:22]
  assign _T_27 = weightMatrixReadFirstColumn ? 7'h0 : weightAddrDataWire; // @[ProcessingElement.scala 254:44]
  assign _T_28 = padEqMpy & io_padCtrl_fromTopIO_pSumEnqOrProduct_bits; // @[ProcessingElement.scala 259:41]
  assign _T_29 = io_padCtrl_fromTopIO_doLoadEn & io_dataStream_opsIO_ready; // @[ProcessingElement.scala 261:39]
  assign _GEN_1 = 5'h1 == value ? psDataSPad_1 : psDataSPad_0; // @[ProcessingElement.scala 262:30]
  assign _GEN_2 = 5'h2 == value ? psDataSPad_2 : _GEN_1; // @[ProcessingElement.scala 262:30]
  assign _GEN_3 = 5'h3 == value ? psDataSPad_3 : _GEN_2; // @[ProcessingElement.scala 262:30]
  assign _GEN_4 = 5'h4 == value ? psDataSPad_4 : _GEN_3; // @[ProcessingElement.scala 262:30]
  assign _GEN_5 = 5'h5 == value ? psDataSPad_5 : _GEN_4; // @[ProcessingElement.scala 262:30]
  assign _GEN_6 = 5'h6 == value ? psDataSPad_6 : _GEN_5; // @[ProcessingElement.scala 262:30]
  assign _GEN_7 = 5'h7 == value ? psDataSPad_7 : _GEN_6; // @[ProcessingElement.scala 262:30]
  assign _GEN_8 = 5'h8 == value ? psDataSPad_8 : _GEN_7; // @[ProcessingElement.scala 262:30]
  assign _GEN_9 = 5'h9 == value ? psDataSPad_9 : _GEN_8; // @[ProcessingElement.scala 262:30]
  assign _GEN_10 = 5'ha == value ? psDataSPad_10 : _GEN_9; // @[ProcessingElement.scala 262:30]
  assign _GEN_11 = 5'hb == value ? psDataSPad_11 : _GEN_10; // @[ProcessingElement.scala 262:30]
  assign _GEN_12 = 5'hc == value ? psDataSPad_12 : _GEN_11; // @[ProcessingElement.scala 262:30]
  assign _GEN_13 = 5'hd == value ? psDataSPad_13 : _GEN_12; // @[ProcessingElement.scala 262:30]
  assign _GEN_14 = 5'he == value ? psDataSPad_14 : _GEN_13; // @[ProcessingElement.scala 262:30]
  assign _GEN_15 = 5'hf == value ? psDataSPad_15 : _GEN_14; // @[ProcessingElement.scala 262:30]
  assign _GEN_16 = 5'h10 == value ? psDataSPad_16 : _GEN_15; // @[ProcessingElement.scala 262:30]
  assign _GEN_17 = 5'h11 == value ? psDataSPad_17 : _GEN_16; // @[ProcessingElement.scala 262:30]
  assign _GEN_18 = 5'h12 == value ? psDataSPad_18 : _GEN_17; // @[ProcessingElement.scala 262:30]
  assign _GEN_19 = 5'h13 == value ? psDataSPad_19 : _GEN_18; // @[ProcessingElement.scala 262:30]
  assign _GEN_20 = 5'h14 == value ? psDataSPad_20 : _GEN_19; // @[ProcessingElement.scala 262:30]
  assign _GEN_21 = 5'h15 == value ? psDataSPad_21 : _GEN_20; // @[ProcessingElement.scala 262:30]
  assign _GEN_22 = 5'h16 == value ? psDataSPad_22 : _GEN_21; // @[ProcessingElement.scala 262:30]
  assign _GEN_23 = 5'h17 == value ? psDataSPad_23 : _GEN_22; // @[ProcessingElement.scala 262:30]
  assign _GEN_24 = 5'h18 == value ? psDataSPad_24 : _GEN_23; // @[ProcessingElement.scala 262:30]
  assign _GEN_25 = 5'h19 == value ? psDataSPad_25 : _GEN_24; // @[ProcessingElement.scala 262:30]
  assign _GEN_26 = 5'h1a == value ? psDataSPad_26 : _GEN_25; // @[ProcessingElement.scala 262:30]
  assign _GEN_27 = 5'h1b == value ? psDataSPad_27 : _GEN_26; // @[ProcessingElement.scala 262:30]
  assign _GEN_28 = 5'h1c == value ? psDataSPad_28 : _GEN_27; // @[ProcessingElement.scala 262:30]
  assign _GEN_29 = 5'h1d == value ? psDataSPad_29 : _GEN_28; // @[ProcessingElement.scala 262:30]
  assign _GEN_30 = 5'h1e == value ? psDataSPad_30 : _GEN_29; // @[ProcessingElement.scala 262:30]
  assign _T_30 = value == 5'h18; // @[Counter.scala 37:24]
  assign _T_32 = value + 5'h1; // @[Counter.scala 38:22]
  assign _GEN_32 = _T_30 ? 5'h0 : _T_32; // @[Counter.scala 40:21]
  assign _GEN_35 = _T_29 ? _GEN_32 : value; // @[ProcessingElement.scala 261:69]
  assign _T_34 = pSumSPadLoadReg + productReg; // @[ProcessingElement.scala 271:50]
  assign pSumResultWire = padEqWB ? _T_34 : 20'h0; // @[ProcessingElement.scala 271:24]
  assign iactAddrDataWire = SPadSeq_0_io_commonIO_readOutData; // @[ProcessingElement.scala 158:36 ProcessingElement.scala 219:20]
  assign mightIactZeroColumnWire = iactAddrDataWire == 4'hf; // @[ProcessingElement.scala 287:47]
  assign mightWeightZeroColumnWire = weightAddrDataWire == 7'h7f; // @[ProcessingElement.scala 288:51]
  assign iactDataIndexWire = SPadSeq_1_io_commonIO_columnNum; // @[ProcessingElement.scala 159:37 ProcessingElement.scala 227:21]
  assign _T_39 = iactDataIndexWire + 4'h1; // @[ProcessingElement.scala 289:66]
  assign mightIactIdxIncWire = iactAddrDataWire == _T_39; // @[ProcessingElement.scala 289:43]
  assign weightDataIndexWire = SPadSeq_3_io_commonIO_columnNum[3:0]; // @[ProcessingElement.scala 172:39 ProcessingElement.scala 249:23]
  assign _T_42 = weightDataIndexWire + 4'h1; // @[ProcessingElement.scala 290:72]
  assign _GEN_491 = {{3'd0}, _T_42}; // @[ProcessingElement.scala 290:47]
  assign mightWeightIdxIncWire = weightAddrDataWire == _GEN_491; // @[ProcessingElement.scala 290:47]
  assign mightIactReadFinish = _T_39 == io_dataStream_iactIOs_dataIOs_streamLen; // @[ProcessingElement.scala 291:52]
  assign _GEN_492 = {{4'd0}, _T_42}; // @[ProcessingElement.scala 292:56]
  assign mightWeightReadFinish = _GEN_492 == io_dataStream_weightIOs_dataIOs_streamLen; // @[ProcessingElement.scala 292:56]
  assign _T_50 = padEqIA & mightIactZeroColumnWire; // @[ProcessingElement.scala 293:38]
  assign _T_51 = padEqWA & mightWeightZeroColumnWire; // @[ProcessingElement.scala 293:80]
  assign _T_52 = padEqWB & mightWeightIdxIncWire; // @[ProcessingElement.scala 293:122]
  assign _T_53 = _T_51 | _T_52; // @[ProcessingElement.scala 293:110]
  assign _T_54 = _T_53 & mightIactIdxIncWire; // @[ProcessingElement.scala 293:149]
  assign _T_56 = sPad == 3'h4; // @[ProcessingElement.scala 294:49]
  assign _T_57 = padEqMpy | _T_56; // @[ProcessingElement.scala 294:41]
  assign _T_60 = padEqIA & ~mightIactZeroColumnWire; // @[ProcessingElement.scala 295:38]
  assign _T_62 = _T_60 & ~iactDataSPadFirstReadReg; // @[ProcessingElement.scala 295:66]
  assign _T_67 = _T_53 & ~mightIactIdxIncWire; // @[ProcessingElement.scala 295:179]
  assign _T_70 = padEqWA & ~mightWeightZeroColumnWire; // @[ProcessingElement.scala 296:40]
  assign _T_72 = _T_70 & ~weightDataSPadFirstRead; // @[ProcessingElement.scala 296:70]
  assign _T_74 = padEqWB & ~mightWeightIdxIncWire; // @[ProcessingElement.scala 296:111]
  assign _T_76 = padEqID | padEqWA; // @[ProcessingElement.scala 297:35]
  assign _T_78 = padEqID & weightDataSPadFirstRead; // @[ProcessingElement.scala 298:35]
  assign weightDataIdxMuxWire = _T_78 & ~weightMatrixReadFirstColumn; // @[ProcessingElement.scala 298:62]
  assign _T_82 = iactMatrixRowWire - 4'h1; // @[ProcessingElement.scala 299:76]
  assign _T_84 = padEqWA & weightDataSPadFirstRead; // @[ProcessingElement.scala 300:34]
  assign _T_87 = padEqWB & mightIactReadFinish; // @[ProcessingElement.scala 301:45]
  assign _T_89 = iactMatrixColumnReg * 4'h4; // @[ProcessingElement.scala 302:64]
  assign _GEN_493 = {{3'd0}, weightMatrixRowReg}; // @[ProcessingElement.scala 302:43]
  assign _T_91 = _GEN_493 + _T_89; // @[ProcessingElement.scala 302:43]
  assign _T_92 = 3'h0 == sPad; // @[Conditional.scala 37:30]
  assign _GEN_41 = io_padCtrl_doMACEn | weightDataSPadFirstRead; // @[ProcessingElement.scala 305:32]
  assign _GEN_42 = io_padCtrl_doMACEn | iactDataSPadFirstReadReg; // @[ProcessingElement.scala 305:32]
  assign _T_93 = 3'h1 == sPad; // @[Conditional.scala 37:30]
  assign _T_95 = iactZeroColumnNumber + 4'h1; // @[ProcessingElement.scala 314:54]
  assign _GEN_48 = mightIactZeroColumnWire | iactSPadZeroColumnReg; // @[ProcessingElement.scala 311:38]
  assign _T_96 = 3'h2 == sPad; // @[Conditional.scala 37:30]
  assign _T_97 = 3'h3 == sPad; // @[Conditional.scala 37:30]
  assign _T_99 = iactMatrixColumnReg + 4'h1; // @[ProcessingElement.scala 329:56]
  assign _T_101 = _T_99 + iactZeroColumnNumber; // @[ProcessingElement.scala 329:62]
  assign _GEN_55 = mightIactIdxIncWire ? 1'h0 : 1'h1; // @[ProcessingElement.scala 325:36]
  assign _GEN_63 = mightWeightZeroColumnWire & _GEN_55; // @[ProcessingElement.scala 324:40]
  assign _GEN_66 = mightWeightZeroColumnWire | weightDataSPadFirstRead; // @[ProcessingElement.scala 324:40]
  assign _T_104 = 3'h4 == sPad; // @[Conditional.scala 37:30]
  assign _T_105 = 3'h5 == sPad; // @[Conditional.scala 37:30]
  assign _T_106 = 3'h6 == sPad; // @[Conditional.scala 37:30]
  assign _GEN_72 = io_dataStream_ipsIO_valid | _T_28; // @[ProcessingElement.scala 351:42]
  assign _T_107 = weightMatrixDataReg * iactMatrixDataWire; // @[ProcessingElement.scala 359:44]
  assign _GEN_76 = io_padCtrl_fromTopIO_pSumEnqOrProduct_bits ? _GEN_72 : _T_28; // @[ProcessingElement.scala 350:57]
  assign _T_108 = 3'h7 == sPad; // @[Conditional.scala 37:30]
  assign psDataSpadIdxWire = _T_91[4:0]; // @[ProcessingElement.scala 286:37 ProcessingElement.scala 302:21]
  assign _T_109 = mightIactReadFinish & mightWeightReadFinish; // @[ProcessingElement.scala 365:33]
  assign _GEN_114 = mightWeightIdxIncWire & _GEN_55; // @[ProcessingElement.scala 370:38]
  assign _GEN_128 = _T_109 ? weightDataSPadFirstRead : mightWeightIdxIncWire; // @[ProcessingElement.scala 365:59]
  assign _GEN_170 = _T_108 ? _GEN_128 : weightDataSPadFirstRead; // @[Conditional.scala 39:67]
  assign _GEN_175 = _T_106 ? _GEN_76 : _T_28; // @[Conditional.scala 39:67]
  assign _GEN_215 = _T_106 ? weightDataSPadFirstRead : _GEN_170; // @[Conditional.scala 39:67]
  assign _GEN_225 = _T_105 ? _T_28 : _GEN_175; // @[Conditional.scala 39:67]
  assign _GEN_260 = _T_105 ? weightDataSPadFirstRead : _GEN_215; // @[Conditional.scala 39:67]
  assign _GEN_270 = _T_104 ? _T_28 : _GEN_225; // @[Conditional.scala 39:67]
  assign _GEN_305 = _T_104 ? weightDataSPadFirstRead : _GEN_260; // @[Conditional.scala 39:67]
  assign _GEN_313 = _T_97 ? _GEN_66 : _GEN_305; // @[Conditional.scala 39:67]
  assign _GEN_319 = _T_97 ? _T_28 : _GEN_270; // @[Conditional.scala 39:67]
  assign _GEN_358 = _T_96 ? 1'h0 : iactDataSPadFirstReadReg; // @[Conditional.scala 39:67]
  assign _GEN_359 = _T_96 ? weightDataSPadFirstRead : _GEN_313; // @[Conditional.scala 39:67]
  assign _GEN_365 = _T_96 ? _T_28 : _GEN_319; // @[Conditional.scala 39:67]
  assign _GEN_404 = _T_93 | _GEN_359; // @[Conditional.scala 39:67]
  assign _GEN_407 = _T_93 ? iactDataSPadFirstReadReg : _GEN_358; // @[Conditional.scala 39:67]
  assign _GEN_411 = _T_93 ? _T_28 : _GEN_365; // @[Conditional.scala 39:67]
  assign _GEN_450 = _T_92 ? _GEN_41 : _GEN_404; // @[Conditional.scala 40:58]
  assign _GEN_451 = _T_92 ? _GEN_42 : _GEN_407; // @[Conditional.scala 40:58]
  assign io_padCtrl_fromTopIO_pSumEnqOrProduct_ready = sPad == 3'h6; // @[ProcessingElement.scala 270:47]
  assign io_padCtrl_fromTopIO_calFinish = _T_87 & mightWeightReadFinish; // @[ProcessingElement.scala 301:34]
  assign io_dataStream_ipsIO_ready = _T_92 ? _T_28 : _GEN_411; // @[ProcessingElement.scala 259:29 ProcessingElement.scala 354:37]
  assign io_dataStream_opsIO_valid = io_padCtrl_fromTopIO_doLoadEn & io_dataStream_opsIO_ready; // @[ProcessingElement.scala 263:31 ProcessingElement.scala 266:31]
  assign io_dataStream_opsIO_bits = 5'h1f == value ? psDataSPad_31 : _GEN_30; // @[ProcessingElement.scala 262:30]
  assign io_dataStream_iactIOs_dataIOs_writeInDataIO_ready = SPadSeq_1_io_commonIO_dataLenFinIO_writeInDataIO_ready; // @[ProcessingElement.scala 226:41]
  assign io_dataStream_iactIOs_dataIOs_writeFin = SPadSeq_1_io_commonIO_dataLenFinIO_writeFin; // @[ProcessingElement.scala 226:41]
  assign io_dataStream_iactIOs_addrIOs_writeInDataIO_ready = SPadSeq_0_io_commonIO_dataLenFinIO_writeInDataIO_ready; // @[ProcessingElement.scala 217:41]
  assign io_dataStream_iactIOs_addrIOs_writeFin = SPadSeq_0_io_commonIO_dataLenFinIO_writeFin; // @[ProcessingElement.scala 217:41]
  assign io_dataStream_weightIOs_dataIOs_writeInDataIO_ready = SPadSeq_3_io_commonIO_dataLenFinIO_writeInDataIO_ready; // @[ProcessingElement.scala 248:43]
  assign io_dataStream_weightIOs_dataIOs_writeFin = SPadSeq_3_io_commonIO_dataLenFinIO_writeFin; // @[ProcessingElement.scala 248:43]
  assign io_dataStream_weightIOs_addrIOs_writeInDataIO_ready = SPadSeq_2_io_commonIO_dataLenFinIO_writeInDataIO_ready; // @[ProcessingElement.scala 237:43]
  assign io_dataStream_weightIOs_addrIOs_writeFin = SPadSeq_2_io_commonIO_dataLenFinIO_writeFin; // @[ProcessingElement.scala 237:43]
  assign io_debugIO_iactMatrixData = {_T_12,_T_9}; // @[ProcessingElement.scala 392:31]
  assign io_debugIO_iactMatrixRow = {_T_15,_T_14}; // @[ProcessingElement.scala 393:30]
  assign io_debugIO_iactMatrixColumn = iactMatrixColumnReg; // @[ProcessingElement.scala 391:33]
  assign io_debugIO_iactAddrInc = _T_50 | _T_54; // @[ProcessingElement.scala 395:28]
  assign io_debugIO_iactDataInc = _T_62 | _T_67; // @[ProcessingElement.scala 396:28]
  assign io_debugIO_iactAddrIdx = SPadSeq_0_io_commonIO_columnNum; // @[ProcessingElement.scala 394:28]
  assign io_debugIO_weightAddrSPadReadOut = SPadSeq_2_io_commonIO_readOutData; // @[ProcessingElement.scala 399:38]
  assign io_debugIO_weightMatrixData = {_T_22,_T_19}; // @[ProcessingElement.scala 397:33]
  assign io_debugIO_weightMatrixRow = {_T_25,_T_24}; // @[ProcessingElement.scala 398:32]
  assign io_debugIO_productResult = productReg; // @[ProcessingElement.scala 400:30]
  assign io_debugIO_pSumResult = padEqWB ? _T_34 : 20'h0; // @[ProcessingElement.scala 401:27]
  assign io_debugIO_pSumLoad = pSumSPadLoadReg; // @[ProcessingElement.scala 402:25]
  assign io_debugIO_weightAddrInIdx = weightDataIdxMuxWire ? _T_82 : iactMatrixRowWire; // @[ProcessingElement.scala 403:32]
  assign io_debugIO_sPadState = sPad; // @[ProcessingElement.scala 404:26]
  assign SPadSeq_0_clock = clock;
  assign SPadSeq_0_reset = reset;
  assign SPadSeq_0_io_commonIO_writeEn = io_padCtrl_fromTopIO_doLoadEn; // @[ProcessingElement.scala 214:37]
  assign SPadSeq_0_io_commonIO_dataLenFinIO_writeInDataIO_valid = io_dataStream_iactIOs_addrIOs_writeInDataIO_valid; // @[ProcessingElement.scala 217:41]
  assign SPadSeq_0_io_commonIO_dataLenFinIO_writeInDataIO_bits_data = io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data; // @[ProcessingElement.scala 217:41]
  assign SPadSeq_0_io_commonIO_dataLenFinIO_streamLen = io_dataStream_iactIOs_addrIOs_streamLen; // @[ProcessingElement.scala 217:41]
  assign SPadSeq_0_io_addrIO_indexInc = _T_50 | _T_54; // @[ProcessingElement.scala 221:35]
  assign SPadSeq_1_clock = clock;
  assign SPadSeq_1_reset = reset;
  assign SPadSeq_1_io_commonIO_writeEn = io_padCtrl_fromTopIO_doLoadEn; // @[ProcessingElement.scala 214:37]
  assign SPadSeq_1_io_commonIO_dataLenFinIO_writeInDataIO_valid = io_dataStream_iactIOs_dataIOs_writeInDataIO_valid; // @[ProcessingElement.scala 226:41]
  assign SPadSeq_1_io_commonIO_dataLenFinIO_writeInDataIO_bits_data = io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data; // @[ProcessingElement.scala 226:41]
  assign SPadSeq_1_io_commonIO_dataLenFinIO_streamLen = io_dataStream_iactIOs_dataIOs_streamLen; // @[ProcessingElement.scala 226:41]
  assign SPadSeq_1_io_dataIO_indexInc = _T_62 | _T_67; // @[ProcessingElement.scala 232:35]
  assign SPadSeq_2_clock = clock;
  assign SPadSeq_2_reset = reset;
  assign SPadSeq_2_io_commonIO_writeEn = io_padCtrl_fromTopIO_doLoadEn; // @[ProcessingElement.scala 214:37]
  assign SPadSeq_2_io_commonIO_dataLenFinIO_writeInDataIO_valid = io_dataStream_weightIOs_addrIOs_writeInDataIO_valid; // @[ProcessingElement.scala 237:43]
  assign SPadSeq_2_io_commonIO_dataLenFinIO_writeInDataIO_bits_data = io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data; // @[ProcessingElement.scala 237:43]
  assign SPadSeq_2_io_commonIO_dataLenFinIO_streamLen = io_dataStream_weightIOs_addrIOs_streamLen; // @[ProcessingElement.scala 237:43]
  assign SPadSeq_2_io_addrIO_readInIdx = weightDataIdxMuxWire ? _T_82 : iactMatrixRowWire; // @[ProcessingElement.scala 241:38]
  assign SPadSeq_2_io_addrIO_indexInc = _T_57 & mightWeightZeroColumnWire; // @[ProcessingElement.scala 244:37]
  assign SPadSeq_2_io_addrIO_readInIdxEn = _T_76 & weightDataSPadFirstRead; // @[ProcessingElement.scala 245:40]
  assign SPadSeq_3_clock = clock;
  assign SPadSeq_3_reset = reset;
  assign SPadSeq_3_io_commonIO_readEn = iactDataSPadReadEnReg; // @[ProcessingElement.scala 253:37]
  assign SPadSeq_3_io_commonIO_writeEn = io_padCtrl_fromTopIO_doLoadEn; // @[ProcessingElement.scala 214:37]
  assign SPadSeq_3_io_commonIO_dataLenFinIO_writeInDataIO_valid = io_dataStream_weightIOs_dataIOs_writeInDataIO_valid; // @[ProcessingElement.scala 248:43]
  assign SPadSeq_3_io_commonIO_dataLenFinIO_writeInDataIO_bits_data = io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data; // @[ProcessingElement.scala 248:43]
  assign SPadSeq_3_io_commonIO_dataLenFinIO_streamLen = io_dataStream_weightIOs_dataIOs_streamLen; // @[ProcessingElement.scala 248:43]
  assign SPadSeq_3_io_dataIO_readInIdx = {{1'd0}, _T_27}; // @[ProcessingElement.scala 254:38]
  assign SPadSeq_3_io_dataIO_indexInc = _T_72 | _T_74; // @[ProcessingElement.scala 255:37]
  assign SPadSeq_3_io_dataIO_readInIdxEn = _T_84 & ~mightWeightZeroColumnWire; // @[ProcessingElement.scala 256:40]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  psDataSPad_0 = _RAND_0[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  psDataSPad_1 = _RAND_1[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  psDataSPad_2 = _RAND_2[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  psDataSPad_3 = _RAND_3[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  psDataSPad_4 = _RAND_4[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  psDataSPad_5 = _RAND_5[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  psDataSPad_6 = _RAND_6[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  psDataSPad_7 = _RAND_7[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  psDataSPad_8 = _RAND_8[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  psDataSPad_9 = _RAND_9[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  psDataSPad_10 = _RAND_10[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  psDataSPad_11 = _RAND_11[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  psDataSPad_12 = _RAND_12[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  psDataSPad_13 = _RAND_13[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {1{`RANDOM}};
  psDataSPad_14 = _RAND_14[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  psDataSPad_15 = _RAND_15[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_16 = {1{`RANDOM}};
  psDataSPad_16 = _RAND_16[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_17 = {1{`RANDOM}};
  psDataSPad_17 = _RAND_17[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_18 = {1{`RANDOM}};
  psDataSPad_18 = _RAND_18[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_19 = {1{`RANDOM}};
  psDataSPad_19 = _RAND_19[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_20 = {1{`RANDOM}};
  psDataSPad_20 = _RAND_20[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_21 = {1{`RANDOM}};
  psDataSPad_21 = _RAND_21[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_22 = {1{`RANDOM}};
  psDataSPad_22 = _RAND_22[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_23 = {1{`RANDOM}};
  psDataSPad_23 = _RAND_23[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_24 = {1{`RANDOM}};
  psDataSPad_24 = _RAND_24[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_25 = {1{`RANDOM}};
  psDataSPad_25 = _RAND_25[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_26 = {1{`RANDOM}};
  psDataSPad_26 = _RAND_26[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_27 = {1{`RANDOM}};
  psDataSPad_27 = _RAND_27[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_28 = {1{`RANDOM}};
  psDataSPad_28 = _RAND_28[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_29 = {1{`RANDOM}};
  psDataSPad_29 = _RAND_29[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_30 = {1{`RANDOM}};
  psDataSPad_30 = _RAND_30[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_31 = {1{`RANDOM}};
  psDataSPad_31 = _RAND_31[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_32 = {1{`RANDOM}};
  iactDataSPadReadEnReg = _RAND_32[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_33 = {1{`RANDOM}};
  iactMatrixColumnReg = _RAND_33[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_34 = {1{`RANDOM}};
  iactZeroColumnNumber = _RAND_34[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_35 = {1{`RANDOM}};
  iactDataSPadFirstReadReg = _RAND_35[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_36 = {1{`RANDOM}};
  weightDataSPadFirstRead = _RAND_36[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_37 = {1{`RANDOM}};
  productReg = _RAND_37[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_38 = {1{`RANDOM}};
  pSumSPadLoadReg = _RAND_38[19:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_39 = {1{`RANDOM}};
  sPad = _RAND_39[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_40 = {1{`RANDOM}};
  value = _RAND_40[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_41 = {1{`RANDOM}};
  iactSPadZeroColumnReg = _RAND_41[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      psDataSPad_0 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h0 == psDataSpadIdxWire) begin
                      if (padEqWB) begin
                        psDataSPad_0 <= _T_34;
                      end else begin
                        psDataSPad_0 <= 20'h0;
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_1 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h1 == psDataSpadIdxWire) begin
                      if (padEqWB) begin
                        psDataSPad_1 <= _T_34;
                      end else begin
                        psDataSPad_1 <= 20'h0;
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_2 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h2 == psDataSpadIdxWire) begin
                      if (padEqWB) begin
                        psDataSPad_2 <= _T_34;
                      end else begin
                        psDataSPad_2 <= 20'h0;
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_3 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h3 == psDataSpadIdxWire) begin
                      if (padEqWB) begin
                        psDataSPad_3 <= _T_34;
                      end else begin
                        psDataSPad_3 <= 20'h0;
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_4 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h4 == psDataSpadIdxWire) begin
                      psDataSPad_4 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_5 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h5 == psDataSpadIdxWire) begin
                      psDataSPad_5 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_6 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h6 == psDataSpadIdxWire) begin
                      psDataSPad_6 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_7 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h7 == psDataSpadIdxWire) begin
                      psDataSPad_7 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_8 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h8 == psDataSpadIdxWire) begin
                      psDataSPad_8 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_9 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h9 == psDataSpadIdxWire) begin
                      psDataSPad_9 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_10 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'ha == psDataSpadIdxWire) begin
                      psDataSPad_10 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_11 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'hb == psDataSpadIdxWire) begin
                      psDataSPad_11 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_12 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'hc == psDataSpadIdxWire) begin
                      psDataSPad_12 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_13 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'hd == psDataSpadIdxWire) begin
                      psDataSPad_13 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_14 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'he == psDataSpadIdxWire) begin
                      psDataSPad_14 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_15 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'hf == psDataSpadIdxWire) begin
                      psDataSPad_15 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_16 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h10 == psDataSpadIdxWire) begin
                      psDataSPad_16 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_17 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h11 == psDataSpadIdxWire) begin
                      psDataSPad_17 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_18 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h12 == psDataSpadIdxWire) begin
                      psDataSPad_18 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_19 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h13 == psDataSpadIdxWire) begin
                      psDataSPad_19 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_20 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h14 == psDataSpadIdxWire) begin
                      psDataSPad_20 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_21 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h15 == psDataSpadIdxWire) begin
                      psDataSPad_21 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_22 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h16 == psDataSpadIdxWire) begin
                      psDataSPad_22 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_23 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h17 == psDataSpadIdxWire) begin
                      psDataSPad_23 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_24 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h18 == psDataSpadIdxWire) begin
                      psDataSPad_24 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_25 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h19 == psDataSpadIdxWire) begin
                      psDataSPad_25 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_26 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h1a == psDataSpadIdxWire) begin
                      psDataSPad_26 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_27 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h1b == psDataSpadIdxWire) begin
                      psDataSPad_27 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_28 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h1c == psDataSpadIdxWire) begin
                      psDataSPad_28 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_29 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h1d == psDataSpadIdxWire) begin
                      psDataSPad_29 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_30 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h1e == psDataSpadIdxWire) begin
                      psDataSPad_30 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      psDataSPad_31 <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (!(_T_106)) begin
                  if (_T_108) begin
                    if (5'h1f == psDataSpadIdxWire) begin
                      psDataSPad_31 <= pSumResultWire;
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      iactDataSPadReadEnReg <= 1'h0;
    end else if (_T_92) begin
      if (io_padCtrl_doMACEn) begin
        iactDataSPadReadEnReg <= 1'h0;
      end
    end else if (_T_93) begin
      if (mightIactZeroColumnWire) begin
        iactDataSPadReadEnReg <= 1'h0;
      end else begin
        iactDataSPadReadEnReg <= 1'h1;
      end
    end else if (_T_96) begin
      iactDataSPadReadEnReg <= 1'h0;
    end else if (_T_97) begin
      iactDataSPadReadEnReg <= _GEN_63;
    end else if (!(_T_104)) begin
      if (_T_105) begin
        iactDataSPadReadEnReg <= 1'h0;
      end else if (!(_T_106)) begin
        if (_T_108) begin
          if (!(_T_109)) begin
            iactDataSPadReadEnReg <= _GEN_114;
          end
        end
      end
    end
    if (reset) begin
      iactMatrixColumnReg <= 4'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (_T_97) begin
            if (mightWeightZeroColumnWire) begin
              if (mightIactIdxIncWire) begin
                if (iactSPadZeroColumnReg) begin
                  iactMatrixColumnReg <= _T_101;
                end else begin
                  iactMatrixColumnReg <= _T_99;
                end
              end
            end
          end else if (!(_T_104)) begin
            if (!(_T_105)) begin
              if (!(_T_106)) begin
                if (_T_108) begin
                  if (_T_109) begin
                    iactMatrixColumnReg <= 4'h0;
                  end else if (mightWeightIdxIncWire) begin
                    if (mightIactIdxIncWire) begin
                      if (iactSPadZeroColumnReg) begin
                        iactMatrixColumnReg <= _T_101;
                      end else begin
                        iactMatrixColumnReg <= _T_99;
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      iactZeroColumnNumber <= 4'h0;
    end else if (!(_T_92)) begin
      if (_T_93) begin
        if (mightIactZeroColumnWire) begin
          iactZeroColumnNumber <= _T_95;
        end
      end else if (!(_T_96)) begin
        if (_T_97) begin
          if (mightWeightZeroColumnWire) begin
            if (mightIactIdxIncWire) begin
              if (iactSPadZeroColumnReg) begin
                iactZeroColumnNumber <= 4'h0;
              end
            end
          end
        end else if (!(_T_104)) begin
          if (!(_T_105)) begin
            if (!(_T_106)) begin
              if (_T_108) begin
                if (!(_T_109)) begin
                  if (mightWeightIdxIncWire) begin
                    if (mightIactIdxIncWire) begin
                      if (iactSPadZeroColumnReg) begin
                        iactZeroColumnNumber <= 4'h0;
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    iactDataSPadFirstReadReg <= reset | _GEN_451;
    weightDataSPadFirstRead <= reset | _GEN_450;
    if (reset) begin
      productReg <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (_T_106) begin
                  if (io_padCtrl_fromTopIO_pSumEnqOrProduct_bits) begin
                    if (io_dataStream_ipsIO_valid) begin
                      productReg <= io_dataStream_ipsIO_bits;
                    end
                  end else begin
                    productReg <= {{4'd0}, _T_107};
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      pSumSPadLoadReg <= 20'h0;
    end else if (!(_T_92)) begin
      if (!(_T_93)) begin
        if (!(_T_96)) begin
          if (!(_T_97)) begin
            if (!(_T_104)) begin
              if (!(_T_105)) begin
                if (_T_106) begin
                  if (io_padCtrl_fromTopIO_pSumEnqOrProduct_bits) begin
                    if (io_dataStream_ipsIO_valid) begin
                      if (5'h1f == value) begin
                        pSumSPadLoadReg <= psDataSPad_31;
                      end else if (5'h1e == value) begin
                        pSumSPadLoadReg <= psDataSPad_30;
                      end else if (5'h1d == value) begin
                        pSumSPadLoadReg <= psDataSPad_29;
                      end else if (5'h1c == value) begin
                        pSumSPadLoadReg <= psDataSPad_28;
                      end else if (5'h1b == value) begin
                        pSumSPadLoadReg <= psDataSPad_27;
                      end else if (5'h1a == value) begin
                        pSumSPadLoadReg <= psDataSPad_26;
                      end else if (5'h19 == value) begin
                        pSumSPadLoadReg <= psDataSPad_25;
                      end else if (5'h18 == value) begin
                        pSumSPadLoadReg <= psDataSPad_24;
                      end else if (5'h17 == value) begin
                        pSumSPadLoadReg <= psDataSPad_23;
                      end else if (5'h16 == value) begin
                        pSumSPadLoadReg <= psDataSPad_22;
                      end else if (5'h15 == value) begin
                        pSumSPadLoadReg <= psDataSPad_21;
                      end else if (5'h14 == value) begin
                        pSumSPadLoadReg <= psDataSPad_20;
                      end else if (5'h13 == value) begin
                        pSumSPadLoadReg <= psDataSPad_19;
                      end else if (5'h12 == value) begin
                        pSumSPadLoadReg <= psDataSPad_18;
                      end else if (5'h11 == value) begin
                        pSumSPadLoadReg <= psDataSPad_17;
                      end else if (5'h10 == value) begin
                        pSumSPadLoadReg <= psDataSPad_16;
                      end else if (5'hf == value) begin
                        pSumSPadLoadReg <= psDataSPad_15;
                      end else if (5'he == value) begin
                        pSumSPadLoadReg <= psDataSPad_14;
                      end else if (5'hd == value) begin
                        pSumSPadLoadReg <= psDataSPad_13;
                      end else if (5'hc == value) begin
                        pSumSPadLoadReg <= psDataSPad_12;
                      end else if (5'hb == value) begin
                        pSumSPadLoadReg <= psDataSPad_11;
                      end else if (5'ha == value) begin
                        pSumSPadLoadReg <= psDataSPad_10;
                      end else if (5'h9 == value) begin
                        pSumSPadLoadReg <= psDataSPad_9;
                      end else if (5'h8 == value) begin
                        pSumSPadLoadReg <= psDataSPad_8;
                      end else if (5'h7 == value) begin
                        pSumSPadLoadReg <= psDataSPad_7;
                      end else if (5'h6 == value) begin
                        pSumSPadLoadReg <= psDataSPad_6;
                      end else if (5'h5 == value) begin
                        pSumSPadLoadReg <= psDataSPad_5;
                      end else if (5'h4 == value) begin
                        pSumSPadLoadReg <= psDataSPad_4;
                      end else if (5'h3 == value) begin
                        pSumSPadLoadReg <= psDataSPad_3;
                      end else if (5'h2 == value) begin
                        pSumSPadLoadReg <= psDataSPad_2;
                      end else if (5'h1 == value) begin
                        pSumSPadLoadReg <= psDataSPad_1;
                      end else begin
                        pSumSPadLoadReg <= psDataSPad_0;
                      end
                    end
                  end else if (5'h1f == value) begin
                    pSumSPadLoadReg <= psDataSPad_31;
                  end else if (5'h1e == value) begin
                    pSumSPadLoadReg <= psDataSPad_30;
                  end else if (5'h1d == value) begin
                    pSumSPadLoadReg <= psDataSPad_29;
                  end else if (5'h1c == value) begin
                    pSumSPadLoadReg <= psDataSPad_28;
                  end else if (5'h1b == value) begin
                    pSumSPadLoadReg <= psDataSPad_27;
                  end else if (5'h1a == value) begin
                    pSumSPadLoadReg <= psDataSPad_26;
                  end else if (5'h19 == value) begin
                    pSumSPadLoadReg <= psDataSPad_25;
                  end else if (5'h18 == value) begin
                    pSumSPadLoadReg <= psDataSPad_24;
                  end else if (5'h17 == value) begin
                    pSumSPadLoadReg <= psDataSPad_23;
                  end else if (5'h16 == value) begin
                    pSumSPadLoadReg <= psDataSPad_22;
                  end else if (5'h15 == value) begin
                    pSumSPadLoadReg <= psDataSPad_21;
                  end else if (5'h14 == value) begin
                    pSumSPadLoadReg <= psDataSPad_20;
                  end else if (5'h13 == value) begin
                    pSumSPadLoadReg <= psDataSPad_19;
                  end else if (5'h12 == value) begin
                    pSumSPadLoadReg <= psDataSPad_18;
                  end else if (5'h11 == value) begin
                    pSumSPadLoadReg <= psDataSPad_17;
                  end else if (5'h10 == value) begin
                    pSumSPadLoadReg <= psDataSPad_16;
                  end else if (5'hf == value) begin
                    pSumSPadLoadReg <= psDataSPad_15;
                  end else if (5'he == value) begin
                    pSumSPadLoadReg <= psDataSPad_14;
                  end else if (5'hd == value) begin
                    pSumSPadLoadReg <= psDataSPad_13;
                  end else if (5'hc == value) begin
                    pSumSPadLoadReg <= psDataSPad_12;
                  end else if (5'hb == value) begin
                    pSumSPadLoadReg <= psDataSPad_11;
                  end else if (5'ha == value) begin
                    pSumSPadLoadReg <= psDataSPad_10;
                  end else if (5'h9 == value) begin
                    pSumSPadLoadReg <= psDataSPad_9;
                  end else if (5'h8 == value) begin
                    pSumSPadLoadReg <= psDataSPad_8;
                  end else if (5'h7 == value) begin
                    pSumSPadLoadReg <= psDataSPad_7;
                  end else if (5'h6 == value) begin
                    pSumSPadLoadReg <= psDataSPad_6;
                  end else if (5'h5 == value) begin
                    pSumSPadLoadReg <= psDataSPad_5;
                  end else if (5'h4 == value) begin
                    pSumSPadLoadReg <= psDataSPad_4;
                  end else if (5'h3 == value) begin
                    pSumSPadLoadReg <= psDataSPad_3;
                  end else if (5'h2 == value) begin
                    pSumSPadLoadReg <= psDataSPad_2;
                  end else if (5'h1 == value) begin
                    pSumSPadLoadReg <= psDataSPad_1;
                  end else begin
                    pSumSPadLoadReg <= psDataSPad_0;
                  end
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      sPad <= 3'h0;
    end else if (_T_92) begin
      if (io_padCtrl_doMACEn) begin
        sPad <= 3'h1;
      end
    end else if (_T_93) begin
      if (mightIactZeroColumnWire) begin
        sPad <= 3'h1;
      end else begin
        sPad <= 3'h2;
      end
    end else if (_T_96) begin
      sPad <= 3'h3;
    end else if (_T_97) begin
      if (mightWeightZeroColumnWire) begin
        if (mightIactIdxIncWire) begin
          sPad <= 3'h1;
        end else begin
          sPad <= 3'h2;
        end
      end else begin
        sPad <= 3'h4;
      end
    end else if (_T_104) begin
      sPad <= 3'h5;
    end else if (_T_105) begin
      sPad <= 3'h6;
    end else if (_T_106) begin
      if (io_padCtrl_fromTopIO_pSumEnqOrProduct_bits) begin
        if (io_dataStream_ipsIO_valid) begin
          sPad <= 3'h7;
        end
      end else begin
        sPad <= 3'h7;
      end
    end else if (_T_108) begin
      if (_T_109) begin
        sPad <= 3'h0;
      end else if (mightWeightIdxIncWire) begin
        if (mightIactIdxIncWire) begin
          sPad <= 3'h1;
        end else begin
          sPad <= 3'h2;
        end
      end else begin
        sPad <= 3'h4;
      end
    end
    if (reset) begin
      value <= 5'h0;
    end else if (_T_92) begin
      if (_T_29) begin
        if (_T_30) begin
          value <= 5'h0;
        end else begin
          value <= _T_32;
        end
      end
    end else if (_T_93) begin
      if (_T_29) begin
        if (_T_30) begin
          value <= 5'h0;
        end else begin
          value <= _T_32;
        end
      end
    end else if (_T_96) begin
      if (_T_29) begin
        if (_T_30) begin
          value <= 5'h0;
        end else begin
          value <= _T_32;
        end
      end
    end else if (_T_97) begin
      if (_T_29) begin
        if (_T_30) begin
          value <= 5'h0;
        end else begin
          value <= _T_32;
        end
      end
    end else if (_T_104) begin
      value <= _GEN_35;
    end else if (_T_105) begin
      value <= psDataSpadIdxWire;
    end else if (_T_106) begin
      value <= _GEN_35;
    end else if (_T_108) begin
      if (_T_109) begin
        value <= 5'h0;
      end else begin
        value <= _GEN_35;
      end
    end else begin
      value <= _GEN_35;
    end
    if (reset) begin
      iactSPadZeroColumnReg <= 1'h0;
    end else if (!(_T_92)) begin
      if (_T_93) begin
        iactSPadZeroColumnReg <= _GEN_48;
      end else if (!(_T_96)) begin
        if (_T_97) begin
          if (mightWeightZeroColumnWire) begin
            if (mightIactIdxIncWire) begin
              if (iactSPadZeroColumnReg) begin
                iactSPadZeroColumnReg <= 1'h0;
              end
            end
          end
        end else if (!(_T_104)) begin
          if (!(_T_105)) begin
            if (!(_T_106)) begin
              if (_T_108) begin
                if (!(_T_109)) begin
                  if (mightWeightIdxIncWire) begin
                    if (mightIactIdxIncWire) begin
                      if (iactSPadZeroColumnReg) begin
                        iactSPadZeroColumnReg <= 1'h0;
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
  end
endmodule
module Queue(
  input        clock,
  input        reset,
  output       io_enq_ready,
  input        io_enq_valid,
  input  [3:0] io_enq_bits_data,
  input        io_deq_ready,
  output       io_deq_valid,
  output [3:0] io_deq_bits_data
);
  reg [3:0] _T_data [0:3]; // @[Decoupled.scala 209:24]
  reg [31:0] _RAND_0;
  wire [3:0] _T_data__T_22_data; // @[Decoupled.scala 209:24]
  wire [1:0] _T_data__T_22_addr; // @[Decoupled.scala 209:24]
  wire [3:0] _T_data__T_12_data; // @[Decoupled.scala 209:24]
  wire [1:0] _T_data__T_12_addr; // @[Decoupled.scala 209:24]
  wire  _T_data__T_12_mask; // @[Decoupled.scala 209:24]
  wire  _T_data__T_12_en; // @[Decoupled.scala 209:24]
  reg [1:0] _T_1; // @[Counter.scala 29:33]
  reg [31:0] _RAND_1;
  reg [1:0] _T_2; // @[Counter.scala 29:33]
  reg [31:0] _RAND_2;
  reg  _T_3; // @[Decoupled.scala 212:35]
  reg [31:0] _RAND_3;
  wire  _T_4; // @[Decoupled.scala 214:41]
  wire  _T_6; // @[Decoupled.scala 215:33]
  wire  _T_7; // @[Decoupled.scala 216:32]
  wire  _T_8; // @[Decoupled.scala 40:37]
  wire  _T_10; // @[Decoupled.scala 40:37]
  wire [1:0] _T_15; // @[Counter.scala 38:22]
  wire  _GEN_9; // @[Decoupled.scala 240:27]
  wire  _GEN_12; // @[Decoupled.scala 237:18]
  wire [1:0] _T_18; // @[Counter.scala 38:22]
  wire  _GEN_11; // @[Decoupled.scala 237:18]
  wire  _T_19; // @[Decoupled.scala 227:16]
  assign _T_data__T_22_addr = _T_2;
  assign _T_data__T_22_data = _T_data[_T_data__T_22_addr]; // @[Decoupled.scala 209:24]
  assign _T_data__T_12_data = io_enq_bits_data;
  assign _T_data__T_12_addr = _T_1;
  assign _T_data__T_12_mask = 1'h1;
  assign _T_data__T_12_en = _T_6 ? _GEN_9 : _T_8;
  assign _T_4 = _T_1 == _T_2; // @[Decoupled.scala 214:41]
  assign _T_6 = _T_4 & ~_T_3; // @[Decoupled.scala 215:33]
  assign _T_7 = _T_4 & _T_3; // @[Decoupled.scala 216:32]
  assign _T_8 = io_enq_ready & io_enq_valid; // @[Decoupled.scala 40:37]
  assign _T_10 = io_deq_ready & io_deq_valid; // @[Decoupled.scala 40:37]
  assign _T_15 = _T_1 + 2'h1; // @[Counter.scala 38:22]
  assign _GEN_9 = io_deq_ready ? 1'h0 : _T_8; // @[Decoupled.scala 240:27]
  assign _GEN_12 = _T_6 ? _GEN_9 : _T_8; // @[Decoupled.scala 237:18]
  assign _T_18 = _T_2 + 2'h1; // @[Counter.scala 38:22]
  assign _GEN_11 = _T_6 ? 1'h0 : _T_10; // @[Decoupled.scala 237:18]
  assign _T_19 = _GEN_12 != _GEN_11; // @[Decoupled.scala 227:16]
  assign io_enq_ready = ~_T_7; // @[Decoupled.scala 232:16]
  assign io_deq_valid = io_enq_valid | ~_T_6; // @[Decoupled.scala 231:16 Decoupled.scala 236:40]
  assign io_deq_bits_data = _T_6 ? io_enq_bits_data : _T_data__T_22_data; // @[Decoupled.scala 233:15 Decoupled.scala 238:19]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  _RAND_0 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 4; initvar = initvar+1)
    _T_data[initvar] = _RAND_0[3:0];
  `endif // RANDOMIZE_MEM_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_1 = _RAND_1[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_2 = _RAND_2[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_3 = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if(_T_data__T_12_en & _T_data__T_12_mask) begin
      _T_data[_T_data__T_12_addr] <= _T_data__T_12_data; // @[Decoupled.scala 209:24]
    end
    if (reset) begin
      _T_1 <= 2'h0;
    end else if (_GEN_12) begin
      _T_1 <= _T_15;
    end
    if (reset) begin
      _T_2 <= 2'h0;
    end else if (_GEN_11) begin
      _T_2 <= _T_18;
    end
    if (reset) begin
      _T_3 <= 1'h0;
    end else if (_T_19) begin
      if (_T_6) begin
        if (io_deq_ready) begin
          _T_3 <= 1'h0;
        end else begin
          _T_3 <= _T_8;
        end
      end else begin
        _T_3 <= _T_8;
      end
    end
  end
endmodule
module Queue_1(
  input         clock,
  input         reset,
  output        io_enq_ready,
  input         io_enq_valid,
  input  [11:0] io_enq_bits_data,
  input         io_deq_ready,
  output        io_deq_valid,
  output [11:0] io_deq_bits_data
);
  reg [11:0] _T_data [0:3]; // @[Decoupled.scala 209:24]
  reg [31:0] _RAND_0;
  wire [11:0] _T_data__T_22_data; // @[Decoupled.scala 209:24]
  wire [1:0] _T_data__T_22_addr; // @[Decoupled.scala 209:24]
  wire [11:0] _T_data__T_12_data; // @[Decoupled.scala 209:24]
  wire [1:0] _T_data__T_12_addr; // @[Decoupled.scala 209:24]
  wire  _T_data__T_12_mask; // @[Decoupled.scala 209:24]
  wire  _T_data__T_12_en; // @[Decoupled.scala 209:24]
  reg [1:0] _T_1; // @[Counter.scala 29:33]
  reg [31:0] _RAND_1;
  reg [1:0] _T_2; // @[Counter.scala 29:33]
  reg [31:0] _RAND_2;
  reg  _T_3; // @[Decoupled.scala 212:35]
  reg [31:0] _RAND_3;
  wire  _T_4; // @[Decoupled.scala 214:41]
  wire  _T_6; // @[Decoupled.scala 215:33]
  wire  _T_7; // @[Decoupled.scala 216:32]
  wire  _T_8; // @[Decoupled.scala 40:37]
  wire  _T_10; // @[Decoupled.scala 40:37]
  wire [1:0] _T_15; // @[Counter.scala 38:22]
  wire  _GEN_9; // @[Decoupled.scala 240:27]
  wire  _GEN_12; // @[Decoupled.scala 237:18]
  wire [1:0] _T_18; // @[Counter.scala 38:22]
  wire  _GEN_11; // @[Decoupled.scala 237:18]
  wire  _T_19; // @[Decoupled.scala 227:16]
  assign _T_data__T_22_addr = _T_2;
  assign _T_data__T_22_data = _T_data[_T_data__T_22_addr]; // @[Decoupled.scala 209:24]
  assign _T_data__T_12_data = io_enq_bits_data;
  assign _T_data__T_12_addr = _T_1;
  assign _T_data__T_12_mask = 1'h1;
  assign _T_data__T_12_en = _T_6 ? _GEN_9 : _T_8;
  assign _T_4 = _T_1 == _T_2; // @[Decoupled.scala 214:41]
  assign _T_6 = _T_4 & ~_T_3; // @[Decoupled.scala 215:33]
  assign _T_7 = _T_4 & _T_3; // @[Decoupled.scala 216:32]
  assign _T_8 = io_enq_ready & io_enq_valid; // @[Decoupled.scala 40:37]
  assign _T_10 = io_deq_ready & io_deq_valid; // @[Decoupled.scala 40:37]
  assign _T_15 = _T_1 + 2'h1; // @[Counter.scala 38:22]
  assign _GEN_9 = io_deq_ready ? 1'h0 : _T_8; // @[Decoupled.scala 240:27]
  assign _GEN_12 = _T_6 ? _GEN_9 : _T_8; // @[Decoupled.scala 237:18]
  assign _T_18 = _T_2 + 2'h1; // @[Counter.scala 38:22]
  assign _GEN_11 = _T_6 ? 1'h0 : _T_10; // @[Decoupled.scala 237:18]
  assign _T_19 = _GEN_12 != _GEN_11; // @[Decoupled.scala 227:16]
  assign io_enq_ready = ~_T_7; // @[Decoupled.scala 232:16]
  assign io_deq_valid = io_enq_valid | ~_T_6; // @[Decoupled.scala 231:16 Decoupled.scala 236:40]
  assign io_deq_bits_data = _T_6 ? io_enq_bits_data : _T_data__T_22_data; // @[Decoupled.scala 233:15 Decoupled.scala 238:19]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  _RAND_0 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 4; initvar = initvar+1)
    _T_data[initvar] = _RAND_0[11:0];
  `endif // RANDOMIZE_MEM_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_1 = _RAND_1[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_2 = _RAND_2[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_3 = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if(_T_data__T_12_en & _T_data__T_12_mask) begin
      _T_data[_T_data__T_12_addr] <= _T_data__T_12_data; // @[Decoupled.scala 209:24]
    end
    if (reset) begin
      _T_1 <= 2'h0;
    end else if (_GEN_12) begin
      _T_1 <= _T_15;
    end
    if (reset) begin
      _T_2 <= 2'h0;
    end else if (_GEN_11) begin
      _T_2 <= _T_18;
    end
    if (reset) begin
      _T_3 <= 1'h0;
    end else if (_T_19) begin
      if (_T_6) begin
        if (io_deq_ready) begin
          _T_3 <= 1'h0;
        end else begin
          _T_3 <= _T_8;
        end
      end else begin
        _T_3 <= _T_8;
      end
    end
  end
endmodule
module Queue_2(
  input        clock,
  input        reset,
  output       io_enq_ready,
  input        io_enq_valid,
  input  [6:0] io_enq_bits_data,
  input        io_deq_ready,
  output       io_deq_valid,
  output [6:0] io_deq_bits_data
);
  reg [6:0] _T_data [0:3]; // @[Decoupled.scala 209:24]
  reg [31:0] _RAND_0;
  wire [6:0] _T_data__T_22_data; // @[Decoupled.scala 209:24]
  wire [1:0] _T_data__T_22_addr; // @[Decoupled.scala 209:24]
  wire [6:0] _T_data__T_12_data; // @[Decoupled.scala 209:24]
  wire [1:0] _T_data__T_12_addr; // @[Decoupled.scala 209:24]
  wire  _T_data__T_12_mask; // @[Decoupled.scala 209:24]
  wire  _T_data__T_12_en; // @[Decoupled.scala 209:24]
  reg [1:0] _T_1; // @[Counter.scala 29:33]
  reg [31:0] _RAND_1;
  reg [1:0] _T_2; // @[Counter.scala 29:33]
  reg [31:0] _RAND_2;
  reg  _T_3; // @[Decoupled.scala 212:35]
  reg [31:0] _RAND_3;
  wire  _T_4; // @[Decoupled.scala 214:41]
  wire  _T_6; // @[Decoupled.scala 215:33]
  wire  _T_7; // @[Decoupled.scala 216:32]
  wire  _T_8; // @[Decoupled.scala 40:37]
  wire  _T_10; // @[Decoupled.scala 40:37]
  wire [1:0] _T_15; // @[Counter.scala 38:22]
  wire  _GEN_9; // @[Decoupled.scala 240:27]
  wire  _GEN_12; // @[Decoupled.scala 237:18]
  wire [1:0] _T_18; // @[Counter.scala 38:22]
  wire  _GEN_11; // @[Decoupled.scala 237:18]
  wire  _T_19; // @[Decoupled.scala 227:16]
  assign _T_data__T_22_addr = _T_2;
  assign _T_data__T_22_data = _T_data[_T_data__T_22_addr]; // @[Decoupled.scala 209:24]
  assign _T_data__T_12_data = io_enq_bits_data;
  assign _T_data__T_12_addr = _T_1;
  assign _T_data__T_12_mask = 1'h1;
  assign _T_data__T_12_en = _T_6 ? _GEN_9 : _T_8;
  assign _T_4 = _T_1 == _T_2; // @[Decoupled.scala 214:41]
  assign _T_6 = _T_4 & ~_T_3; // @[Decoupled.scala 215:33]
  assign _T_7 = _T_4 & _T_3; // @[Decoupled.scala 216:32]
  assign _T_8 = io_enq_ready & io_enq_valid; // @[Decoupled.scala 40:37]
  assign _T_10 = io_deq_ready & io_deq_valid; // @[Decoupled.scala 40:37]
  assign _T_15 = _T_1 + 2'h1; // @[Counter.scala 38:22]
  assign _GEN_9 = io_deq_ready ? 1'h0 : _T_8; // @[Decoupled.scala 240:27]
  assign _GEN_12 = _T_6 ? _GEN_9 : _T_8; // @[Decoupled.scala 237:18]
  assign _T_18 = _T_2 + 2'h1; // @[Counter.scala 38:22]
  assign _GEN_11 = _T_6 ? 1'h0 : _T_10; // @[Decoupled.scala 237:18]
  assign _T_19 = _GEN_12 != _GEN_11; // @[Decoupled.scala 227:16]
  assign io_enq_ready = ~_T_7; // @[Decoupled.scala 232:16]
  assign io_deq_valid = io_enq_valid | ~_T_6; // @[Decoupled.scala 231:16 Decoupled.scala 236:40]
  assign io_deq_bits_data = _T_6 ? io_enq_bits_data : _T_data__T_22_data; // @[Decoupled.scala 233:15 Decoupled.scala 238:19]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  _RAND_0 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 4; initvar = initvar+1)
    _T_data[initvar] = _RAND_0[6:0];
  `endif // RANDOMIZE_MEM_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_1 = _RAND_1[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_2 = _RAND_2[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_3 = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if(_T_data__T_12_en & _T_data__T_12_mask) begin
      _T_data[_T_data__T_12_addr] <= _T_data__T_12_data; // @[Decoupled.scala 209:24]
    end
    if (reset) begin
      _T_1 <= 2'h0;
    end else if (_GEN_12) begin
      _T_1 <= _T_15;
    end
    if (reset) begin
      _T_2 <= 2'h0;
    end else if (_GEN_11) begin
      _T_2 <= _T_18;
    end
    if (reset) begin
      _T_3 <= 1'h0;
    end else if (_T_19) begin
      if (_T_6) begin
        if (io_deq_ready) begin
          _T_3 <= 1'h0;
        end else begin
          _T_3 <= _T_8;
        end
      end else begin
        _T_3 <= _T_8;
      end
    end
  end
endmodule
module Queue_4(
  input         clock,
  input         reset,
  output        io_enq_ready,
  input         io_enq_valid,
  input  [19:0] io_enq_bits,
  input         io_deq_ready,
  output        io_deq_valid,
  output [19:0] io_deq_bits
);
  reg [19:0] _T [0:3]; // @[Decoupled.scala 209:24]
  reg [31:0] _RAND_0;
  wire [19:0] _T__T_22_data; // @[Decoupled.scala 209:24]
  wire [1:0] _T__T_22_addr; // @[Decoupled.scala 209:24]
  wire [19:0] _T__T_12_data; // @[Decoupled.scala 209:24]
  wire [1:0] _T__T_12_addr; // @[Decoupled.scala 209:24]
  wire  _T__T_12_mask; // @[Decoupled.scala 209:24]
  wire  _T__T_12_en; // @[Decoupled.scala 209:24]
  reg [1:0] _T_1; // @[Counter.scala 29:33]
  reg [31:0] _RAND_1;
  reg [1:0] _T_2; // @[Counter.scala 29:33]
  reg [31:0] _RAND_2;
  reg  _T_3; // @[Decoupled.scala 212:35]
  reg [31:0] _RAND_3;
  wire  _T_4; // @[Decoupled.scala 214:41]
  wire  _T_6; // @[Decoupled.scala 215:33]
  wire  _T_7; // @[Decoupled.scala 216:32]
  wire  _T_8; // @[Decoupled.scala 40:37]
  wire  _T_10; // @[Decoupled.scala 40:37]
  wire [1:0] _T_15; // @[Counter.scala 38:22]
  wire  _GEN_9; // @[Decoupled.scala 240:27]
  wire  _GEN_12; // @[Decoupled.scala 237:18]
  wire [1:0] _T_18; // @[Counter.scala 38:22]
  wire  _GEN_11; // @[Decoupled.scala 237:18]
  wire  _T_19; // @[Decoupled.scala 227:16]
  assign _T__T_22_addr = _T_2;
  assign _T__T_22_data = _T[_T__T_22_addr]; // @[Decoupled.scala 209:24]
  assign _T__T_12_data = io_enq_bits;
  assign _T__T_12_addr = _T_1;
  assign _T__T_12_mask = 1'h1;
  assign _T__T_12_en = _T_6 ? _GEN_9 : _T_8;
  assign _T_4 = _T_1 == _T_2; // @[Decoupled.scala 214:41]
  assign _T_6 = _T_4 & ~_T_3; // @[Decoupled.scala 215:33]
  assign _T_7 = _T_4 & _T_3; // @[Decoupled.scala 216:32]
  assign _T_8 = io_enq_ready & io_enq_valid; // @[Decoupled.scala 40:37]
  assign _T_10 = io_deq_ready & io_deq_valid; // @[Decoupled.scala 40:37]
  assign _T_15 = _T_1 + 2'h1; // @[Counter.scala 38:22]
  assign _GEN_9 = io_deq_ready ? 1'h0 : _T_8; // @[Decoupled.scala 240:27]
  assign _GEN_12 = _T_6 ? _GEN_9 : _T_8; // @[Decoupled.scala 237:18]
  assign _T_18 = _T_2 + 2'h1; // @[Counter.scala 38:22]
  assign _GEN_11 = _T_6 ? 1'h0 : _T_10; // @[Decoupled.scala 237:18]
  assign _T_19 = _GEN_12 != _GEN_11; // @[Decoupled.scala 227:16]
  assign io_enq_ready = ~_T_7; // @[Decoupled.scala 232:16]
  assign io_deq_valid = io_enq_valid | ~_T_6; // @[Decoupled.scala 231:16 Decoupled.scala 236:40]
  assign io_deq_bits = _T_6 ? io_enq_bits : _T__T_22_data; // @[Decoupled.scala 233:15 Decoupled.scala 238:19]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  _RAND_0 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 4; initvar = initvar+1)
    _T[initvar] = _RAND_0[19:0];
  `endif // RANDOMIZE_MEM_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_1 = _RAND_1[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_2 = _RAND_2[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_3 = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if(_T__T_12_en & _T__T_12_mask) begin
      _T[_T__T_12_addr] <= _T__T_12_data; // @[Decoupled.scala 209:24]
    end
    if (reset) begin
      _T_1 <= 2'h0;
    end else if (_GEN_12) begin
      _T_1 <= _T_15;
    end
    if (reset) begin
      _T_2 <= 2'h0;
    end else if (_GEN_11) begin
      _T_2 <= _T_18;
    end
    if (reset) begin
      _T_3 <= 1'h0;
    end else if (_T_19) begin
      if (_T_6) begin
        if (io_deq_ready) begin
          _T_3 <= 1'h0;
        end else begin
          _T_3 <= _T_8;
        end
      end else begin
        _T_3 <= _T_8;
      end
    end
  end
endmodule
module ProcessingElement(
  input         clock,
  input         reset,
  output        io_dataStream_ipsIO_ready,
  input         io_dataStream_ipsIO_valid,
  input  [19:0] io_dataStream_ipsIO_bits,
  input         io_dataStream_opsIO_ready,
  output        io_dataStream_opsIO_valid,
  output [19:0] io_dataStream_opsIO_bits,
  output        io_dataStream_iactIOs_dataIOs_writeInDataIO_ready,
  input         io_dataStream_iactIOs_dataIOs_writeInDataIO_valid,
  input  [11:0] io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data,
  input  [3:0]  io_dataStream_iactIOs_dataIOs_streamLen,
  output        io_dataStream_iactIOs_dataIOs_writeFin,
  output        io_dataStream_iactIOs_addrIOs_writeInDataIO_ready,
  input         io_dataStream_iactIOs_addrIOs_writeInDataIO_valid,
  input  [3:0]  io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data,
  input  [3:0]  io_dataStream_iactIOs_addrIOs_streamLen,
  output        io_dataStream_iactIOs_addrIOs_writeFin,
  output        io_dataStream_weightIOs_dataIOs_writeInDataIO_ready,
  input         io_dataStream_weightIOs_dataIOs_writeInDataIO_valid,
  input  [11:0] io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data,
  input  [7:0]  io_dataStream_weightIOs_dataIOs_streamLen,
  output        io_dataStream_weightIOs_dataIOs_writeFin,
  output        io_dataStream_weightIOs_addrIOs_writeInDataIO_ready,
  input         io_dataStream_weightIOs_addrIOs_writeInDataIO_valid,
  input  [6:0]  io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data,
  input  [3:0]  io_dataStream_weightIOs_addrIOs_streamLen,
  output        io_dataStream_weightIOs_addrIOs_writeFin,
  output        io_topCtrl_pSumEnqOrProduct_ready,
  input         io_topCtrl_pSumEnqOrProduct_valid,
  input         io_topCtrl_pSumEnqOrProduct_bits,
  input         io_topCtrl_doLoadEn,
  output        io_topCtrl_writeFinish,
  output        io_topCtrl_calFinish,
  output [1:0]  io_debugIO_peControlDebugIO_peState,
  output        io_debugIO_peControlDebugIO_doMACEnDebug,
  output [7:0]  io_debugIO_peSPadDebugIO_iactMatrixData,
  output [3:0]  io_debugIO_peSPadDebugIO_iactMatrixRow,
  output [3:0]  io_debugIO_peSPadDebugIO_iactMatrixColumn,
  output        io_debugIO_peSPadDebugIO_iactAddrInc,
  output        io_debugIO_peSPadDebugIO_iactDataInc,
  output [3:0]  io_debugIO_peSPadDebugIO_iactAddrIdx,
  output [6:0]  io_debugIO_peSPadDebugIO_weightAddrSPadReadOut,
  output [7:0]  io_debugIO_peSPadDebugIO_weightMatrixData,
  output [3:0]  io_debugIO_peSPadDebugIO_weightMatrixRow,
  output [19:0] io_debugIO_peSPadDebugIO_productResult,
  output [19:0] io_debugIO_peSPadDebugIO_pSumResult,
  output [19:0] io_debugIO_peSPadDebugIO_pSumLoad,
  output [3:0]  io_debugIO_peSPadDebugIO_weightAddrInIdx,
  output [2:0]  io_debugIO_peSPadDebugIO_sPadState
);
  wire  peCtrl_clock; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_reset; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_io_ctrlPad_doMACEn; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_io_ctrlPad_fromTopIO_pSumEnqOrProduct_ready; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_io_ctrlPad_fromTopIO_pSumEnqOrProduct_bits; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_io_ctrlPad_fromTopIO_doLoadEn; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_io_ctrlPad_fromTopIO_calFinish; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_io_ctrlTop_pSumEnqOrProduct_ready; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_io_ctrlTop_pSumEnqOrProduct_bits; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_io_ctrlTop_doLoadEn; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_io_ctrlTop_calFinish; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_io_ctrlTop_writeFinish; // @[ProcessingElement.scala 12:48]
  wire [1:0] peCtrl_io_debugIO_peState; // @[ProcessingElement.scala 12:48]
  wire  peCtrl_io_debugIO_doMACEnDebug; // @[ProcessingElement.scala 12:48]
  wire  pePad_clock; // @[ProcessingElement.scala 13:43]
  wire  pePad_reset; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_padCtrl_doMACEn; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_padCtrl_fromTopIO_pSumEnqOrProduct_ready; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_padCtrl_fromTopIO_pSumEnqOrProduct_bits; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_padCtrl_fromTopIO_doLoadEn; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_padCtrl_fromTopIO_calFinish; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_ipsIO_ready; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_ipsIO_valid; // @[ProcessingElement.scala 13:43]
  wire [19:0] pePad_io_dataStream_ipsIO_bits; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_opsIO_ready; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_opsIO_valid; // @[ProcessingElement.scala 13:43]
  wire [19:0] pePad_io_dataStream_opsIO_bits; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_iactIOs_dataIOs_writeInDataIO_ready; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_iactIOs_dataIOs_writeInDataIO_valid; // @[ProcessingElement.scala 13:43]
  wire [11:0] pePad_io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data; // @[ProcessingElement.scala 13:43]
  wire [3:0] pePad_io_dataStream_iactIOs_dataIOs_streamLen; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_iactIOs_dataIOs_writeFin; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_iactIOs_addrIOs_writeInDataIO_ready; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_iactIOs_addrIOs_writeInDataIO_valid; // @[ProcessingElement.scala 13:43]
  wire [3:0] pePad_io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data; // @[ProcessingElement.scala 13:43]
  wire [3:0] pePad_io_dataStream_iactIOs_addrIOs_streamLen; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_iactIOs_addrIOs_writeFin; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_weightIOs_dataIOs_writeInDataIO_ready; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_weightIOs_dataIOs_writeInDataIO_valid; // @[ProcessingElement.scala 13:43]
  wire [11:0] pePad_io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data; // @[ProcessingElement.scala 13:43]
  wire [7:0] pePad_io_dataStream_weightIOs_dataIOs_streamLen; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_weightIOs_dataIOs_writeFin; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_weightIOs_addrIOs_writeInDataIO_ready; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_weightIOs_addrIOs_writeInDataIO_valid; // @[ProcessingElement.scala 13:43]
  wire [6:0] pePad_io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data; // @[ProcessingElement.scala 13:43]
  wire [3:0] pePad_io_dataStream_weightIOs_addrIOs_streamLen; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_dataStream_weightIOs_addrIOs_writeFin; // @[ProcessingElement.scala 13:43]
  wire [7:0] pePad_io_debugIO_iactMatrixData; // @[ProcessingElement.scala 13:43]
  wire [3:0] pePad_io_debugIO_iactMatrixRow; // @[ProcessingElement.scala 13:43]
  wire [3:0] pePad_io_debugIO_iactMatrixColumn; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_debugIO_iactAddrInc; // @[ProcessingElement.scala 13:43]
  wire  pePad_io_debugIO_iactDataInc; // @[ProcessingElement.scala 13:43]
  wire [3:0] pePad_io_debugIO_iactAddrIdx; // @[ProcessingElement.scala 13:43]
  wire [6:0] pePad_io_debugIO_weightAddrSPadReadOut; // @[ProcessingElement.scala 13:43]
  wire [7:0] pePad_io_debugIO_weightMatrixData; // @[ProcessingElement.scala 13:43]
  wire [3:0] pePad_io_debugIO_weightMatrixRow; // @[ProcessingElement.scala 13:43]
  wire [19:0] pePad_io_debugIO_productResult; // @[ProcessingElement.scala 13:43]
  wire [19:0] pePad_io_debugIO_pSumResult; // @[ProcessingElement.scala 13:43]
  wire [19:0] pePad_io_debugIO_pSumLoad; // @[ProcessingElement.scala 13:43]
  wire [3:0] pePad_io_debugIO_weightAddrInIdx; // @[ProcessingElement.scala 13:43]
  wire [2:0] pePad_io_debugIO_sPadState; // @[ProcessingElement.scala 13:43]
  wire  Queue_clock; // @[Decoupled.scala 287:21]
  wire  Queue_reset; // @[Decoupled.scala 287:21]
  wire  Queue_io_enq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_io_enq_valid; // @[Decoupled.scala 287:21]
  wire [3:0] Queue_io_enq_bits_data; // @[Decoupled.scala 287:21]
  wire  Queue_io_deq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_io_deq_valid; // @[Decoupled.scala 287:21]
  wire [3:0] Queue_io_deq_bits_data; // @[Decoupled.scala 287:21]
  wire  Queue_1_clock; // @[Decoupled.scala 287:21]
  wire  Queue_1_reset; // @[Decoupled.scala 287:21]
  wire  Queue_1_io_enq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_1_io_enq_valid; // @[Decoupled.scala 287:21]
  wire [11:0] Queue_1_io_enq_bits_data; // @[Decoupled.scala 287:21]
  wire  Queue_1_io_deq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_1_io_deq_valid; // @[Decoupled.scala 287:21]
  wire [11:0] Queue_1_io_deq_bits_data; // @[Decoupled.scala 287:21]
  wire  Queue_2_clock; // @[Decoupled.scala 287:21]
  wire  Queue_2_reset; // @[Decoupled.scala 287:21]
  wire  Queue_2_io_enq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_2_io_enq_valid; // @[Decoupled.scala 287:21]
  wire [6:0] Queue_2_io_enq_bits_data; // @[Decoupled.scala 287:21]
  wire  Queue_2_io_deq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_2_io_deq_valid; // @[Decoupled.scala 287:21]
  wire [6:0] Queue_2_io_deq_bits_data; // @[Decoupled.scala 287:21]
  wire  Queue_3_clock; // @[Decoupled.scala 287:21]
  wire  Queue_3_reset; // @[Decoupled.scala 287:21]
  wire  Queue_3_io_enq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_3_io_enq_valid; // @[Decoupled.scala 287:21]
  wire [11:0] Queue_3_io_enq_bits_data; // @[Decoupled.scala 287:21]
  wire  Queue_3_io_deq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_3_io_deq_valid; // @[Decoupled.scala 287:21]
  wire [11:0] Queue_3_io_deq_bits_data; // @[Decoupled.scala 287:21]
  wire  Queue_4_clock; // @[Decoupled.scala 287:21]
  wire  Queue_4_reset; // @[Decoupled.scala 287:21]
  wire  Queue_4_io_enq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_4_io_enq_valid; // @[Decoupled.scala 287:21]
  wire [19:0] Queue_4_io_enq_bits; // @[Decoupled.scala 287:21]
  wire  Queue_4_io_deq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_4_io_deq_valid; // @[Decoupled.scala 287:21]
  wire [19:0] Queue_4_io_deq_bits; // @[Decoupled.scala 287:21]
  wire  Queue_5_clock; // @[Decoupled.scala 287:21]
  wire  Queue_5_reset; // @[Decoupled.scala 287:21]
  wire  Queue_5_io_enq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_5_io_enq_valid; // @[Decoupled.scala 287:21]
  wire [19:0] Queue_5_io_enq_bits; // @[Decoupled.scala 287:21]
  wire  Queue_5_io_deq_ready; // @[Decoupled.scala 287:21]
  wire  Queue_5_io_deq_valid; // @[Decoupled.scala 287:21]
  wire [19:0] Queue_5_io_deq_bits; // @[Decoupled.scala 287:21]
  reg  writeFinishRegVec_0; // @[ProcessingElement.scala 36:45]
  reg [31:0] _RAND_0;
  reg  writeFinishRegVec_1; // @[ProcessingElement.scala 36:45]
  reg [31:0] _RAND_1;
  reg  writeFinishRegVec_2; // @[ProcessingElement.scala 36:45]
  reg [31:0] _RAND_2;
  reg  writeFinishRegVec_3; // @[ProcessingElement.scala 36:45]
  reg [31:0] _RAND_3;
  wire  _GEN_0; // @[ProcessingElement.scala 38:32]
  wire  _GEN_2; // @[ProcessingElement.scala 38:32]
  wire  _GEN_4; // @[ProcessingElement.scala 38:32]
  wire  _GEN_6; // @[ProcessingElement.scala 38:32]
  wire  _T_1; // @[ProcessingElement.scala 45:49]
  wire  _T_2; // @[ProcessingElement.scala 45:49]
  ProcessingElementControl peCtrl ( // @[ProcessingElement.scala 12:48]
    .clock(peCtrl_clock),
    .reset(peCtrl_reset),
    .io_ctrlPad_doMACEn(peCtrl_io_ctrlPad_doMACEn),
    .io_ctrlPad_fromTopIO_pSumEnqOrProduct_ready(peCtrl_io_ctrlPad_fromTopIO_pSumEnqOrProduct_ready),
    .io_ctrlPad_fromTopIO_pSumEnqOrProduct_bits(peCtrl_io_ctrlPad_fromTopIO_pSumEnqOrProduct_bits),
    .io_ctrlPad_fromTopIO_doLoadEn(peCtrl_io_ctrlPad_fromTopIO_doLoadEn),
    .io_ctrlPad_fromTopIO_calFinish(peCtrl_io_ctrlPad_fromTopIO_calFinish),
    .io_ctrlTop_pSumEnqOrProduct_ready(peCtrl_io_ctrlTop_pSumEnqOrProduct_ready),
    .io_ctrlTop_pSumEnqOrProduct_bits(peCtrl_io_ctrlTop_pSumEnqOrProduct_bits),
    .io_ctrlTop_doLoadEn(peCtrl_io_ctrlTop_doLoadEn),
    .io_ctrlTop_calFinish(peCtrl_io_ctrlTop_calFinish),
    .io_ctrlTop_writeFinish(peCtrl_io_ctrlTop_writeFinish),
    .io_debugIO_peState(peCtrl_io_debugIO_peState),
    .io_debugIO_doMACEnDebug(peCtrl_io_debugIO_doMACEnDebug)
  );
  ProcessingElementPad pePad ( // @[ProcessingElement.scala 13:43]
    .clock(pePad_clock),
    .reset(pePad_reset),
    .io_padCtrl_doMACEn(pePad_io_padCtrl_doMACEn),
    .io_padCtrl_fromTopIO_pSumEnqOrProduct_ready(pePad_io_padCtrl_fromTopIO_pSumEnqOrProduct_ready),
    .io_padCtrl_fromTopIO_pSumEnqOrProduct_bits(pePad_io_padCtrl_fromTopIO_pSumEnqOrProduct_bits),
    .io_padCtrl_fromTopIO_doLoadEn(pePad_io_padCtrl_fromTopIO_doLoadEn),
    .io_padCtrl_fromTopIO_calFinish(pePad_io_padCtrl_fromTopIO_calFinish),
    .io_dataStream_ipsIO_ready(pePad_io_dataStream_ipsIO_ready),
    .io_dataStream_ipsIO_valid(pePad_io_dataStream_ipsIO_valid),
    .io_dataStream_ipsIO_bits(pePad_io_dataStream_ipsIO_bits),
    .io_dataStream_opsIO_ready(pePad_io_dataStream_opsIO_ready),
    .io_dataStream_opsIO_valid(pePad_io_dataStream_opsIO_valid),
    .io_dataStream_opsIO_bits(pePad_io_dataStream_opsIO_bits),
    .io_dataStream_iactIOs_dataIOs_writeInDataIO_ready(pePad_io_dataStream_iactIOs_dataIOs_writeInDataIO_ready),
    .io_dataStream_iactIOs_dataIOs_writeInDataIO_valid(pePad_io_dataStream_iactIOs_dataIOs_writeInDataIO_valid),
    .io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data(pePad_io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data),
    .io_dataStream_iactIOs_dataIOs_streamLen(pePad_io_dataStream_iactIOs_dataIOs_streamLen),
    .io_dataStream_iactIOs_dataIOs_writeFin(pePad_io_dataStream_iactIOs_dataIOs_writeFin),
    .io_dataStream_iactIOs_addrIOs_writeInDataIO_ready(pePad_io_dataStream_iactIOs_addrIOs_writeInDataIO_ready),
    .io_dataStream_iactIOs_addrIOs_writeInDataIO_valid(pePad_io_dataStream_iactIOs_addrIOs_writeInDataIO_valid),
    .io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data(pePad_io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data),
    .io_dataStream_iactIOs_addrIOs_streamLen(pePad_io_dataStream_iactIOs_addrIOs_streamLen),
    .io_dataStream_iactIOs_addrIOs_writeFin(pePad_io_dataStream_iactIOs_addrIOs_writeFin),
    .io_dataStream_weightIOs_dataIOs_writeInDataIO_ready(pePad_io_dataStream_weightIOs_dataIOs_writeInDataIO_ready),
    .io_dataStream_weightIOs_dataIOs_writeInDataIO_valid(pePad_io_dataStream_weightIOs_dataIOs_writeInDataIO_valid),
    .io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data(pePad_io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data),
    .io_dataStream_weightIOs_dataIOs_streamLen(pePad_io_dataStream_weightIOs_dataIOs_streamLen),
    .io_dataStream_weightIOs_dataIOs_writeFin(pePad_io_dataStream_weightIOs_dataIOs_writeFin),
    .io_dataStream_weightIOs_addrIOs_writeInDataIO_ready(pePad_io_dataStream_weightIOs_addrIOs_writeInDataIO_ready),
    .io_dataStream_weightIOs_addrIOs_writeInDataIO_valid(pePad_io_dataStream_weightIOs_addrIOs_writeInDataIO_valid),
    .io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data(pePad_io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data),
    .io_dataStream_weightIOs_addrIOs_streamLen(pePad_io_dataStream_weightIOs_addrIOs_streamLen),
    .io_dataStream_weightIOs_addrIOs_writeFin(pePad_io_dataStream_weightIOs_addrIOs_writeFin),
    .io_debugIO_iactMatrixData(pePad_io_debugIO_iactMatrixData),
    .io_debugIO_iactMatrixRow(pePad_io_debugIO_iactMatrixRow),
    .io_debugIO_iactMatrixColumn(pePad_io_debugIO_iactMatrixColumn),
    .io_debugIO_iactAddrInc(pePad_io_debugIO_iactAddrInc),
    .io_debugIO_iactDataInc(pePad_io_debugIO_iactDataInc),
    .io_debugIO_iactAddrIdx(pePad_io_debugIO_iactAddrIdx),
    .io_debugIO_weightAddrSPadReadOut(pePad_io_debugIO_weightAddrSPadReadOut),
    .io_debugIO_weightMatrixData(pePad_io_debugIO_weightMatrixData),
    .io_debugIO_weightMatrixRow(pePad_io_debugIO_weightMatrixRow),
    .io_debugIO_productResult(pePad_io_debugIO_productResult),
    .io_debugIO_pSumResult(pePad_io_debugIO_pSumResult),
    .io_debugIO_pSumLoad(pePad_io_debugIO_pSumLoad),
    .io_debugIO_weightAddrInIdx(pePad_io_debugIO_weightAddrInIdx),
    .io_debugIO_sPadState(pePad_io_debugIO_sPadState)
  );
  Queue Queue ( // @[Decoupled.scala 287:21]
    .clock(Queue_clock),
    .reset(Queue_reset),
    .io_enq_ready(Queue_io_enq_ready),
    .io_enq_valid(Queue_io_enq_valid),
    .io_enq_bits_data(Queue_io_enq_bits_data),
    .io_deq_ready(Queue_io_deq_ready),
    .io_deq_valid(Queue_io_deq_valid),
    .io_deq_bits_data(Queue_io_deq_bits_data)
  );
  Queue_1 Queue_1 ( // @[Decoupled.scala 287:21]
    .clock(Queue_1_clock),
    .reset(Queue_1_reset),
    .io_enq_ready(Queue_1_io_enq_ready),
    .io_enq_valid(Queue_1_io_enq_valid),
    .io_enq_bits_data(Queue_1_io_enq_bits_data),
    .io_deq_ready(Queue_1_io_deq_ready),
    .io_deq_valid(Queue_1_io_deq_valid),
    .io_deq_bits_data(Queue_1_io_deq_bits_data)
  );
  Queue_2 Queue_2 ( // @[Decoupled.scala 287:21]
    .clock(Queue_2_clock),
    .reset(Queue_2_reset),
    .io_enq_ready(Queue_2_io_enq_ready),
    .io_enq_valid(Queue_2_io_enq_valid),
    .io_enq_bits_data(Queue_2_io_enq_bits_data),
    .io_deq_ready(Queue_2_io_deq_ready),
    .io_deq_valid(Queue_2_io_deq_valid),
    .io_deq_bits_data(Queue_2_io_deq_bits_data)
  );
  Queue_1 Queue_3 ( // @[Decoupled.scala 287:21]
    .clock(Queue_3_clock),
    .reset(Queue_3_reset),
    .io_enq_ready(Queue_3_io_enq_ready),
    .io_enq_valid(Queue_3_io_enq_valid),
    .io_enq_bits_data(Queue_3_io_enq_bits_data),
    .io_deq_ready(Queue_3_io_deq_ready),
    .io_deq_valid(Queue_3_io_deq_valid),
    .io_deq_bits_data(Queue_3_io_deq_bits_data)
  );
  Queue_4 Queue_4 ( // @[Decoupled.scala 287:21]
    .clock(Queue_4_clock),
    .reset(Queue_4_reset),
    .io_enq_ready(Queue_4_io_enq_ready),
    .io_enq_valid(Queue_4_io_enq_valid),
    .io_enq_bits(Queue_4_io_enq_bits),
    .io_deq_ready(Queue_4_io_deq_ready),
    .io_deq_valid(Queue_4_io_deq_valid),
    .io_deq_bits(Queue_4_io_deq_bits)
  );
  Queue_4 Queue_5 ( // @[Decoupled.scala 287:21]
    .clock(Queue_5_clock),
    .reset(Queue_5_reset),
    .io_enq_ready(Queue_5_io_enq_ready),
    .io_enq_valid(Queue_5_io_enq_valid),
    .io_enq_bits(Queue_5_io_enq_bits),
    .io_deq_ready(Queue_5_io_deq_ready),
    .io_deq_valid(Queue_5_io_deq_valid),
    .io_deq_bits(Queue_5_io_deq_bits)
  );
  assign _GEN_0 = pePad_io_dataStream_iactIOs_addrIOs_writeFin | writeFinishRegVec_0; // @[ProcessingElement.scala 38:32]
  assign _GEN_2 = pePad_io_dataStream_iactIOs_dataIOs_writeFin | writeFinishRegVec_1; // @[ProcessingElement.scala 38:32]
  assign _GEN_4 = pePad_io_dataStream_weightIOs_addrIOs_writeFin | writeFinishRegVec_2; // @[ProcessingElement.scala 38:32]
  assign _GEN_6 = pePad_io_dataStream_weightIOs_dataIOs_writeFin | writeFinishRegVec_3; // @[ProcessingElement.scala 38:32]
  assign _T_1 = writeFinishRegVec_0 & writeFinishRegVec_1; // @[ProcessingElement.scala 45:49]
  assign _T_2 = _T_1 & writeFinishRegVec_2; // @[ProcessingElement.scala 45:49]
  assign io_dataStream_ipsIO_ready = Queue_4_io_enq_ready; // @[Decoupled.scala 290:17]
  assign io_dataStream_opsIO_valid = Queue_5_io_deq_valid; // @[ProcessingElement.scala 49:23]
  assign io_dataStream_opsIO_bits = Queue_5_io_deq_bits; // @[ProcessingElement.scala 49:23]
  assign io_dataStream_iactIOs_dataIOs_writeInDataIO_ready = Queue_1_io_enq_ready; // @[Decoupled.scala 290:17]
  assign io_dataStream_iactIOs_dataIOs_writeFin = pePad_io_dataStream_iactIOs_dataIOs_writeFin; // @[ProcessingElement.scala 25:55]
  assign io_dataStream_iactIOs_addrIOs_writeInDataIO_ready = Queue_io_enq_ready; // @[Decoupled.scala 290:17]
  assign io_dataStream_iactIOs_addrIOs_writeFin = pePad_io_dataStream_iactIOs_addrIOs_writeFin; // @[ProcessingElement.scala 24:55]
  assign io_dataStream_weightIOs_dataIOs_writeInDataIO_ready = Queue_3_io_enq_ready; // @[Decoupled.scala 290:17]
  assign io_dataStream_weightIOs_dataIOs_writeFin = pePad_io_dataStream_weightIOs_dataIOs_writeFin; // @[ProcessingElement.scala 25:55]
  assign io_dataStream_weightIOs_addrIOs_writeInDataIO_ready = Queue_2_io_enq_ready; // @[Decoupled.scala 290:17]
  assign io_dataStream_weightIOs_addrIOs_writeFin = pePad_io_dataStream_weightIOs_addrIOs_writeFin; // @[ProcessingElement.scala 24:55]
  assign io_topCtrl_pSumEnqOrProduct_ready = peCtrl_io_ctrlTop_pSumEnqOrProduct_ready; // @[ProcessingElement.scala 31:31]
  assign io_topCtrl_writeFinish = _T_2 & writeFinishRegVec_3; // @[ProcessingElement.scala 46:26]
  assign io_topCtrl_calFinish = peCtrl_io_ctrlTop_calFinish; // @[ProcessingElement.scala 32:24]
  assign io_debugIO_peControlDebugIO_peState = peCtrl_io_debugIO_peState; // @[ProcessingElement.scala 51:33]
  assign io_debugIO_peControlDebugIO_doMACEnDebug = peCtrl_io_debugIO_doMACEnDebug; // @[ProcessingElement.scala 51:33]
  assign io_debugIO_peSPadDebugIO_iactMatrixData = pePad_io_debugIO_iactMatrixData; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_iactMatrixRow = pePad_io_debugIO_iactMatrixRow; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_iactMatrixColumn = pePad_io_debugIO_iactMatrixColumn; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_iactAddrInc = pePad_io_debugIO_iactAddrInc; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_iactDataInc = pePad_io_debugIO_iactDataInc; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_iactAddrIdx = pePad_io_debugIO_iactAddrIdx; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_weightAddrSPadReadOut = pePad_io_debugIO_weightAddrSPadReadOut; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_weightMatrixData = pePad_io_debugIO_weightMatrixData; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_weightMatrixRow = pePad_io_debugIO_weightMatrixRow; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_productResult = pePad_io_debugIO_productResult; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_pSumResult = pePad_io_debugIO_pSumResult; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_pSumLoad = pePad_io_debugIO_pSumLoad; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_weightAddrInIdx = pePad_io_debugIO_weightAddrInIdx; // @[ProcessingElement.scala 52:30]
  assign io_debugIO_peSPadDebugIO_sPadState = pePad_io_debugIO_sPadState; // @[ProcessingElement.scala 52:30]
  assign peCtrl_clock = clock;
  assign peCtrl_reset = reset;
  assign peCtrl_io_ctrlPad_fromTopIO_pSumEnqOrProduct_ready = pePad_io_padCtrl_fromTopIO_pSumEnqOrProduct_ready; // @[ProcessingElement.scala 30:21]
  assign peCtrl_io_ctrlPad_fromTopIO_calFinish = pePad_io_padCtrl_fromTopIO_calFinish; // @[ProcessingElement.scala 30:21]
  assign peCtrl_io_ctrlTop_pSumEnqOrProduct_bits = io_topCtrl_pSumEnqOrProduct_bits; // @[ProcessingElement.scala 31:31]
  assign peCtrl_io_ctrlTop_doLoadEn = io_topCtrl_doLoadEn; // @[ProcessingElement.scala 33:30]
  assign peCtrl_io_ctrlTop_writeFinish = _T_2 & writeFinishRegVec_3; // @[ProcessingElement.scala 47:33]
  assign pePad_clock = clock;
  assign pePad_reset = reset;
  assign pePad_io_padCtrl_doMACEn = peCtrl_io_ctrlPad_doMACEn; // @[ProcessingElement.scala 30:21]
  assign pePad_io_padCtrl_fromTopIO_pSumEnqOrProduct_bits = peCtrl_io_ctrlPad_fromTopIO_pSumEnqOrProduct_bits; // @[ProcessingElement.scala 30:21]
  assign pePad_io_padCtrl_fromTopIO_doLoadEn = peCtrl_io_ctrlPad_fromTopIO_doLoadEn; // @[ProcessingElement.scala 30:21]
  assign pePad_io_dataStream_ipsIO_valid = Queue_4_io_deq_valid; // @[ProcessingElement.scala 48:29]
  assign pePad_io_dataStream_ipsIO_bits = Queue_4_io_deq_bits; // @[ProcessingElement.scala 48:29]
  assign pePad_io_dataStream_opsIO_ready = Queue_5_io_enq_ready; // @[Decoupled.scala 290:17]
  assign pePad_io_dataStream_iactIOs_dataIOs_writeInDataIO_valid = Queue_1_io_deq_valid; // @[ProcessingElement.scala 16:55]
  assign pePad_io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data = Queue_1_io_deq_bits_data; // @[ProcessingElement.scala 16:55]
  assign pePad_io_dataStream_iactIOs_dataIOs_streamLen = io_dataStream_iactIOs_dataIOs_streamLen; // @[ProcessingElement.scala 23:56]
  assign pePad_io_dataStream_iactIOs_addrIOs_writeInDataIO_valid = Queue_io_deq_valid; // @[ProcessingElement.scala 15:55]
  assign pePad_io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data = Queue_io_deq_bits_data; // @[ProcessingElement.scala 15:55]
  assign pePad_io_dataStream_iactIOs_addrIOs_streamLen = io_dataStream_iactIOs_addrIOs_streamLen; // @[ProcessingElement.scala 22:56]
  assign pePad_io_dataStream_weightIOs_dataIOs_writeInDataIO_valid = Queue_3_io_deq_valid; // @[ProcessingElement.scala 18:57]
  assign pePad_io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data = Queue_3_io_deq_bits_data; // @[ProcessingElement.scala 18:57]
  assign pePad_io_dataStream_weightIOs_dataIOs_streamLen = io_dataStream_weightIOs_dataIOs_streamLen; // @[ProcessingElement.scala 23:56]
  assign pePad_io_dataStream_weightIOs_addrIOs_writeInDataIO_valid = Queue_2_io_deq_valid; // @[ProcessingElement.scala 17:57]
  assign pePad_io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data = Queue_2_io_deq_bits_data; // @[ProcessingElement.scala 17:57]
  assign pePad_io_dataStream_weightIOs_addrIOs_streamLen = io_dataStream_weightIOs_addrIOs_streamLen; // @[ProcessingElement.scala 22:56]
  assign Queue_clock = clock;
  assign Queue_reset = reset;
  assign Queue_io_enq_valid = io_dataStream_iactIOs_addrIOs_writeInDataIO_valid; // @[Decoupled.scala 288:22]
  assign Queue_io_enq_bits_data = io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data; // @[Decoupled.scala 289:21]
  assign Queue_io_deq_ready = pePad_io_dataStream_iactIOs_addrIOs_writeInDataIO_ready; // @[ProcessingElement.scala 15:55]
  assign Queue_1_clock = clock;
  assign Queue_1_reset = reset;
  assign Queue_1_io_enq_valid = io_dataStream_iactIOs_dataIOs_writeInDataIO_valid; // @[Decoupled.scala 288:22]
  assign Queue_1_io_enq_bits_data = io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data; // @[Decoupled.scala 289:21]
  assign Queue_1_io_deq_ready = pePad_io_dataStream_iactIOs_dataIOs_writeInDataIO_ready; // @[ProcessingElement.scala 16:55]
  assign Queue_2_clock = clock;
  assign Queue_2_reset = reset;
  assign Queue_2_io_enq_valid = io_dataStream_weightIOs_addrIOs_writeInDataIO_valid; // @[Decoupled.scala 288:22]
  assign Queue_2_io_enq_bits_data = io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data; // @[Decoupled.scala 289:21]
  assign Queue_2_io_deq_ready = pePad_io_dataStream_weightIOs_addrIOs_writeInDataIO_ready; // @[ProcessingElement.scala 17:57]
  assign Queue_3_clock = clock;
  assign Queue_3_reset = reset;
  assign Queue_3_io_enq_valid = io_dataStream_weightIOs_dataIOs_writeInDataIO_valid; // @[Decoupled.scala 288:22]
  assign Queue_3_io_enq_bits_data = io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data; // @[Decoupled.scala 289:21]
  assign Queue_3_io_deq_ready = pePad_io_dataStream_weightIOs_dataIOs_writeInDataIO_ready; // @[ProcessingElement.scala 18:57]
  assign Queue_4_clock = clock;
  assign Queue_4_reset = reset;
  assign Queue_4_io_enq_valid = io_dataStream_ipsIO_valid; // @[Decoupled.scala 288:22]
  assign Queue_4_io_enq_bits = io_dataStream_ipsIO_bits; // @[Decoupled.scala 289:21]
  assign Queue_4_io_deq_ready = pePad_io_dataStream_ipsIO_ready; // @[ProcessingElement.scala 48:29]
  assign Queue_5_clock = clock;
  assign Queue_5_reset = reset;
  assign Queue_5_io_enq_valid = pePad_io_dataStream_opsIO_valid; // @[Decoupled.scala 288:22]
  assign Queue_5_io_enq_bits = pePad_io_dataStream_opsIO_bits; // @[Decoupled.scala 289:21]
  assign Queue_5_io_deq_ready = io_dataStream_opsIO_ready; // @[ProcessingElement.scala 49:23]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  writeFinishRegVec_0 = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  writeFinishRegVec_1 = _RAND_1[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  writeFinishRegVec_2 = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  writeFinishRegVec_3 = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      writeFinishRegVec_0 <= 1'h0;
    end else if (io_topCtrl_calFinish) begin
      writeFinishRegVec_0 <= 1'h0;
    end else begin
      writeFinishRegVec_0 <= _GEN_0;
    end
    if (reset) begin
      writeFinishRegVec_1 <= 1'h0;
    end else if (io_topCtrl_calFinish) begin
      writeFinishRegVec_1 <= 1'h0;
    end else begin
      writeFinishRegVec_1 <= _GEN_2;
    end
    if (reset) begin
      writeFinishRegVec_2 <= 1'h0;
    end else if (io_topCtrl_calFinish) begin
      writeFinishRegVec_2 <= 1'h0;
    end else begin
      writeFinishRegVec_2 <= _GEN_4;
    end
    if (reset) begin
      writeFinishRegVec_3 <= 1'h0;
    end else if (io_topCtrl_calFinish) begin
      writeFinishRegVec_3 <= 1'h0;
    end else begin
      writeFinishRegVec_3 <= _GEN_6;
    end
  end
endmodule
