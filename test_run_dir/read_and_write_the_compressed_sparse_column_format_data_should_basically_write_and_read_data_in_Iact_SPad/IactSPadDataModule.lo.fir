circuit IactSPadDataModule :
  module IactSPadDataModule :
    input clock : Clock
    input reset : UInt<1>
    output io_commonIO_columnNum : UInt<4>
    output io_commonIO_readOutData : UInt<12>
    input io_commonIO_readEn : UInt<1>
    output io_commonIO_writeIdx : UInt<4>
    output io_commonIO_dataLenFinIO_streamDecoupledDataIO_ready : UInt<1>
    input io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid : UInt<1>
    input io_commonIO_dataLenFinIO_streamDecoupledDataIO_bits_data : UInt<12>
    input io_commonIO_dataLenFinIO_streamLen : UInt<4>
    output io_commonIO_dataLenFinIO_writeFin : UInt<1>
    input io_addrIO_readInIdx : UInt<4>
    output io_addrIO_nextReadOutData : UInt<12>
    input io_dataIO_readInIdx : UInt<4>
  
    reg dataLenReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[ProcessingElement.scala 257:33]
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[ProcessingElement.scala 260:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[ProcessingElement.scala 261:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 265:53]
    node _T_1 = tail(_T, 1) @[ProcessingElement.scala 265:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[ProcessingElement.scala 265:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 266:51]
    node _T_4 = tail(_T_3, 1) @[ProcessingElement.scala 266:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[ProcessingElement.scala 266:35]
    node _T_6 = add(padWriteIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 270:42]
    node _T_7 = tail(_T_6, 1) @[ProcessingElement.scala 270:42]
    node writeWrapWire = _T_2 @[ProcessingElement.scala 263:33 ProcessingElement.scala 265:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_7) @[ProcessingElement.scala 271:26]
    node _GEN_1 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 268:32]
    node _GEN_2 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_0, padWriteIndexReg) @[ProcessingElement.scala 268:32]
    node _T_8 = add(padReadIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 279:40]
    node _T_9 = tail(_T_8, 1) @[ProcessingElement.scala 279:40]
    node readWrapWire = _T_5 @[ProcessingElement.scala 264:32 ProcessingElement.scala 266:16]
    node _GEN_3 = mux(readWrapWire, UInt<1>("h0"), _T_9) @[ProcessingElement.scala 280:25]
    node _GEN_4 = mux(io_commonIO_readEn, _GEN_3, padReadIndexReg) @[ProcessingElement.scala 278:29]
    node _T_10 = and(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, writeWrapWire) @[ProcessingElement.scala 286:62]
    reg iactDataSPad_0 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_0) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_1 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_1) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_2 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_2) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_3 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_3) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_4 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_4) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_5 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_5) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_6 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_6) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_7 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_7) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_8 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_8) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_9 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_9) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_10 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_10) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_11 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_11) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_12 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_12) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_13 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_13) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_14 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_14) @[ProcessingElement.scala 208:40]
    reg iactDataSPad_15 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), iactDataSPad_15) @[ProcessingElement.scala 208:40]
    node _iactDataSPad_padWriteIndexReg = io_commonIO_dataLenFinIO_streamDecoupledDataIO_bits_data @[ProcessingElement.scala 211:36 ProcessingElement.scala 211:36]
    node _GEN_5 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_0) @[ProcessingElement.scala 211:36]
    node _GEN_6 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_1) @[ProcessingElement.scala 211:36]
    node _GEN_7 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_2) @[ProcessingElement.scala 211:36]
    node _GEN_8 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_3) @[ProcessingElement.scala 211:36]
    node _GEN_9 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_4) @[ProcessingElement.scala 211:36]
    node _GEN_10 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_5) @[ProcessingElement.scala 211:36]
    node _GEN_11 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_6) @[ProcessingElement.scala 211:36]
    node _GEN_12 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_7) @[ProcessingElement.scala 211:36]
    node _GEN_13 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_8) @[ProcessingElement.scala 211:36]
    node _GEN_14 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_9) @[ProcessingElement.scala 211:36]
    node _GEN_15 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_10) @[ProcessingElement.scala 211:36]
    node _GEN_16 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_11) @[ProcessingElement.scala 211:36]
    node _GEN_17 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_12) @[ProcessingElement.scala 211:36]
    node _GEN_18 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_13) @[ProcessingElement.scala 211:36]
    node _GEN_19 = mux(eq(UInt<4>("he"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_14) @[ProcessingElement.scala 211:36]
    node _GEN_20 = mux(eq(UInt<4>("hf"), padWriteIndexReg), _iactDataSPad_padWriteIndexReg, iactDataSPad_15) @[ProcessingElement.scala 211:36]
    node _GEN_21 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_5, iactDataSPad_0) @[ProcessingElement.scala 210:32]
    node _GEN_22 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_6, iactDataSPad_1) @[ProcessingElement.scala 210:32]
    node _GEN_23 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_7, iactDataSPad_2) @[ProcessingElement.scala 210:32]
    node _GEN_24 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_8, iactDataSPad_3) @[ProcessingElement.scala 210:32]
    node _GEN_25 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_9, iactDataSPad_4) @[ProcessingElement.scala 210:32]
    node _GEN_26 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_10, iactDataSPad_5) @[ProcessingElement.scala 210:32]
    node _GEN_27 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_11, iactDataSPad_6) @[ProcessingElement.scala 210:32]
    node _GEN_28 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_12, iactDataSPad_7) @[ProcessingElement.scala 210:32]
    node _GEN_29 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_13, iactDataSPad_8) @[ProcessingElement.scala 210:32]
    node _GEN_30 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_14, iactDataSPad_9) @[ProcessingElement.scala 210:32]
    node _GEN_31 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_15, iactDataSPad_10) @[ProcessingElement.scala 210:32]
    node _GEN_32 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_16, iactDataSPad_11) @[ProcessingElement.scala 210:32]
    node _GEN_33 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_17, iactDataSPad_12) @[ProcessingElement.scala 210:32]
    node _GEN_34 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_18, iactDataSPad_13) @[ProcessingElement.scala 210:32]
    node _GEN_35 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_19, iactDataSPad_14) @[ProcessingElement.scala 210:32]
    node _GEN_36 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_20, iactDataSPad_15) @[ProcessingElement.scala 210:32]
    node _GEN_37 = validif(eq(UInt<1>("h0"), padReadIndexReg), iactDataSPad_0) @[ProcessingElement.scala 214:12]
    node _GEN_38 = mux(eq(UInt<1>("h1"), padReadIndexReg), iactDataSPad_1, _GEN_37) @[ProcessingElement.scala 214:12]
    node _GEN_39 = mux(eq(UInt<2>("h2"), padReadIndexReg), iactDataSPad_2, _GEN_38) @[ProcessingElement.scala 214:12]
    node _GEN_40 = mux(eq(UInt<2>("h3"), padReadIndexReg), iactDataSPad_3, _GEN_39) @[ProcessingElement.scala 214:12]
    node _GEN_41 = mux(eq(UInt<3>("h4"), padReadIndexReg), iactDataSPad_4, _GEN_40) @[ProcessingElement.scala 214:12]
    node _GEN_42 = mux(eq(UInt<3>("h5"), padReadIndexReg), iactDataSPad_5, _GEN_41) @[ProcessingElement.scala 214:12]
    node _GEN_43 = mux(eq(UInt<3>("h6"), padReadIndexReg), iactDataSPad_6, _GEN_42) @[ProcessingElement.scala 214:12]
    node _GEN_44 = mux(eq(UInt<3>("h7"), padReadIndexReg), iactDataSPad_7, _GEN_43) @[ProcessingElement.scala 214:12]
    node _GEN_45 = mux(eq(UInt<4>("h8"), padReadIndexReg), iactDataSPad_8, _GEN_44) @[ProcessingElement.scala 214:12]
    node _GEN_46 = mux(eq(UInt<4>("h9"), padReadIndexReg), iactDataSPad_9, _GEN_45) @[ProcessingElement.scala 214:12]
    node _GEN_47 = mux(eq(UInt<4>("ha"), padReadIndexReg), iactDataSPad_10, _GEN_46) @[ProcessingElement.scala 214:12]
    node _GEN_48 = mux(eq(UInt<4>("hb"), padReadIndexReg), iactDataSPad_11, _GEN_47) @[ProcessingElement.scala 214:12]
    node _GEN_49 = mux(eq(UInt<4>("hc"), padReadIndexReg), iactDataSPad_12, _GEN_48) @[ProcessingElement.scala 214:12]
    node _GEN_50 = mux(eq(UInt<4>("hd"), padReadIndexReg), iactDataSPad_13, _GEN_49) @[ProcessingElement.scala 214:12]
    node _GEN_51 = mux(eq(UInt<4>("he"), padReadIndexReg), iactDataSPad_14, _GEN_50) @[ProcessingElement.scala 214:12]
    node _GEN_52 = mux(eq(UInt<4>("hf"), padReadIndexReg), iactDataSPad_15, _GEN_51) @[ProcessingElement.scala 214:12]
    node _iactDataSPad_padReadIndexReg = _GEN_52 @[ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12 ProcessingElement.scala 214:12]
    node dataWire = _iactDataSPad_padReadIndexReg @[ProcessingElement.scala 258:28 ProcessingElement.scala 214:12]
    node _T_11_0 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_1 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_2 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_3 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_4 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_5 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_6 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_7 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_8 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_9 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_10 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_11 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_12 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_13 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_14 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    node _T_11_15 = UInt<12>("h0") @[ProcessingElement.scala 208:48 ProcessingElement.scala 208:48]
    io_commonIO_columnNum <= padReadIndexReg @[ProcessingElement.scala 287:25]
    io_commonIO_readOutData <= dataWire @[ProcessingElement.scala 215:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[ProcessingElement.scala 285:24]
    io_commonIO_dataLenFinIO_streamDecoupledDataIO_ready <= _GEN_1 @[ProcessingElement.scala 269:27 ProcessingElement.scala 275:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_10 @[ProcessingElement.scala 286:37]
    io_addrIO_nextReadOutData is invalid
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[ProcessingElement.scala 259:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[ProcessingElement.scala 270:22 ProcessingElement.scala 272:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_4) @[ProcessingElement.scala 279:21 ProcessingElement.scala 281:23]
    iactDataSPad_0 <= mux(reset, _T_11_0, _GEN_21) @[ProcessingElement.scala 211:36]
    iactDataSPad_1 <= mux(reset, _T_11_1, _GEN_22) @[ProcessingElement.scala 211:36]
    iactDataSPad_2 <= mux(reset, _T_11_2, _GEN_23) @[ProcessingElement.scala 211:36]
    iactDataSPad_3 <= mux(reset, _T_11_3, _GEN_24) @[ProcessingElement.scala 211:36]
    iactDataSPad_4 <= mux(reset, _T_11_4, _GEN_25) @[ProcessingElement.scala 211:36]
    iactDataSPad_5 <= mux(reset, _T_11_5, _GEN_26) @[ProcessingElement.scala 211:36]
    iactDataSPad_6 <= mux(reset, _T_11_6, _GEN_27) @[ProcessingElement.scala 211:36]
    iactDataSPad_7 <= mux(reset, _T_11_7, _GEN_28) @[ProcessingElement.scala 211:36]
    iactDataSPad_8 <= mux(reset, _T_11_8, _GEN_29) @[ProcessingElement.scala 211:36]
    iactDataSPad_9 <= mux(reset, _T_11_9, _GEN_30) @[ProcessingElement.scala 211:36]
    iactDataSPad_10 <= mux(reset, _T_11_10, _GEN_31) @[ProcessingElement.scala 211:36]
    iactDataSPad_11 <= mux(reset, _T_11_11, _GEN_32) @[ProcessingElement.scala 211:36]
    iactDataSPad_12 <= mux(reset, _T_11_12, _GEN_33) @[ProcessingElement.scala 211:36]
    iactDataSPad_13 <= mux(reset, _T_11_13, _GEN_34) @[ProcessingElement.scala 211:36]
    iactDataSPad_14 <= mux(reset, _T_11_14, _GEN_35) @[ProcessingElement.scala 211:36]
    iactDataSPad_15 <= mux(reset, _T_11_15, _GEN_36) @[ProcessingElement.scala 211:36]
