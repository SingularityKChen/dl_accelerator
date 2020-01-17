circuit IactSPadAddrModule :
  module IactSPadAddrModule :
    input clock : Clock
    input reset : UInt<1>
    output io_commonIO_columnNum : UInt<4>
    output io_commonIO_readOutData : UInt<4>
    input io_commonIO_readEn : UInt<1>
    output io_commonIO_writeIdx : UInt<4>
    output io_commonIO_dataLenFinIO_streamDecoupledDataIO_ready : UInt<1>
    input io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid : UInt<1>
    input io_commonIO_dataLenFinIO_streamDecoupledDataIO_bits_data : UInt<4>
    input io_commonIO_dataLenFinIO_streamLen : UInt<4>
    output io_commonIO_dataLenFinIO_writeFin : UInt<1>
    input io_addrIO_readInIdx : UInt<4>
    output io_addrIO_nextReadOutData : UInt<4>
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
    reg iactAddrSPad_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPad_0) @[ProcessingElement.scala 190:40]
    reg iactAddrSPad_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPad_1) @[ProcessingElement.scala 190:40]
    reg iactAddrSPad_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPad_2) @[ProcessingElement.scala 190:40]
    reg iactAddrSPad_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPad_3) @[ProcessingElement.scala 190:40]
    reg iactAddrSPad_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPad_4) @[ProcessingElement.scala 190:40]
    reg iactAddrSPad_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPad_5) @[ProcessingElement.scala 190:40]
    reg iactAddrSPad_6 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPad_6) @[ProcessingElement.scala 190:40]
    reg iactAddrSPad_7 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPad_7) @[ProcessingElement.scala 190:40]
    reg iactAddrSPad_8 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPad_8) @[ProcessingElement.scala 190:40]
    reg dataWireReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataWireReg) @[Reg.scala 27:20]
    node _T_12 = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 198:58]
    node _T_13 = tail(_T_12, 1) @[ProcessingElement.scala 198:58]
    node _T_14 = eq(padReadIndexReg, _T_13) @[ProcessingElement.scala 198:43]
    node dataWireReadLastElem = _T_14 @[ProcessingElement.scala 197:40 ProcessingElement.scala 198:24]
    node _T_15 = add(padReadIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 201:75]
    node _T_16 = tail(_T_15, 1) @[ProcessingElement.scala 201:75]
    node _GEN_24 = validif(eq(UInt<1>("h0"), _T_16), iactAddrSPad_0) @[ProcessingElement.scala 201:18]
    node _GEN_25 = mux(eq(UInt<1>("h1"), _T_16), iactAddrSPad_1, _GEN_24) @[ProcessingElement.scala 201:18]
    node _GEN_26 = mux(eq(UInt<2>("h2"), _T_16), iactAddrSPad_2, _GEN_25) @[ProcessingElement.scala 201:18]
    node _GEN_27 = mux(eq(UInt<2>("h3"), _T_16), iactAddrSPad_3, _GEN_26) @[ProcessingElement.scala 201:18]
    node _GEN_28 = mux(eq(UInt<3>("h4"), _T_16), iactAddrSPad_4, _GEN_27) @[ProcessingElement.scala 201:18]
    node _GEN_29 = mux(eq(UInt<3>("h5"), _T_16), iactAddrSPad_5, _GEN_28) @[ProcessingElement.scala 201:18]
    node _GEN_30 = mux(eq(UInt<3>("h6"), _T_16), iactAddrSPad_6, _GEN_29) @[ProcessingElement.scala 201:18]
    node _GEN_31 = mux(eq(UInt<3>("h7"), _T_16), iactAddrSPad_7, _GEN_30) @[ProcessingElement.scala 201:18]
    node _GEN_32 = mux(eq(UInt<4>("h8"), _T_16), iactAddrSPad_8, _GEN_31) @[ProcessingElement.scala 201:18]
    node _iactAddrSPad_T_16 = _GEN_32 @[ProcessingElement.scala 201:18 ProcessingElement.scala 201:18 ProcessingElement.scala 201:18 ProcessingElement.scala 201:18 ProcessingElement.scala 201:18 ProcessingElement.scala 201:18 ProcessingElement.scala 201:18 ProcessingElement.scala 201:18 ProcessingElement.scala 201:18 ProcessingElement.scala 201:18]
    node _T_17 = mux(dataWireReadLastElem, UInt<1>("h0"), _iactAddrSPad_T_16) @[ProcessingElement.scala 201:18]
    node dataWire = _T_17 @[ProcessingElement.scala 258:28 ProcessingElement.scala 201:12]
    node _GEN_5 = mux(io_commonIO_readEn, dataWire, dataWireReg) @[Reg.scala 28:19]
    node _iactAddrSPad_padWriteIndexReg = io_commonIO_dataLenFinIO_streamDecoupledDataIO_bits_data @[ProcessingElement.scala 195:36 ProcessingElement.scala 195:36]
    node _GEN_6 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _iactAddrSPad_padWriteIndexReg, iactAddrSPad_0) @[ProcessingElement.scala 195:36]
    node _GEN_7 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _iactAddrSPad_padWriteIndexReg, iactAddrSPad_1) @[ProcessingElement.scala 195:36]
    node _GEN_8 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _iactAddrSPad_padWriteIndexReg, iactAddrSPad_2) @[ProcessingElement.scala 195:36]
    node _GEN_9 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _iactAddrSPad_padWriteIndexReg, iactAddrSPad_3) @[ProcessingElement.scala 195:36]
    node _GEN_10 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _iactAddrSPad_padWriteIndexReg, iactAddrSPad_4) @[ProcessingElement.scala 195:36]
    node _GEN_11 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _iactAddrSPad_padWriteIndexReg, iactAddrSPad_5) @[ProcessingElement.scala 195:36]
    node _GEN_12 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _iactAddrSPad_padWriteIndexReg, iactAddrSPad_6) @[ProcessingElement.scala 195:36]
    node _GEN_13 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _iactAddrSPad_padWriteIndexReg, iactAddrSPad_7) @[ProcessingElement.scala 195:36]
    node _GEN_14 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _iactAddrSPad_padWriteIndexReg, iactAddrSPad_8) @[ProcessingElement.scala 195:36]
    node _GEN_15 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_6, iactAddrSPad_0) @[ProcessingElement.scala 194:32]
    node _GEN_16 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_7, iactAddrSPad_1) @[ProcessingElement.scala 194:32]
    node _GEN_17 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_8, iactAddrSPad_2) @[ProcessingElement.scala 194:32]
    node _GEN_18 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_9, iactAddrSPad_3) @[ProcessingElement.scala 194:32]
    node _GEN_19 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_10, iactAddrSPad_4) @[ProcessingElement.scala 194:32]
    node _GEN_20 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_11, iactAddrSPad_5) @[ProcessingElement.scala 194:32]
    node _GEN_21 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_12, iactAddrSPad_6) @[ProcessingElement.scala 194:32]
    node _GEN_22 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_13, iactAddrSPad_7) @[ProcessingElement.scala 194:32]
    node _GEN_23 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_14, iactAddrSPad_8) @[ProcessingElement.scala 194:32]
    node _T_11_0 = UInt<4>("h0") @[ProcessingElement.scala 190:48 ProcessingElement.scala 190:48]
    node _T_11_1 = UInt<4>("h0") @[ProcessingElement.scala 190:48 ProcessingElement.scala 190:48]
    node _T_11_2 = UInt<4>("h0") @[ProcessingElement.scala 190:48 ProcessingElement.scala 190:48]
    node _T_11_3 = UInt<4>("h0") @[ProcessingElement.scala 190:48 ProcessingElement.scala 190:48]
    node _T_11_4 = UInt<4>("h0") @[ProcessingElement.scala 190:48 ProcessingElement.scala 190:48]
    node _T_11_5 = UInt<4>("h0") @[ProcessingElement.scala 190:48 ProcessingElement.scala 190:48]
    node _T_11_6 = UInt<4>("h0") @[ProcessingElement.scala 190:48 ProcessingElement.scala 190:48]
    node _T_11_7 = UInt<4>("h0") @[ProcessingElement.scala 190:48 ProcessingElement.scala 190:48]
    node _T_11_8 = UInt<4>("h0") @[ProcessingElement.scala 190:48 ProcessingElement.scala 190:48]
    io_commonIO_columnNum <= padReadIndexReg @[ProcessingElement.scala 287:25]
    io_commonIO_readOutData <= dataWireReg @[ProcessingElement.scala 203:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[ProcessingElement.scala 285:24]
    io_commonIO_dataLenFinIO_streamDecoupledDataIO_ready <= _GEN_1 @[ProcessingElement.scala 269:27 ProcessingElement.scala 275:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_10 @[ProcessingElement.scala 286:37]
    io_addrIO_nextReadOutData <= dataWire @[ProcessingElement.scala 202:29]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[ProcessingElement.scala 259:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[ProcessingElement.scala 270:22 ProcessingElement.scala 272:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_4) @[ProcessingElement.scala 279:21 ProcessingElement.scala 281:23]
    iactAddrSPad_0 <= mux(reset, _T_11_0, _GEN_15) @[ProcessingElement.scala 195:36]
    iactAddrSPad_1 <= mux(reset, _T_11_1, _GEN_16) @[ProcessingElement.scala 195:36]
    iactAddrSPad_2 <= mux(reset, _T_11_2, _GEN_17) @[ProcessingElement.scala 195:36]
    iactAddrSPad_3 <= mux(reset, _T_11_3, _GEN_18) @[ProcessingElement.scala 195:36]
    iactAddrSPad_4 <= mux(reset, _T_11_4, _GEN_19) @[ProcessingElement.scala 195:36]
    iactAddrSPad_5 <= mux(reset, _T_11_5, _GEN_20) @[ProcessingElement.scala 195:36]
    iactAddrSPad_6 <= mux(reset, _T_11_6, _GEN_21) @[ProcessingElement.scala 195:36]
    iactAddrSPad_7 <= mux(reset, _T_11_7, _GEN_22) @[ProcessingElement.scala 195:36]
    iactAddrSPad_8 <= mux(reset, _T_11_8, _GEN_23) @[ProcessingElement.scala 195:36]
    dataWireReg <= mux(reset, UInt<1>("h0"), _GEN_5) @[Reg.scala 28:23]
