circuit SimplyCombineAddrDataSPad :
  module SPadAddrModule :
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
    input io_addrIO_indexInc : UInt<1>
    input io_dataIO_readInIdx : UInt<4>
  
    reg dataLenReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[ProcessingElement.scala 236:33]
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[ProcessingElement.scala 239:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[ProcessingElement.scala 241:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 247:53]
    node _T_1 = tail(_T, 1) @[ProcessingElement.scala 247:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[ProcessingElement.scala 247:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 248:51]
    node _T_4 = tail(_T_3, 1) @[ProcessingElement.scala 248:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[ProcessingElement.scala 248:35]
    node _T_6 = add(padWriteIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 252:42]
    node _T_7 = tail(_T_6, 1) @[ProcessingElement.scala 252:42]
    node writeWrapWire = _T_2 @[ProcessingElement.scala 244:33 ProcessingElement.scala 247:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_7) @[ProcessingElement.scala 253:26]
    node _GEN_1 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 250:32]
    node _GEN_2 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_0, padWriteIndexReg) @[ProcessingElement.scala 250:32]
    node _T_8 = add(padReadIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 261:40]
    node _T_9 = tail(_T_8, 1) @[ProcessingElement.scala 261:40]
    node readWrapWire = _T_5 @[ProcessingElement.scala 245:32 ProcessingElement.scala 248:16]
    node _GEN_3 = mux(readWrapWire, UInt<1>("h0"), _T_9) @[ProcessingElement.scala 262:25]
    node readIndexInc = io_addrIO_indexInc @[ProcessingElement.scala 246:32 ProcessingElement.scala 204:16]
    node _GEN_4 = mux(readIndexInc, _GEN_3, padReadIndexReg) @[ProcessingElement.scala 260:23]
    node _T_10 = and(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, writeWrapWire) @[ProcessingElement.scala 268:62]
    reg addrSPad_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_0) @[ProcessingElement.scala 196:36]
    reg addrSPad_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_1) @[ProcessingElement.scala 196:36]
    reg addrSPad_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_2) @[ProcessingElement.scala 196:36]
    reg addrSPad_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_3) @[ProcessingElement.scala 196:36]
    reg addrSPad_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_4) @[ProcessingElement.scala 196:36]
    reg addrSPad_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_5) @[ProcessingElement.scala 196:36]
    reg addrSPad_6 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_6) @[ProcessingElement.scala 196:36]
    reg addrSPad_7 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_7) @[ProcessingElement.scala 196:36]
    reg addrSPad_8 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_8) @[ProcessingElement.scala 196:36]
    node _addrSPad_padWriteIndexReg = io_commonIO_dataLenFinIO_streamDecoupledDataIO_bits_data @[ProcessingElement.scala 199:32 ProcessingElement.scala 199:32]
    node _GEN_5 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_0) @[ProcessingElement.scala 199:32]
    node _GEN_6 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_1) @[ProcessingElement.scala 199:32]
    node _GEN_7 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_2) @[ProcessingElement.scala 199:32]
    node _GEN_8 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_3) @[ProcessingElement.scala 199:32]
    node _GEN_9 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_4) @[ProcessingElement.scala 199:32]
    node _GEN_10 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_5) @[ProcessingElement.scala 199:32]
    node _GEN_11 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_6) @[ProcessingElement.scala 199:32]
    node _GEN_12 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_7) @[ProcessingElement.scala 199:32]
    node _GEN_13 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_8) @[ProcessingElement.scala 199:32]
    node _GEN_14 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_5, addrSPad_0) @[ProcessingElement.scala 198:32]
    node _GEN_15 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_6, addrSPad_1) @[ProcessingElement.scala 198:32]
    node _GEN_16 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_7, addrSPad_2) @[ProcessingElement.scala 198:32]
    node _GEN_17 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_8, addrSPad_3) @[ProcessingElement.scala 198:32]
    node _GEN_18 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_9, addrSPad_4) @[ProcessingElement.scala 198:32]
    node _GEN_19 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_10, addrSPad_5) @[ProcessingElement.scala 198:32]
    node _GEN_20 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_11, addrSPad_6) @[ProcessingElement.scala 198:32]
    node _GEN_21 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_12, addrSPad_7) @[ProcessingElement.scala 198:32]
    node _GEN_22 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_13, addrSPad_8) @[ProcessingElement.scala 198:32]
    node _GEN_23 = validif(eq(UInt<1>("h0"), padReadIndexReg), addrSPad_0) @[ProcessingElement.scala 202:12]
    node _GEN_24 = mux(eq(UInt<1>("h1"), padReadIndexReg), addrSPad_1, _GEN_23) @[ProcessingElement.scala 202:12]
    node _GEN_25 = mux(eq(UInt<2>("h2"), padReadIndexReg), addrSPad_2, _GEN_24) @[ProcessingElement.scala 202:12]
    node _GEN_26 = mux(eq(UInt<2>("h3"), padReadIndexReg), addrSPad_3, _GEN_25) @[ProcessingElement.scala 202:12]
    node _GEN_27 = mux(eq(UInt<3>("h4"), padReadIndexReg), addrSPad_4, _GEN_26) @[ProcessingElement.scala 202:12]
    node _GEN_28 = mux(eq(UInt<3>("h5"), padReadIndexReg), addrSPad_5, _GEN_27) @[ProcessingElement.scala 202:12]
    node _GEN_29 = mux(eq(UInt<3>("h6"), padReadIndexReg), addrSPad_6, _GEN_28) @[ProcessingElement.scala 202:12]
    node _GEN_30 = mux(eq(UInt<3>("h7"), padReadIndexReg), addrSPad_7, _GEN_29) @[ProcessingElement.scala 202:12]
    node _GEN_31 = mux(eq(UInt<4>("h8"), padReadIndexReg), addrSPad_8, _GEN_30) @[ProcessingElement.scala 202:12]
    node _addrSPad_padReadIndexReg = _GEN_31 @[ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12]
    node dataWire = _addrSPad_padReadIndexReg @[ProcessingElement.scala 237:28 ProcessingElement.scala 202:12]
    node _T_11_0 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_1 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_2 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_3 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_4 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_5 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_6 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_7 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_8 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    io_commonIO_columnNum <= padReadIndexReg @[ProcessingElement.scala 269:25]
    io_commonIO_readOutData <= dataWire @[ProcessingElement.scala 203:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[ProcessingElement.scala 267:24]
    io_commonIO_dataLenFinIO_streamDecoupledDataIO_ready <= _GEN_1 @[ProcessingElement.scala 251:27 ProcessingElement.scala 257:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_10 @[ProcessingElement.scala 268:37]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[ProcessingElement.scala 238:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[ProcessingElement.scala 252:22 ProcessingElement.scala 254:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_4) @[ProcessingElement.scala 261:21 ProcessingElement.scala 263:23]
    addrSPad_0 <= mux(reset, _T_11_0, _GEN_14) @[ProcessingElement.scala 199:32]
    addrSPad_1 <= mux(reset, _T_11_1, _GEN_15) @[ProcessingElement.scala 199:32]
    addrSPad_2 <= mux(reset, _T_11_2, _GEN_16) @[ProcessingElement.scala 199:32]
    addrSPad_3 <= mux(reset, _T_11_3, _GEN_17) @[ProcessingElement.scala 199:32]
    addrSPad_4 <= mux(reset, _T_11_4, _GEN_18) @[ProcessingElement.scala 199:32]
    addrSPad_5 <= mux(reset, _T_11_5, _GEN_19) @[ProcessingElement.scala 199:32]
    addrSPad_6 <= mux(reset, _T_11_6, _GEN_20) @[ProcessingElement.scala 199:32]
    addrSPad_7 <= mux(reset, _T_11_7, _GEN_21) @[ProcessingElement.scala 199:32]
    addrSPad_8 <= mux(reset, _T_11_8, _GEN_22) @[ProcessingElement.scala 199:32]

  module SPadDataModule :
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
    input io_addrIO_indexInc : UInt<1>
    input io_dataIO_readInIdx : UInt<4>
  
    reg dataLenReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[ProcessingElement.scala 236:33]
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[ProcessingElement.scala 239:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[ProcessingElement.scala 241:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 247:53]
    node _T_1 = tail(_T, 1) @[ProcessingElement.scala 247:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[ProcessingElement.scala 247:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 248:51]
    node _T_4 = tail(_T_3, 1) @[ProcessingElement.scala 248:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[ProcessingElement.scala 248:35]
    node _T_6 = add(padWriteIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 252:42]
    node _T_7 = tail(_T_6, 1) @[ProcessingElement.scala 252:42]
    node writeWrapWire = _T_2 @[ProcessingElement.scala 244:33 ProcessingElement.scala 247:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_7) @[ProcessingElement.scala 253:26]
    node _GEN_1 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 250:32]
    node _GEN_2 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_0, padWriteIndexReg) @[ProcessingElement.scala 250:32]
    node _T_8 = add(padReadIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 261:40]
    node _T_9 = tail(_T_8, 1) @[ProcessingElement.scala 261:40]
    node readWrapWire = _T_5 @[ProcessingElement.scala 245:32 ProcessingElement.scala 248:16]
    node _GEN_3 = mux(readWrapWire, UInt<1>("h0"), _T_9) @[ProcessingElement.scala 262:25]
    node readIndexInc = io_commonIO_readEn @[ProcessingElement.scala 246:32 ProcessingElement.scala 225:16]
    node _GEN_4 = mux(readIndexInc, _GEN_3, padReadIndexReg) @[ProcessingElement.scala 260:23]
    node _T_10 = and(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, writeWrapWire) @[ProcessingElement.scala 268:62]
    reg _T_12_0 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_0) @[ProcessingElement.scala 218:38]
    reg _T_12_1 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_1) @[ProcessingElement.scala 218:38]
    reg _T_12_2 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_2) @[ProcessingElement.scala 218:38]
    reg _T_12_3 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_3) @[ProcessingElement.scala 218:38]
    reg _T_12_4 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_4) @[ProcessingElement.scala 218:38]
    reg _T_12_5 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_5) @[ProcessingElement.scala 218:38]
    reg _T_12_6 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_6) @[ProcessingElement.scala 218:38]
    reg _T_12_7 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_7) @[ProcessingElement.scala 218:38]
    reg _T_12_8 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_8) @[ProcessingElement.scala 218:38]
    reg _T_12_9 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_9) @[ProcessingElement.scala 218:38]
    reg _T_12_10 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_10) @[ProcessingElement.scala 218:38]
    reg _T_12_11 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_11) @[ProcessingElement.scala 218:38]
    reg _T_12_12 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_12) @[ProcessingElement.scala 218:38]
    reg _T_12_13 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_13) @[ProcessingElement.scala 218:38]
    reg _T_12_14 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_14) @[ProcessingElement.scala 218:38]
    reg _T_12_15 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_15) @[ProcessingElement.scala 218:38]
    node _T_12_padWriteIndexReg = io_commonIO_dataLenFinIO_streamDecoupledDataIO_bits_data @[ProcessingElement.scala 220:34 ProcessingElement.scala 220:34]
    node _GEN_5 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_0) @[ProcessingElement.scala 220:34]
    node _GEN_6 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_1) @[ProcessingElement.scala 220:34]
    node _GEN_7 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_2) @[ProcessingElement.scala 220:34]
    node _GEN_8 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_3) @[ProcessingElement.scala 220:34]
    node _GEN_9 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_4) @[ProcessingElement.scala 220:34]
    node _GEN_10 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_5) @[ProcessingElement.scala 220:34]
    node _GEN_11 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_6) @[ProcessingElement.scala 220:34]
    node _GEN_12 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_7) @[ProcessingElement.scala 220:34]
    node _GEN_13 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_8) @[ProcessingElement.scala 220:34]
    node _GEN_14 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_9) @[ProcessingElement.scala 220:34]
    node _GEN_15 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_10) @[ProcessingElement.scala 220:34]
    node _GEN_16 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_11) @[ProcessingElement.scala 220:34]
    node _GEN_17 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_12) @[ProcessingElement.scala 220:34]
    node _GEN_18 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_13) @[ProcessingElement.scala 220:34]
    node _GEN_19 = mux(eq(UInt<4>("he"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_14) @[ProcessingElement.scala 220:34]
    node _GEN_20 = mux(eq(UInt<4>("hf"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_15) @[ProcessingElement.scala 220:34]
    node _GEN_21 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_5, _T_12_0) @[ProcessingElement.scala 219:34]
    node _GEN_22 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_6, _T_12_1) @[ProcessingElement.scala 219:34]
    node _GEN_23 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_7, _T_12_2) @[ProcessingElement.scala 219:34]
    node _GEN_24 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_8, _T_12_3) @[ProcessingElement.scala 219:34]
    node _GEN_25 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_9, _T_12_4) @[ProcessingElement.scala 219:34]
    node _GEN_26 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_10, _T_12_5) @[ProcessingElement.scala 219:34]
    node _GEN_27 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_11, _T_12_6) @[ProcessingElement.scala 219:34]
    node _GEN_28 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_12, _T_12_7) @[ProcessingElement.scala 219:34]
    node _GEN_29 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_13, _T_12_8) @[ProcessingElement.scala 219:34]
    node _GEN_30 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_14, _T_12_9) @[ProcessingElement.scala 219:34]
    node _GEN_31 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_15, _T_12_10) @[ProcessingElement.scala 219:34]
    node _GEN_32 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_16, _T_12_11) @[ProcessingElement.scala 219:34]
    node _GEN_33 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_17, _T_12_12) @[ProcessingElement.scala 219:34]
    node _GEN_34 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_18, _T_12_13) @[ProcessingElement.scala 219:34]
    node _GEN_35 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_19, _T_12_14) @[ProcessingElement.scala 219:34]
    node _GEN_36 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_20, _T_12_15) @[ProcessingElement.scala 219:34]
    node _GEN_37 = validif(eq(UInt<1>("h0"), padReadIndexReg), _T_12_0) @[ProcessingElement.scala 223:14]
    node _GEN_38 = mux(eq(UInt<1>("h1"), padReadIndexReg), _T_12_1, _GEN_37) @[ProcessingElement.scala 223:14]
    node _GEN_39 = mux(eq(UInt<2>("h2"), padReadIndexReg), _T_12_2, _GEN_38) @[ProcessingElement.scala 223:14]
    node _GEN_40 = mux(eq(UInt<2>("h3"), padReadIndexReg), _T_12_3, _GEN_39) @[ProcessingElement.scala 223:14]
    node _GEN_41 = mux(eq(UInt<3>("h4"), padReadIndexReg), _T_12_4, _GEN_40) @[ProcessingElement.scala 223:14]
    node _GEN_42 = mux(eq(UInt<3>("h5"), padReadIndexReg), _T_12_5, _GEN_41) @[ProcessingElement.scala 223:14]
    node _GEN_43 = mux(eq(UInt<3>("h6"), padReadIndexReg), _T_12_6, _GEN_42) @[ProcessingElement.scala 223:14]
    node _GEN_44 = mux(eq(UInt<3>("h7"), padReadIndexReg), _T_12_7, _GEN_43) @[ProcessingElement.scala 223:14]
    node _GEN_45 = mux(eq(UInt<4>("h8"), padReadIndexReg), _T_12_8, _GEN_44) @[ProcessingElement.scala 223:14]
    node _GEN_46 = mux(eq(UInt<4>("h9"), padReadIndexReg), _T_12_9, _GEN_45) @[ProcessingElement.scala 223:14]
    node _GEN_47 = mux(eq(UInt<4>("ha"), padReadIndexReg), _T_12_10, _GEN_46) @[ProcessingElement.scala 223:14]
    node _GEN_48 = mux(eq(UInt<4>("hb"), padReadIndexReg), _T_12_11, _GEN_47) @[ProcessingElement.scala 223:14]
    node _GEN_49 = mux(eq(UInt<4>("hc"), padReadIndexReg), _T_12_12, _GEN_48) @[ProcessingElement.scala 223:14]
    node _GEN_50 = mux(eq(UInt<4>("hd"), padReadIndexReg), _T_12_13, _GEN_49) @[ProcessingElement.scala 223:14]
    node _GEN_51 = mux(eq(UInt<4>("he"), padReadIndexReg), _T_12_14, _GEN_50) @[ProcessingElement.scala 223:14]
    node _GEN_52 = mux(eq(UInt<4>("hf"), padReadIndexReg), _T_12_15, _GEN_51) @[ProcessingElement.scala 223:14]
    node _T_12_padReadIndexReg = _GEN_52 @[ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14 ProcessingElement.scala 223:14]
    node dataWire = _T_12_padReadIndexReg @[ProcessingElement.scala 237:28 ProcessingElement.scala 223:14]
    node _T_11_0 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_1 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_2 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_3 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_4 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_5 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_6 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_7 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_8 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_9 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_10 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_11 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_12 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_13 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_14 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    node _T_11_15 = UInt<12>("h0") @[ProcessingElement.scala 218:46 ProcessingElement.scala 218:46]
    io_commonIO_columnNum <= padReadIndexReg @[ProcessingElement.scala 269:25]
    io_commonIO_readOutData <= dataWire @[ProcessingElement.scala 226:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[ProcessingElement.scala 267:24]
    io_commonIO_dataLenFinIO_streamDecoupledDataIO_ready <= _GEN_1 @[ProcessingElement.scala 251:27 ProcessingElement.scala 257:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_10 @[ProcessingElement.scala 268:37]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[ProcessingElement.scala 238:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[ProcessingElement.scala 252:22 ProcessingElement.scala 254:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_4) @[ProcessingElement.scala 261:21 ProcessingElement.scala 263:23]
    _T_12_0 <= mux(reset, _T_11_0, _GEN_21) @[ProcessingElement.scala 220:34]
    _T_12_1 <= mux(reset, _T_11_1, _GEN_22) @[ProcessingElement.scala 220:34]
    _T_12_2 <= mux(reset, _T_11_2, _GEN_23) @[ProcessingElement.scala 220:34]
    _T_12_3 <= mux(reset, _T_11_3, _GEN_24) @[ProcessingElement.scala 220:34]
    _T_12_4 <= mux(reset, _T_11_4, _GEN_25) @[ProcessingElement.scala 220:34]
    _T_12_5 <= mux(reset, _T_11_5, _GEN_26) @[ProcessingElement.scala 220:34]
    _T_12_6 <= mux(reset, _T_11_6, _GEN_27) @[ProcessingElement.scala 220:34]
    _T_12_7 <= mux(reset, _T_11_7, _GEN_28) @[ProcessingElement.scala 220:34]
    _T_12_8 <= mux(reset, _T_11_8, _GEN_29) @[ProcessingElement.scala 220:34]
    _T_12_9 <= mux(reset, _T_11_9, _GEN_30) @[ProcessingElement.scala 220:34]
    _T_12_10 <= mux(reset, _T_11_10, _GEN_31) @[ProcessingElement.scala 220:34]
    _T_12_11 <= mux(reset, _T_11_11, _GEN_32) @[ProcessingElement.scala 220:34]
    _T_12_12 <= mux(reset, _T_11_12, _GEN_33) @[ProcessingElement.scala 220:34]
    _T_12_13 <= mux(reset, _T_11_13, _GEN_34) @[ProcessingElement.scala 220:34]
    _T_12_14 <= mux(reset, _T_11_14, _GEN_35) @[ProcessingElement.scala 220:34]
    _T_12_15 <= mux(reset, _T_11_15, _GEN_36) @[ProcessingElement.scala 220:34]

  module SimplyCombineAddrDataSPad :
    input clock : Clock
    input reset : UInt<1>
    output io_iactIOs_dataIOs_streamDecoupledDataIO_ready : UInt<1>
    input io_iactIOs_dataIOs_streamDecoupledDataIO_valid : UInt<1>
    input io_iactIOs_dataIOs_streamDecoupledDataIO_bits_data : UInt<12>
    input io_iactIOs_dataIOs_streamLen : UInt<4>
    output io_iactIOs_dataIOs_writeFin : UInt<1>
    output io_iactIOs_addrIOs_streamDecoupledDataIO_ready : UInt<1>
    input io_iactIOs_addrIOs_streamDecoupledDataIO_valid : UInt<1>
    input io_iactIOs_addrIOs_streamDecoupledDataIO_bits_data : UInt<4>
    input io_iactIOs_addrIOs_streamLen : UInt<4>
    output io_iactIOs_addrIOs_writeFin : UInt<1>
    output io_iactAddrWriteIdx : UInt<4>
    input io_iactDataReq : UInt<1>
    output io_iactMatrixColumn : UInt<4>
    output io_iactMatrixRow : UInt<4>
    output io_iactMatrixData : UInt<8>
    output io_iactMatrixDataBin : UInt<12>
    output io_iactAddrReadEn : UInt<1>
    output io_iactAddrReadData : UInt<4>
    output io_iactDataReadIndex : UInt<4>
    output io_iactDataWriteIdx : UInt<4>
  
    inst iactAddrSPad of SPadAddrModule @[CompressedSparseColumnSpecTest.scala 57:46]
    inst iactDataSPad of SPadDataModule @[CompressedSparseColumnSpecTest.scala 58:46]
    reg sPad : UInt<2>, clock with :
      reset => (UInt<1>("h0"), sPad) @[CompressedSparseColumnSpecTest.scala 61:29]
    node _T = eq(sPad, UInt<2>("h2")) @[CompressedSparseColumnSpecTest.scala 63:32]
    node iactAddrDataWire = iactAddrSPad.io_commonIO_readOutData @[CompressedSparseColumnSpecTest.scala 65:38 CompressedSparseColumnSpecTest.scala 82:22]
    node _T_1 = eq(iactAddrDataWire, UInt<4>("hf")) @[CompressedSparseColumnSpecTest.scala 68:48]
    reg iactAddrSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPadReadEnReg) @[Reg.scala 15:16]
    node iactASPRERegEnable = _T @[CompressedSparseColumnSpecTest.scala 62:40 CompressedSparseColumnSpecTest.scala 63:24]
    node _GEN_0 = mux(iactASPRERegEnable, UInt<1>("h0"), iactAddrSPadReadEnReg) @[Reg.scala 16:19]
    reg iactDataSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactDataSPadReadEnReg) @[CompressedSparseColumnSpecTest.scala 70:46]
    node _T_2 = eq(sPad, UInt<2>("h2")) @[CompressedSparseColumnSpecTest.scala 73:38]
    node iactDataIndexWire = iactDataSPad.io_commonIO_columnNum @[CompressedSparseColumnSpecTest.scala 66:39 CompressedSparseColumnSpecTest.scala 89:23]
    node _T_3 = eq(iactAddrDataWire, iactDataIndexWire) @[CompressedSparseColumnSpecTest.scala 73:76]
    node _T_4 = and(_T_2, _T_3) @[CompressedSparseColumnSpecTest.scala 73:55]
    node _T_5 = eq(sPad, UInt<2>("h0")) @[CompressedSparseColumnSpecTest.scala 73:110]
    node iactSPadZeroColumnWire = _T_1 @[CompressedSparseColumnSpecTest.scala 67:44 CompressedSparseColumnSpecTest.scala 68:28]
    node _T_6 = and(_T_5, iactSPadZeroColumnWire) @[CompressedSparseColumnSpecTest.scala 73:123]
    node _T_7 = or(_T_4, _T_6) @[CompressedSparseColumnSpecTest.scala 73:100]
    node iactDataCountVec_0 = bits(iactDataSPad.io_commonIO_readOutData, 0, 0) @[CompressedSparseColumnSpecTest.scala 92:76]
    node iactDataCountVec_1 = bits(iactDataSPad.io_commonIO_readOutData, 1, 1) @[CompressedSparseColumnSpecTest.scala 92:76]
    node iactDataCountVec_2 = bits(iactDataSPad.io_commonIO_readOutData, 2, 2) @[CompressedSparseColumnSpecTest.scala 92:76]
    node iactDataCountVec_3 = bits(iactDataSPad.io_commonIO_readOutData, 3, 3) @[CompressedSparseColumnSpecTest.scala 92:76]
    node iactDataCountVec_4 = bits(iactDataSPad.io_commonIO_readOutData, 4, 4) @[CompressedSparseColumnSpecTest.scala 92:76]
    node iactDataCountVec_5 = bits(iactDataSPad.io_commonIO_readOutData, 5, 5) @[CompressedSparseColumnSpecTest.scala 92:76]
    node iactDataCountVec_6 = bits(iactDataSPad.io_commonIO_readOutData, 6, 6) @[CompressedSparseColumnSpecTest.scala 92:76]
    node iactDataCountVec_7 = bits(iactDataSPad.io_commonIO_readOutData, 7, 7) @[CompressedSparseColumnSpecTest.scala 92:76]
    node iactDataCountVec_8 = bits(iactDataSPad.io_commonIO_readOutData, 8, 8) @[CompressedSparseColumnSpecTest.scala 92:76]
    node iactDataCountVec_9 = bits(iactDataSPad.io_commonIO_readOutData, 9, 9) @[CompressedSparseColumnSpecTest.scala 92:76]
    node iactDataCountVec_10 = bits(iactDataSPad.io_commonIO_readOutData, 10, 10) @[CompressedSparseColumnSpecTest.scala 92:76]
    node iactDataCountVec_11 = bits(iactDataSPad.io_commonIO_readOutData, 11, 11) @[CompressedSparseColumnSpecTest.scala 92:76]
    node _T_8 = cat(iactDataCountVec_5, iactDataCountVec_4) @[Cat.scala 29:58]
    node _T_9 = cat(iactDataCountVec_7, iactDataCountVec_6) @[Cat.scala 29:58]
    node _T_10 = cat(_T_9, _T_8) @[Cat.scala 29:58]
    node _T_11 = cat(iactDataCountVec_9, iactDataCountVec_8) @[Cat.scala 29:58]
    node _T_12 = cat(iactDataCountVec_11, iactDataCountVec_10) @[Cat.scala 29:58]
    node _T_13 = cat(_T_12, _T_11) @[Cat.scala 29:58]
    node _T_14 = cat(_T_13, _T_10) @[Cat.scala 29:58]
    node _T_15 = cat(iactDataCountVec_1, iactDataCountVec_0) @[Cat.scala 29:58]
    node _T_16 = cat(iactDataCountVec_3, iactDataCountVec_2) @[Cat.scala 29:58]
    node _T_17 = cat(_T_16, _T_15) @[Cat.scala 29:58]
    node _T_18 = eq(UInt<2>("h0"), sPad) @[Conditional.scala 37:30]
    node iactAddrIndexWire = iactAddrSPad.io_commonIO_columnNum @[CompressedSparseColumnSpecTest.scala 64:39 CompressedSparseColumnSpecTest.scala 80:23]
    node _T_19 = eq(iactAddrIndexWire, UInt<1>("h0")) @[CompressedSparseColumnSpecTest.scala 112:37]
    node _GEN_1 = mux(_T_19, UInt<1>("h1"), _GEN_0) @[CompressedSparseColumnSpecTest.scala 112:46]
    node _GEN_2 = mux(iactSPadZeroColumnWire, UInt<2>("h0"), UInt<2>("h1")) @[CompressedSparseColumnSpecTest.scala 106:41]
    node _GEN_3 = mux(iactSPadZeroColumnWire, UInt<1>("h1"), _GEN_1) @[CompressedSparseColumnSpecTest.scala 106:41]
    node _GEN_4 = mux(iactSPadZeroColumnWire, UInt<1>("h0"), iactDataSPadReadEnReg) @[CompressedSparseColumnSpecTest.scala 106:41]
    node _GEN_5 = mux(io_iactDataReq, _GEN_2, sPad) @[CompressedSparseColumnSpecTest.scala 105:30]
    node _GEN_6 = mux(io_iactDataReq, _GEN_3, _GEN_0) @[CompressedSparseColumnSpecTest.scala 105:30]
    node _GEN_7 = mux(io_iactDataReq, _GEN_4, iactDataSPadReadEnReg) @[CompressedSparseColumnSpecTest.scala 105:30]
    node _T_20 = eq(UInt<2>("h1"), sPad) @[Conditional.scala 37:30]
    node _T_21 = eq(UInt<2>("h2"), sPad) @[Conditional.scala 37:30]
    node _T_22 = eq(iactAddrDataWire, iactDataIndexWire) @[CompressedSparseColumnSpecTest.scala 137:51]
    node _GEN_8 = mux(_T_21, UInt<2>("h0"), sPad) @[Conditional.scala 39:67]
    node _GEN_9 = mux(_T_21, UInt<1>("h0"), iactDataSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_10 = mux(_T_21, _T_22, _GEN_0) @[Conditional.scala 39:67]
    node _GEN_11 = mux(_T_20, UInt<2>("h2"), _GEN_8) @[Conditional.scala 39:67]
    node _GEN_12 = mux(_T_20, UInt<1>("h0"), _GEN_10) @[Conditional.scala 39:67]
    node _GEN_13 = mux(_T_20, UInt<1>("h1"), _GEN_9) @[Conditional.scala 39:67]
    node _GEN_14 = mux(_T_18, _GEN_5, _GEN_11) @[Conditional.scala 40:58]
    node _GEN_15 = mux(_T_18, _GEN_6, _GEN_12) @[Conditional.scala 40:58]
    node _GEN_16 = mux(_T_18, _GEN_7, _GEN_13) @[Conditional.scala 40:58]
    node iactAddrSPadIdxIncWire = _T_7 @[CompressedSparseColumnSpecTest.scala 71:44 CompressedSparseColumnSpecTest.scala 73:28]
    io_iactIOs_dataIOs_streamDecoupledDataIO_ready <= iactDataSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_ready @[CompressedSparseColumnSpecTest.scala 76:43]
    io_iactIOs_dataIOs_writeFin <= iactDataSPad.io_commonIO_dataLenFinIO_writeFin @[CompressedSparseColumnSpecTest.scala 76:43]
    io_iactIOs_addrIOs_streamDecoupledDataIO_ready <= iactAddrSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_ready @[CompressedSparseColumnSpecTest.scala 75:43]
    io_iactIOs_addrIOs_writeFin <= iactAddrSPad.io_commonIO_dataLenFinIO_writeFin @[CompressedSparseColumnSpecTest.scala 75:43]
    io_iactAddrWriteIdx <= iactAddrSPad.io_commonIO_writeIdx @[CompressedSparseColumnSpecTest.scala 77:25]
    io_iactMatrixColumn <= iactAddrIndexWire @[CompressedSparseColumnSpecTest.scala 81:25]
    io_iactMatrixRow <= _T_17 @[CompressedSparseColumnSpecTest.scala 94:22]
    io_iactMatrixData <= _T_14 @[CompressedSparseColumnSpecTest.scala 93:23]
    io_iactMatrixDataBin <= iactDataSPad.io_commonIO_readOutData @[CompressedSparseColumnSpecTest.scala 91:26]
    io_iactAddrReadEn <= iactAddrSPadReadEnReg @[CompressedSparseColumnSpecTest.scala 85:23]
    io_iactAddrReadData <= iactAddrDataWire @[CompressedSparseColumnSpecTest.scala 83:25]
    io_iactDataReadIndex <= iactDataIndexWire @[CompressedSparseColumnSpecTest.scala 90:26]
    io_iactDataWriteIdx <= iactDataSPad.io_commonIO_writeIdx @[CompressedSparseColumnSpecTest.scala 78:25]
    iactAddrSPad.clock <= clock
    iactAddrSPad.reset <= reset
    iactAddrSPad.io_commonIO_readEn <= iactAddrSPadReadEnReg @[CompressedSparseColumnSpecTest.scala 84:37]
    iactAddrSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid <= io_iactIOs_addrIOs_streamDecoupledDataIO_valid @[CompressedSparseColumnSpecTest.scala 75:43]
    iactAddrSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_bits_data <= io_iactIOs_addrIOs_streamDecoupledDataIO_bits_data @[CompressedSparseColumnSpecTest.scala 75:43]
    iactAddrSPad.io_commonIO_dataLenFinIO_streamLen <= io_iactIOs_addrIOs_streamLen @[CompressedSparseColumnSpecTest.scala 75:43]
    iactAddrSPad.io_addrIO_readInIdx is invalid
    iactAddrSPad.io_addrIO_indexInc <= iactAddrSPadIdxIncWire @[CompressedSparseColumnSpecTest.scala 72:37]
    iactAddrSPad.io_dataIO_readInIdx is invalid
    iactDataSPad.clock <= clock
    iactDataSPad.reset <= reset
    iactDataSPad.io_commonIO_readEn <= iactDataSPadReadEnReg @[CompressedSparseColumnSpecTest.scala 95:37 CompressedSparseColumnSpecTest.scala 96:37]
    iactDataSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid <= io_iactIOs_dataIOs_streamDecoupledDataIO_valid @[CompressedSparseColumnSpecTest.scala 76:43]
    iactDataSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_bits_data <= io_iactIOs_dataIOs_streamDecoupledDataIO_bits_data @[CompressedSparseColumnSpecTest.scala 76:43]
    iactDataSPad.io_commonIO_dataLenFinIO_streamLen <= io_iactIOs_dataIOs_streamLen @[CompressedSparseColumnSpecTest.scala 76:43]
    iactDataSPad.io_addrIO_readInIdx is invalid
    iactDataSPad.io_addrIO_indexInc is invalid
    iactDataSPad.io_dataIO_readInIdx is invalid
    sPad <= mux(reset, UInt<2>("h0"), _GEN_14) @[CompressedSparseColumnSpecTest.scala 107:18 CompressedSparseColumnSpecTest.scala 111:18 CompressedSparseColumnSpecTest.scala 120:14 CompressedSparseColumnSpecTest.scala 135:14]
    iactAddrSPadReadEnReg <= _GEN_15 @[Reg.scala 16:23 CompressedSparseColumnSpecTest.scala 108:35 CompressedSparseColumnSpecTest.scala 113:37 CompressedSparseColumnSpecTest.scala 121:31 CompressedSparseColumnSpecTest.scala 137:31]
    iactDataSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_16) @[CompressedSparseColumnSpecTest.scala 109:35 CompressedSparseColumnSpecTest.scala 122:31 CompressedSparseColumnSpecTest.scala 136:31]
