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
    input io_dataIO_indexInc : UInt<1>
  
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
    reg addrSPad_9 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_9) @[ProcessingElement.scala 196:36]
    reg addrSPad_10 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_10) @[ProcessingElement.scala 196:36]
    reg addrSPad_11 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_11) @[ProcessingElement.scala 196:36]
    reg addrSPad_12 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_12) @[ProcessingElement.scala 196:36]
    reg addrSPad_13 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_13) @[ProcessingElement.scala 196:36]
    reg addrSPad_14 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_14) @[ProcessingElement.scala 196:36]
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
    node _GEN_14 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_9) @[ProcessingElement.scala 199:32]
    node _GEN_15 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_10) @[ProcessingElement.scala 199:32]
    node _GEN_16 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_11) @[ProcessingElement.scala 199:32]
    node _GEN_17 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_12) @[ProcessingElement.scala 199:32]
    node _GEN_18 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_13) @[ProcessingElement.scala 199:32]
    node _GEN_19 = mux(eq(UInt<4>("he"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_14) @[ProcessingElement.scala 199:32]
    node _GEN_20 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_5, addrSPad_0) @[ProcessingElement.scala 198:32]
    node _GEN_21 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_6, addrSPad_1) @[ProcessingElement.scala 198:32]
    node _GEN_22 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_7, addrSPad_2) @[ProcessingElement.scala 198:32]
    node _GEN_23 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_8, addrSPad_3) @[ProcessingElement.scala 198:32]
    node _GEN_24 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_9, addrSPad_4) @[ProcessingElement.scala 198:32]
    node _GEN_25 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_10, addrSPad_5) @[ProcessingElement.scala 198:32]
    node _GEN_26 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_11, addrSPad_6) @[ProcessingElement.scala 198:32]
    node _GEN_27 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_12, addrSPad_7) @[ProcessingElement.scala 198:32]
    node _GEN_28 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_13, addrSPad_8) @[ProcessingElement.scala 198:32]
    node _GEN_29 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_14, addrSPad_9) @[ProcessingElement.scala 198:32]
    node _GEN_30 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_15, addrSPad_10) @[ProcessingElement.scala 198:32]
    node _GEN_31 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_16, addrSPad_11) @[ProcessingElement.scala 198:32]
    node _GEN_32 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_17, addrSPad_12) @[ProcessingElement.scala 198:32]
    node _GEN_33 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_18, addrSPad_13) @[ProcessingElement.scala 198:32]
    node _GEN_34 = mux(io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid, _GEN_19, addrSPad_14) @[ProcessingElement.scala 198:32]
    node _GEN_35 = validif(eq(UInt<1>("h0"), padReadIndexReg), addrSPad_0) @[ProcessingElement.scala 202:12]
    node _GEN_36 = mux(eq(UInt<1>("h1"), padReadIndexReg), addrSPad_1, _GEN_35) @[ProcessingElement.scala 202:12]
    node _GEN_37 = mux(eq(UInt<2>("h2"), padReadIndexReg), addrSPad_2, _GEN_36) @[ProcessingElement.scala 202:12]
    node _GEN_38 = mux(eq(UInt<2>("h3"), padReadIndexReg), addrSPad_3, _GEN_37) @[ProcessingElement.scala 202:12]
    node _GEN_39 = mux(eq(UInt<3>("h4"), padReadIndexReg), addrSPad_4, _GEN_38) @[ProcessingElement.scala 202:12]
    node _GEN_40 = mux(eq(UInt<3>("h5"), padReadIndexReg), addrSPad_5, _GEN_39) @[ProcessingElement.scala 202:12]
    node _GEN_41 = mux(eq(UInt<3>("h6"), padReadIndexReg), addrSPad_6, _GEN_40) @[ProcessingElement.scala 202:12]
    node _GEN_42 = mux(eq(UInt<3>("h7"), padReadIndexReg), addrSPad_7, _GEN_41) @[ProcessingElement.scala 202:12]
    node _GEN_43 = mux(eq(UInt<4>("h8"), padReadIndexReg), addrSPad_8, _GEN_42) @[ProcessingElement.scala 202:12]
    node _GEN_44 = mux(eq(UInt<4>("h9"), padReadIndexReg), addrSPad_9, _GEN_43) @[ProcessingElement.scala 202:12]
    node _GEN_45 = mux(eq(UInt<4>("ha"), padReadIndexReg), addrSPad_10, _GEN_44) @[ProcessingElement.scala 202:12]
    node _GEN_46 = mux(eq(UInt<4>("hb"), padReadIndexReg), addrSPad_11, _GEN_45) @[ProcessingElement.scala 202:12]
    node _GEN_47 = mux(eq(UInt<4>("hc"), padReadIndexReg), addrSPad_12, _GEN_46) @[ProcessingElement.scala 202:12]
    node _GEN_48 = mux(eq(UInt<4>("hd"), padReadIndexReg), addrSPad_13, _GEN_47) @[ProcessingElement.scala 202:12]
    node _GEN_49 = mux(eq(UInt<4>("he"), padReadIndexReg), addrSPad_14, _GEN_48) @[ProcessingElement.scala 202:12]
    node _addrSPad_padReadIndexReg = _GEN_49 @[ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12 ProcessingElement.scala 202:12]
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
    node _T_11_9 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_10 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_11 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_12 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_13 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    node _T_11_14 = UInt<4>("h0") @[ProcessingElement.scala 196:44 ProcessingElement.scala 196:44]
    io_commonIO_columnNum <= padReadIndexReg @[ProcessingElement.scala 269:25]
    io_commonIO_readOutData <= dataWire @[ProcessingElement.scala 203:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[ProcessingElement.scala 267:24]
    io_commonIO_dataLenFinIO_streamDecoupledDataIO_ready <= _GEN_1 @[ProcessingElement.scala 251:27 ProcessingElement.scala 257:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_10 @[ProcessingElement.scala 268:37]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[ProcessingElement.scala 238:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[ProcessingElement.scala 252:22 ProcessingElement.scala 254:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_4) @[ProcessingElement.scala 261:21 ProcessingElement.scala 263:23]
    addrSPad_0 <= mux(reset, _T_11_0, _GEN_20) @[ProcessingElement.scala 199:32]
    addrSPad_1 <= mux(reset, _T_11_1, _GEN_21) @[ProcessingElement.scala 199:32]
    addrSPad_2 <= mux(reset, _T_11_2, _GEN_22) @[ProcessingElement.scala 199:32]
    addrSPad_3 <= mux(reset, _T_11_3, _GEN_23) @[ProcessingElement.scala 199:32]
    addrSPad_4 <= mux(reset, _T_11_4, _GEN_24) @[ProcessingElement.scala 199:32]
    addrSPad_5 <= mux(reset, _T_11_5, _GEN_25) @[ProcessingElement.scala 199:32]
    addrSPad_6 <= mux(reset, _T_11_6, _GEN_26) @[ProcessingElement.scala 199:32]
    addrSPad_7 <= mux(reset, _T_11_7, _GEN_27) @[ProcessingElement.scala 199:32]
    addrSPad_8 <= mux(reset, _T_11_8, _GEN_28) @[ProcessingElement.scala 199:32]
    addrSPad_9 <= mux(reset, _T_11_9, _GEN_29) @[ProcessingElement.scala 199:32]
    addrSPad_10 <= mux(reset, _T_11_10, _GEN_30) @[ProcessingElement.scala 199:32]
    addrSPad_11 <= mux(reset, _T_11_11, _GEN_31) @[ProcessingElement.scala 199:32]
    addrSPad_12 <= mux(reset, _T_11_12, _GEN_32) @[ProcessingElement.scala 199:32]
    addrSPad_13 <= mux(reset, _T_11_13, _GEN_33) @[ProcessingElement.scala 199:32]
    addrSPad_14 <= mux(reset, _T_11_14, _GEN_34) @[ProcessingElement.scala 199:32]

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
    input io_dataIO_indexInc : UInt<1>
  
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
    node readIndexInc = io_dataIO_indexInc @[ProcessingElement.scala 246:32 ProcessingElement.scala 225:16]
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
  
    inst iactAddrSPad of SPadAddrModule @[CompressedSparseColumnSpecTest.scala 107:46]
    inst iactDataSPad of SPadDataModule @[CompressedSparseColumnSpecTest.scala 108:46]
    reg sPad : UInt<2>, clock with :
      reset => (UInt<1>("h0"), sPad) @[CompressedSparseColumnSpecTest.scala 111:29]
    reg iactSPadZeroColumnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactSPadZeroColumnReg) @[CompressedSparseColumnSpecTest.scala 116:46]
    reg iactAddrSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPadReadEnReg) @[Reg.scala 15:16]
    node _T_18 = eq(sPad, UInt<2>("h0")) @[CompressedSparseColumnSpecTest.scala 128:33]
    node _T_19 = eq(sPad, UInt<2>("h1")) @[CompressedSparseColumnSpecTest.scala 128:56]
    node _T_20 = and(_T_19, iactSPadZeroColumnReg) @[CompressedSparseColumnSpecTest.scala 128:73]
    node _T_21 = or(_T_18, _T_20) @[CompressedSparseColumnSpecTest.scala 128:46]
    node iactASPRERegEnable = _T_21 @[CompressedSparseColumnSpecTest.scala 112:40 CompressedSparseColumnSpecTest.scala 128:24]
    node _GEN_0 = mux(iactASPRERegEnable, UInt<1>("h0"), iactAddrSPadReadEnReg) @[Reg.scala 16:19]
    reg iactDataSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactDataSPadReadEnReg) @[CompressedSparseColumnSpecTest.scala 118:46]
    reg iactMatrixColumnReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactMatrixColumnReg) @[CompressedSparseColumnSpecTest.scala 121:44]
    reg iactZeroColumnNumber : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactZeroColumnNumber) @[CompressedSparseColumnSpecTest.scala 122:45]
    reg iactDataSPadFirstRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactDataSPadFirstRead) @[CompressedSparseColumnSpecTest.scala 123:46]
    node _T = eq(sPad, UInt<2>("h2")) @[CompressedSparseColumnSpecTest.scala 126:38]
    node iactDataIndexWire = iactDataSPad.io_commonIO_columnNum @[CompressedSparseColumnSpecTest.scala 115:39 CompressedSparseColumnSpecTest.scala 143:23]
    node _T_1 = add(iactDataIndexWire, UInt<1>("h1")) @[CompressedSparseColumnSpecTest.scala 126:99]
    node _T_2 = tail(_T_1, 1) @[CompressedSparseColumnSpecTest.scala 126:99]
    node iactAddrDataWire = iactAddrSPad.io_commonIO_readOutData @[CompressedSparseColumnSpecTest.scala 114:38 CompressedSparseColumnSpecTest.scala 136:22]
    node _T_3 = eq(iactAddrDataWire, _T_2) @[CompressedSparseColumnSpecTest.scala 126:76]
    node _T_4 = and(_T, _T_3) @[CompressedSparseColumnSpecTest.scala 126:55]
    node _T_5 = eq(sPad, UInt<2>("h1")) @[CompressedSparseColumnSpecTest.scala 126:118]
    node _T_6 = eq(iactAddrDataWire, UInt<4>("hf")) @[CompressedSparseColumnSpecTest.scala 126:156]
    node _T_7 = and(_T_5, _T_6) @[CompressedSparseColumnSpecTest.scala 126:135]
    node _T_8 = or(_T_4, _T_7) @[CompressedSparseColumnSpecTest.scala 126:108]
    node _T_9 = eq(sPad, UInt<2>("h1")) @[CompressedSparseColumnSpecTest.scala 127:38]
    node _T_10 = eq(iactSPadZeroColumnReg, UInt<1>("h0")) @[CompressedSparseColumnSpecTest.scala 127:58]
    node _T_11 = and(_T_9, _T_10) @[CompressedSparseColumnSpecTest.scala 127:55]
    node _T_12 = eq(iactDataSPadFirstRead, UInt<1>("h0")) @[CompressedSparseColumnSpecTest.scala 127:84]
    node _T_13 = and(_T_11, _T_12) @[CompressedSparseColumnSpecTest.scala 127:81]
    node _T_14 = eq(sPad, UInt<2>("h2")) @[CompressedSparseColumnSpecTest.scala 127:118]
    node iactAddrSPadIdxIncWire = _T_8 @[CompressedSparseColumnSpecTest.scala 119:44 CompressedSparseColumnSpecTest.scala 126:28]
    node _T_15 = eq(iactAddrSPadIdxIncWire, UInt<1>("h0")) @[CompressedSparseColumnSpecTest.scala 127:138]
    node _T_16 = and(_T_14, _T_15) @[CompressedSparseColumnSpecTest.scala 127:135]
    node _T_17 = or(_T_13, _T_16) @[CompressedSparseColumnSpecTest.scala 127:108]
    node iactDataCountVec_0 = bits(iactDataSPad.io_commonIO_readOutData, 0, 0) @[CompressedSparseColumnSpecTest.scala 146:76]
    node iactDataCountVec_1 = bits(iactDataSPad.io_commonIO_readOutData, 1, 1) @[CompressedSparseColumnSpecTest.scala 146:76]
    node iactDataCountVec_2 = bits(iactDataSPad.io_commonIO_readOutData, 2, 2) @[CompressedSparseColumnSpecTest.scala 146:76]
    node iactDataCountVec_3 = bits(iactDataSPad.io_commonIO_readOutData, 3, 3) @[CompressedSparseColumnSpecTest.scala 146:76]
    node iactDataCountVec_4 = bits(iactDataSPad.io_commonIO_readOutData, 4, 4) @[CompressedSparseColumnSpecTest.scala 146:76]
    node iactDataCountVec_5 = bits(iactDataSPad.io_commonIO_readOutData, 5, 5) @[CompressedSparseColumnSpecTest.scala 146:76]
    node iactDataCountVec_6 = bits(iactDataSPad.io_commonIO_readOutData, 6, 6) @[CompressedSparseColumnSpecTest.scala 146:76]
    node iactDataCountVec_7 = bits(iactDataSPad.io_commonIO_readOutData, 7, 7) @[CompressedSparseColumnSpecTest.scala 146:76]
    node iactDataCountVec_8 = bits(iactDataSPad.io_commonIO_readOutData, 8, 8) @[CompressedSparseColumnSpecTest.scala 146:76]
    node iactDataCountVec_9 = bits(iactDataSPad.io_commonIO_readOutData, 9, 9) @[CompressedSparseColumnSpecTest.scala 146:76]
    node iactDataCountVec_10 = bits(iactDataSPad.io_commonIO_readOutData, 10, 10) @[CompressedSparseColumnSpecTest.scala 146:76]
    node iactDataCountVec_11 = bits(iactDataSPad.io_commonIO_readOutData, 11, 11) @[CompressedSparseColumnSpecTest.scala 146:76]
    node _T_22 = cat(iactDataCountVec_5, iactDataCountVec_4) @[Cat.scala 29:58]
    node _T_23 = cat(iactDataCountVec_7, iactDataCountVec_6) @[Cat.scala 29:58]
    node _T_24 = cat(_T_23, _T_22) @[Cat.scala 29:58]
    node _T_25 = cat(iactDataCountVec_9, iactDataCountVec_8) @[Cat.scala 29:58]
    node _T_26 = cat(iactDataCountVec_11, iactDataCountVec_10) @[Cat.scala 29:58]
    node _T_27 = cat(_T_26, _T_25) @[Cat.scala 29:58]
    node _T_28 = cat(_T_27, _T_24) @[Cat.scala 29:58]
    node _T_29 = cat(iactDataCountVec_1, iactDataCountVec_0) @[Cat.scala 29:58]
    node _T_30 = cat(iactDataCountVec_3, iactDataCountVec_2) @[Cat.scala 29:58]
    node _T_31 = cat(_T_30, _T_29) @[Cat.scala 29:58]
    node _T_32 = eq(UInt<2>("h0"), sPad) @[Conditional.scala 37:30]
    node _GEN_1 = mux(io_iactDataReq, UInt<2>("h1"), sPad) @[CompressedSparseColumnSpecTest.scala 159:30]
    node _GEN_2 = mux(io_iactDataReq, UInt<1>("h1"), _GEN_0) @[CompressedSparseColumnSpecTest.scala 159:30]
    node _GEN_3 = mux(io_iactDataReq, UInt<1>("h0"), iactDataSPadReadEnReg) @[CompressedSparseColumnSpecTest.scala 159:30]
    node _GEN_4 = mux(io_iactDataReq, UInt<1>("h1"), iactDataSPadFirstRead) @[CompressedSparseColumnSpecTest.scala 159:30]
    node _T_33 = eq(UInt<2>("h1"), sPad) @[Conditional.scala 37:30]
    node _T_34 = eq(iactAddrDataWire, UInt<4>("hf")) @[CompressedSparseColumnSpecTest.scala 168:32]
    node _T_35 = add(iactZeroColumnNumber, UInt<1>("h1")) @[CompressedSparseColumnSpecTest.scala 173:56]
    node _T_36 = tail(_T_35, 1) @[CompressedSparseColumnSpecTest.scala 173:56]
    node _GEN_5 = mux(_T_34, UInt<2>("h1"), UInt<2>("h2")) @[CompressedSparseColumnSpecTest.scala 168:54]
    node _GEN_6 = mux(_T_34, UInt<1>("h1"), iactSPadZeroColumnReg) @[CompressedSparseColumnSpecTest.scala 168:54]
    node _GEN_7 = mux(_T_34, UInt<1>("h1"), UInt<1>("h0")) @[CompressedSparseColumnSpecTest.scala 168:54]
    node _GEN_8 = mux(_T_34, UInt<1>("h0"), UInt<1>("h1")) @[CompressedSparseColumnSpecTest.scala 168:54]
    node _GEN_9 = mux(_T_34, _T_36, iactZeroColumnNumber) @[CompressedSparseColumnSpecTest.scala 168:54]
    node _T_37 = eq(UInt<2>("h2"), sPad) @[Conditional.scala 37:30]
    node _T_38 = add(iactMatrixColumnReg, UInt<1>("h1")) @[CompressedSparseColumnSpecTest.scala 190:56]
    node _T_39 = tail(_T_38, 1) @[CompressedSparseColumnSpecTest.scala 190:56]
    node _T_40 = add(_T_39, iactZeroColumnNumber) @[CompressedSparseColumnSpecTest.scala 190:62]
    node _T_41 = tail(_T_40, 1) @[CompressedSparseColumnSpecTest.scala 190:62]
    node _T_42 = add(iactMatrixColumnReg, UInt<1>("h1")) @[CompressedSparseColumnSpecTest.scala 195:56]
    node _T_43 = tail(_T_42, 1) @[CompressedSparseColumnSpecTest.scala 195:56]
    node _GEN_10 = mux(iactSPadZeroColumnReg, _T_41, _T_43) @[CompressedSparseColumnSpecTest.scala 189:40]
    node _GEN_11 = mux(iactSPadZeroColumnReg, UInt<1>("h0"), iactZeroColumnNumber) @[CompressedSparseColumnSpecTest.scala 189:40]
    node _GEN_12 = mux(iactAddrSPadIdxIncWire, UInt<2>("h1"), UInt<2>("h2")) @[CompressedSparseColumnSpecTest.scala 183:39]
    node _GEN_13 = mux(iactAddrSPadIdxIncWire, UInt<1>("h0"), iactSPadZeroColumnReg) @[CompressedSparseColumnSpecTest.scala 183:39]
    node _GEN_14 = mux(iactAddrSPadIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[CompressedSparseColumnSpecTest.scala 183:39]
    node _GEN_15 = mux(iactAddrSPadIdxIncWire, UInt<1>("h0"), UInt<1>("h1")) @[CompressedSparseColumnSpecTest.scala 183:39]
    node _GEN_16 = mux(iactAddrSPadIdxIncWire, _GEN_10, iactMatrixColumnReg) @[CompressedSparseColumnSpecTest.scala 183:39]
    node _GEN_17 = mux(iactAddrSPadIdxIncWire, _GEN_11, iactZeroColumnNumber) @[CompressedSparseColumnSpecTest.scala 183:39]
    node _T_44 = eq(iactAddrDataWire, iactDataIndexWire) @[CompressedSparseColumnSpecTest.scala 202:51]
    node _GEN_18 = mux(_T_37, UInt<1>("h0"), iactDataSPadFirstRead) @[Conditional.scala 39:67]
    node _GEN_19 = mux(_T_37, _GEN_12, sPad) @[Conditional.scala 39:67]
    node _GEN_20 = mux(_T_37, _GEN_13, iactSPadZeroColumnReg) @[Conditional.scala 39:67]
    node _GEN_21 = mux(_T_37, _T_44, _GEN_0) @[Conditional.scala 39:67]
    node _GEN_22 = mux(_T_37, _GEN_15, iactDataSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_23 = mux(_T_37, _GEN_16, iactMatrixColumnReg) @[Conditional.scala 39:67]
    node _GEN_24 = mux(_T_37, _GEN_17, iactZeroColumnNumber) @[Conditional.scala 39:67]
    node _GEN_25 = mux(_T_33, _GEN_5, _GEN_19) @[Conditional.scala 39:67]
    node _GEN_26 = mux(_T_33, _GEN_6, _GEN_20) @[Conditional.scala 39:67]
    node _GEN_27 = mux(_T_33, _GEN_7, _GEN_21) @[Conditional.scala 39:67]
    node _GEN_28 = mux(_T_33, _GEN_8, _GEN_22) @[Conditional.scala 39:67]
    node _GEN_29 = mux(_T_33, _GEN_9, _GEN_24) @[Conditional.scala 39:67]
    node _GEN_30 = mux(_T_33, iactDataSPadFirstRead, _GEN_18) @[Conditional.scala 39:67]
    node _GEN_31 = mux(_T_33, iactMatrixColumnReg, _GEN_23) @[Conditional.scala 39:67]
    node _GEN_32 = mux(_T_32, _GEN_1, _GEN_25) @[Conditional.scala 40:58]
    node _GEN_33 = mux(_T_32, _GEN_2, _GEN_27) @[Conditional.scala 40:58]
    node _GEN_34 = mux(_T_32, _GEN_3, _GEN_28) @[Conditional.scala 40:58]
    node _GEN_35 = mux(_T_32, _GEN_4, _GEN_30) @[Conditional.scala 40:58]
    node _GEN_36 = mux(_T_32, iactSPadZeroColumnReg, _GEN_26) @[Conditional.scala 40:58]
    node _GEN_37 = mux(_T_32, iactZeroColumnNumber, _GEN_29) @[Conditional.scala 40:58]
    node _GEN_38 = mux(_T_32, iactMatrixColumnReg, _GEN_31) @[Conditional.scala 40:58]
    node iactAddrIndexWire = iactAddrSPad.io_commonIO_columnNum @[CompressedSparseColumnSpecTest.scala 113:39 CompressedSparseColumnSpecTest.scala 134:23]
    node iactDataSPadIdxIncWire = _T_17 @[CompressedSparseColumnSpecTest.scala 120:44 CompressedSparseColumnSpecTest.scala 127:28]
    io_iactIOs_dataIOs_streamDecoupledDataIO_ready <= iactDataSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_ready @[CompressedSparseColumnSpecTest.scala 130:43]
    io_iactIOs_dataIOs_writeFin <= iactDataSPad.io_commonIO_dataLenFinIO_writeFin @[CompressedSparseColumnSpecTest.scala 130:43]
    io_iactIOs_addrIOs_streamDecoupledDataIO_ready <= iactAddrSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_ready @[CompressedSparseColumnSpecTest.scala 129:43]
    io_iactIOs_addrIOs_writeFin <= iactAddrSPad.io_commonIO_dataLenFinIO_writeFin @[CompressedSparseColumnSpecTest.scala 129:43]
    io_iactAddrWriteIdx <= iactAddrSPad.io_commonIO_writeIdx @[CompressedSparseColumnSpecTest.scala 131:25]
    io_iactMatrixColumn <= iactMatrixColumnReg @[CompressedSparseColumnSpecTest.scala 135:25]
    io_iactMatrixRow <= _T_31 @[CompressedSparseColumnSpecTest.scala 148:22]
    io_iactMatrixData <= _T_28 @[CompressedSparseColumnSpecTest.scala 147:23]
    io_iactMatrixDataBin <= iactDataSPad.io_commonIO_readOutData @[CompressedSparseColumnSpecTest.scala 145:26]
    io_iactAddrReadEn <= iactAddrSPadReadEnReg @[CompressedSparseColumnSpecTest.scala 139:23]
    io_iactAddrReadData <= iactAddrDataWire @[CompressedSparseColumnSpecTest.scala 137:25]
    io_iactDataReadIndex <= iactDataIndexWire @[CompressedSparseColumnSpecTest.scala 144:26]
    io_iactDataWriteIdx <= iactDataSPad.io_commonIO_writeIdx @[CompressedSparseColumnSpecTest.scala 132:25]
    iactAddrSPad.clock <= clock
    iactAddrSPad.reset <= reset
    iactAddrSPad.io_commonIO_readEn <= iactAddrSPadReadEnReg @[CompressedSparseColumnSpecTest.scala 138:37]
    iactAddrSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid <= io_iactIOs_addrIOs_streamDecoupledDataIO_valid @[CompressedSparseColumnSpecTest.scala 129:43]
    iactAddrSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_bits_data <= io_iactIOs_addrIOs_streamDecoupledDataIO_bits_data @[CompressedSparseColumnSpecTest.scala 129:43]
    iactAddrSPad.io_commonIO_dataLenFinIO_streamLen <= io_iactIOs_addrIOs_streamLen @[CompressedSparseColumnSpecTest.scala 129:43]
    iactAddrSPad.io_addrIO_readInIdx is invalid
    iactAddrSPad.io_addrIO_indexInc <= iactAddrSPadIdxIncWire @[CompressedSparseColumnSpecTest.scala 124:37]
    iactAddrSPad.io_dataIO_readInIdx is invalid
    iactAddrSPad.io_dataIO_indexInc is invalid
    iactDataSPad.clock <= clock
    iactDataSPad.reset <= reset
    iactDataSPad.io_commonIO_readEn <= iactDataSPadReadEnReg @[CompressedSparseColumnSpecTest.scala 149:37 CompressedSparseColumnSpecTest.scala 150:37]
    iactDataSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_valid <= io_iactIOs_dataIOs_streamDecoupledDataIO_valid @[CompressedSparseColumnSpecTest.scala 130:43]
    iactDataSPad.io_commonIO_dataLenFinIO_streamDecoupledDataIO_bits_data <= io_iactIOs_dataIOs_streamDecoupledDataIO_bits_data @[CompressedSparseColumnSpecTest.scala 130:43]
    iactDataSPad.io_commonIO_dataLenFinIO_streamLen <= io_iactIOs_dataIOs_streamLen @[CompressedSparseColumnSpecTest.scala 130:43]
    iactDataSPad.io_addrIO_readInIdx is invalid
    iactDataSPad.io_addrIO_indexInc is invalid
    iactDataSPad.io_dataIO_readInIdx is invalid
    iactDataSPad.io_dataIO_indexInc <= iactDataSPadIdxIncWire @[CompressedSparseColumnSpecTest.scala 125:37]
    sPad <= mux(reset, UInt<2>("h0"), _GEN_32) @[CompressedSparseColumnSpecTest.scala 160:16 CompressedSparseColumnSpecTest.scala 169:16 CompressedSparseColumnSpecTest.scala 175:16 CompressedSparseColumnSpecTest.scala 182:14 CompressedSparseColumnSpecTest.scala 184:16 CompressedSparseColumnSpecTest.scala 198:16]
    iactSPadZeroColumnReg <= mux(reset, UInt<1>("h0"), _GEN_36) @[CompressedSparseColumnSpecTest.scala 170:33 CompressedSparseColumnSpecTest.scala 185:33]
    iactAddrSPadReadEnReg <= _GEN_33 @[Reg.scala 16:23 CompressedSparseColumnSpecTest.scala 161:33 CompressedSparseColumnSpecTest.scala 171:33 CompressedSparseColumnSpecTest.scala 176:33 CompressedSparseColumnSpecTest.scala 187:33 CompressedSparseColumnSpecTest.scala 199:33 CompressedSparseColumnSpecTest.scala 202:31]
    iactDataSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_34) @[CompressedSparseColumnSpecTest.scala 162:33 CompressedSparseColumnSpecTest.scala 172:33 CompressedSparseColumnSpecTest.scala 177:33 CompressedSparseColumnSpecTest.scala 188:33 CompressedSparseColumnSpecTest.scala 200:33]
    iactMatrixColumnReg <= mux(reset, UInt<4>("h0"), _GEN_38) @[CompressedSparseColumnSpecTest.scala 190:33 CompressedSparseColumnSpecTest.scala 195:33]
    iactZeroColumnNumber <= mux(reset, UInt<4>("h0"), _GEN_37) @[CompressedSparseColumnSpecTest.scala 173:32 CompressedSparseColumnSpecTest.scala 192:34]
    iactDataSPadFirstRead <= mux(reset, UInt<1>("h1"), _GEN_35) @[CompressedSparseColumnSpecTest.scala 163:33 CompressedSparseColumnSpecTest.scala 181:31]
