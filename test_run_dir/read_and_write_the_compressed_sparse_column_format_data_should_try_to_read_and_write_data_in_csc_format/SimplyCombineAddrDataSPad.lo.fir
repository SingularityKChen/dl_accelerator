circuit SimplyCombineAddrDataSPad :
  module SPadAddrModule :
    input clock : Clock
    input reset : UInt<1>
    output io_commonIO_columnNum : UInt<4>
    output io_commonIO_readOutData : UInt<4>
    input io_commonIO_readEn : UInt<1>
    output io_commonIO_writeIdx : UInt<4>
    output io_commonIO_dataLenFinIO_writeInDataIO_ready : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_valid : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_bits_data : UInt<4>
    input io_commonIO_dataLenFinIO_streamLen : UInt<4>
    output io_commonIO_dataLenFinIO_writeFin : UInt<1>
    input io_addrIO_readInIdx : UInt<4>
    input io_addrIO_indexInc : UInt<1>
    input io_dataIO_readInIdx : UInt<4>
    input io_dataIO_indexInc : UInt<1>
  
    reg dataLenReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[ProcessingElement.scala 397:33]
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[ProcessingElement.scala 400:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[ProcessingElement.scala 401:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 406:53]
    node _T_1 = tail(_T, 1) @[ProcessingElement.scala 406:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[ProcessingElement.scala 406:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 407:51]
    node _T_4 = tail(_T_3, 1) @[ProcessingElement.scala 407:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[ProcessingElement.scala 407:35]
    node _T_6 = add(padWriteIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 411:42]
    node _T_7 = tail(_T_6, 1) @[ProcessingElement.scala 411:42]
    node writeWrapWire = _T_2 @[ProcessingElement.scala 403:33 ProcessingElement.scala 406:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_7) @[ProcessingElement.scala 412:26]
    node _GEN_1 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 409:32]
    node _GEN_2 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_0, padWriteIndexReg) @[ProcessingElement.scala 409:32]
    node _T_8 = add(padReadIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 420:40]
    node _T_9 = tail(_T_8, 1) @[ProcessingElement.scala 420:40]
    node readWrapWire = _T_5 @[ProcessingElement.scala 404:32 ProcessingElement.scala 407:16]
    node _GEN_3 = mux(readWrapWire, UInt<1>("h0"), _T_9) @[ProcessingElement.scala 421:25]
    node readIndexInc = io_addrIO_indexInc @[ProcessingElement.scala 405:32 ProcessingElement.scala 365:16]
    node _GEN_4 = mux(readIndexInc, _GEN_3, padReadIndexReg) @[ProcessingElement.scala 419:23]
    node _T_10 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, writeWrapWire) @[ProcessingElement.scala 427:62]
    reg addrSPad_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_0) @[ProcessingElement.scala 357:36]
    reg addrSPad_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_1) @[ProcessingElement.scala 357:36]
    reg addrSPad_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_2) @[ProcessingElement.scala 357:36]
    reg addrSPad_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_3) @[ProcessingElement.scala 357:36]
    reg addrSPad_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_4) @[ProcessingElement.scala 357:36]
    reg addrSPad_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_5) @[ProcessingElement.scala 357:36]
    reg addrSPad_6 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_6) @[ProcessingElement.scala 357:36]
    reg addrSPad_7 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_7) @[ProcessingElement.scala 357:36]
    reg addrSPad_8 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_8) @[ProcessingElement.scala 357:36]
    reg addrSPad_9 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_9) @[ProcessingElement.scala 357:36]
    reg addrSPad_10 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_10) @[ProcessingElement.scala 357:36]
    reg addrSPad_11 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_11) @[ProcessingElement.scala 357:36]
    reg addrSPad_12 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_12) @[ProcessingElement.scala 357:36]
    reg addrSPad_13 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_13) @[ProcessingElement.scala 357:36]
    reg addrSPad_14 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_14) @[ProcessingElement.scala 357:36]
    node _addrSPad_padWriteIndexReg = io_commonIO_dataLenFinIO_writeInDataIO_bits_data @[ProcessingElement.scala 360:32 ProcessingElement.scala 360:32]
    node _GEN_5 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_0) @[ProcessingElement.scala 360:32]
    node _GEN_6 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_1) @[ProcessingElement.scala 360:32]
    node _GEN_7 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_2) @[ProcessingElement.scala 360:32]
    node _GEN_8 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_3) @[ProcessingElement.scala 360:32]
    node _GEN_9 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_4) @[ProcessingElement.scala 360:32]
    node _GEN_10 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_5) @[ProcessingElement.scala 360:32]
    node _GEN_11 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_6) @[ProcessingElement.scala 360:32]
    node _GEN_12 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_7) @[ProcessingElement.scala 360:32]
    node _GEN_13 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_8) @[ProcessingElement.scala 360:32]
    node _GEN_14 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_9) @[ProcessingElement.scala 360:32]
    node _GEN_15 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_10) @[ProcessingElement.scala 360:32]
    node _GEN_16 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_11) @[ProcessingElement.scala 360:32]
    node _GEN_17 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_12) @[ProcessingElement.scala 360:32]
    node _GEN_18 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_13) @[ProcessingElement.scala 360:32]
    node _GEN_19 = mux(eq(UInt<4>("he"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_14) @[ProcessingElement.scala 360:32]
    node _GEN_20 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_5, addrSPad_0) @[ProcessingElement.scala 359:32]
    node _GEN_21 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_6, addrSPad_1) @[ProcessingElement.scala 359:32]
    node _GEN_22 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_7, addrSPad_2) @[ProcessingElement.scala 359:32]
    node _GEN_23 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_8, addrSPad_3) @[ProcessingElement.scala 359:32]
    node _GEN_24 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_9, addrSPad_4) @[ProcessingElement.scala 359:32]
    node _GEN_25 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_10, addrSPad_5) @[ProcessingElement.scala 359:32]
    node _GEN_26 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_11, addrSPad_6) @[ProcessingElement.scala 359:32]
    node _GEN_27 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_12, addrSPad_7) @[ProcessingElement.scala 359:32]
    node _GEN_28 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_13, addrSPad_8) @[ProcessingElement.scala 359:32]
    node _GEN_29 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_14, addrSPad_9) @[ProcessingElement.scala 359:32]
    node _GEN_30 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_15, addrSPad_10) @[ProcessingElement.scala 359:32]
    node _GEN_31 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_16, addrSPad_11) @[ProcessingElement.scala 359:32]
    node _GEN_32 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_17, addrSPad_12) @[ProcessingElement.scala 359:32]
    node _GEN_33 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_18, addrSPad_13) @[ProcessingElement.scala 359:32]
    node _GEN_34 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_19, addrSPad_14) @[ProcessingElement.scala 359:32]
    node _GEN_35 = validif(eq(UInt<1>("h0"), padReadIndexReg), addrSPad_0) @[ProcessingElement.scala 363:12]
    node _GEN_36 = mux(eq(UInt<1>("h1"), padReadIndexReg), addrSPad_1, _GEN_35) @[ProcessingElement.scala 363:12]
    node _GEN_37 = mux(eq(UInt<2>("h2"), padReadIndexReg), addrSPad_2, _GEN_36) @[ProcessingElement.scala 363:12]
    node _GEN_38 = mux(eq(UInt<2>("h3"), padReadIndexReg), addrSPad_3, _GEN_37) @[ProcessingElement.scala 363:12]
    node _GEN_39 = mux(eq(UInt<3>("h4"), padReadIndexReg), addrSPad_4, _GEN_38) @[ProcessingElement.scala 363:12]
    node _GEN_40 = mux(eq(UInt<3>("h5"), padReadIndexReg), addrSPad_5, _GEN_39) @[ProcessingElement.scala 363:12]
    node _GEN_41 = mux(eq(UInt<3>("h6"), padReadIndexReg), addrSPad_6, _GEN_40) @[ProcessingElement.scala 363:12]
    node _GEN_42 = mux(eq(UInt<3>("h7"), padReadIndexReg), addrSPad_7, _GEN_41) @[ProcessingElement.scala 363:12]
    node _GEN_43 = mux(eq(UInt<4>("h8"), padReadIndexReg), addrSPad_8, _GEN_42) @[ProcessingElement.scala 363:12]
    node _GEN_44 = mux(eq(UInt<4>("h9"), padReadIndexReg), addrSPad_9, _GEN_43) @[ProcessingElement.scala 363:12]
    node _GEN_45 = mux(eq(UInt<4>("ha"), padReadIndexReg), addrSPad_10, _GEN_44) @[ProcessingElement.scala 363:12]
    node _GEN_46 = mux(eq(UInt<4>("hb"), padReadIndexReg), addrSPad_11, _GEN_45) @[ProcessingElement.scala 363:12]
    node _GEN_47 = mux(eq(UInt<4>("hc"), padReadIndexReg), addrSPad_12, _GEN_46) @[ProcessingElement.scala 363:12]
    node _GEN_48 = mux(eq(UInt<4>("hd"), padReadIndexReg), addrSPad_13, _GEN_47) @[ProcessingElement.scala 363:12]
    node _GEN_49 = mux(eq(UInt<4>("he"), padReadIndexReg), addrSPad_14, _GEN_48) @[ProcessingElement.scala 363:12]
    node _addrSPad_padReadIndexReg = _GEN_49 @[ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12 ProcessingElement.scala 363:12]
    node dataWire = _addrSPad_padReadIndexReg @[ProcessingElement.scala 398:28 ProcessingElement.scala 363:12]
    node _T_11_0 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_1 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_2 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_3 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_4 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_5 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_6 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_7 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_8 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_9 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_10 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_11 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_12 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_13 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    node _T_11_14 = UInt<4>("h0") @[ProcessingElement.scala 357:44 ProcessingElement.scala 357:44]
    io_commonIO_columnNum <= padReadIndexReg @[ProcessingElement.scala 428:25]
    io_commonIO_readOutData <= dataWire @[ProcessingElement.scala 364:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[ProcessingElement.scala 426:24]
    io_commonIO_dataLenFinIO_writeInDataIO_ready <= _GEN_1 @[ProcessingElement.scala 410:27 ProcessingElement.scala 416:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_10 @[ProcessingElement.scala 427:37]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[ProcessingElement.scala 399:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[ProcessingElement.scala 411:22 ProcessingElement.scala 413:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_4) @[ProcessingElement.scala 420:21 ProcessingElement.scala 422:23]
    addrSPad_0 <= mux(reset, _T_11_0, _GEN_20) @[ProcessingElement.scala 360:32]
    addrSPad_1 <= mux(reset, _T_11_1, _GEN_21) @[ProcessingElement.scala 360:32]
    addrSPad_2 <= mux(reset, _T_11_2, _GEN_22) @[ProcessingElement.scala 360:32]
    addrSPad_3 <= mux(reset, _T_11_3, _GEN_23) @[ProcessingElement.scala 360:32]
    addrSPad_4 <= mux(reset, _T_11_4, _GEN_24) @[ProcessingElement.scala 360:32]
    addrSPad_5 <= mux(reset, _T_11_5, _GEN_25) @[ProcessingElement.scala 360:32]
    addrSPad_6 <= mux(reset, _T_11_6, _GEN_26) @[ProcessingElement.scala 360:32]
    addrSPad_7 <= mux(reset, _T_11_7, _GEN_27) @[ProcessingElement.scala 360:32]
    addrSPad_8 <= mux(reset, _T_11_8, _GEN_28) @[ProcessingElement.scala 360:32]
    addrSPad_9 <= mux(reset, _T_11_9, _GEN_29) @[ProcessingElement.scala 360:32]
    addrSPad_10 <= mux(reset, _T_11_10, _GEN_30) @[ProcessingElement.scala 360:32]
    addrSPad_11 <= mux(reset, _T_11_11, _GEN_31) @[ProcessingElement.scala 360:32]
    addrSPad_12 <= mux(reset, _T_11_12, _GEN_32) @[ProcessingElement.scala 360:32]
    addrSPad_13 <= mux(reset, _T_11_13, _GEN_33) @[ProcessingElement.scala 360:32]
    addrSPad_14 <= mux(reset, _T_11_14, _GEN_34) @[ProcessingElement.scala 360:32]

  module SPadDataModule :
    input clock : Clock
    input reset : UInt<1>
    output io_commonIO_columnNum : UInt<4>
    output io_commonIO_readOutData : UInt<12>
    input io_commonIO_readEn : UInt<1>
    output io_commonIO_writeIdx : UInt<4>
    output io_commonIO_dataLenFinIO_writeInDataIO_ready : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_valid : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_bits_data : UInt<12>
    input io_commonIO_dataLenFinIO_streamLen : UInt<4>
    output io_commonIO_dataLenFinIO_writeFin : UInt<1>
    input io_addrIO_readInIdx : UInt<4>
    input io_addrIO_indexInc : UInt<1>
    input io_dataIO_readInIdx : UInt<4>
    input io_dataIO_indexInc : UInt<1>
  
    reg dataLenReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[ProcessingElement.scala 397:33]
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[ProcessingElement.scala 400:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[ProcessingElement.scala 401:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 406:53]
    node _T_1 = tail(_T, 1) @[ProcessingElement.scala 406:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[ProcessingElement.scala 406:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 407:51]
    node _T_4 = tail(_T_3, 1) @[ProcessingElement.scala 407:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[ProcessingElement.scala 407:35]
    node _T_6 = add(padWriteIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 411:42]
    node _T_7 = tail(_T_6, 1) @[ProcessingElement.scala 411:42]
    node writeWrapWire = _T_2 @[ProcessingElement.scala 403:33 ProcessingElement.scala 406:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_7) @[ProcessingElement.scala 412:26]
    node _GEN_1 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 409:32]
    node _GEN_2 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_0, padWriteIndexReg) @[ProcessingElement.scala 409:32]
    node _T_8 = add(padReadIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 420:40]
    node _T_9 = tail(_T_8, 1) @[ProcessingElement.scala 420:40]
    node readWrapWire = _T_5 @[ProcessingElement.scala 404:32 ProcessingElement.scala 407:16]
    node _GEN_3 = mux(readWrapWire, UInt<1>("h0"), _T_9) @[ProcessingElement.scala 421:25]
    node readIndexInc = io_dataIO_indexInc @[ProcessingElement.scala 405:32 ProcessingElement.scala 386:16]
    node _GEN_4 = mux(readIndexInc, _GEN_3, padReadIndexReg) @[ProcessingElement.scala 419:23]
    node _T_10 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, writeWrapWire) @[ProcessingElement.scala 427:62]
    reg _T_12_0 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_0) @[ProcessingElement.scala 379:38]
    reg _T_12_1 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_1) @[ProcessingElement.scala 379:38]
    reg _T_12_2 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_2) @[ProcessingElement.scala 379:38]
    reg _T_12_3 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_3) @[ProcessingElement.scala 379:38]
    reg _T_12_4 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_4) @[ProcessingElement.scala 379:38]
    reg _T_12_5 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_5) @[ProcessingElement.scala 379:38]
    reg _T_12_6 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_6) @[ProcessingElement.scala 379:38]
    reg _T_12_7 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_7) @[ProcessingElement.scala 379:38]
    reg _T_12_8 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_8) @[ProcessingElement.scala 379:38]
    reg _T_12_9 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_9) @[ProcessingElement.scala 379:38]
    reg _T_12_10 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_10) @[ProcessingElement.scala 379:38]
    reg _T_12_11 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_11) @[ProcessingElement.scala 379:38]
    reg _T_12_12 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_12) @[ProcessingElement.scala 379:38]
    reg _T_12_13 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_13) @[ProcessingElement.scala 379:38]
    reg _T_12_14 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_14) @[ProcessingElement.scala 379:38]
    reg _T_12_15 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_12_15) @[ProcessingElement.scala 379:38]
    node _T_12_padWriteIndexReg = io_commonIO_dataLenFinIO_writeInDataIO_bits_data @[ProcessingElement.scala 381:34 ProcessingElement.scala 381:34]
    node _GEN_5 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_0) @[ProcessingElement.scala 381:34]
    node _GEN_6 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_1) @[ProcessingElement.scala 381:34]
    node _GEN_7 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_2) @[ProcessingElement.scala 381:34]
    node _GEN_8 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_3) @[ProcessingElement.scala 381:34]
    node _GEN_9 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_4) @[ProcessingElement.scala 381:34]
    node _GEN_10 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_5) @[ProcessingElement.scala 381:34]
    node _GEN_11 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_6) @[ProcessingElement.scala 381:34]
    node _GEN_12 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_7) @[ProcessingElement.scala 381:34]
    node _GEN_13 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_8) @[ProcessingElement.scala 381:34]
    node _GEN_14 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_9) @[ProcessingElement.scala 381:34]
    node _GEN_15 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_10) @[ProcessingElement.scala 381:34]
    node _GEN_16 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_11) @[ProcessingElement.scala 381:34]
    node _GEN_17 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_12) @[ProcessingElement.scala 381:34]
    node _GEN_18 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_13) @[ProcessingElement.scala 381:34]
    node _GEN_19 = mux(eq(UInt<4>("he"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_14) @[ProcessingElement.scala 381:34]
    node _GEN_20 = mux(eq(UInt<4>("hf"), padWriteIndexReg), _T_12_padWriteIndexReg, _T_12_15) @[ProcessingElement.scala 381:34]
    node _GEN_21 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_5, _T_12_0) @[ProcessingElement.scala 380:34]
    node _GEN_22 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_6, _T_12_1) @[ProcessingElement.scala 380:34]
    node _GEN_23 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_7, _T_12_2) @[ProcessingElement.scala 380:34]
    node _GEN_24 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_8, _T_12_3) @[ProcessingElement.scala 380:34]
    node _GEN_25 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_9, _T_12_4) @[ProcessingElement.scala 380:34]
    node _GEN_26 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_10, _T_12_5) @[ProcessingElement.scala 380:34]
    node _GEN_27 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_11, _T_12_6) @[ProcessingElement.scala 380:34]
    node _GEN_28 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_12, _T_12_7) @[ProcessingElement.scala 380:34]
    node _GEN_29 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_13, _T_12_8) @[ProcessingElement.scala 380:34]
    node _GEN_30 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_14, _T_12_9) @[ProcessingElement.scala 380:34]
    node _GEN_31 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_15, _T_12_10) @[ProcessingElement.scala 380:34]
    node _GEN_32 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_16, _T_12_11) @[ProcessingElement.scala 380:34]
    node _GEN_33 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_17, _T_12_12) @[ProcessingElement.scala 380:34]
    node _GEN_34 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_18, _T_12_13) @[ProcessingElement.scala 380:34]
    node _GEN_35 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_19, _T_12_14) @[ProcessingElement.scala 380:34]
    node _GEN_36 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_20, _T_12_15) @[ProcessingElement.scala 380:34]
    node _GEN_37 = validif(eq(UInt<1>("h0"), padReadIndexReg), _T_12_0) @[ProcessingElement.scala 384:14]
    node _GEN_38 = mux(eq(UInt<1>("h1"), padReadIndexReg), _T_12_1, _GEN_37) @[ProcessingElement.scala 384:14]
    node _GEN_39 = mux(eq(UInt<2>("h2"), padReadIndexReg), _T_12_2, _GEN_38) @[ProcessingElement.scala 384:14]
    node _GEN_40 = mux(eq(UInt<2>("h3"), padReadIndexReg), _T_12_3, _GEN_39) @[ProcessingElement.scala 384:14]
    node _GEN_41 = mux(eq(UInt<3>("h4"), padReadIndexReg), _T_12_4, _GEN_40) @[ProcessingElement.scala 384:14]
    node _GEN_42 = mux(eq(UInt<3>("h5"), padReadIndexReg), _T_12_5, _GEN_41) @[ProcessingElement.scala 384:14]
    node _GEN_43 = mux(eq(UInt<3>("h6"), padReadIndexReg), _T_12_6, _GEN_42) @[ProcessingElement.scala 384:14]
    node _GEN_44 = mux(eq(UInt<3>("h7"), padReadIndexReg), _T_12_7, _GEN_43) @[ProcessingElement.scala 384:14]
    node _GEN_45 = mux(eq(UInt<4>("h8"), padReadIndexReg), _T_12_8, _GEN_44) @[ProcessingElement.scala 384:14]
    node _GEN_46 = mux(eq(UInt<4>("h9"), padReadIndexReg), _T_12_9, _GEN_45) @[ProcessingElement.scala 384:14]
    node _GEN_47 = mux(eq(UInt<4>("ha"), padReadIndexReg), _T_12_10, _GEN_46) @[ProcessingElement.scala 384:14]
    node _GEN_48 = mux(eq(UInt<4>("hb"), padReadIndexReg), _T_12_11, _GEN_47) @[ProcessingElement.scala 384:14]
    node _GEN_49 = mux(eq(UInt<4>("hc"), padReadIndexReg), _T_12_12, _GEN_48) @[ProcessingElement.scala 384:14]
    node _GEN_50 = mux(eq(UInt<4>("hd"), padReadIndexReg), _T_12_13, _GEN_49) @[ProcessingElement.scala 384:14]
    node _GEN_51 = mux(eq(UInt<4>("he"), padReadIndexReg), _T_12_14, _GEN_50) @[ProcessingElement.scala 384:14]
    node _GEN_52 = mux(eq(UInt<4>("hf"), padReadIndexReg), _T_12_15, _GEN_51) @[ProcessingElement.scala 384:14]
    node _T_12_padReadIndexReg = _GEN_52 @[ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14 ProcessingElement.scala 384:14]
    node dataWire = _T_12_padReadIndexReg @[ProcessingElement.scala 398:28 ProcessingElement.scala 384:14]
    node _T_11_0 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_1 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_2 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_3 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_4 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_5 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_6 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_7 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_8 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_9 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_10 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_11 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_12 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_13 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_14 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    node _T_11_15 = UInt<12>("h0") @[ProcessingElement.scala 379:46 ProcessingElement.scala 379:46]
    io_commonIO_columnNum <= padReadIndexReg @[ProcessingElement.scala 428:25]
    io_commonIO_readOutData <= dataWire @[ProcessingElement.scala 387:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[ProcessingElement.scala 426:24]
    io_commonIO_dataLenFinIO_writeInDataIO_ready <= _GEN_1 @[ProcessingElement.scala 410:27 ProcessingElement.scala 416:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_10 @[ProcessingElement.scala 427:37]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[ProcessingElement.scala 399:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[ProcessingElement.scala 411:22 ProcessingElement.scala 413:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_4) @[ProcessingElement.scala 420:21 ProcessingElement.scala 422:23]
    _T_12_0 <= mux(reset, _T_11_0, _GEN_21) @[ProcessingElement.scala 381:34]
    _T_12_1 <= mux(reset, _T_11_1, _GEN_22) @[ProcessingElement.scala 381:34]
    _T_12_2 <= mux(reset, _T_11_2, _GEN_23) @[ProcessingElement.scala 381:34]
    _T_12_3 <= mux(reset, _T_11_3, _GEN_24) @[ProcessingElement.scala 381:34]
    _T_12_4 <= mux(reset, _T_11_4, _GEN_25) @[ProcessingElement.scala 381:34]
    _T_12_5 <= mux(reset, _T_11_5, _GEN_26) @[ProcessingElement.scala 381:34]
    _T_12_6 <= mux(reset, _T_11_6, _GEN_27) @[ProcessingElement.scala 381:34]
    _T_12_7 <= mux(reset, _T_11_7, _GEN_28) @[ProcessingElement.scala 381:34]
    _T_12_8 <= mux(reset, _T_11_8, _GEN_29) @[ProcessingElement.scala 381:34]
    _T_12_9 <= mux(reset, _T_11_9, _GEN_30) @[ProcessingElement.scala 381:34]
    _T_12_10 <= mux(reset, _T_11_10, _GEN_31) @[ProcessingElement.scala 381:34]
    _T_12_11 <= mux(reset, _T_11_11, _GEN_32) @[ProcessingElement.scala 381:34]
    _T_12_12 <= mux(reset, _T_11_12, _GEN_33) @[ProcessingElement.scala 381:34]
    _T_12_13 <= mux(reset, _T_11_13, _GEN_34) @[ProcessingElement.scala 381:34]
    _T_12_14 <= mux(reset, _T_11_14, _GEN_35) @[ProcessingElement.scala 381:34]
    _T_12_15 <= mux(reset, _T_11_15, _GEN_36) @[ProcessingElement.scala 381:34]

  module SimplyCombineAddrDataSPad :
    input clock : Clock
    input reset : UInt<1>
    output io_iactIOs_dataIOs_writeInDataIO_ready : UInt<1>
    input io_iactIOs_dataIOs_writeInDataIO_valid : UInt<1>
    input io_iactIOs_dataIOs_writeInDataIO_bits_data : UInt<12>
    input io_iactIOs_dataIOs_streamLen : UInt<4>
    output io_iactIOs_dataIOs_writeFin : UInt<1>
    output io_iactIOs_addrIOs_writeInDataIO_ready : UInt<1>
    input io_iactIOs_addrIOs_writeInDataIO_valid : UInt<1>
    input io_iactIOs_addrIOs_writeInDataIO_bits_data : UInt<4>
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
  
    inst iactAddrSPad of SPadAddrModule @[ProcessingElement.scala 256:44]
    inst iactDataSPad of SPadDataModule @[ProcessingElement.scala 257:44]
    reg sPad : UInt<2>, clock with :
      reset => (UInt<1>("h0"), sPad) @[ProcessingElement.scala 260:27]
    reg iactSPadZeroColumnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactSPadZeroColumnReg) @[ProcessingElement.scala 264:44]
    reg iactAddrSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPadReadEnReg) @[ProcessingElement.scala 265:44]
    reg iactDataSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactDataSPadReadEnReg) @[ProcessingElement.scala 266:44]
    reg iactMatrixColumnReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactMatrixColumnReg) @[ProcessingElement.scala 269:42]
    reg iactZeroColumnNumber : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactZeroColumnNumber) @[ProcessingElement.scala 270:43]
    reg iactDataSPadFirstRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactDataSPadFirstRead) @[ProcessingElement.scala 271:44]
    node _T = eq(sPad, UInt<2>("h2")) @[ProcessingElement.scala 274:36]
    node iactDataIndexWire = iactDataSPad.io_commonIO_columnNum @[ProcessingElement.scala 263:37 ProcessingElement.scala 290:21]
    node _T_1 = add(iactDataIndexWire, UInt<1>("h1")) @[ProcessingElement.scala 274:97]
    node _T_2 = tail(_T_1, 1) @[ProcessingElement.scala 274:97]
    node iactAddrDataWire = iactAddrSPad.io_commonIO_readOutData @[ProcessingElement.scala 262:36 ProcessingElement.scala 283:20]
    node _T_3 = eq(iactAddrDataWire, _T_2) @[ProcessingElement.scala 274:74]
    node _T_4 = and(_T, _T_3) @[ProcessingElement.scala 274:53]
    node _T_5 = eq(sPad, UInt<2>("h1")) @[ProcessingElement.scala 274:116]
    node _T_6 = eq(iactAddrDataWire, UInt<4>("hf")) @[ProcessingElement.scala 274:154]
    node _T_7 = and(_T_5, _T_6) @[ProcessingElement.scala 274:133]
    node _T_8 = or(_T_4, _T_7) @[ProcessingElement.scala 274:106]
    node _T_9 = eq(sPad, UInt<2>("h1")) @[ProcessingElement.scala 275:36]
    node _T_10 = eq(iactSPadZeroColumnReg, UInt<1>("h0")) @[ProcessingElement.scala 275:56]
    node _T_11 = and(_T_9, _T_10) @[ProcessingElement.scala 275:53]
    node _T_12 = eq(iactDataSPadFirstRead, UInt<1>("h0")) @[ProcessingElement.scala 275:82]
    node _T_13 = and(_T_11, _T_12) @[ProcessingElement.scala 275:79]
    node _T_14 = eq(sPad, UInt<2>("h2")) @[ProcessingElement.scala 275:116]
    node iactAddrSPadIdxIncWire = _T_8 @[ProcessingElement.scala 267:42 ProcessingElement.scala 274:26]
    node _T_15 = eq(iactAddrSPadIdxIncWire, UInt<1>("h0")) @[ProcessingElement.scala 275:136]
    node _T_16 = and(_T_14, _T_15) @[ProcessingElement.scala 275:133]
    node _T_17 = or(_T_13, _T_16) @[ProcessingElement.scala 275:106]
    node iactDataCountVec_0 = bits(iactDataSPad.io_commonIO_readOutData, 0, 0) @[ProcessingElement.scala 293:74]
    node iactDataCountVec_1 = bits(iactDataSPad.io_commonIO_readOutData, 1, 1) @[ProcessingElement.scala 293:74]
    node iactDataCountVec_2 = bits(iactDataSPad.io_commonIO_readOutData, 2, 2) @[ProcessingElement.scala 293:74]
    node iactDataCountVec_3 = bits(iactDataSPad.io_commonIO_readOutData, 3, 3) @[ProcessingElement.scala 293:74]
    node iactDataCountVec_4 = bits(iactDataSPad.io_commonIO_readOutData, 4, 4) @[ProcessingElement.scala 293:74]
    node iactDataCountVec_5 = bits(iactDataSPad.io_commonIO_readOutData, 5, 5) @[ProcessingElement.scala 293:74]
    node iactDataCountVec_6 = bits(iactDataSPad.io_commonIO_readOutData, 6, 6) @[ProcessingElement.scala 293:74]
    node iactDataCountVec_7 = bits(iactDataSPad.io_commonIO_readOutData, 7, 7) @[ProcessingElement.scala 293:74]
    node iactDataCountVec_8 = bits(iactDataSPad.io_commonIO_readOutData, 8, 8) @[ProcessingElement.scala 293:74]
    node iactDataCountVec_9 = bits(iactDataSPad.io_commonIO_readOutData, 9, 9) @[ProcessingElement.scala 293:74]
    node iactDataCountVec_10 = bits(iactDataSPad.io_commonIO_readOutData, 10, 10) @[ProcessingElement.scala 293:74]
    node iactDataCountVec_11 = bits(iactDataSPad.io_commonIO_readOutData, 11, 11) @[ProcessingElement.scala 293:74]
    node _T_18 = cat(iactDataCountVec_5, iactDataCountVec_4) @[Cat.scala 29:58]
    node _T_19 = cat(iactDataCountVec_7, iactDataCountVec_6) @[Cat.scala 29:58]
    node _T_20 = cat(_T_19, _T_18) @[Cat.scala 29:58]
    node _T_21 = cat(iactDataCountVec_9, iactDataCountVec_8) @[Cat.scala 29:58]
    node _T_22 = cat(iactDataCountVec_11, iactDataCountVec_10) @[Cat.scala 29:58]
    node _T_23 = cat(_T_22, _T_21) @[Cat.scala 29:58]
    node _T_24 = cat(_T_23, _T_20) @[Cat.scala 29:58]
    node _T_25 = cat(iactDataCountVec_1, iactDataCountVec_0) @[Cat.scala 29:58]
    node _T_26 = cat(iactDataCountVec_3, iactDataCountVec_2) @[Cat.scala 29:58]
    node _T_27 = cat(_T_26, _T_25) @[Cat.scala 29:58]
    node _T_28 = eq(UInt<2>("h0"), sPad) @[Conditional.scala 37:30]
    node _GEN_0 = mux(io_iactDataReq, UInt<2>("h1"), sPad) @[ProcessingElement.scala 305:28]
    node _GEN_1 = mux(io_iactDataReq, UInt<1>("h1"), iactAddrSPadReadEnReg) @[ProcessingElement.scala 305:28]
    node _GEN_2 = mux(io_iactDataReq, UInt<1>("h0"), iactDataSPadReadEnReg) @[ProcessingElement.scala 305:28]
    node _GEN_3 = mux(io_iactDataReq, UInt<1>("h1"), iactDataSPadFirstRead) @[ProcessingElement.scala 305:28]
    node _T_29 = eq(UInt<2>("h1"), sPad) @[Conditional.scala 37:30]
    node _T_30 = eq(iactAddrDataWire, UInt<4>("hf")) @[ProcessingElement.scala 314:30]
    node _T_31 = add(iactZeroColumnNumber, UInt<1>("h1")) @[ProcessingElement.scala 319:54]
    node _T_32 = tail(_T_31, 1) @[ProcessingElement.scala 319:54]
    node _GEN_4 = mux(_T_30, UInt<2>("h1"), UInt<2>("h2")) @[ProcessingElement.scala 314:52]
    node _GEN_5 = mux(_T_30, UInt<1>("h1"), iactSPadZeroColumnReg) @[ProcessingElement.scala 314:52]
    node _GEN_6 = mux(_T_30, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 314:52]
    node _GEN_7 = mux(_T_30, UInt<1>("h0"), UInt<1>("h1")) @[ProcessingElement.scala 314:52]
    node _GEN_8 = mux(_T_30, _T_32, iactZeroColumnNumber) @[ProcessingElement.scala 314:52]
    node _T_33 = eq(UInt<2>("h2"), sPad) @[Conditional.scala 37:30]
    node _T_34 = sub(io_iactIOs_dataIOs_streamLen, UInt<1>("h1")) @[ProcessingElement.scala 328:64]
    node _T_35 = tail(_T_34, 1) @[ProcessingElement.scala 328:64]
    node _T_36 = eq(iactDataIndexWire, _T_35) @[ProcessingElement.scala 328:31]
    node _GEN_9 = mux(_T_36, UInt<2>("h0"), sPad) @[ProcessingElement.scala 328:71]
    node _GEN_10 = mux(_T_36, UInt<1>("h0"), iactDataSPadReadEnReg) @[ProcessingElement.scala 328:71]
    node _GEN_11 = mux(_T_36, UInt<1>("h0"), iactSPadZeroColumnReg) @[ProcessingElement.scala 328:71]
    node _T_37 = add(iactMatrixColumnReg, UInt<1>("h1")) @[ProcessingElement.scala 340:54]
    node _T_38 = tail(_T_37, 1) @[ProcessingElement.scala 340:54]
    node _T_39 = add(_T_38, iactZeroColumnNumber) @[ProcessingElement.scala 340:60]
    node _T_40 = tail(_T_39, 1) @[ProcessingElement.scala 340:60]
    node _T_41 = add(iactMatrixColumnReg, UInt<1>("h1")) @[ProcessingElement.scala 344:54]
    node _T_42 = tail(_T_41, 1) @[ProcessingElement.scala 344:54]
    node _GEN_12 = mux(iactSPadZeroColumnReg, _T_40, _T_42) @[ProcessingElement.scala 339:38]
    node _GEN_13 = mux(iactSPadZeroColumnReg, UInt<1>("h0"), iactZeroColumnNumber) @[ProcessingElement.scala 339:38]
    node _GEN_14 = mux(iactAddrSPadIdxIncWire, UInt<2>("h1"), UInt<2>("h2")) @[ProcessingElement.scala 333:37]
    node _GEN_15 = mux(iactAddrSPadIdxIncWire, UInt<1>("h0"), _GEN_11) @[ProcessingElement.scala 333:37]
    node _GEN_16 = mux(iactAddrSPadIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 333:37]
    node _GEN_17 = mux(iactAddrSPadIdxIncWire, UInt<1>("h0"), UInt<1>("h1")) @[ProcessingElement.scala 333:37]
    node _GEN_18 = mux(iactAddrSPadIdxIncWire, _GEN_12, iactMatrixColumnReg) @[ProcessingElement.scala 333:37]
    node _GEN_19 = mux(iactAddrSPadIdxIncWire, _GEN_13, iactZeroColumnNumber) @[ProcessingElement.scala 333:37]
    node _GEN_20 = mux(_T_33, UInt<1>("h0"), iactDataSPadFirstRead) @[Conditional.scala 39:67]
    node _GEN_21 = mux(_T_33, _GEN_14, sPad) @[Conditional.scala 39:67]
    node _GEN_22 = mux(_T_33, _GEN_17, iactDataSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_23 = mux(_T_33, _GEN_15, iactSPadZeroColumnReg) @[Conditional.scala 39:67]
    node _GEN_24 = mux(_T_33, _GEN_16, iactAddrSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_25 = mux(_T_33, _GEN_18, iactMatrixColumnReg) @[Conditional.scala 39:67]
    node _GEN_26 = mux(_T_33, _GEN_19, iactZeroColumnNumber) @[Conditional.scala 39:67]
    node _GEN_27 = mux(_T_29, _GEN_4, _GEN_21) @[Conditional.scala 39:67]
    node _GEN_28 = mux(_T_29, _GEN_5, _GEN_23) @[Conditional.scala 39:67]
    node _GEN_29 = mux(_T_29, _GEN_6, _GEN_24) @[Conditional.scala 39:67]
    node _GEN_30 = mux(_T_29, _GEN_7, _GEN_22) @[Conditional.scala 39:67]
    node _GEN_31 = mux(_T_29, _GEN_8, _GEN_26) @[Conditional.scala 39:67]
    node _GEN_32 = mux(_T_29, iactDataSPadFirstRead, _GEN_20) @[Conditional.scala 39:67]
    node _GEN_33 = mux(_T_29, iactMatrixColumnReg, _GEN_25) @[Conditional.scala 39:67]
    node _GEN_34 = mux(_T_28, _GEN_0, _GEN_27) @[Conditional.scala 40:58]
    node _GEN_35 = mux(_T_28, _GEN_1, _GEN_29) @[Conditional.scala 40:58]
    node _GEN_36 = mux(_T_28, _GEN_2, _GEN_30) @[Conditional.scala 40:58]
    node _GEN_37 = mux(_T_28, _GEN_3, _GEN_32) @[Conditional.scala 40:58]
    node _GEN_38 = mux(_T_28, iactSPadZeroColumnReg, _GEN_28) @[Conditional.scala 40:58]
    node _GEN_39 = mux(_T_28, iactZeroColumnNumber, _GEN_31) @[Conditional.scala 40:58]
    node _GEN_40 = mux(_T_28, iactMatrixColumnReg, _GEN_33) @[Conditional.scala 40:58]
    node iactAddrIndexWire = iactAddrSPad.io_commonIO_columnNum @[ProcessingElement.scala 261:37 ProcessingElement.scala 281:21]
    node iactDataSPadIdxIncWire = _T_17 @[ProcessingElement.scala 268:42 ProcessingElement.scala 275:26]
    io_iactIOs_dataIOs_writeInDataIO_ready <= iactDataSPad.io_commonIO_dataLenFinIO_writeInDataIO_ready @[ProcessingElement.scala 277:41]
    io_iactIOs_dataIOs_writeFin <= iactDataSPad.io_commonIO_dataLenFinIO_writeFin @[ProcessingElement.scala 277:41]
    io_iactIOs_addrIOs_writeInDataIO_ready <= iactAddrSPad.io_commonIO_dataLenFinIO_writeInDataIO_ready @[ProcessingElement.scala 276:41]
    io_iactIOs_addrIOs_writeFin <= iactAddrSPad.io_commonIO_dataLenFinIO_writeFin @[ProcessingElement.scala 276:41]
    io_iactAddrWriteIdx <= iactAddrSPad.io_commonIO_writeIdx @[ProcessingElement.scala 278:23]
    io_iactMatrixColumn <= iactMatrixColumnReg @[ProcessingElement.scala 282:23]
    io_iactMatrixRow <= _T_27 @[ProcessingElement.scala 295:20]
    io_iactMatrixData <= _T_24 @[ProcessingElement.scala 294:21]
    io_iactMatrixDataBin <= iactDataSPad.io_commonIO_readOutData @[ProcessingElement.scala 292:24]
    io_iactAddrReadEn <= iactAddrSPadReadEnReg @[ProcessingElement.scala 286:21]
    io_iactAddrReadData <= iactAddrDataWire @[ProcessingElement.scala 284:23]
    io_iactDataReadIndex <= iactDataIndexWire @[ProcessingElement.scala 291:24]
    io_iactDataWriteIdx <= iactDataSPad.io_commonIO_writeIdx @[ProcessingElement.scala 279:23]
    iactAddrSPad.clock <= clock
    iactAddrSPad.reset <= reset
    iactAddrSPad.io_commonIO_readEn <= iactAddrSPadReadEnReg @[ProcessingElement.scala 285:35]
    iactAddrSPad.io_commonIO_dataLenFinIO_writeInDataIO_valid <= io_iactIOs_addrIOs_writeInDataIO_valid @[ProcessingElement.scala 276:41]
    iactAddrSPad.io_commonIO_dataLenFinIO_writeInDataIO_bits_data <= io_iactIOs_addrIOs_writeInDataIO_bits_data @[ProcessingElement.scala 276:41]
    iactAddrSPad.io_commonIO_dataLenFinIO_streamLen <= io_iactIOs_addrIOs_streamLen @[ProcessingElement.scala 276:41]
    iactAddrSPad.io_addrIO_readInIdx is invalid
    iactAddrSPad.io_addrIO_indexInc <= iactAddrSPadIdxIncWire @[ProcessingElement.scala 272:35]
    iactAddrSPad.io_dataIO_readInIdx is invalid
    iactAddrSPad.io_dataIO_indexInc is invalid
    iactDataSPad.clock <= clock
    iactDataSPad.reset <= reset
    iactDataSPad.io_commonIO_readEn <= iactDataSPadReadEnReg @[ProcessingElement.scala 296:35]
    iactDataSPad.io_commonIO_dataLenFinIO_writeInDataIO_valid <= io_iactIOs_dataIOs_writeInDataIO_valid @[ProcessingElement.scala 277:41]
    iactDataSPad.io_commonIO_dataLenFinIO_writeInDataIO_bits_data <= io_iactIOs_dataIOs_writeInDataIO_bits_data @[ProcessingElement.scala 277:41]
    iactDataSPad.io_commonIO_dataLenFinIO_streamLen <= io_iactIOs_dataIOs_streamLen @[ProcessingElement.scala 277:41]
    iactDataSPad.io_addrIO_readInIdx is invalid
    iactDataSPad.io_addrIO_indexInc is invalid
    iactDataSPad.io_dataIO_readInIdx is invalid
    iactDataSPad.io_dataIO_indexInc <= iactDataSPadIdxIncWire @[ProcessingElement.scala 273:35]
    sPad <= mux(reset, UInt<2>("h0"), _GEN_34) @[ProcessingElement.scala 306:14 ProcessingElement.scala 315:14 ProcessingElement.scala 321:14 ProcessingElement.scala 329:14 ProcessingElement.scala 334:14 ProcessingElement.scala 347:14]
    iactSPadZeroColumnReg <= mux(reset, UInt<1>("h0"), _GEN_38) @[ProcessingElement.scala 316:31 ProcessingElement.scala 331:31 ProcessingElement.scala 335:31]
    iactAddrSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_35) @[ProcessingElement.scala 307:31 ProcessingElement.scala 317:31 ProcessingElement.scala 322:31 ProcessingElement.scala 337:31 ProcessingElement.scala 348:31]
    iactDataSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_36) @[ProcessingElement.scala 308:31 ProcessingElement.scala 318:31 ProcessingElement.scala 323:31 ProcessingElement.scala 330:31 ProcessingElement.scala 338:31 ProcessingElement.scala 349:31]
    iactMatrixColumnReg <= mux(reset, UInt<4>("h0"), _GEN_40) @[ProcessingElement.scala 340:31 ProcessingElement.scala 344:31]
    iactZeroColumnNumber <= mux(reset, UInt<4>("h0"), _GEN_39) @[ProcessingElement.scala 319:30 ProcessingElement.scala 342:32]
    iactDataSPadFirstRead <= mux(reset, UInt<1>("h1"), _GEN_37) @[ProcessingElement.scala 309:31 ProcessingElement.scala 327:29]
