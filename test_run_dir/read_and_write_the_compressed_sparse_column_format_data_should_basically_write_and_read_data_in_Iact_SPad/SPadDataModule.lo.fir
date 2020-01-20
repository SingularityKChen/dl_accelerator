circuit SPadDataModule :
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
