circuit SPadAddrModule :
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
    input io_dataIO_readInIdxEn : UInt<1>
  
    reg dataLenReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[ProcessingElement.scala 534:33]
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[ProcessingElement.scala 537:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[ProcessingElement.scala 538:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 543:53]
    node _T_1 = tail(_T, 1) @[ProcessingElement.scala 543:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[ProcessingElement.scala 543:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[ProcessingElement.scala 544:51]
    node _T_4 = tail(_T_3, 1) @[ProcessingElement.scala 544:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[ProcessingElement.scala 544:35]
    node _T_6 = add(padWriteIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 548:42]
    node _T_7 = tail(_T_6, 1) @[ProcessingElement.scala 548:42]
    node writeWrapWire = _T_2 @[ProcessingElement.scala 540:33 ProcessingElement.scala 543:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_7) @[ProcessingElement.scala 549:26]
    node _GEN_1 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 546:32]
    node _GEN_2 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_0, padWriteIndexReg) @[ProcessingElement.scala 546:32]
    node _T_8 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, writeWrapWire) @[ProcessingElement.scala 557:62]
    reg addrSPad_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_0) @[ProcessingElement.scala 469:36]
    reg addrSPad_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_1) @[ProcessingElement.scala 469:36]
    reg addrSPad_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_2) @[ProcessingElement.scala 469:36]
    reg addrSPad_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_3) @[ProcessingElement.scala 469:36]
    reg addrSPad_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_4) @[ProcessingElement.scala 469:36]
    reg addrSPad_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_5) @[ProcessingElement.scala 469:36]
    reg addrSPad_6 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_6) @[ProcessingElement.scala 469:36]
    reg addrSPad_7 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_7) @[ProcessingElement.scala 469:36]
    reg addrSPad_8 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_8) @[ProcessingElement.scala 469:36]
    node _addrSPad_padWriteIndexReg = io_commonIO_dataLenFinIO_writeInDataIO_bits_data @[ProcessingElement.scala 472:32 ProcessingElement.scala 472:32]
    node _GEN_3 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_0) @[ProcessingElement.scala 472:32]
    node _GEN_4 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_1) @[ProcessingElement.scala 472:32]
    node _GEN_5 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_2) @[ProcessingElement.scala 472:32]
    node _GEN_6 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_3) @[ProcessingElement.scala 472:32]
    node _GEN_7 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_4) @[ProcessingElement.scala 472:32]
    node _GEN_8 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_5) @[ProcessingElement.scala 472:32]
    node _GEN_9 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_6) @[ProcessingElement.scala 472:32]
    node _GEN_10 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_7) @[ProcessingElement.scala 472:32]
    node _GEN_11 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_8) @[ProcessingElement.scala 472:32]
    node _GEN_12 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_3, addrSPad_0) @[ProcessingElement.scala 471:32]
    node _GEN_13 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_4, addrSPad_1) @[ProcessingElement.scala 471:32]
    node _GEN_14 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_5, addrSPad_2) @[ProcessingElement.scala 471:32]
    node _GEN_15 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_6, addrSPad_3) @[ProcessingElement.scala 471:32]
    node _GEN_16 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_7, addrSPad_4) @[ProcessingElement.scala 471:32]
    node _GEN_17 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_8, addrSPad_5) @[ProcessingElement.scala 471:32]
    node _GEN_18 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_9, addrSPad_6) @[ProcessingElement.scala 471:32]
    node _GEN_19 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_10, addrSPad_7) @[ProcessingElement.scala 471:32]
    node _GEN_20 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_11, addrSPad_8) @[ProcessingElement.scala 471:32]
    node _T_10 = add(padReadIndexReg, UInt<1>("h1")) @[ProcessingElement.scala 476:40]
    node _T_11 = tail(_T_10, 1) @[ProcessingElement.scala 476:40]
    node readWrapWire = _T_5 @[ProcessingElement.scala 541:32 ProcessingElement.scala 544:16]
    node _GEN_21 = mux(readWrapWire, UInt<1>("h0"), _T_11) @[ProcessingElement.scala 477:25]
    node readIndexInc = io_addrIO_indexInc @[ProcessingElement.scala 542:32 ProcessingElement.scala 484:16]
    node _GEN_22 = mux(readIndexInc, _GEN_21, padReadIndexReg) @[ProcessingElement.scala 475:23]
    node _GEN_23 = validif(eq(UInt<1>("h0"), padReadIndexReg), addrSPad_0) @[ProcessingElement.scala 482:12]
    node _GEN_24 = mux(eq(UInt<1>("h1"), padReadIndexReg), addrSPad_1, _GEN_23) @[ProcessingElement.scala 482:12]
    node _GEN_25 = mux(eq(UInt<2>("h2"), padReadIndexReg), addrSPad_2, _GEN_24) @[ProcessingElement.scala 482:12]
    node _GEN_26 = mux(eq(UInt<2>("h3"), padReadIndexReg), addrSPad_3, _GEN_25) @[ProcessingElement.scala 482:12]
    node _GEN_27 = mux(eq(UInt<3>("h4"), padReadIndexReg), addrSPad_4, _GEN_26) @[ProcessingElement.scala 482:12]
    node _GEN_28 = mux(eq(UInt<3>("h5"), padReadIndexReg), addrSPad_5, _GEN_27) @[ProcessingElement.scala 482:12]
    node _GEN_29 = mux(eq(UInt<3>("h6"), padReadIndexReg), addrSPad_6, _GEN_28) @[ProcessingElement.scala 482:12]
    node _GEN_30 = mux(eq(UInt<3>("h7"), padReadIndexReg), addrSPad_7, _GEN_29) @[ProcessingElement.scala 482:12]
    node _GEN_31 = mux(eq(UInt<4>("h8"), padReadIndexReg), addrSPad_8, _GEN_30) @[ProcessingElement.scala 482:12]
    node _addrSPad_padReadIndexReg = _GEN_31 @[ProcessingElement.scala 482:12 ProcessingElement.scala 482:12 ProcessingElement.scala 482:12 ProcessingElement.scala 482:12 ProcessingElement.scala 482:12 ProcessingElement.scala 482:12 ProcessingElement.scala 482:12 ProcessingElement.scala 482:12 ProcessingElement.scala 482:12 ProcessingElement.scala 482:12]
    node dataWire = _addrSPad_padReadIndexReg @[ProcessingElement.scala 535:28 ProcessingElement.scala 482:12]
    node _T_9_0 = UInt<4>("h0") @[ProcessingElement.scala 469:44 ProcessingElement.scala 469:44]
    node _T_9_1 = UInt<4>("h0") @[ProcessingElement.scala 469:44 ProcessingElement.scala 469:44]
    node _T_9_2 = UInt<4>("h0") @[ProcessingElement.scala 469:44 ProcessingElement.scala 469:44]
    node _T_9_3 = UInt<4>("h0") @[ProcessingElement.scala 469:44 ProcessingElement.scala 469:44]
    node _T_9_4 = UInt<4>("h0") @[ProcessingElement.scala 469:44 ProcessingElement.scala 469:44]
    node _T_9_5 = UInt<4>("h0") @[ProcessingElement.scala 469:44 ProcessingElement.scala 469:44]
    node _T_9_6 = UInt<4>("h0") @[ProcessingElement.scala 469:44 ProcessingElement.scala 469:44]
    node _T_9_7 = UInt<4>("h0") @[ProcessingElement.scala 469:44 ProcessingElement.scala 469:44]
    node _T_9_8 = UInt<4>("h0") @[ProcessingElement.scala 469:44 ProcessingElement.scala 469:44]
    io_commonIO_columnNum <= padReadIndexReg @[ProcessingElement.scala 558:25]
    io_commonIO_readOutData <= dataWire @[ProcessingElement.scala 483:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[ProcessingElement.scala 556:24]
    io_commonIO_dataLenFinIO_writeInDataIO_ready <= _GEN_1 @[ProcessingElement.scala 547:27 ProcessingElement.scala 553:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_8 @[ProcessingElement.scala 557:37]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[ProcessingElement.scala 536:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[ProcessingElement.scala 548:22 ProcessingElement.scala 550:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_22) @[ProcessingElement.scala 476:21 ProcessingElement.scala 478:23]
    addrSPad_0 <= mux(reset, _T_9_0, _GEN_12) @[ProcessingElement.scala 472:32]
    addrSPad_1 <= mux(reset, _T_9_1, _GEN_13) @[ProcessingElement.scala 472:32]
    addrSPad_2 <= mux(reset, _T_9_2, _GEN_14) @[ProcessingElement.scala 472:32]
    addrSPad_3 <= mux(reset, _T_9_3, _GEN_15) @[ProcessingElement.scala 472:32]
    addrSPad_4 <= mux(reset, _T_9_4, _GEN_16) @[ProcessingElement.scala 472:32]
    addrSPad_5 <= mux(reset, _T_9_5, _GEN_17) @[ProcessingElement.scala 472:32]
    addrSPad_6 <= mux(reset, _T_9_6, _GEN_18) @[ProcessingElement.scala 472:32]
    addrSPad_7 <= mux(reset, _T_9_7, _GEN_19) @[ProcessingElement.scala 472:32]
    addrSPad_8 <= mux(reset, _T_9_8, _GEN_20) @[ProcessingElement.scala 472:32]
