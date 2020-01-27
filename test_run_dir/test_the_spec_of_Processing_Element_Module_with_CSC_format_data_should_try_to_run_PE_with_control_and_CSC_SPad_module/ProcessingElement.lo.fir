circuit ProcessingElement :
  module ProcessingElementControl :
    input clock : Clock
    input reset : UInt<1>
    output io_ctrlPad_doMACEn : UInt<1>
    input io_ctrlPad_fromTopIO_pSumEnqOrProduct_ready : UInt<1>
    output io_ctrlPad_fromTopIO_pSumEnqOrProduct_valid : UInt<1>
    output io_ctrlPad_fromTopIO_pSumEnqOrProduct_bits : UInt<1>
    output io_ctrlPad_fromTopIO_doLoadEn : UInt<1>
    input io_ctrlPad_fromTopIO_writeFinish : UInt<1>
    input io_ctrlPad_fromTopIO_calFinish : UInt<1>
    output io_ctrlTop_pSumEnqOrProduct_ready : UInt<1>
    input io_ctrlTop_pSumEnqOrProduct_valid : UInt<1>
    input io_ctrlTop_pSumEnqOrProduct_bits : UInt<1>
    input io_ctrlTop_doLoadEn : UInt<1>
    output io_ctrlTop_writeFinish : UInt<1>
    output io_ctrlTop_calFinish : UInt<1>
  
    reg stateMac : UInt<2>, clock with :
      reset => (UInt<1>("h0"), stateMac) @[ProcessingElement.scala 49:31]
    node _T = eq(stateMac, UInt<2>("h2")) @[ProcessingElement.scala 50:53]
    node _T_1 = mux(_T, io_ctrlPad_fromTopIO_pSumEnqOrProduct_ready, UInt<1>("h0")) @[ProcessingElement.scala 50:43]
    node _T_2 = eq(stateMac, UInt<2>("h2")) @[ProcessingElement.scala 51:63]
    node _T_3 = mux(_T_2, io_ctrlTop_pSumEnqOrProduct_valid, UInt<1>("h0")) @[ProcessingElement.scala 51:53]
    node _T_4 = eq(stateMac, UInt<2>("h2")) @[ProcessingElement.scala 54:34]
    node _T_5 = eq(UInt<2>("h0"), stateMac) @[Conditional.scala 37:30]
    node _GEN_0 = mux(io_ctrlTop_doLoadEn, UInt<2>("h1"), stateMac) @[ProcessingElement.scala 57:34]
    node _T_6 = eq(UInt<2>("h1"), stateMac) @[Conditional.scala 37:30]
    node _GEN_1 = mux(io_ctrlPad_fromTopIO_writeFinish, UInt<2>("h2"), stateMac) @[ProcessingElement.scala 62:47]
    node _T_7 = eq(UInt<2>("h2"), stateMac) @[Conditional.scala 37:30]
    node _GEN_2 = mux(io_ctrlPad_fromTopIO_calFinish, UInt<2>("h0"), stateMac) @[ProcessingElement.scala 67:45]
    node _GEN_3 = mux(_T_7, _GEN_2, stateMac) @[Conditional.scala 39:67]
    node _GEN_4 = mux(_T_6, _GEN_1, _GEN_3) @[Conditional.scala 39:67]
    node _GEN_5 = mux(_T_5, _GEN_0, _GEN_4) @[Conditional.scala 40:58]
    io_ctrlPad_doMACEn <= _T_4 @[ProcessingElement.scala 54:22]
    io_ctrlPad_fromTopIO_pSumEnqOrProduct_valid <= _T_3 @[ProcessingElement.scala 51:47]
    io_ctrlPad_fromTopIO_pSumEnqOrProduct_bits <= io_ctrlTop_pSumEnqOrProduct_bits @[ProcessingElement.scala 52:46]
    io_ctrlPad_fromTopIO_doLoadEn <= io_ctrlTop_doLoadEn @[ProcessingElement.scala 53:33]
    io_ctrlTop_pSumEnqOrProduct_ready <= _T_1 @[ProcessingElement.scala 50:37]
    io_ctrlTop_writeFinish <= io_ctrlPad_fromTopIO_writeFinish @[ProcessingElement.scala 40:26]
    io_ctrlTop_calFinish <= io_ctrlPad_fromTopIO_calFinish @[ProcessingElement.scala 41:24]
    stateMac <= mux(reset, UInt<2>("h0"), _GEN_5) @[ProcessingElement.scala 58:18 ProcessingElement.scala 63:18 ProcessingElement.scala 68:18]

  module SPadAddrModule :
    input clock : Clock
    input reset : UInt<1>
    output io_commonIO_columnNum : UInt<4>
    output io_commonIO_readOutData : UInt<4>
    input io_commonIO_readEn : UInt<1>
    input io_commonIO_writeEn : UInt<1>
    output io_commonIO_writeIdx : UInt<4>
    output io_commonIO_dataLenFinIO_writeInDataIO_ready : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_valid : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_bits_data : UInt<4>
    input io_commonIO_dataLenFinIO_streamLen : UInt<4>
    output io_commonIO_dataLenFinIO_writeFin : UInt<1>
    input io_addrIO_readInIdx : UInt<4>
    input io_addrIO_indexInc : UInt<1>
    input io_addrIO_readInIdxEn : UInt<1>
    input io_dataIO_readInIdx : UInt<4>
    input io_dataIO_indexInc : UInt<1>
    input io_dataIO_readInIdxEn : UInt<1>
  
    reg dataLenReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[SPadModule.scala 79:33]
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 82:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 83:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 88:53]
    node _T_1 = tail(_T, 1) @[SPadModule.scala 88:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[SPadModule.scala 88:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 89:51]
    node _T_4 = tail(_T_3, 1) @[SPadModule.scala 89:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[SPadModule.scala 89:35]
    node _T_6 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 91:31]
    node _T_7 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 93:42]
    node _T_8 = tail(_T_7, 1) @[SPadModule.scala 93:42]
    node writeWrapWire = _T_2 @[SPadModule.scala 85:33 SPadModule.scala 88:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_8) @[SPadModule.scala 94:26]
    node _GEN_1 = mux(_T_6, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 91:55]
    node _GEN_2 = mux(_T_6, _GEN_0, padWriteIndexReg) @[SPadModule.scala 91:55]
    node _T_9 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, writeWrapWire) @[SPadModule.scala 102:62]
    reg addrSPad_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_0) @[SPadModule.scala 14:36]
    reg addrSPad_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_1) @[SPadModule.scala 14:36]
    reg addrSPad_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_2) @[SPadModule.scala 14:36]
    reg addrSPad_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_3) @[SPadModule.scala 14:36]
    reg addrSPad_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_4) @[SPadModule.scala 14:36]
    reg addrSPad_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_5) @[SPadModule.scala 14:36]
    reg addrSPad_6 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_6) @[SPadModule.scala 14:36]
    reg addrSPad_7 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_7) @[SPadModule.scala 14:36]
    reg addrSPad_8 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_8) @[SPadModule.scala 14:36]
    node _T_11 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 16:31]
    node _addrSPad_padWriteIndexReg = io_commonIO_dataLenFinIO_writeInDataIO_bits_data @[SPadModule.scala 17:32 SPadModule.scala 17:32]
    node _GEN_3 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_0) @[SPadModule.scala 17:32]
    node _GEN_4 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_1) @[SPadModule.scala 17:32]
    node _GEN_5 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_2) @[SPadModule.scala 17:32]
    node _GEN_6 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_3) @[SPadModule.scala 17:32]
    node _GEN_7 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_4) @[SPadModule.scala 17:32]
    node _GEN_8 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_5) @[SPadModule.scala 17:32]
    node _GEN_9 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_6) @[SPadModule.scala 17:32]
    node _GEN_10 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_7) @[SPadModule.scala 17:32]
    node _GEN_11 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_8) @[SPadModule.scala 17:32]
    node _GEN_12 = mux(_T_11, _GEN_3, addrSPad_0) @[SPadModule.scala 16:55]
    node _GEN_13 = mux(_T_11, _GEN_4, addrSPad_1) @[SPadModule.scala 16:55]
    node _GEN_14 = mux(_T_11, _GEN_5, addrSPad_2) @[SPadModule.scala 16:55]
    node _GEN_15 = mux(_T_11, _GEN_6, addrSPad_3) @[SPadModule.scala 16:55]
    node _GEN_16 = mux(_T_11, _GEN_7, addrSPad_4) @[SPadModule.scala 16:55]
    node _GEN_17 = mux(_T_11, _GEN_8, addrSPad_5) @[SPadModule.scala 16:55]
    node _GEN_18 = mux(_T_11, _GEN_9, addrSPad_6) @[SPadModule.scala 16:55]
    node _GEN_19 = mux(_T_11, _GEN_10, addrSPad_7) @[SPadModule.scala 16:55]
    node _GEN_20 = mux(_T_11, _GEN_11, addrSPad_8) @[SPadModule.scala 16:55]
    node _T_12 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 21:40]
    node _T_13 = tail(_T_12, 1) @[SPadModule.scala 21:40]
    node readWrapWire = _T_5 @[SPadModule.scala 86:32 SPadModule.scala 89:16]
    node _GEN_21 = mux(readWrapWire, UInt<1>("h0"), _T_13) @[SPadModule.scala 22:25]
    node readIndexInc = io_addrIO_indexInc @[SPadModule.scala 87:32 SPadModule.scala 29:16]
    node _GEN_22 = mux(readIndexInc, _GEN_21, padReadIndexReg) @[SPadModule.scala 20:23]
    node _GEN_23 = validif(eq(UInt<1>("h0"), padReadIndexReg), addrSPad_0) @[SPadModule.scala 27:12]
    node _GEN_24 = mux(eq(UInt<1>("h1"), padReadIndexReg), addrSPad_1, _GEN_23) @[SPadModule.scala 27:12]
    node _GEN_25 = mux(eq(UInt<2>("h2"), padReadIndexReg), addrSPad_2, _GEN_24) @[SPadModule.scala 27:12]
    node _GEN_26 = mux(eq(UInt<2>("h3"), padReadIndexReg), addrSPad_3, _GEN_25) @[SPadModule.scala 27:12]
    node _GEN_27 = mux(eq(UInt<3>("h4"), padReadIndexReg), addrSPad_4, _GEN_26) @[SPadModule.scala 27:12]
    node _GEN_28 = mux(eq(UInt<3>("h5"), padReadIndexReg), addrSPad_5, _GEN_27) @[SPadModule.scala 27:12]
    node _GEN_29 = mux(eq(UInt<3>("h6"), padReadIndexReg), addrSPad_6, _GEN_28) @[SPadModule.scala 27:12]
    node _GEN_30 = mux(eq(UInt<3>("h7"), padReadIndexReg), addrSPad_7, _GEN_29) @[SPadModule.scala 27:12]
    node _GEN_31 = mux(eq(UInt<4>("h8"), padReadIndexReg), addrSPad_8, _GEN_30) @[SPadModule.scala 27:12]
    node _addrSPad_padReadIndexReg = _GEN_31 @[SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12]
    node dataWire = _addrSPad_padReadIndexReg @[SPadModule.scala 80:28 SPadModule.scala 27:12]
    node _T_10_0 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_1 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_2 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_3 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_4 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_5 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_6 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_7 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_8 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    io_commonIO_columnNum <= padReadIndexReg @[SPadModule.scala 103:25]
    io_commonIO_readOutData <= dataWire @[SPadModule.scala 28:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[SPadModule.scala 101:24]
    io_commonIO_dataLenFinIO_writeInDataIO_ready <= _GEN_1 @[SPadModule.scala 92:27 SPadModule.scala 98:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_9 @[SPadModule.scala 102:37]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[SPadModule.scala 81:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[SPadModule.scala 93:22 SPadModule.scala 95:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_22) @[SPadModule.scala 21:21 SPadModule.scala 23:23]
    addrSPad_0 <= mux(reset, _T_10_0, _GEN_12) @[SPadModule.scala 17:32]
    addrSPad_1 <= mux(reset, _T_10_1, _GEN_13) @[SPadModule.scala 17:32]
    addrSPad_2 <= mux(reset, _T_10_2, _GEN_14) @[SPadModule.scala 17:32]
    addrSPad_3 <= mux(reset, _T_10_3, _GEN_15) @[SPadModule.scala 17:32]
    addrSPad_4 <= mux(reset, _T_10_4, _GEN_16) @[SPadModule.scala 17:32]
    addrSPad_5 <= mux(reset, _T_10_5, _GEN_17) @[SPadModule.scala 17:32]
    addrSPad_6 <= mux(reset, _T_10_6, _GEN_18) @[SPadModule.scala 17:32]
    addrSPad_7 <= mux(reset, _T_10_7, _GEN_19) @[SPadModule.scala 17:32]
    addrSPad_8 <= mux(reset, _T_10_8, _GEN_20) @[SPadModule.scala 17:32]

  module SPadDataModule :
    input clock : Clock
    input reset : UInt<1>
    output io_commonIO_columnNum : UInt<4>
    output io_commonIO_readOutData : UInt<12>
    input io_commonIO_readEn : UInt<1>
    input io_commonIO_writeEn : UInt<1>
    output io_commonIO_writeIdx : UInt<4>
    output io_commonIO_dataLenFinIO_writeInDataIO_ready : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_valid : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_bits_data : UInt<12>
    input io_commonIO_dataLenFinIO_streamLen : UInt<4>
    output io_commonIO_dataLenFinIO_writeFin : UInt<1>
    input io_addrIO_readInIdx : UInt<4>
    input io_addrIO_indexInc : UInt<1>
    input io_addrIO_readInIdxEn : UInt<1>
    input io_dataIO_readInIdx : UInt<4>
    input io_dataIO_indexInc : UInt<1>
    input io_dataIO_readInIdxEn : UInt<1>
  
    reg dataLenReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[SPadModule.scala 79:33]
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 82:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 83:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 88:53]
    node _T_1 = tail(_T, 1) @[SPadModule.scala 88:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[SPadModule.scala 88:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 89:51]
    node _T_4 = tail(_T_3, 1) @[SPadModule.scala 89:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[SPadModule.scala 89:35]
    node _T_6 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 91:31]
    node _T_7 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 93:42]
    node _T_8 = tail(_T_7, 1) @[SPadModule.scala 93:42]
    node writeWrapWire = _T_2 @[SPadModule.scala 85:33 SPadModule.scala 88:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_8) @[SPadModule.scala 94:26]
    node _GEN_1 = mux(_T_6, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 91:55]
    node _GEN_2 = mux(_T_6, _GEN_0, padWriteIndexReg) @[SPadModule.scala 91:55]
    node _T_9 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, writeWrapWire) @[SPadModule.scala 102:62]
    reg _T_11_0 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_0) @[SPadModule.scala 54:38]
    reg _T_11_1 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_1) @[SPadModule.scala 54:38]
    reg _T_11_2 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_2) @[SPadModule.scala 54:38]
    reg _T_11_3 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_3) @[SPadModule.scala 54:38]
    reg _T_11_4 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_4) @[SPadModule.scala 54:38]
    reg _T_11_5 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_5) @[SPadModule.scala 54:38]
    reg _T_11_6 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_6) @[SPadModule.scala 54:38]
    reg _T_11_7 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_7) @[SPadModule.scala 54:38]
    reg _T_11_8 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_8) @[SPadModule.scala 54:38]
    reg _T_11_9 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_9) @[SPadModule.scala 54:38]
    reg _T_11_10 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_10) @[SPadModule.scala 54:38]
    reg _T_11_11 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_11) @[SPadModule.scala 54:38]
    reg _T_11_12 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_12) @[SPadModule.scala 54:38]
    reg _T_11_13 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_13) @[SPadModule.scala 54:38]
    reg _T_11_14 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_14) @[SPadModule.scala 54:38]
    reg _T_11_15 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_11_15) @[SPadModule.scala 54:38]
    node _T_12 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 55:33]
    node _T_11_padWriteIndexReg = io_commonIO_dataLenFinIO_writeInDataIO_bits_data @[SPadModule.scala 56:34 SPadModule.scala 56:34]
    node _GEN_3 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_0) @[SPadModule.scala 56:34]
    node _GEN_4 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_1) @[SPadModule.scala 56:34]
    node _GEN_5 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_2) @[SPadModule.scala 56:34]
    node _GEN_6 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_3) @[SPadModule.scala 56:34]
    node _GEN_7 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_4) @[SPadModule.scala 56:34]
    node _GEN_8 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_5) @[SPadModule.scala 56:34]
    node _GEN_9 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_6) @[SPadModule.scala 56:34]
    node _GEN_10 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_7) @[SPadModule.scala 56:34]
    node _GEN_11 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_8) @[SPadModule.scala 56:34]
    node _GEN_12 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_9) @[SPadModule.scala 56:34]
    node _GEN_13 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_10) @[SPadModule.scala 56:34]
    node _GEN_14 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_11) @[SPadModule.scala 56:34]
    node _GEN_15 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_12) @[SPadModule.scala 56:34]
    node _GEN_16 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_13) @[SPadModule.scala 56:34]
    node _GEN_17 = mux(eq(UInt<4>("he"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_14) @[SPadModule.scala 56:34]
    node _GEN_18 = mux(eq(UInt<4>("hf"), padWriteIndexReg), _T_11_padWriteIndexReg, _T_11_15) @[SPadModule.scala 56:34]
    node _GEN_19 = mux(_T_12, _GEN_3, _T_11_0) @[SPadModule.scala 55:57]
    node _GEN_20 = mux(_T_12, _GEN_4, _T_11_1) @[SPadModule.scala 55:57]
    node _GEN_21 = mux(_T_12, _GEN_5, _T_11_2) @[SPadModule.scala 55:57]
    node _GEN_22 = mux(_T_12, _GEN_6, _T_11_3) @[SPadModule.scala 55:57]
    node _GEN_23 = mux(_T_12, _GEN_7, _T_11_4) @[SPadModule.scala 55:57]
    node _GEN_24 = mux(_T_12, _GEN_8, _T_11_5) @[SPadModule.scala 55:57]
    node _GEN_25 = mux(_T_12, _GEN_9, _T_11_6) @[SPadModule.scala 55:57]
    node _GEN_26 = mux(_T_12, _GEN_10, _T_11_7) @[SPadModule.scala 55:57]
    node _GEN_27 = mux(_T_12, _GEN_11, _T_11_8) @[SPadModule.scala 55:57]
    node _GEN_28 = mux(_T_12, _GEN_12, _T_11_9) @[SPadModule.scala 55:57]
    node _GEN_29 = mux(_T_12, _GEN_13, _T_11_10) @[SPadModule.scala 55:57]
    node _GEN_30 = mux(_T_12, _GEN_14, _T_11_11) @[SPadModule.scala 55:57]
    node _GEN_31 = mux(_T_12, _GEN_15, _T_11_12) @[SPadModule.scala 55:57]
    node _GEN_32 = mux(_T_12, _GEN_16, _T_11_13) @[SPadModule.scala 55:57]
    node _GEN_33 = mux(_T_12, _GEN_17, _T_11_14) @[SPadModule.scala 55:57]
    node _GEN_34 = mux(_T_12, _GEN_18, _T_11_15) @[SPadModule.scala 55:57]
    node _T_13 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 60:42]
    node _T_14 = tail(_T_13, 1) @[SPadModule.scala 60:42]
    node readWrapWire = _T_5 @[SPadModule.scala 86:32 SPadModule.scala 89:16]
    node _GEN_35 = mux(readWrapWire, UInt<1>("h0"), _T_14) @[SPadModule.scala 61:27]
    node readIndexInc = io_dataIO_indexInc @[SPadModule.scala 87:32 SPadModule.scala 68:16]
    node _GEN_36 = mux(readIndexInc, _GEN_35, padReadIndexReg) @[SPadModule.scala 59:25]
    node _GEN_37 = validif(eq(UInt<1>("h0"), padReadIndexReg), _T_11_0) @[SPadModule.scala 66:14]
    node _GEN_38 = mux(eq(UInt<1>("h1"), padReadIndexReg), _T_11_1, _GEN_37) @[SPadModule.scala 66:14]
    node _GEN_39 = mux(eq(UInt<2>("h2"), padReadIndexReg), _T_11_2, _GEN_38) @[SPadModule.scala 66:14]
    node _GEN_40 = mux(eq(UInt<2>("h3"), padReadIndexReg), _T_11_3, _GEN_39) @[SPadModule.scala 66:14]
    node _GEN_41 = mux(eq(UInt<3>("h4"), padReadIndexReg), _T_11_4, _GEN_40) @[SPadModule.scala 66:14]
    node _GEN_42 = mux(eq(UInt<3>("h5"), padReadIndexReg), _T_11_5, _GEN_41) @[SPadModule.scala 66:14]
    node _GEN_43 = mux(eq(UInt<3>("h6"), padReadIndexReg), _T_11_6, _GEN_42) @[SPadModule.scala 66:14]
    node _GEN_44 = mux(eq(UInt<3>("h7"), padReadIndexReg), _T_11_7, _GEN_43) @[SPadModule.scala 66:14]
    node _GEN_45 = mux(eq(UInt<4>("h8"), padReadIndexReg), _T_11_8, _GEN_44) @[SPadModule.scala 66:14]
    node _GEN_46 = mux(eq(UInt<4>("h9"), padReadIndexReg), _T_11_9, _GEN_45) @[SPadModule.scala 66:14]
    node _GEN_47 = mux(eq(UInt<4>("ha"), padReadIndexReg), _T_11_10, _GEN_46) @[SPadModule.scala 66:14]
    node _GEN_48 = mux(eq(UInt<4>("hb"), padReadIndexReg), _T_11_11, _GEN_47) @[SPadModule.scala 66:14]
    node _GEN_49 = mux(eq(UInt<4>("hc"), padReadIndexReg), _T_11_12, _GEN_48) @[SPadModule.scala 66:14]
    node _GEN_50 = mux(eq(UInt<4>("hd"), padReadIndexReg), _T_11_13, _GEN_49) @[SPadModule.scala 66:14]
    node _GEN_51 = mux(eq(UInt<4>("he"), padReadIndexReg), _T_11_14, _GEN_50) @[SPadModule.scala 66:14]
    node _GEN_52 = mux(eq(UInt<4>("hf"), padReadIndexReg), _T_11_15, _GEN_51) @[SPadModule.scala 66:14]
    node _T_11_padReadIndexReg = _GEN_52 @[SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14]
    node dataWire = _T_11_padReadIndexReg @[SPadModule.scala 80:28 SPadModule.scala 66:14]
    node _T_10_0 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_1 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_2 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_3 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_4 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_5 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_6 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_7 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_8 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_9 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_10 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_11 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_12 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_13 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_14 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_10_15 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    io_commonIO_columnNum <= padReadIndexReg @[SPadModule.scala 103:25]
    io_commonIO_readOutData <= dataWire @[SPadModule.scala 69:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[SPadModule.scala 101:24]
    io_commonIO_dataLenFinIO_writeInDataIO_ready <= _GEN_1 @[SPadModule.scala 92:27 SPadModule.scala 98:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_9 @[SPadModule.scala 102:37]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[SPadModule.scala 81:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[SPadModule.scala 93:22 SPadModule.scala 95:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_36) @[SPadModule.scala 60:23 SPadModule.scala 62:25]
    _T_11_0 <= mux(reset, _T_10_0, _GEN_19) @[SPadModule.scala 56:34]
    _T_11_1 <= mux(reset, _T_10_1, _GEN_20) @[SPadModule.scala 56:34]
    _T_11_2 <= mux(reset, _T_10_2, _GEN_21) @[SPadModule.scala 56:34]
    _T_11_3 <= mux(reset, _T_10_3, _GEN_22) @[SPadModule.scala 56:34]
    _T_11_4 <= mux(reset, _T_10_4, _GEN_23) @[SPadModule.scala 56:34]
    _T_11_5 <= mux(reset, _T_10_5, _GEN_24) @[SPadModule.scala 56:34]
    _T_11_6 <= mux(reset, _T_10_6, _GEN_25) @[SPadModule.scala 56:34]
    _T_11_7 <= mux(reset, _T_10_7, _GEN_26) @[SPadModule.scala 56:34]
    _T_11_8 <= mux(reset, _T_10_8, _GEN_27) @[SPadModule.scala 56:34]
    _T_11_9 <= mux(reset, _T_10_9, _GEN_28) @[SPadModule.scala 56:34]
    _T_11_10 <= mux(reset, _T_10_10, _GEN_29) @[SPadModule.scala 56:34]
    _T_11_11 <= mux(reset, _T_10_11, _GEN_30) @[SPadModule.scala 56:34]
    _T_11_12 <= mux(reset, _T_10_12, _GEN_31) @[SPadModule.scala 56:34]
    _T_11_13 <= mux(reset, _T_10_13, _GEN_32) @[SPadModule.scala 56:34]
    _T_11_14 <= mux(reset, _T_10_14, _GEN_33) @[SPadModule.scala 56:34]
    _T_11_15 <= mux(reset, _T_10_15, _GEN_34) @[SPadModule.scala 56:34]

  module WeightSPadAddrModule :
    input clock : Clock
    input reset : UInt<1>
    output io_commonIO_columnNum : UInt<4>
    output io_commonIO_readOutData : UInt<7>
    input io_commonIO_readEn : UInt<1>
    input io_commonIO_writeEn : UInt<1>
    output io_commonIO_writeIdx : UInt<4>
    output io_commonIO_dataLenFinIO_writeInDataIO_ready : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_valid : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_bits_data : UInt<7>
    input io_commonIO_dataLenFinIO_streamLen : UInt<4>
    output io_commonIO_dataLenFinIO_writeFin : UInt<1>
    input io_addrIO_readInIdx : UInt<4>
    input io_addrIO_indexInc : UInt<1>
    input io_addrIO_readInIdxEn : UInt<1>
    input io_dataIO_readInIdx : UInt<4>
    input io_dataIO_indexInc : UInt<1>
    input io_dataIO_readInIdxEn : UInt<1>
  
    reg dataLenReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[SPadModule.scala 79:33]
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 82:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 83:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 88:53]
    node _T_1 = tail(_T, 1) @[SPadModule.scala 88:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[SPadModule.scala 88:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 89:51]
    node _T_4 = tail(_T_3, 1) @[SPadModule.scala 89:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[SPadModule.scala 89:35]
    node _T_6 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 91:31]
    node _T_7 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 93:42]
    node _T_8 = tail(_T_7, 1) @[SPadModule.scala 93:42]
    node writeWrapWire = _T_2 @[SPadModule.scala 85:33 SPadModule.scala 88:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_8) @[SPadModule.scala 94:26]
    node _GEN_1 = mux(_T_6, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 91:55]
    node _GEN_2 = mux(_T_6, _GEN_0, padWriteIndexReg) @[SPadModule.scala 91:55]
    node _T_9 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, writeWrapWire) @[SPadModule.scala 102:62]
    reg addrSPad_0 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_0) @[SPadModule.scala 14:36]
    reg addrSPad_1 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_1) @[SPadModule.scala 14:36]
    reg addrSPad_2 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_2) @[SPadModule.scala 14:36]
    reg addrSPad_3 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_3) @[SPadModule.scala 14:36]
    reg addrSPad_4 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_4) @[SPadModule.scala 14:36]
    reg addrSPad_5 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_5) @[SPadModule.scala 14:36]
    reg addrSPad_6 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_6) @[SPadModule.scala 14:36]
    reg addrSPad_7 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_7) @[SPadModule.scala 14:36]
    reg addrSPad_8 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_8) @[SPadModule.scala 14:36]
    reg addrSPad_9 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_9) @[SPadModule.scala 14:36]
    reg addrSPad_10 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_10) @[SPadModule.scala 14:36]
    reg addrSPad_11 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_11) @[SPadModule.scala 14:36]
    reg addrSPad_12 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_12) @[SPadModule.scala 14:36]
    reg addrSPad_13 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_13) @[SPadModule.scala 14:36]
    reg addrSPad_14 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_14) @[SPadModule.scala 14:36]
    reg addrSPad_15 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addrSPad_15) @[SPadModule.scala 14:36]
    node _T_11 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 16:31]
    node _addrSPad_padWriteIndexReg = io_commonIO_dataLenFinIO_writeInDataIO_bits_data @[SPadModule.scala 17:32 SPadModule.scala 17:32]
    node _GEN_3 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_0) @[SPadModule.scala 17:32]
    node _GEN_4 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_1) @[SPadModule.scala 17:32]
    node _GEN_5 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_2) @[SPadModule.scala 17:32]
    node _GEN_6 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_3) @[SPadModule.scala 17:32]
    node _GEN_7 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_4) @[SPadModule.scala 17:32]
    node _GEN_8 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_5) @[SPadModule.scala 17:32]
    node _GEN_9 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_6) @[SPadModule.scala 17:32]
    node _GEN_10 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_7) @[SPadModule.scala 17:32]
    node _GEN_11 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_8) @[SPadModule.scala 17:32]
    node _GEN_12 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_9) @[SPadModule.scala 17:32]
    node _GEN_13 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_10) @[SPadModule.scala 17:32]
    node _GEN_14 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_11) @[SPadModule.scala 17:32]
    node _GEN_15 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_12) @[SPadModule.scala 17:32]
    node _GEN_16 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_13) @[SPadModule.scala 17:32]
    node _GEN_17 = mux(eq(UInt<4>("he"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_14) @[SPadModule.scala 17:32]
    node _GEN_18 = mux(eq(UInt<4>("hf"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_15) @[SPadModule.scala 17:32]
    node _GEN_19 = mux(_T_11, _GEN_3, addrSPad_0) @[SPadModule.scala 16:55]
    node _GEN_20 = mux(_T_11, _GEN_4, addrSPad_1) @[SPadModule.scala 16:55]
    node _GEN_21 = mux(_T_11, _GEN_5, addrSPad_2) @[SPadModule.scala 16:55]
    node _GEN_22 = mux(_T_11, _GEN_6, addrSPad_3) @[SPadModule.scala 16:55]
    node _GEN_23 = mux(_T_11, _GEN_7, addrSPad_4) @[SPadModule.scala 16:55]
    node _GEN_24 = mux(_T_11, _GEN_8, addrSPad_5) @[SPadModule.scala 16:55]
    node _GEN_25 = mux(_T_11, _GEN_9, addrSPad_6) @[SPadModule.scala 16:55]
    node _GEN_26 = mux(_T_11, _GEN_10, addrSPad_7) @[SPadModule.scala 16:55]
    node _GEN_27 = mux(_T_11, _GEN_11, addrSPad_8) @[SPadModule.scala 16:55]
    node _GEN_28 = mux(_T_11, _GEN_12, addrSPad_9) @[SPadModule.scala 16:55]
    node _GEN_29 = mux(_T_11, _GEN_13, addrSPad_10) @[SPadModule.scala 16:55]
    node _GEN_30 = mux(_T_11, _GEN_14, addrSPad_11) @[SPadModule.scala 16:55]
    node _GEN_31 = mux(_T_11, _GEN_15, addrSPad_12) @[SPadModule.scala 16:55]
    node _GEN_32 = mux(_T_11, _GEN_16, addrSPad_13) @[SPadModule.scala 16:55]
    node _GEN_33 = mux(_T_11, _GEN_17, addrSPad_14) @[SPadModule.scala 16:55]
    node _GEN_34 = mux(_T_11, _GEN_18, addrSPad_15) @[SPadModule.scala 16:55]
    node _T_12 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 21:40]
    node _T_13 = tail(_T_12, 1) @[SPadModule.scala 21:40]
    node readWrapWire = _T_5 @[SPadModule.scala 86:32 SPadModule.scala 89:16]
    node _GEN_35 = mux(readWrapWire, UInt<1>("h0"), _T_13) @[SPadModule.scala 22:25]
    node readIndexInc = io_addrIO_indexInc @[SPadModule.scala 87:32 SPadModule.scala 29:16]
    node _GEN_36 = mux(readIndexInc, _GEN_35, padReadIndexReg) @[SPadModule.scala 20:23]
    node _GEN_37 = validif(eq(UInt<1>("h0"), padReadIndexReg), addrSPad_0) @[SPadModule.scala 27:12]
    node _GEN_38 = mux(eq(UInt<1>("h1"), padReadIndexReg), addrSPad_1, _GEN_37) @[SPadModule.scala 27:12]
    node _GEN_39 = mux(eq(UInt<2>("h2"), padReadIndexReg), addrSPad_2, _GEN_38) @[SPadModule.scala 27:12]
    node _GEN_40 = mux(eq(UInt<2>("h3"), padReadIndexReg), addrSPad_3, _GEN_39) @[SPadModule.scala 27:12]
    node _GEN_41 = mux(eq(UInt<3>("h4"), padReadIndexReg), addrSPad_4, _GEN_40) @[SPadModule.scala 27:12]
    node _GEN_42 = mux(eq(UInt<3>("h5"), padReadIndexReg), addrSPad_5, _GEN_41) @[SPadModule.scala 27:12]
    node _GEN_43 = mux(eq(UInt<3>("h6"), padReadIndexReg), addrSPad_6, _GEN_42) @[SPadModule.scala 27:12]
    node _GEN_44 = mux(eq(UInt<3>("h7"), padReadIndexReg), addrSPad_7, _GEN_43) @[SPadModule.scala 27:12]
    node _GEN_45 = mux(eq(UInt<4>("h8"), padReadIndexReg), addrSPad_8, _GEN_44) @[SPadModule.scala 27:12]
    node _GEN_46 = mux(eq(UInt<4>("h9"), padReadIndexReg), addrSPad_9, _GEN_45) @[SPadModule.scala 27:12]
    node _GEN_47 = mux(eq(UInt<4>("ha"), padReadIndexReg), addrSPad_10, _GEN_46) @[SPadModule.scala 27:12]
    node _GEN_48 = mux(eq(UInt<4>("hb"), padReadIndexReg), addrSPad_11, _GEN_47) @[SPadModule.scala 27:12]
    node _GEN_49 = mux(eq(UInt<4>("hc"), padReadIndexReg), addrSPad_12, _GEN_48) @[SPadModule.scala 27:12]
    node _GEN_50 = mux(eq(UInt<4>("hd"), padReadIndexReg), addrSPad_13, _GEN_49) @[SPadModule.scala 27:12]
    node _GEN_51 = mux(eq(UInt<4>("he"), padReadIndexReg), addrSPad_14, _GEN_50) @[SPadModule.scala 27:12]
    node _GEN_52 = mux(eq(UInt<4>("hf"), padReadIndexReg), addrSPad_15, _GEN_51) @[SPadModule.scala 27:12]
    node _GEN_53 = mux(io_addrIO_readInIdxEn, io_addrIO_readInIdx, _GEN_36) @[SPadModule.scala 7:32]
    node _addrSPad_padReadIndexReg = _GEN_52 @[SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12]
    node dataWire = _addrSPad_padReadIndexReg @[SPadModule.scala 80:28 SPadModule.scala 27:12]
    node _T_10_0 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_1 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_2 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_3 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_4 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_5 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_6 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_7 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_8 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_9 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_10 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_11 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_12 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_13 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_14 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_10_15 = UInt<7>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    io_commonIO_columnNum <= padReadIndexReg @[SPadModule.scala 103:25]
    io_commonIO_readOutData <= dataWire @[SPadModule.scala 28:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[SPadModule.scala 101:24]
    io_commonIO_dataLenFinIO_writeInDataIO_ready <= _GEN_1 @[SPadModule.scala 92:27 SPadModule.scala 98:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_9 @[SPadModule.scala 102:37]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[SPadModule.scala 81:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[SPadModule.scala 93:22 SPadModule.scala 95:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_53) @[SPadModule.scala 21:21 SPadModule.scala 23:23 SPadModule.scala 8:21]
    addrSPad_0 <= mux(reset, _T_10_0, _GEN_19) @[SPadModule.scala 17:32]
    addrSPad_1 <= mux(reset, _T_10_1, _GEN_20) @[SPadModule.scala 17:32]
    addrSPad_2 <= mux(reset, _T_10_2, _GEN_21) @[SPadModule.scala 17:32]
    addrSPad_3 <= mux(reset, _T_10_3, _GEN_22) @[SPadModule.scala 17:32]
    addrSPad_4 <= mux(reset, _T_10_4, _GEN_23) @[SPadModule.scala 17:32]
    addrSPad_5 <= mux(reset, _T_10_5, _GEN_24) @[SPadModule.scala 17:32]
    addrSPad_6 <= mux(reset, _T_10_6, _GEN_25) @[SPadModule.scala 17:32]
    addrSPad_7 <= mux(reset, _T_10_7, _GEN_26) @[SPadModule.scala 17:32]
    addrSPad_8 <= mux(reset, _T_10_8, _GEN_27) @[SPadModule.scala 17:32]
    addrSPad_9 <= mux(reset, _T_10_9, _GEN_28) @[SPadModule.scala 17:32]
    addrSPad_10 <= mux(reset, _T_10_10, _GEN_29) @[SPadModule.scala 17:32]
    addrSPad_11 <= mux(reset, _T_10_11, _GEN_30) @[SPadModule.scala 17:32]
    addrSPad_12 <= mux(reset, _T_10_12, _GEN_31) @[SPadModule.scala 17:32]
    addrSPad_13 <= mux(reset, _T_10_13, _GEN_32) @[SPadModule.scala 17:32]
    addrSPad_14 <= mux(reset, _T_10_14, _GEN_33) @[SPadModule.scala 17:32]
    addrSPad_15 <= mux(reset, _T_10_15, _GEN_34) @[SPadModule.scala 17:32]

  module SPadDataModule_1 :
    input clock : Clock
    input reset : UInt<1>
    output io_commonIO_columnNum : UInt<8>
    output io_commonIO_readOutData : UInt<12>
    input io_commonIO_readEn : UInt<1>
    input io_commonIO_writeEn : UInt<1>
    output io_commonIO_writeIdx : UInt<8>
    output io_commonIO_dataLenFinIO_writeInDataIO_ready : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_valid : UInt<1>
    input io_commonIO_dataLenFinIO_writeInDataIO_bits_data : UInt<12>
    input io_commonIO_dataLenFinIO_streamLen : UInt<8>
    output io_commonIO_dataLenFinIO_writeFin : UInt<1>
    input io_addrIO_readInIdx : UInt<8>
    input io_addrIO_indexInc : UInt<1>
    input io_addrIO_readInIdxEn : UInt<1>
    input io_dataIO_readInIdx : UInt<8>
    input io_dataIO_indexInc : UInt<1>
    input io_dataIO_readInIdxEn : UInt<1>
  
    mem _T_10 : @[SPadModule.scala 35:50]
      data-type => UInt<12>
      depth => 192
      read-latency => 1
      write-latency => 1
      reader => _T_18
      writer => _T_12
      read-under-write => undefined
    reg dataLenReg : UInt<8>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[SPadModule.scala 79:33]
    reg padWriteIndexReg : UInt<8>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 82:39]
    reg padReadIndexReg : UInt<8>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 83:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 88:53]
    node _T_1 = tail(_T, 1) @[SPadModule.scala 88:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[SPadModule.scala 88:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 89:51]
    node _T_4 = tail(_T_3, 1) @[SPadModule.scala 89:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[SPadModule.scala 89:35]
    node _T_6 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 91:31]
    node _T_7 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 93:42]
    node _T_8 = tail(_T_7, 1) @[SPadModule.scala 93:42]
    node writeWrapWire = _T_2 @[SPadModule.scala 85:33 SPadModule.scala 88:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_8) @[SPadModule.scala 94:26]
    node _GEN_1 = mux(_T_6, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 91:55]
    node _GEN_2 = mux(_T_6, _GEN_0, padWriteIndexReg) @[SPadModule.scala 91:55]
    node _T_9 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, writeWrapWire) @[SPadModule.scala 102:62]
    node _T_11 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 37:33]
    node _GEN_3 = validif(_T_11, padWriteIndexReg) @[SPadModule.scala 37:57]
    node _GEN_4 = validif(_T_11, clock) @[SPadModule.scala 37:57]
    node _GEN_5 = mux(_T_11, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 37:57]
    node _GEN_6 = validif(_T_11, UInt<1>("h1")) @[SPadModule.scala 37:57]
    node _GEN_7 = validif(_T_11, io_commonIO_dataLenFinIO_writeInDataIO_bits_data) @[SPadModule.scala 37:57]
    node _T_13 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 45:44]
    node _T_14 = tail(_T_13, 1) @[SPadModule.scala 45:44]
    node readWrapWire = _T_5 @[SPadModule.scala 86:32 SPadModule.scala 89:16]
    node _GEN_8 = mux(readWrapWire, UInt<1>("h0"), _T_14) @[SPadModule.scala 46:29]
    node readIndexInc = io_dataIO_indexInc @[SPadModule.scala 87:32 SPadModule.scala 68:16]
    node _GEN_9 = mux(readIndexInc, _GEN_8, padReadIndexReg) @[SPadModule.scala 44:27]
    node _GEN_10 = mux(io_dataIO_readInIdxEn, io_dataIO_readInIdx, _GEN_9) @[SPadModule.scala 41:34]
    node _GEN_11 = validif(io_commonIO_readEn, padReadIndexReg) @[SPadModule.scala 52:30]
    node _T_15 = _GEN_11 @[SPadModule.scala 52:30 SPadModule.scala 52:30]
    node _T_16 = or(_T_15, UInt<8>("h0")) @[SPadModule.scala 52:30]
    node _T_17 = bits(_T_16, 7, 0) @[SPadModule.scala 52:30]
    node _GEN_12 = mux(io_commonIO_readEn, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 52:30]
    node _GEN_13 = validif(io_commonIO_readEn, _T_17) @[SPadModule.scala 52:30]
    node _GEN_14 = validif(io_commonIO_readEn, clock) @[SPadModule.scala 52:30]
    node dataWire = _T_10._T_18.data @[SPadModule.scala 80:28 SPadModule.scala 52:14]
    io_commonIO_columnNum <= padReadIndexReg @[SPadModule.scala 103:25]
    io_commonIO_readOutData <= dataWire @[SPadModule.scala 69:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[SPadModule.scala 101:24]
    io_commonIO_dataLenFinIO_writeInDataIO_ready <= _GEN_1 @[SPadModule.scala 92:27 SPadModule.scala 98:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_9 @[SPadModule.scala 102:37]
    dataLenReg <= mux(reset, UInt<8>("h9"), io_commonIO_dataLenFinIO_streamLen) @[SPadModule.scala 81:14]
    padWriteIndexReg <= mux(reset, UInt<8>("h0"), _GEN_2) @[SPadModule.scala 93:22 SPadModule.scala 95:24]
    padReadIndexReg <= mux(reset, UInt<8>("h0"), _GEN_10) @[SPadModule.scala 42:23 SPadModule.scala 45:25 SPadModule.scala 47:27]
    _T_10._T_18.addr <= _GEN_13 @[SPadModule.scala 52:30]
    _T_10._T_18.en <= _GEN_12 @[SPadModule.scala 35:50 SPadModule.scala 52:30]
    _T_10._T_18.clk <= _GEN_14 @[SPadModule.scala 52:30]
    _T_10._T_12.addr <= _GEN_3
    _T_10._T_12.en <= _GEN_5 @[SPadModule.scala 35:50]
    _T_10._T_12.clk <= _GEN_4
    _T_10._T_12.data <= _GEN_7
    _T_10._T_12.mask <= _GEN_6

  module ProcessingElementPad :
    input clock : Clock
    input reset : UInt<1>
    input io_padCtrl_doMACEn : UInt<1>
    output io_padCtrl_fromTopIO_pSumEnqOrProduct_ready : UInt<1>
    input io_padCtrl_fromTopIO_pSumEnqOrProduct_valid : UInt<1>
    input io_padCtrl_fromTopIO_pSumEnqOrProduct_bits : UInt<1>
    input io_padCtrl_fromTopIO_doLoadEn : UInt<1>
    output io_padCtrl_fromTopIO_writeFinish : UInt<1>
    output io_padCtrl_fromTopIO_calFinish : UInt<1>
    output io_dataStream_ipsIO_ready : UInt<1>
    input io_dataStream_ipsIO_valid : UInt<1>
    input io_dataStream_ipsIO_bits : UInt<20>
    input io_dataStream_opsIO_ready : UInt<1>
    output io_dataStream_opsIO_valid : UInt<1>
    output io_dataStream_opsIO_bits : UInt<20>
    output io_dataStream_iactIOs_dataIOs_writeInDataIO_ready : UInt<1>
    input io_dataStream_iactIOs_dataIOs_writeInDataIO_valid : UInt<1>
    input io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data : UInt<12>
    input io_dataStream_iactIOs_dataIOs_streamLen : UInt<4>
    output io_dataStream_iactIOs_dataIOs_writeFin : UInt<1>
    output io_dataStream_iactIOs_addrIOs_writeInDataIO_ready : UInt<1>
    input io_dataStream_iactIOs_addrIOs_writeInDataIO_valid : UInt<1>
    input io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data : UInt<4>
    input io_dataStream_iactIOs_addrIOs_streamLen : UInt<4>
    output io_dataStream_iactIOs_addrIOs_writeFin : UInt<1>
    output io_dataStream_weightIOs_dataIOs_writeInDataIO_ready : UInt<1>
    input io_dataStream_weightIOs_dataIOs_writeInDataIO_valid : UInt<1>
    input io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data : UInt<12>
    input io_dataStream_weightIOs_dataIOs_streamLen : UInt<8>
    output io_dataStream_weightIOs_dataIOs_writeFin : UInt<1>
    output io_dataStream_weightIOs_addrIOs_writeInDataIO_ready : UInt<1>
    input io_dataStream_weightIOs_addrIOs_writeInDataIO_valid : UInt<1>
    input io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data : UInt<7>
    input io_dataStream_weightIOs_addrIOs_streamLen : UInt<4>
    output io_dataStream_weightIOs_addrIOs_writeFin : UInt<1>
    output io_debugIO_iactMatrixData : UInt<8>
    output io_debugIO_iactMatrixRow : UInt<4>
    output io_debugIO_iactMatrixColumn : UInt<4>
    output io_debugIO_iactAddrInc : UInt<1>
    output io_debugIO_iactDataInc : UInt<1>
    output io_debugIO_iactAddrIdx : UInt<4>
    output io_debugIO_weightAddrSPadReadOut : UInt<7>
    output io_debugIO_weightMatrixData : UInt<8>
    output io_debugIO_weightMatrixRow : UInt<4>
    output io_debugIO_productResult : UInt<20>
    output io_debugIO_pSumResult : UInt<20>
    output io_debugIO_pSumLoad : UInt<20>
    output io_debugIO_weightAddrInIdx : UInt<4>
    output io_debugIO_sPadState : UInt<3>
  
    inst SPadSeq_0 of SPadAddrModule @[ProcessingElement.scala 123:44]
    inst SPadSeq_1 of SPadDataModule @[ProcessingElement.scala 124:44]
    inst SPadSeq_2 of WeightSPadAddrModule @[ProcessingElement.scala 125:46]
    inst SPadSeq_3 of SPadDataModule_1 @[ProcessingElement.scala 126:46]
    reg psDataSPad_0 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_0) @[ProcessingElement.scala 122:38]
    reg psDataSPad_1 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_1) @[ProcessingElement.scala 122:38]
    reg psDataSPad_2 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_2) @[ProcessingElement.scala 122:38]
    reg psDataSPad_3 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_3) @[ProcessingElement.scala 122:38]
    reg psDataSPad_4 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_4) @[ProcessingElement.scala 122:38]
    reg psDataSPad_5 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_5) @[ProcessingElement.scala 122:38]
    reg psDataSPad_6 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_6) @[ProcessingElement.scala 122:38]
    reg psDataSPad_7 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_7) @[ProcessingElement.scala 122:38]
    reg psDataSPad_8 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_8) @[ProcessingElement.scala 122:38]
    reg psDataSPad_9 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_9) @[ProcessingElement.scala 122:38]
    reg psDataSPad_10 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_10) @[ProcessingElement.scala 122:38]
    reg psDataSPad_11 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_11) @[ProcessingElement.scala 122:38]
    reg psDataSPad_12 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_12) @[ProcessingElement.scala 122:38]
    reg psDataSPad_13 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_13) @[ProcessingElement.scala 122:38]
    reg psDataSPad_14 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_14) @[ProcessingElement.scala 122:38]
    reg psDataSPad_15 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_15) @[ProcessingElement.scala 122:38]
    reg psDataSPad_16 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_16) @[ProcessingElement.scala 122:38]
    reg psDataSPad_17 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_17) @[ProcessingElement.scala 122:38]
    reg psDataSPad_18 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_18) @[ProcessingElement.scala 122:38]
    reg psDataSPad_19 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_19) @[ProcessingElement.scala 122:38]
    reg psDataSPad_20 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_20) @[ProcessingElement.scala 122:38]
    reg psDataSPad_21 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_21) @[ProcessingElement.scala 122:38]
    reg psDataSPad_22 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_22) @[ProcessingElement.scala 122:38]
    reg psDataSPad_23 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_23) @[ProcessingElement.scala 122:38]
    reg psDataSPad_24 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_24) @[ProcessingElement.scala 122:38]
    reg psDataSPad_25 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_25) @[ProcessingElement.scala 122:38]
    reg psDataSPad_26 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_26) @[ProcessingElement.scala 122:38]
    reg psDataSPad_27 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_27) @[ProcessingElement.scala 122:38]
    reg psDataSPad_28 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_28) @[ProcessingElement.scala 122:38]
    reg psDataSPad_29 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_29) @[ProcessingElement.scala 122:38]
    reg psDataSPad_30 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_30) @[ProcessingElement.scala 122:38]
    reg psDataSPad_31 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), psDataSPad_31) @[ProcessingElement.scala 122:38]
    reg iactAddrSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPadReadEnReg) @[ProcessingElement.scala 131:44]
    reg iactDataSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactDataSPadReadEnReg) @[ProcessingElement.scala 132:44]
    reg iactMatrixColumnReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactMatrixColumnReg) @[ProcessingElement.scala 135:42]
    reg iactZeroColumnNumber : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactZeroColumnNumber) @[ProcessingElement.scala 136:43]
    reg iactDataSPadFirstReadReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactDataSPadFirstReadReg) @[ProcessingElement.scala 137:47]
    reg weightAddrSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), weightAddrSPadReadEnReg) @[ProcessingElement.scala 144:46]
    reg weightDataSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), weightDataSPadReadEnReg) @[ProcessingElement.scala 145:46]
    reg weightDataSPadFirstRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), weightDataSPadFirstRead) @[ProcessingElement.scala 150:46]
    reg productReg : UInt<20>, clock with :
      reset => (UInt<1>("h0"), productReg) @[ProcessingElement.scala 157:33]
    reg pSumSPadLoadReg : UInt<20>, clock with :
      reset => (UInt<1>("h0"), pSumSPadLoadReg) @[ProcessingElement.scala 158:38]
    reg sPad : UInt<3>, clock with :
      reset => (UInt<1>("h0"), sPad) @[ProcessingElement.scala 170:27]
    node _T_1 = eq(sPad, UInt<3>("h1")) @[ProcessingElement.scala 176:19]
    node _T_2 = eq(sPad, UInt<3>("h6")) @[ProcessingElement.scala 177:20]
    node _T_3 = eq(sPad, UInt<3>("h7")) @[ProcessingElement.scala 178:19]
    node _T_4 = eq(sPad, UInt<3>("h3")) @[ProcessingElement.scala 179:19]
    node _T_5 = eq(sPad, UInt<3>("h2")) @[ProcessingElement.scala 180:19]
    node iactDataCountVec_3 = bits(SPadSeq_1.io_commonIO_readOutData, 3, 3) @[ProcessingElement.scala 198:74]
    node iactDataCountVec_2 = bits(SPadSeq_1.io_commonIO_readOutData, 2, 2) @[ProcessingElement.scala 198:74]
    node _T_15 = cat(iactDataCountVec_3, iactDataCountVec_2) @[Cat.scala 29:58]
    node iactDataCountVec_1 = bits(SPadSeq_1.io_commonIO_readOutData, 1, 1) @[ProcessingElement.scala 198:74]
    node iactDataCountVec_0 = bits(SPadSeq_1.io_commonIO_readOutData, 0, 0) @[ProcessingElement.scala 198:74]
    node _T_14 = cat(iactDataCountVec_1, iactDataCountVec_0) @[Cat.scala 29:58]
    node _T_16 = cat(_T_15, _T_14) @[Cat.scala 29:58]
    node iactMatrixRowWire = _T_16 @[ProcessingElement.scala 138:37 ProcessingElement.scala 200:21]
    node _T_6 = eq(iactMatrixRowWire, UInt<1>("h0")) @[ProcessingElement.scala 181:52]
    node iactDataCountVec_4 = bits(SPadSeq_1.io_commonIO_readOutData, 4, 4) @[ProcessingElement.scala 198:74]
    node iactDataCountVec_5 = bits(SPadSeq_1.io_commonIO_readOutData, 5, 5) @[ProcessingElement.scala 198:74]
    node iactDataCountVec_6 = bits(SPadSeq_1.io_commonIO_readOutData, 6, 6) @[ProcessingElement.scala 198:74]
    node iactDataCountVec_7 = bits(SPadSeq_1.io_commonIO_readOutData, 7, 7) @[ProcessingElement.scala 198:74]
    node iactDataCountVec_8 = bits(SPadSeq_1.io_commonIO_readOutData, 8, 8) @[ProcessingElement.scala 198:74]
    node iactDataCountVec_9 = bits(SPadSeq_1.io_commonIO_readOutData, 9, 9) @[ProcessingElement.scala 198:74]
    node iactDataCountVec_10 = bits(SPadSeq_1.io_commonIO_readOutData, 10, 10) @[ProcessingElement.scala 198:74]
    node iactDataCountVec_11 = bits(SPadSeq_1.io_commonIO_readOutData, 11, 11) @[ProcessingElement.scala 198:74]
    node _T_7 = cat(iactDataCountVec_5, iactDataCountVec_4) @[Cat.scala 29:58]
    node _T_8 = cat(iactDataCountVec_7, iactDataCountVec_6) @[Cat.scala 29:58]
    node _T_9 = cat(_T_8, _T_7) @[Cat.scala 29:58]
    node _T_10 = cat(iactDataCountVec_9, iactDataCountVec_8) @[Cat.scala 29:58]
    node _T_11 = cat(iactDataCountVec_11, iactDataCountVec_10) @[Cat.scala 29:58]
    node _T_12 = cat(_T_11, _T_10) @[Cat.scala 29:58]
    node _T_13 = cat(_T_12, _T_9) @[Cat.scala 29:58]
    node weightDataCountVec_0 = bits(SPadSeq_3.io_commonIO_readOutData, 0, 0) @[ProcessingElement.scala 220:78]
    node weightDataCountVec_1 = bits(SPadSeq_3.io_commonIO_readOutData, 1, 1) @[ProcessingElement.scala 220:78]
    node weightDataCountVec_2 = bits(SPadSeq_3.io_commonIO_readOutData, 2, 2) @[ProcessingElement.scala 220:78]
    node weightDataCountVec_3 = bits(SPadSeq_3.io_commonIO_readOutData, 3, 3) @[ProcessingElement.scala 220:78]
    node weightDataCountVec_4 = bits(SPadSeq_3.io_commonIO_readOutData, 4, 4) @[ProcessingElement.scala 220:78]
    node weightDataCountVec_5 = bits(SPadSeq_3.io_commonIO_readOutData, 5, 5) @[ProcessingElement.scala 220:78]
    node weightDataCountVec_6 = bits(SPadSeq_3.io_commonIO_readOutData, 6, 6) @[ProcessingElement.scala 220:78]
    node weightDataCountVec_7 = bits(SPadSeq_3.io_commonIO_readOutData, 7, 7) @[ProcessingElement.scala 220:78]
    node weightDataCountVec_8 = bits(SPadSeq_3.io_commonIO_readOutData, 8, 8) @[ProcessingElement.scala 220:78]
    node weightDataCountVec_9 = bits(SPadSeq_3.io_commonIO_readOutData, 9, 9) @[ProcessingElement.scala 220:78]
    node weightDataCountVec_10 = bits(SPadSeq_3.io_commonIO_readOutData, 10, 10) @[ProcessingElement.scala 220:78]
    node weightDataCountVec_11 = bits(SPadSeq_3.io_commonIO_readOutData, 11, 11) @[ProcessingElement.scala 220:78]
    node _T_17 = cat(weightDataCountVec_5, weightDataCountVec_4) @[Cat.scala 29:58]
    node _T_18 = cat(weightDataCountVec_7, weightDataCountVec_6) @[Cat.scala 29:58]
    node _T_19 = cat(_T_18, _T_17) @[Cat.scala 29:58]
    node _T_20 = cat(weightDataCountVec_9, weightDataCountVec_8) @[Cat.scala 29:58]
    node _T_21 = cat(weightDataCountVec_11, weightDataCountVec_10) @[Cat.scala 29:58]
    node _T_22 = cat(_T_21, _T_20) @[Cat.scala 29:58]
    node _T_23 = cat(_T_22, _T_19) @[Cat.scala 29:58]
    node _T_24 = cat(weightDataCountVec_1, weightDataCountVec_0) @[Cat.scala 29:58]
    node _T_25 = cat(weightDataCountVec_3, weightDataCountVec_2) @[Cat.scala 29:58]
    node _T_26 = cat(_T_25, _T_24) @[Cat.scala 29:58]
    node weightMatrixReadFirstColumn = _T_6 @[ProcessingElement.scala 155:47 ProcessingElement.scala 181:31]
    node weightAddrDataWire = SPadSeq_2.io_commonIO_readOutData @[ProcessingElement.scala 142:38 ProcessingElement.scala 209:22]
    node _T_27 = mux(weightMatrixReadFirstColumn, UInt<1>("h0"), weightAddrDataWire) @[ProcessingElement.scala 224:44]
    node padEqMpy = _T_2 @[ProcessingElement.scala 174:28 ProcessingElement.scala 177:12]
    node _T_28 = and(padEqMpy, io_padCtrl_fromTopIO_pSumEnqOrProduct_bits) @[ProcessingElement.scala 229:41]
    reg value : UInt<5>, clock with :
      reset => (UInt<1>("h0"), value) @[Counter.scala 29:33]
    node _T_29 = and(io_padCtrl_fromTopIO_doLoadEn, io_dataStream_opsIO_ready) @[ProcessingElement.scala 231:39]
    node _GEN_0 = validif(eq(UInt<1>("h0"), value), psDataSPad_0) @[ProcessingElement.scala 232:30]
    node _GEN_1 = mux(eq(UInt<1>("h1"), value), psDataSPad_1, _GEN_0) @[ProcessingElement.scala 232:30]
    node _GEN_2 = mux(eq(UInt<2>("h2"), value), psDataSPad_2, _GEN_1) @[ProcessingElement.scala 232:30]
    node _GEN_3 = mux(eq(UInt<2>("h3"), value), psDataSPad_3, _GEN_2) @[ProcessingElement.scala 232:30]
    node _GEN_4 = mux(eq(UInt<3>("h4"), value), psDataSPad_4, _GEN_3) @[ProcessingElement.scala 232:30]
    node _GEN_5 = mux(eq(UInt<3>("h5"), value), psDataSPad_5, _GEN_4) @[ProcessingElement.scala 232:30]
    node _GEN_6 = mux(eq(UInt<3>("h6"), value), psDataSPad_6, _GEN_5) @[ProcessingElement.scala 232:30]
    node _GEN_7 = mux(eq(UInt<3>("h7"), value), psDataSPad_7, _GEN_6) @[ProcessingElement.scala 232:30]
    node _GEN_8 = mux(eq(UInt<4>("h8"), value), psDataSPad_8, _GEN_7) @[ProcessingElement.scala 232:30]
    node _GEN_9 = mux(eq(UInt<4>("h9"), value), psDataSPad_9, _GEN_8) @[ProcessingElement.scala 232:30]
    node _GEN_10 = mux(eq(UInt<4>("ha"), value), psDataSPad_10, _GEN_9) @[ProcessingElement.scala 232:30]
    node _GEN_11 = mux(eq(UInt<4>("hb"), value), psDataSPad_11, _GEN_10) @[ProcessingElement.scala 232:30]
    node _GEN_12 = mux(eq(UInt<4>("hc"), value), psDataSPad_12, _GEN_11) @[ProcessingElement.scala 232:30]
    node _GEN_13 = mux(eq(UInt<4>("hd"), value), psDataSPad_13, _GEN_12) @[ProcessingElement.scala 232:30]
    node _GEN_14 = mux(eq(UInt<4>("he"), value), psDataSPad_14, _GEN_13) @[ProcessingElement.scala 232:30]
    node _GEN_15 = mux(eq(UInt<4>("hf"), value), psDataSPad_15, _GEN_14) @[ProcessingElement.scala 232:30]
    node _GEN_16 = mux(eq(UInt<5>("h10"), value), psDataSPad_16, _GEN_15) @[ProcessingElement.scala 232:30]
    node _GEN_17 = mux(eq(UInt<5>("h11"), value), psDataSPad_17, _GEN_16) @[ProcessingElement.scala 232:30]
    node _GEN_18 = mux(eq(UInt<5>("h12"), value), psDataSPad_18, _GEN_17) @[ProcessingElement.scala 232:30]
    node _GEN_19 = mux(eq(UInt<5>("h13"), value), psDataSPad_19, _GEN_18) @[ProcessingElement.scala 232:30]
    node _GEN_20 = mux(eq(UInt<5>("h14"), value), psDataSPad_20, _GEN_19) @[ProcessingElement.scala 232:30]
    node _GEN_21 = mux(eq(UInt<5>("h15"), value), psDataSPad_21, _GEN_20) @[ProcessingElement.scala 232:30]
    node _GEN_22 = mux(eq(UInt<5>("h16"), value), psDataSPad_22, _GEN_21) @[ProcessingElement.scala 232:30]
    node _GEN_23 = mux(eq(UInt<5>("h17"), value), psDataSPad_23, _GEN_22) @[ProcessingElement.scala 232:30]
    node _GEN_24 = mux(eq(UInt<5>("h18"), value), psDataSPad_24, _GEN_23) @[ProcessingElement.scala 232:30]
    node _GEN_25 = mux(eq(UInt<5>("h19"), value), psDataSPad_25, _GEN_24) @[ProcessingElement.scala 232:30]
    node _GEN_26 = mux(eq(UInt<5>("h1a"), value), psDataSPad_26, _GEN_25) @[ProcessingElement.scala 232:30]
    node _GEN_27 = mux(eq(UInt<5>("h1b"), value), psDataSPad_27, _GEN_26) @[ProcessingElement.scala 232:30]
    node _GEN_28 = mux(eq(UInt<5>("h1c"), value), psDataSPad_28, _GEN_27) @[ProcessingElement.scala 232:30]
    node _GEN_29 = mux(eq(UInt<5>("h1d"), value), psDataSPad_29, _GEN_28) @[ProcessingElement.scala 232:30]
    node _GEN_30 = mux(eq(UInt<5>("h1e"), value), psDataSPad_30, _GEN_29) @[ProcessingElement.scala 232:30]
    node _GEN_31 = mux(eq(UInt<5>("h1f"), value), psDataSPad_31, _GEN_30) @[ProcessingElement.scala 232:30]
    node _T_30 = eq(value, UInt<5>("h18")) @[Counter.scala 37:24]
    node _T_31 = add(value, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_32 = tail(_T_31, 1) @[Counter.scala 38:22]
    node _GEN_32 = mux(_T_30, UInt<1>("h0"), _T_32) @[Counter.scala 40:21]
    node _psDataSPad_value = _GEN_31 @[ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30 ProcessingElement.scala 232:30]
    node _GEN_33 = validif(_T_29, _psDataSPad_value) @[ProcessingElement.scala 231:69]
    node _GEN_34 = mux(_T_29, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 231:69]
    node _GEN_35 = mux(_T_29, _GEN_32, value) @[ProcessingElement.scala 231:69]
    node _GEN_36 = mux(SPadSeq_0.io_commonIO_dataLenFinIO_writeFin, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 243:57]
    node _GEN_37 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), _GEN_36) @[ProcessingElement.scala 246:31]
    node _GEN_38 = mux(SPadSeq_1.io_commonIO_dataLenFinIO_writeFin, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 243:57]
    node _GEN_39 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), _GEN_38) @[ProcessingElement.scala 246:31]
    node _GEN_40 = mux(SPadSeq_2.io_commonIO_dataLenFinIO_writeFin, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 243:57]
    node _GEN_41 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), _GEN_40) @[ProcessingElement.scala 246:31]
    node _GEN_42 = mux(SPadSeq_3.io_commonIO_dataLenFinIO_writeFin, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 243:57]
    node _GEN_43 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), _GEN_42) @[ProcessingElement.scala 246:31]
    node SPadWriteFinReg_0 = _GEN_37 @[ProcessingElement.scala 241:43 ProcessingElement.scala 241:43 ProcessingElement.scala 244:26 ProcessingElement.scala 247:26]
    node SPadWriteFinReg_1 = _GEN_39 @[ProcessingElement.scala 241:43 ProcessingElement.scala 241:43 ProcessingElement.scala 244:26 ProcessingElement.scala 247:26]
    node _T_33 = and(SPadWriteFinReg_0, SPadWriteFinReg_1) @[ProcessingElement.scala 250:64]
    node SPadWriteFinReg_2 = _GEN_41 @[ProcessingElement.scala 241:43 ProcessingElement.scala 241:43 ProcessingElement.scala 244:26 ProcessingElement.scala 247:26]
    node _T_34 = and(_T_33, SPadWriteFinReg_2) @[ProcessingElement.scala 250:64]
    node SPadWriteFinReg_3 = _GEN_43 @[ProcessingElement.scala 241:43 ProcessingElement.scala 241:43 ProcessingElement.scala 244:26 ProcessingElement.scala 247:26]
    node _T_35 = and(_T_34, SPadWriteFinReg_3) @[ProcessingElement.scala 250:64]
    node _T_36 = add(pSumSPadLoadReg, productReg) @[ProcessingElement.scala 251:50]
    node _T_37 = tail(_T_36, 1) @[ProcessingElement.scala 251:50]
    node padEqWB = _T_3 @[ProcessingElement.scala 175:27 ProcessingElement.scala 178:11]
    node _T_38 = mux(padEqWB, _T_37, UInt<1>("h0")) @[ProcessingElement.scala 251:24]
    reg pSumResultIdxReg : UInt<20>, clock with :
      reset => (UInt<1>("h0"), pSumResultIdxReg) @[ProcessingElement.scala 257:39]
    reg iactSPadZeroColumnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactSPadZeroColumnReg) @[ProcessingElement.scala 260:44]
    node iactAddrDataWire = SPadSeq_0.io_commonIO_readOutData @[ProcessingElement.scala 129:36 ProcessingElement.scala 189:20]
    node _T_39 = eq(iactAddrDataWire, UInt<4>("hf")) @[ProcessingElement.scala 267:47]
    node _T_40 = eq(weightAddrDataWire, UInt<7>("h7f")) @[ProcessingElement.scala 268:51]
    node iactDataIndexWire = SPadSeq_1.io_commonIO_columnNum @[ProcessingElement.scala 130:37 ProcessingElement.scala 197:21]
    node _T_41 = add(iactDataIndexWire, UInt<1>("h1")) @[ProcessingElement.scala 269:66]
    node _T_42 = tail(_T_41, 1) @[ProcessingElement.scala 269:66]
    node _T_43 = eq(iactAddrDataWire, _T_42) @[ProcessingElement.scala 269:43]
    node weightDataIndexWire = bits(SPadSeq_3.io_commonIO_columnNum, 3, 0) @[ProcessingElement.scala 143:39 ProcessingElement.scala 219:23]
    node _T_44 = add(weightDataIndexWire, UInt<1>("h1")) @[ProcessingElement.scala 270:72]
    node _T_45 = tail(_T_44, 1) @[ProcessingElement.scala 270:72]
    node _T_46 = eq(weightAddrDataWire, _T_45) @[ProcessingElement.scala 270:47]
    node _T_47 = add(iactDataIndexWire, UInt<1>("h1")) @[ProcessingElement.scala 271:45]
    node _T_48 = tail(_T_47, 1) @[ProcessingElement.scala 271:45]
    node _T_49 = eq(_T_48, io_dataStream_iactIOs_dataIOs_streamLen) @[ProcessingElement.scala 271:52]
    node _T_50 = add(weightDataIndexWire, UInt<1>("h1")) @[ProcessingElement.scala 272:49]
    node _T_51 = tail(_T_50, 1) @[ProcessingElement.scala 272:49]
    node _T_52 = eq(_T_51, io_dataStream_weightIOs_dataIOs_streamLen) @[ProcessingElement.scala 272:56]
    node padEqIA = _T_1 @[ProcessingElement.scala 171:27 ProcessingElement.scala 176:11]
    node mightIactZeroColumnWire = _T_39 @[ProcessingElement.scala 259:43 ProcessingElement.scala 267:27]
    node _T_53 = and(padEqIA, mightIactZeroColumnWire) @[ProcessingElement.scala 273:38]
    node padEqWA = _T_4 @[ProcessingElement.scala 173:27 ProcessingElement.scala 179:11]
    node mightWeightZeroColumnWire = _T_40 @[ProcessingElement.scala 262:45 ProcessingElement.scala 268:29]
    node _T_54 = and(padEqWA, mightWeightZeroColumnWire) @[ProcessingElement.scala 273:80]
    node mightWeightIdxIncWire = _T_46 @[ProcessingElement.scala 263:41 ProcessingElement.scala 270:25]
    node _T_55 = and(padEqWB, mightWeightIdxIncWire) @[ProcessingElement.scala 273:122]
    node _T_56 = or(_T_54, _T_55) @[ProcessingElement.scala 273:110]
    node mightIactIdxIncWire = _T_43 @[ProcessingElement.scala 261:39 ProcessingElement.scala 269:23]
    node _T_57 = and(_T_56, mightIactIdxIncWire) @[ProcessingElement.scala 273:149]
    node _T_58 = or(_T_53, _T_57) @[ProcessingElement.scala 273:66]
    node _T_59 = eq(sPad, UInt<3>("h4")) @[ProcessingElement.scala 274:49]
    node _T_60 = or(padEqMpy, _T_59) @[ProcessingElement.scala 274:41]
    node _T_61 = and(_T_60, mightWeightZeroColumnWire) @[ProcessingElement.scala 274:69]
    node _T_62 = eq(mightIactZeroColumnWire, UInt<1>("h0")) @[ProcessingElement.scala 275:41]
    node _T_63 = and(padEqIA, _T_62) @[ProcessingElement.scala 275:38]
    node _T_64 = eq(iactDataSPadFirstReadReg, UInt<1>("h0")) @[ProcessingElement.scala 275:69]
    node _T_65 = and(_T_63, _T_64) @[ProcessingElement.scala 275:66]
    node _T_66 = and(padEqWA, mightWeightZeroColumnWire) @[ProcessingElement.scala 275:110]
    node _T_67 = and(padEqWB, mightWeightIdxIncWire) @[ProcessingElement.scala 275:152]
    node _T_68 = or(_T_66, _T_67) @[ProcessingElement.scala 275:140]
    node _T_69 = eq(mightIactIdxIncWire, UInt<1>("h0")) @[ProcessingElement.scala 275:182]
    node _T_70 = and(_T_68, _T_69) @[ProcessingElement.scala 275:179]
    node _T_71 = or(_T_65, _T_70) @[ProcessingElement.scala 275:96]
    node _T_72 = eq(mightWeightZeroColumnWire, UInt<1>("h0")) @[ProcessingElement.scala 276:43]
    node _T_73 = and(padEqWA, _T_72) @[ProcessingElement.scala 276:40]
    node _T_74 = eq(weightDataSPadFirstRead, UInt<1>("h0")) @[ProcessingElement.scala 276:73]
    node _T_75 = and(_T_73, _T_74) @[ProcessingElement.scala 276:70]
    node _T_76 = eq(mightWeightIdxIncWire, UInt<1>("h0")) @[ProcessingElement.scala 276:114]
    node _T_77 = and(padEqWB, _T_76) @[ProcessingElement.scala 276:111]
    node _T_78 = or(_T_75, _T_77) @[ProcessingElement.scala 276:99]
    node padEqID = _T_5 @[ProcessingElement.scala 172:27 ProcessingElement.scala 180:11]
    node _T_79 = or(padEqID, padEqWA) @[ProcessingElement.scala 277:35]
    node _T_80 = and(_T_79, weightDataSPadFirstRead) @[ProcessingElement.scala 277:47]
    node _T_81 = and(padEqID, weightDataSPadFirstRead) @[ProcessingElement.scala 278:35]
    node _T_82 = eq(weightMatrixReadFirstColumn, UInt<1>("h0")) @[ProcessingElement.scala 278:65]
    node _T_83 = and(_T_81, _T_82) @[ProcessingElement.scala 278:62]
    node _T_84 = sub(iactMatrixRowWire, UInt<1>("h1")) @[ProcessingElement.scala 279:76]
    node _T_85 = tail(_T_84, 1) @[ProcessingElement.scala 279:76]
    node weightDataIdxMuxWire = _T_83 @[ProcessingElement.scala 151:40 ProcessingElement.scala 278:24]
    node _T_86 = mux(weightDataIdxMuxWire, _T_85, iactMatrixRowWire) @[ProcessingElement.scala 279:35]
    node _T_87 = and(padEqWA, weightDataSPadFirstRead) @[ProcessingElement.scala 280:34]
    node _T_88 = eq(mightWeightZeroColumnWire, UInt<1>("h0")) @[ProcessingElement.scala 280:64]
    node _T_89 = and(_T_87, _T_88) @[ProcessingElement.scala 280:61]
    node mightIactReadFinish = _T_49 @[ProcessingElement.scala 264:39 ProcessingElement.scala 271:23]
    node _T_90 = and(padEqWB, mightIactReadFinish) @[ProcessingElement.scala 281:45]
    node mightWeightReadFinish = _T_52 @[ProcessingElement.scala 265:41 ProcessingElement.scala 272:25]
    node _T_91 = and(_T_90, mightWeightReadFinish) @[ProcessingElement.scala 281:68]
    node _T_92 = mul(iactMatrixColumnReg, UInt<3>("h4")) @[ProcessingElement.scala 282:64]
    node weightMatrixRowReg = _T_26 @[ProcessingElement.scala 182:38 ProcessingElement.scala 222:22]
    node _T_93 = add(weightMatrixRowReg, _T_92) @[ProcessingElement.scala 282:43]
    node _T_94 = tail(_T_93, 1) @[ProcessingElement.scala 282:43]
    node _T_95 = eq(UInt<3>("h0"), sPad) @[Conditional.scala 37:30]
    node _GEN_44 = mux(io_padCtrl_doMACEn, UInt<3>("h1"), sPad) @[ProcessingElement.scala 285:32]
    node _GEN_45 = mux(io_padCtrl_doMACEn, UInt<1>("h1"), iactAddrSPadReadEnReg) @[ProcessingElement.scala 285:32]
    node _GEN_46 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), iactDataSPadReadEnReg) @[ProcessingElement.scala 285:32]
    node _GEN_47 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), weightAddrSPadReadEnReg) @[ProcessingElement.scala 285:32]
    node _GEN_48 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), weightDataSPadReadEnReg) @[ProcessingElement.scala 285:32]
    node _GEN_49 = mux(io_padCtrl_doMACEn, UInt<1>("h1"), weightDataSPadFirstRead) @[ProcessingElement.scala 285:32]
    node _GEN_50 = mux(io_padCtrl_doMACEn, UInt<1>("h1"), iactDataSPadFirstReadReg) @[ProcessingElement.scala 285:32]
    node _T_96 = eq(UInt<3>("h1"), sPad) @[Conditional.scala 37:30]
    node _T_97 = add(iactZeroColumnNumber, UInt<1>("h1")) @[ProcessingElement.scala 294:54]
    node _T_98 = tail(_T_97, 1) @[ProcessingElement.scala 294:54]
    node _GEN_51 = mux(mightIactZeroColumnWire, UInt<3>("h1"), UInt<3>("h2")) @[ProcessingElement.scala 291:38]
    node _GEN_52 = mux(mightIactZeroColumnWire, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 291:38]
    node _GEN_53 = mux(mightIactZeroColumnWire, UInt<1>("h0"), UInt<1>("h1")) @[ProcessingElement.scala 291:38]
    node _GEN_54 = mux(mightIactZeroColumnWire, UInt<1>("h0"), UInt<1>("h0")) @[ProcessingElement.scala 291:38]
    node _GEN_55 = mux(mightIactZeroColumnWire, UInt<1>("h1"), UInt<1>("h1")) @[ProcessingElement.scala 291:38]
    node _GEN_56 = mux(mightIactZeroColumnWire, UInt<1>("h1"), iactSPadZeroColumnReg) @[ProcessingElement.scala 291:38]
    node _GEN_57 = mux(mightIactZeroColumnWire, _T_98, iactZeroColumnNumber) @[ProcessingElement.scala 291:38]
    node _T_99 = eq(UInt<3>("h2"), sPad) @[Conditional.scala 37:30]
    node _T_100 = eq(UInt<3>("h3"), sPad) @[Conditional.scala 37:30]
    node _T_101 = add(iactMatrixColumnReg, UInt<1>("h1")) @[ProcessingElement.scala 309:56]
    node _T_102 = tail(_T_101, 1) @[ProcessingElement.scala 309:56]
    node _T_103 = add(_T_102, iactZeroColumnNumber) @[ProcessingElement.scala 309:62]
    node _T_104 = tail(_T_103, 1) @[ProcessingElement.scala 309:62]
    node _T_105 = add(iactMatrixColumnReg, UInt<1>("h1")) @[ProcessingElement.scala 312:56]
    node _T_106 = tail(_T_105, 1) @[ProcessingElement.scala 312:56]
    node _GEN_58 = mux(iactSPadZeroColumnReg, UInt<1>("h0"), iactSPadZeroColumnReg) @[ProcessingElement.scala 307:40]
    node _GEN_59 = mux(iactSPadZeroColumnReg, _T_104, _T_106) @[ProcessingElement.scala 307:40]
    node _GEN_60 = mux(iactSPadZeroColumnReg, UInt<1>("h0"), iactZeroColumnNumber) @[ProcessingElement.scala 307:40]
    node _GEN_61 = mux(mightIactIdxIncWire, UInt<3>("h1"), UInt<3>("h2")) @[ProcessingElement.scala 305:36]
    node _GEN_62 = mux(mightIactIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 305:36]
    node _GEN_63 = mux(mightIactIdxIncWire, UInt<1>("h0"), UInt<1>("h1")) @[ProcessingElement.scala 305:36]
    node _GEN_64 = mux(mightIactIdxIncWire, UInt<1>("h0"), UInt<1>("h0")) @[ProcessingElement.scala 305:36]
    node _GEN_65 = mux(mightIactIdxIncWire, UInt<1>("h1"), UInt<1>("h1")) @[ProcessingElement.scala 305:36]
    node _GEN_66 = mux(mightIactIdxIncWire, _GEN_58, iactSPadZeroColumnReg) @[ProcessingElement.scala 305:36]
    node _GEN_67 = mux(mightIactIdxIncWire, _GEN_59, iactMatrixColumnReg) @[ProcessingElement.scala 305:36]
    node _GEN_68 = mux(mightIactIdxIncWire, _GEN_60, iactZeroColumnNumber) @[ProcessingElement.scala 305:36]
    node _GEN_69 = mux(mightWeightZeroColumnWire, _GEN_61, UInt<3>("h4")) @[ProcessingElement.scala 304:40]
    node _GEN_70 = mux(mightWeightZeroColumnWire, _GEN_62, UInt<1>("h0")) @[ProcessingElement.scala 304:40]
    node _GEN_71 = mux(mightWeightZeroColumnWire, _GEN_63, UInt<1>("h0")) @[ProcessingElement.scala 304:40]
    node _GEN_72 = mux(mightWeightZeroColumnWire, _GEN_64, UInt<1>("h0")) @[ProcessingElement.scala 304:40]
    node _GEN_73 = mux(mightWeightZeroColumnWire, _GEN_64, UInt<1>("h1")) @[ProcessingElement.scala 304:40]
    node _GEN_74 = mux(mightWeightZeroColumnWire, _GEN_65, weightDataSPadFirstRead) @[ProcessingElement.scala 304:40]
    node _GEN_75 = mux(mightWeightZeroColumnWire, _GEN_66, iactSPadZeroColumnReg) @[ProcessingElement.scala 304:40]
    node _GEN_76 = mux(mightWeightZeroColumnWire, _GEN_67, iactMatrixColumnReg) @[ProcessingElement.scala 304:40]
    node _GEN_77 = mux(mightWeightZeroColumnWire, _GEN_68, iactZeroColumnNumber) @[ProcessingElement.scala 304:40]
    node _T_107 = eq(UInt<3>("h4"), sPad) @[Conditional.scala 37:30]
    node _T_108 = eq(UInt<3>("h5"), sPad) @[Conditional.scala 37:30]
    node _T_109 = eq(UInt<3>("h6"), sPad) @[Conditional.scala 37:30]
    node _GEN_78 = mux(io_dataStream_ipsIO_valid, UInt<3>("h7"), sPad) @[ProcessingElement.scala 331:42]
    node _GEN_79 = mux(io_dataStream_ipsIO_valid, io_dataStream_ipsIO_bits, productReg) @[ProcessingElement.scala 331:42]
    node _GEN_80 = mux(io_dataStream_ipsIO_valid, UInt<1>("h1"), _T_28) @[ProcessingElement.scala 331:42]
    node _psDataSPad_value_0 = _GEN_31 @[ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27 ProcessingElement.scala 335:27]
    node _GEN_81 = mux(io_dataStream_ipsIO_valid, _psDataSPad_value_0, pSumSPadLoadReg) @[ProcessingElement.scala 331:42]
    node weightMatrixDataReg = _T_23 @[ProcessingElement.scala 148:39 ProcessingElement.scala 221:23]
    node iactMatrixDataWire = _T_13 @[ProcessingElement.scala 139:38 ProcessingElement.scala 199:22]
    node _T_110 = mul(weightMatrixDataReg, iactMatrixDataWire) @[ProcessingElement.scala 339:44]
    node _GEN_82 = mux(io_padCtrl_fromTopIO_pSumEnqOrProduct_bits, _GEN_78, UInt<3>("h7")) @[ProcessingElement.scala 330:57]
    node _GEN_83 = mux(io_padCtrl_fromTopIO_pSumEnqOrProduct_bits, _GEN_79, _T_110) @[ProcessingElement.scala 330:57]
    node _GEN_84 = mux(io_padCtrl_fromTopIO_pSumEnqOrProduct_bits, _GEN_80, _T_28) @[ProcessingElement.scala 330:57]
    node _psDataSPad_value_1 = _GEN_31 @[ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25 ProcessingElement.scala 340:25]
    node _GEN_85 = mux(io_padCtrl_fromTopIO_pSumEnqOrProduct_bits, _GEN_81, _psDataSPad_value_1) @[ProcessingElement.scala 330:57]
    node _T_111 = eq(UInt<3>("h7"), sPad) @[Conditional.scala 37:30]
    node psDataSpadIdxWire = bits(_T_94, 4, 0) @[ProcessingElement.scala 266:37 ProcessingElement.scala 282:21]
    node pSumResultWire = _T_38 @[ProcessingElement.scala 159:34 ProcessingElement.scala 251:18]
    node _psDataSPad_psDataSpadIdxWire = pSumResultWire @[ProcessingElement.scala 344:37 ProcessingElement.scala 344:37]
    node _GEN_86 = mux(eq(UInt<1>("h0"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_0) @[ProcessingElement.scala 344:37]
    node _GEN_87 = mux(eq(UInt<1>("h1"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_1) @[ProcessingElement.scala 344:37]
    node _GEN_88 = mux(eq(UInt<2>("h2"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_2) @[ProcessingElement.scala 344:37]
    node _GEN_89 = mux(eq(UInt<2>("h3"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_3) @[ProcessingElement.scala 344:37]
    node _GEN_90 = mux(eq(UInt<3>("h4"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_4) @[ProcessingElement.scala 344:37]
    node _GEN_91 = mux(eq(UInt<3>("h5"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_5) @[ProcessingElement.scala 344:37]
    node _GEN_92 = mux(eq(UInt<3>("h6"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_6) @[ProcessingElement.scala 344:37]
    node _GEN_93 = mux(eq(UInt<3>("h7"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_7) @[ProcessingElement.scala 344:37]
    node _GEN_94 = mux(eq(UInt<4>("h8"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_8) @[ProcessingElement.scala 344:37]
    node _GEN_95 = mux(eq(UInt<4>("h9"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_9) @[ProcessingElement.scala 344:37]
    node _GEN_96 = mux(eq(UInt<4>("ha"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_10) @[ProcessingElement.scala 344:37]
    node _GEN_97 = mux(eq(UInt<4>("hb"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_11) @[ProcessingElement.scala 344:37]
    node _GEN_98 = mux(eq(UInt<4>("hc"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_12) @[ProcessingElement.scala 344:37]
    node _GEN_99 = mux(eq(UInt<4>("hd"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_13) @[ProcessingElement.scala 344:37]
    node _GEN_100 = mux(eq(UInt<4>("he"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_14) @[ProcessingElement.scala 344:37]
    node _GEN_101 = mux(eq(UInt<4>("hf"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_15) @[ProcessingElement.scala 344:37]
    node _GEN_102 = mux(eq(UInt<5>("h10"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_16) @[ProcessingElement.scala 344:37]
    node _GEN_103 = mux(eq(UInt<5>("h11"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_17) @[ProcessingElement.scala 344:37]
    node _GEN_104 = mux(eq(UInt<5>("h12"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_18) @[ProcessingElement.scala 344:37]
    node _GEN_105 = mux(eq(UInt<5>("h13"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_19) @[ProcessingElement.scala 344:37]
    node _GEN_106 = mux(eq(UInt<5>("h14"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_20) @[ProcessingElement.scala 344:37]
    node _GEN_107 = mux(eq(UInt<5>("h15"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_21) @[ProcessingElement.scala 344:37]
    node _GEN_108 = mux(eq(UInt<5>("h16"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_22) @[ProcessingElement.scala 344:37]
    node _GEN_109 = mux(eq(UInt<5>("h17"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_23) @[ProcessingElement.scala 344:37]
    node _GEN_110 = mux(eq(UInt<5>("h18"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_24) @[ProcessingElement.scala 344:37]
    node _GEN_111 = mux(eq(UInt<5>("h19"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_25) @[ProcessingElement.scala 344:37]
    node _GEN_112 = mux(eq(UInt<5>("h1a"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_26) @[ProcessingElement.scala 344:37]
    node _GEN_113 = mux(eq(UInt<5>("h1b"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_27) @[ProcessingElement.scala 344:37]
    node _GEN_114 = mux(eq(UInt<5>("h1c"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_28) @[ProcessingElement.scala 344:37]
    node _GEN_115 = mux(eq(UInt<5>("h1d"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_29) @[ProcessingElement.scala 344:37]
    node _GEN_116 = mux(eq(UInt<5>("h1e"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_30) @[ProcessingElement.scala 344:37]
    node _GEN_117 = mux(eq(UInt<5>("h1f"), psDataSpadIdxWire), _psDataSPad_psDataSpadIdxWire, psDataSPad_31) @[ProcessingElement.scala 344:37]
    node _T_112 = and(mightIactReadFinish, mightWeightReadFinish) @[ProcessingElement.scala 345:33]
    node _T_113 = add(iactMatrixColumnReg, UInt<1>("h1")) @[ProcessingElement.scala 355:58]
    node _T_114 = tail(_T_113, 1) @[ProcessingElement.scala 355:58]
    node _T_115 = add(_T_114, iactZeroColumnNumber) @[ProcessingElement.scala 355:64]
    node _T_116 = tail(_T_115, 1) @[ProcessingElement.scala 355:64]
    node _T_117 = add(iactMatrixColumnReg, UInt<1>("h1")) @[ProcessingElement.scala 358:58]
    node _T_118 = tail(_T_117, 1) @[ProcessingElement.scala 358:58]
    node _GEN_118 = mux(iactSPadZeroColumnReg, _T_116, _T_118) @[ProcessingElement.scala 353:42]
    node _GEN_119 = mux(mightIactIdxIncWire, _GEN_118, iactMatrixColumnReg) @[ProcessingElement.scala 351:38]
    node _GEN_120 = mux(mightWeightIdxIncWire, _GEN_61, UInt<3>("h4")) @[ProcessingElement.scala 350:38]
    node _GEN_121 = mux(mightWeightIdxIncWire, _GEN_62, UInt<1>("h0")) @[ProcessingElement.scala 350:38]
    node _GEN_122 = mux(mightWeightIdxIncWire, _GEN_63, UInt<1>("h0")) @[ProcessingElement.scala 350:38]
    node _GEN_123 = mux(mightWeightIdxIncWire, _GEN_64, UInt<1>("h0")) @[ProcessingElement.scala 350:38]
    node _GEN_124 = mux(mightWeightIdxIncWire, _GEN_64, UInt<1>("h1")) @[ProcessingElement.scala 350:38]
    node _GEN_125 = mux(mightWeightIdxIncWire, _GEN_65, UInt<1>("h0")) @[ProcessingElement.scala 350:38]
    node _GEN_126 = mux(mightWeightIdxIncWire, _GEN_66, iactSPadZeroColumnReg) @[ProcessingElement.scala 350:38]
    node _GEN_127 = mux(mightWeightIdxIncWire, _GEN_119, iactMatrixColumnReg) @[ProcessingElement.scala 350:38]
    node _GEN_128 = mux(mightWeightIdxIncWire, _GEN_68, iactZeroColumnNumber) @[ProcessingElement.scala 350:38]
    node _GEN_129 = mux(_T_112, UInt<3>("h0"), _GEN_120) @[ProcessingElement.scala 345:59]
    node _GEN_130 = mux(_T_112, UInt<1>("h0"), _GEN_127) @[ProcessingElement.scala 345:59]
    node _GEN_131 = mux(_T_112, UInt<1>("h0"), _GEN_35) @[ProcessingElement.scala 345:59]
    node _GEN_132 = mux(_T_112, iactAddrSPadReadEnReg, _GEN_121) @[ProcessingElement.scala 345:59]
    node _GEN_133 = mux(_T_112, iactDataSPadReadEnReg, _GEN_122) @[ProcessingElement.scala 345:59]
    node _GEN_134 = mux(_T_112, weightAddrSPadReadEnReg, _GEN_123) @[ProcessingElement.scala 345:59]
    node _GEN_135 = mux(_T_112, weightDataSPadReadEnReg, _GEN_124) @[ProcessingElement.scala 345:59]
    node _GEN_136 = mux(_T_112, weightDataSPadFirstRead, _GEN_125) @[ProcessingElement.scala 345:59]
    node _GEN_137 = mux(_T_112, iactSPadZeroColumnReg, _GEN_126) @[ProcessingElement.scala 345:59]
    node _GEN_138 = mux(_T_112, iactZeroColumnNumber, _GEN_128) @[ProcessingElement.scala 345:59]
    node _GEN_139 = mux(_T_111, _GEN_86, psDataSPad_0) @[Conditional.scala 39:67]
    node _GEN_140 = mux(_T_111, _GEN_87, psDataSPad_1) @[Conditional.scala 39:67]
    node _GEN_141 = mux(_T_111, _GEN_88, psDataSPad_2) @[Conditional.scala 39:67]
    node _GEN_142 = mux(_T_111, _GEN_89, psDataSPad_3) @[Conditional.scala 39:67]
    node _GEN_143 = mux(_T_111, _GEN_90, psDataSPad_4) @[Conditional.scala 39:67]
    node _GEN_144 = mux(_T_111, _GEN_91, psDataSPad_5) @[Conditional.scala 39:67]
    node _GEN_145 = mux(_T_111, _GEN_92, psDataSPad_6) @[Conditional.scala 39:67]
    node _GEN_146 = mux(_T_111, _GEN_93, psDataSPad_7) @[Conditional.scala 39:67]
    node _GEN_147 = mux(_T_111, _GEN_94, psDataSPad_8) @[Conditional.scala 39:67]
    node _GEN_148 = mux(_T_111, _GEN_95, psDataSPad_9) @[Conditional.scala 39:67]
    node _GEN_149 = mux(_T_111, _GEN_96, psDataSPad_10) @[Conditional.scala 39:67]
    node _GEN_150 = mux(_T_111, _GEN_97, psDataSPad_11) @[Conditional.scala 39:67]
    node _GEN_151 = mux(_T_111, _GEN_98, psDataSPad_12) @[Conditional.scala 39:67]
    node _GEN_152 = mux(_T_111, _GEN_99, psDataSPad_13) @[Conditional.scala 39:67]
    node _GEN_153 = mux(_T_111, _GEN_100, psDataSPad_14) @[Conditional.scala 39:67]
    node _GEN_154 = mux(_T_111, _GEN_101, psDataSPad_15) @[Conditional.scala 39:67]
    node _GEN_155 = mux(_T_111, _GEN_102, psDataSPad_16) @[Conditional.scala 39:67]
    node _GEN_156 = mux(_T_111, _GEN_103, psDataSPad_17) @[Conditional.scala 39:67]
    node _GEN_157 = mux(_T_111, _GEN_104, psDataSPad_18) @[Conditional.scala 39:67]
    node _GEN_158 = mux(_T_111, _GEN_105, psDataSPad_19) @[Conditional.scala 39:67]
    node _GEN_159 = mux(_T_111, _GEN_106, psDataSPad_20) @[Conditional.scala 39:67]
    node _GEN_160 = mux(_T_111, _GEN_107, psDataSPad_21) @[Conditional.scala 39:67]
    node _GEN_161 = mux(_T_111, _GEN_108, psDataSPad_22) @[Conditional.scala 39:67]
    node _GEN_162 = mux(_T_111, _GEN_109, psDataSPad_23) @[Conditional.scala 39:67]
    node _GEN_163 = mux(_T_111, _GEN_110, psDataSPad_24) @[Conditional.scala 39:67]
    node _GEN_164 = mux(_T_111, _GEN_111, psDataSPad_25) @[Conditional.scala 39:67]
    node _GEN_165 = mux(_T_111, _GEN_112, psDataSPad_26) @[Conditional.scala 39:67]
    node _GEN_166 = mux(_T_111, _GEN_113, psDataSPad_27) @[Conditional.scala 39:67]
    node _GEN_167 = mux(_T_111, _GEN_114, psDataSPad_28) @[Conditional.scala 39:67]
    node _GEN_168 = mux(_T_111, _GEN_115, psDataSPad_29) @[Conditional.scala 39:67]
    node _GEN_169 = mux(_T_111, _GEN_116, psDataSPad_30) @[Conditional.scala 39:67]
    node _GEN_170 = mux(_T_111, _GEN_117, psDataSPad_31) @[Conditional.scala 39:67]
    node _GEN_171 = mux(_T_111, _GEN_129, sPad) @[Conditional.scala 39:67]
    node _GEN_172 = mux(_T_111, _GEN_130, iactMatrixColumnReg) @[Conditional.scala 39:67]
    node _GEN_173 = mux(_T_111, _GEN_131, _GEN_35) @[Conditional.scala 39:67]
    node _GEN_174 = mux(_T_111, _GEN_132, iactAddrSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_175 = mux(_T_111, _GEN_133, iactDataSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_176 = mux(_T_111, _GEN_134, weightAddrSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_177 = mux(_T_111, _GEN_135, weightDataSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_178 = mux(_T_111, _GEN_136, weightDataSPadFirstRead) @[Conditional.scala 39:67]
    node _GEN_179 = mux(_T_111, _GEN_137, iactSPadZeroColumnReg) @[Conditional.scala 39:67]
    node _GEN_180 = mux(_T_111, _GEN_138, iactZeroColumnNumber) @[Conditional.scala 39:67]
    node _GEN_181 = mux(_T_109, _GEN_82, _GEN_171) @[Conditional.scala 39:67]
    node _GEN_182 = mux(_T_109, _GEN_83, productReg) @[Conditional.scala 39:67]
    node _GEN_183 = mux(_T_109, _GEN_84, _T_28) @[Conditional.scala 39:67]
    node _GEN_184 = mux(_T_109, _GEN_85, pSumSPadLoadReg) @[Conditional.scala 39:67]
    node _GEN_185 = mux(_T_109, psDataSPad_0, _GEN_139) @[Conditional.scala 39:67]
    node _GEN_186 = mux(_T_109, psDataSPad_1, _GEN_140) @[Conditional.scala 39:67]
    node _GEN_187 = mux(_T_109, psDataSPad_2, _GEN_141) @[Conditional.scala 39:67]
    node _GEN_188 = mux(_T_109, psDataSPad_3, _GEN_142) @[Conditional.scala 39:67]
    node _GEN_189 = mux(_T_109, psDataSPad_4, _GEN_143) @[Conditional.scala 39:67]
    node _GEN_190 = mux(_T_109, psDataSPad_5, _GEN_144) @[Conditional.scala 39:67]
    node _GEN_191 = mux(_T_109, psDataSPad_6, _GEN_145) @[Conditional.scala 39:67]
    node _GEN_192 = mux(_T_109, psDataSPad_7, _GEN_146) @[Conditional.scala 39:67]
    node _GEN_193 = mux(_T_109, psDataSPad_8, _GEN_147) @[Conditional.scala 39:67]
    node _GEN_194 = mux(_T_109, psDataSPad_9, _GEN_148) @[Conditional.scala 39:67]
    node _GEN_195 = mux(_T_109, psDataSPad_10, _GEN_149) @[Conditional.scala 39:67]
    node _GEN_196 = mux(_T_109, psDataSPad_11, _GEN_150) @[Conditional.scala 39:67]
    node _GEN_197 = mux(_T_109, psDataSPad_12, _GEN_151) @[Conditional.scala 39:67]
    node _GEN_198 = mux(_T_109, psDataSPad_13, _GEN_152) @[Conditional.scala 39:67]
    node _GEN_199 = mux(_T_109, psDataSPad_14, _GEN_153) @[Conditional.scala 39:67]
    node _GEN_200 = mux(_T_109, psDataSPad_15, _GEN_154) @[Conditional.scala 39:67]
    node _GEN_201 = mux(_T_109, psDataSPad_16, _GEN_155) @[Conditional.scala 39:67]
    node _GEN_202 = mux(_T_109, psDataSPad_17, _GEN_156) @[Conditional.scala 39:67]
    node _GEN_203 = mux(_T_109, psDataSPad_18, _GEN_157) @[Conditional.scala 39:67]
    node _GEN_204 = mux(_T_109, psDataSPad_19, _GEN_158) @[Conditional.scala 39:67]
    node _GEN_205 = mux(_T_109, psDataSPad_20, _GEN_159) @[Conditional.scala 39:67]
    node _GEN_206 = mux(_T_109, psDataSPad_21, _GEN_160) @[Conditional.scala 39:67]
    node _GEN_207 = mux(_T_109, psDataSPad_22, _GEN_161) @[Conditional.scala 39:67]
    node _GEN_208 = mux(_T_109, psDataSPad_23, _GEN_162) @[Conditional.scala 39:67]
    node _GEN_209 = mux(_T_109, psDataSPad_24, _GEN_163) @[Conditional.scala 39:67]
    node _GEN_210 = mux(_T_109, psDataSPad_25, _GEN_164) @[Conditional.scala 39:67]
    node _GEN_211 = mux(_T_109, psDataSPad_26, _GEN_165) @[Conditional.scala 39:67]
    node _GEN_212 = mux(_T_109, psDataSPad_27, _GEN_166) @[Conditional.scala 39:67]
    node _GEN_213 = mux(_T_109, psDataSPad_28, _GEN_167) @[Conditional.scala 39:67]
    node _GEN_214 = mux(_T_109, psDataSPad_29, _GEN_168) @[Conditional.scala 39:67]
    node _GEN_215 = mux(_T_109, psDataSPad_30, _GEN_169) @[Conditional.scala 39:67]
    node _GEN_216 = mux(_T_109, psDataSPad_31, _GEN_170) @[Conditional.scala 39:67]
    node _GEN_217 = mux(_T_109, iactMatrixColumnReg, _GEN_172) @[Conditional.scala 39:67]
    node _GEN_218 = mux(_T_109, _GEN_35, _GEN_173) @[Conditional.scala 39:67]
    node _GEN_219 = mux(_T_109, iactAddrSPadReadEnReg, _GEN_174) @[Conditional.scala 39:67]
    node _GEN_220 = mux(_T_109, iactDataSPadReadEnReg, _GEN_175) @[Conditional.scala 39:67]
    node _GEN_221 = mux(_T_109, weightAddrSPadReadEnReg, _GEN_176) @[Conditional.scala 39:67]
    node _GEN_222 = mux(_T_109, weightDataSPadReadEnReg, _GEN_177) @[Conditional.scala 39:67]
    node _GEN_223 = mux(_T_109, weightDataSPadFirstRead, _GEN_178) @[Conditional.scala 39:67]
    node _GEN_224 = mux(_T_109, iactSPadZeroColumnReg, _GEN_179) @[Conditional.scala 39:67]
    node _GEN_225 = mux(_T_109, iactZeroColumnNumber, _GEN_180) @[Conditional.scala 39:67]
    node _GEN_226 = mux(_T_108, UInt<3>("h6"), _GEN_181) @[Conditional.scala 39:67]
    node _GEN_227 = mux(_T_108, psDataSpadIdxWire, _GEN_218) @[Conditional.scala 39:67]
    node _GEN_228 = mux(_T_108, UInt<1>("h0"), _GEN_219) @[Conditional.scala 39:67]
    node _GEN_229 = mux(_T_108, UInt<1>("h0"), _GEN_220) @[Conditional.scala 39:67]
    node _GEN_230 = mux(_T_108, UInt<1>("h0"), _GEN_221) @[Conditional.scala 39:67]
    node _GEN_231 = mux(_T_108, UInt<1>("h0"), _GEN_222) @[Conditional.scala 39:67]
    node _GEN_232 = mux(_T_108, productReg, _GEN_182) @[Conditional.scala 39:67]
    node _GEN_233 = mux(_T_108, _T_28, _GEN_183) @[Conditional.scala 39:67]
    node _GEN_234 = mux(_T_108, pSumSPadLoadReg, _GEN_184) @[Conditional.scala 39:67]
    node _GEN_235 = mux(_T_108, psDataSPad_0, _GEN_185) @[Conditional.scala 39:67]
    node _GEN_236 = mux(_T_108, psDataSPad_1, _GEN_186) @[Conditional.scala 39:67]
    node _GEN_237 = mux(_T_108, psDataSPad_2, _GEN_187) @[Conditional.scala 39:67]
    node _GEN_238 = mux(_T_108, psDataSPad_3, _GEN_188) @[Conditional.scala 39:67]
    node _GEN_239 = mux(_T_108, psDataSPad_4, _GEN_189) @[Conditional.scala 39:67]
    node _GEN_240 = mux(_T_108, psDataSPad_5, _GEN_190) @[Conditional.scala 39:67]
    node _GEN_241 = mux(_T_108, psDataSPad_6, _GEN_191) @[Conditional.scala 39:67]
    node _GEN_242 = mux(_T_108, psDataSPad_7, _GEN_192) @[Conditional.scala 39:67]
    node _GEN_243 = mux(_T_108, psDataSPad_8, _GEN_193) @[Conditional.scala 39:67]
    node _GEN_244 = mux(_T_108, psDataSPad_9, _GEN_194) @[Conditional.scala 39:67]
    node _GEN_245 = mux(_T_108, psDataSPad_10, _GEN_195) @[Conditional.scala 39:67]
    node _GEN_246 = mux(_T_108, psDataSPad_11, _GEN_196) @[Conditional.scala 39:67]
    node _GEN_247 = mux(_T_108, psDataSPad_12, _GEN_197) @[Conditional.scala 39:67]
    node _GEN_248 = mux(_T_108, psDataSPad_13, _GEN_198) @[Conditional.scala 39:67]
    node _GEN_249 = mux(_T_108, psDataSPad_14, _GEN_199) @[Conditional.scala 39:67]
    node _GEN_250 = mux(_T_108, psDataSPad_15, _GEN_200) @[Conditional.scala 39:67]
    node _GEN_251 = mux(_T_108, psDataSPad_16, _GEN_201) @[Conditional.scala 39:67]
    node _GEN_252 = mux(_T_108, psDataSPad_17, _GEN_202) @[Conditional.scala 39:67]
    node _GEN_253 = mux(_T_108, psDataSPad_18, _GEN_203) @[Conditional.scala 39:67]
    node _GEN_254 = mux(_T_108, psDataSPad_19, _GEN_204) @[Conditional.scala 39:67]
    node _GEN_255 = mux(_T_108, psDataSPad_20, _GEN_205) @[Conditional.scala 39:67]
    node _GEN_256 = mux(_T_108, psDataSPad_21, _GEN_206) @[Conditional.scala 39:67]
    node _GEN_257 = mux(_T_108, psDataSPad_22, _GEN_207) @[Conditional.scala 39:67]
    node _GEN_258 = mux(_T_108, psDataSPad_23, _GEN_208) @[Conditional.scala 39:67]
    node _GEN_259 = mux(_T_108, psDataSPad_24, _GEN_209) @[Conditional.scala 39:67]
    node _GEN_260 = mux(_T_108, psDataSPad_25, _GEN_210) @[Conditional.scala 39:67]
    node _GEN_261 = mux(_T_108, psDataSPad_26, _GEN_211) @[Conditional.scala 39:67]
    node _GEN_262 = mux(_T_108, psDataSPad_27, _GEN_212) @[Conditional.scala 39:67]
    node _GEN_263 = mux(_T_108, psDataSPad_28, _GEN_213) @[Conditional.scala 39:67]
    node _GEN_264 = mux(_T_108, psDataSPad_29, _GEN_214) @[Conditional.scala 39:67]
    node _GEN_265 = mux(_T_108, psDataSPad_30, _GEN_215) @[Conditional.scala 39:67]
    node _GEN_266 = mux(_T_108, psDataSPad_31, _GEN_216) @[Conditional.scala 39:67]
    node _GEN_267 = mux(_T_108, iactMatrixColumnReg, _GEN_217) @[Conditional.scala 39:67]
    node _GEN_268 = mux(_T_108, weightDataSPadFirstRead, _GEN_223) @[Conditional.scala 39:67]
    node _GEN_269 = mux(_T_108, iactSPadZeroColumnReg, _GEN_224) @[Conditional.scala 39:67]
    node _GEN_270 = mux(_T_108, iactZeroColumnNumber, _GEN_225) @[Conditional.scala 39:67]
    node _GEN_271 = mux(_T_107, UInt<3>("h5"), _GEN_226) @[Conditional.scala 39:67]
    node _GEN_272 = mux(_T_107, _GEN_35, _GEN_227) @[Conditional.scala 39:67]
    node _GEN_273 = mux(_T_107, iactAddrSPadReadEnReg, _GEN_228) @[Conditional.scala 39:67]
    node _GEN_274 = mux(_T_107, iactDataSPadReadEnReg, _GEN_229) @[Conditional.scala 39:67]
    node _GEN_275 = mux(_T_107, weightAddrSPadReadEnReg, _GEN_230) @[Conditional.scala 39:67]
    node _GEN_276 = mux(_T_107, weightDataSPadReadEnReg, _GEN_231) @[Conditional.scala 39:67]
    node _GEN_277 = mux(_T_107, productReg, _GEN_232) @[Conditional.scala 39:67]
    node _GEN_278 = mux(_T_107, _T_28, _GEN_233) @[Conditional.scala 39:67]
    node _GEN_279 = mux(_T_107, pSumSPadLoadReg, _GEN_234) @[Conditional.scala 39:67]
    node _GEN_280 = mux(_T_107, psDataSPad_0, _GEN_235) @[Conditional.scala 39:67]
    node _GEN_281 = mux(_T_107, psDataSPad_1, _GEN_236) @[Conditional.scala 39:67]
    node _GEN_282 = mux(_T_107, psDataSPad_2, _GEN_237) @[Conditional.scala 39:67]
    node _GEN_283 = mux(_T_107, psDataSPad_3, _GEN_238) @[Conditional.scala 39:67]
    node _GEN_284 = mux(_T_107, psDataSPad_4, _GEN_239) @[Conditional.scala 39:67]
    node _GEN_285 = mux(_T_107, psDataSPad_5, _GEN_240) @[Conditional.scala 39:67]
    node _GEN_286 = mux(_T_107, psDataSPad_6, _GEN_241) @[Conditional.scala 39:67]
    node _GEN_287 = mux(_T_107, psDataSPad_7, _GEN_242) @[Conditional.scala 39:67]
    node _GEN_288 = mux(_T_107, psDataSPad_8, _GEN_243) @[Conditional.scala 39:67]
    node _GEN_289 = mux(_T_107, psDataSPad_9, _GEN_244) @[Conditional.scala 39:67]
    node _GEN_290 = mux(_T_107, psDataSPad_10, _GEN_245) @[Conditional.scala 39:67]
    node _GEN_291 = mux(_T_107, psDataSPad_11, _GEN_246) @[Conditional.scala 39:67]
    node _GEN_292 = mux(_T_107, psDataSPad_12, _GEN_247) @[Conditional.scala 39:67]
    node _GEN_293 = mux(_T_107, psDataSPad_13, _GEN_248) @[Conditional.scala 39:67]
    node _GEN_294 = mux(_T_107, psDataSPad_14, _GEN_249) @[Conditional.scala 39:67]
    node _GEN_295 = mux(_T_107, psDataSPad_15, _GEN_250) @[Conditional.scala 39:67]
    node _GEN_296 = mux(_T_107, psDataSPad_16, _GEN_251) @[Conditional.scala 39:67]
    node _GEN_297 = mux(_T_107, psDataSPad_17, _GEN_252) @[Conditional.scala 39:67]
    node _GEN_298 = mux(_T_107, psDataSPad_18, _GEN_253) @[Conditional.scala 39:67]
    node _GEN_299 = mux(_T_107, psDataSPad_19, _GEN_254) @[Conditional.scala 39:67]
    node _GEN_300 = mux(_T_107, psDataSPad_20, _GEN_255) @[Conditional.scala 39:67]
    node _GEN_301 = mux(_T_107, psDataSPad_21, _GEN_256) @[Conditional.scala 39:67]
    node _GEN_302 = mux(_T_107, psDataSPad_22, _GEN_257) @[Conditional.scala 39:67]
    node _GEN_303 = mux(_T_107, psDataSPad_23, _GEN_258) @[Conditional.scala 39:67]
    node _GEN_304 = mux(_T_107, psDataSPad_24, _GEN_259) @[Conditional.scala 39:67]
    node _GEN_305 = mux(_T_107, psDataSPad_25, _GEN_260) @[Conditional.scala 39:67]
    node _GEN_306 = mux(_T_107, psDataSPad_26, _GEN_261) @[Conditional.scala 39:67]
    node _GEN_307 = mux(_T_107, psDataSPad_27, _GEN_262) @[Conditional.scala 39:67]
    node _GEN_308 = mux(_T_107, psDataSPad_28, _GEN_263) @[Conditional.scala 39:67]
    node _GEN_309 = mux(_T_107, psDataSPad_29, _GEN_264) @[Conditional.scala 39:67]
    node _GEN_310 = mux(_T_107, psDataSPad_30, _GEN_265) @[Conditional.scala 39:67]
    node _GEN_311 = mux(_T_107, psDataSPad_31, _GEN_266) @[Conditional.scala 39:67]
    node _GEN_312 = mux(_T_107, iactMatrixColumnReg, _GEN_267) @[Conditional.scala 39:67]
    node _GEN_313 = mux(_T_107, weightDataSPadFirstRead, _GEN_268) @[Conditional.scala 39:67]
    node _GEN_314 = mux(_T_107, iactSPadZeroColumnReg, _GEN_269) @[Conditional.scala 39:67]
    node _GEN_315 = mux(_T_107, iactZeroColumnNumber, _GEN_270) @[Conditional.scala 39:67]
    node _GEN_316 = mux(_T_100, _GEN_69, _GEN_271) @[Conditional.scala 39:67]
    node _GEN_317 = mux(_T_100, _GEN_70, _GEN_273) @[Conditional.scala 39:67]
    node _GEN_318 = mux(_T_100, _GEN_71, _GEN_274) @[Conditional.scala 39:67]
    node _GEN_319 = mux(_T_100, _GEN_72, _GEN_275) @[Conditional.scala 39:67]
    node _GEN_320 = mux(_T_100, _GEN_73, _GEN_276) @[Conditional.scala 39:67]
    node _GEN_321 = mux(_T_100, _GEN_74, _GEN_313) @[Conditional.scala 39:67]
    node _GEN_322 = mux(_T_100, _GEN_75, _GEN_314) @[Conditional.scala 39:67]
    node _GEN_323 = mux(_T_100, _GEN_76, _GEN_312) @[Conditional.scala 39:67]
    node _GEN_324 = mux(_T_100, _GEN_77, _GEN_315) @[Conditional.scala 39:67]
    node _GEN_325 = mux(_T_100, _GEN_35, _GEN_272) @[Conditional.scala 39:67]
    node _GEN_326 = mux(_T_100, productReg, _GEN_277) @[Conditional.scala 39:67]
    node _GEN_327 = mux(_T_100, _T_28, _GEN_278) @[Conditional.scala 39:67]
    node _GEN_328 = mux(_T_100, pSumSPadLoadReg, _GEN_279) @[Conditional.scala 39:67]
    node _GEN_329 = mux(_T_100, psDataSPad_0, _GEN_280) @[Conditional.scala 39:67]
    node _GEN_330 = mux(_T_100, psDataSPad_1, _GEN_281) @[Conditional.scala 39:67]
    node _GEN_331 = mux(_T_100, psDataSPad_2, _GEN_282) @[Conditional.scala 39:67]
    node _GEN_332 = mux(_T_100, psDataSPad_3, _GEN_283) @[Conditional.scala 39:67]
    node _GEN_333 = mux(_T_100, psDataSPad_4, _GEN_284) @[Conditional.scala 39:67]
    node _GEN_334 = mux(_T_100, psDataSPad_5, _GEN_285) @[Conditional.scala 39:67]
    node _GEN_335 = mux(_T_100, psDataSPad_6, _GEN_286) @[Conditional.scala 39:67]
    node _GEN_336 = mux(_T_100, psDataSPad_7, _GEN_287) @[Conditional.scala 39:67]
    node _GEN_337 = mux(_T_100, psDataSPad_8, _GEN_288) @[Conditional.scala 39:67]
    node _GEN_338 = mux(_T_100, psDataSPad_9, _GEN_289) @[Conditional.scala 39:67]
    node _GEN_339 = mux(_T_100, psDataSPad_10, _GEN_290) @[Conditional.scala 39:67]
    node _GEN_340 = mux(_T_100, psDataSPad_11, _GEN_291) @[Conditional.scala 39:67]
    node _GEN_341 = mux(_T_100, psDataSPad_12, _GEN_292) @[Conditional.scala 39:67]
    node _GEN_342 = mux(_T_100, psDataSPad_13, _GEN_293) @[Conditional.scala 39:67]
    node _GEN_343 = mux(_T_100, psDataSPad_14, _GEN_294) @[Conditional.scala 39:67]
    node _GEN_344 = mux(_T_100, psDataSPad_15, _GEN_295) @[Conditional.scala 39:67]
    node _GEN_345 = mux(_T_100, psDataSPad_16, _GEN_296) @[Conditional.scala 39:67]
    node _GEN_346 = mux(_T_100, psDataSPad_17, _GEN_297) @[Conditional.scala 39:67]
    node _GEN_347 = mux(_T_100, psDataSPad_18, _GEN_298) @[Conditional.scala 39:67]
    node _GEN_348 = mux(_T_100, psDataSPad_19, _GEN_299) @[Conditional.scala 39:67]
    node _GEN_349 = mux(_T_100, psDataSPad_20, _GEN_300) @[Conditional.scala 39:67]
    node _GEN_350 = mux(_T_100, psDataSPad_21, _GEN_301) @[Conditional.scala 39:67]
    node _GEN_351 = mux(_T_100, psDataSPad_22, _GEN_302) @[Conditional.scala 39:67]
    node _GEN_352 = mux(_T_100, psDataSPad_23, _GEN_303) @[Conditional.scala 39:67]
    node _GEN_353 = mux(_T_100, psDataSPad_24, _GEN_304) @[Conditional.scala 39:67]
    node _GEN_354 = mux(_T_100, psDataSPad_25, _GEN_305) @[Conditional.scala 39:67]
    node _GEN_355 = mux(_T_100, psDataSPad_26, _GEN_306) @[Conditional.scala 39:67]
    node _GEN_356 = mux(_T_100, psDataSPad_27, _GEN_307) @[Conditional.scala 39:67]
    node _GEN_357 = mux(_T_100, psDataSPad_28, _GEN_308) @[Conditional.scala 39:67]
    node _GEN_358 = mux(_T_100, psDataSPad_29, _GEN_309) @[Conditional.scala 39:67]
    node _GEN_359 = mux(_T_100, psDataSPad_30, _GEN_310) @[Conditional.scala 39:67]
    node _GEN_360 = mux(_T_100, psDataSPad_31, _GEN_311) @[Conditional.scala 39:67]
    node _GEN_361 = mux(_T_99, UInt<3>("h3"), _GEN_316) @[Conditional.scala 39:67]
    node _GEN_362 = mux(_T_99, UInt<1>("h0"), _GEN_317) @[Conditional.scala 39:67]
    node _GEN_363 = mux(_T_99, UInt<1>("h0"), _GEN_318) @[Conditional.scala 39:67]
    node _GEN_364 = mux(_T_99, UInt<1>("h1"), _GEN_319) @[Conditional.scala 39:67]
    node _GEN_365 = mux(_T_99, UInt<1>("h0"), _GEN_320) @[Conditional.scala 39:67]
    node _GEN_366 = mux(_T_99, UInt<1>("h0"), iactDataSPadFirstReadReg) @[Conditional.scala 39:67]
    node _GEN_367 = mux(_T_99, weightDataSPadFirstRead, _GEN_321) @[Conditional.scala 39:67]
    node _GEN_368 = mux(_T_99, iactSPadZeroColumnReg, _GEN_322) @[Conditional.scala 39:67]
    node _GEN_369 = mux(_T_99, iactMatrixColumnReg, _GEN_323) @[Conditional.scala 39:67]
    node _GEN_370 = mux(_T_99, iactZeroColumnNumber, _GEN_324) @[Conditional.scala 39:67]
    node _GEN_371 = mux(_T_99, _GEN_35, _GEN_325) @[Conditional.scala 39:67]
    node _GEN_372 = mux(_T_99, productReg, _GEN_326) @[Conditional.scala 39:67]
    node _GEN_373 = mux(_T_99, _T_28, _GEN_327) @[Conditional.scala 39:67]
    node _GEN_374 = mux(_T_99, pSumSPadLoadReg, _GEN_328) @[Conditional.scala 39:67]
    node _GEN_375 = mux(_T_99, psDataSPad_0, _GEN_329) @[Conditional.scala 39:67]
    node _GEN_376 = mux(_T_99, psDataSPad_1, _GEN_330) @[Conditional.scala 39:67]
    node _GEN_377 = mux(_T_99, psDataSPad_2, _GEN_331) @[Conditional.scala 39:67]
    node _GEN_378 = mux(_T_99, psDataSPad_3, _GEN_332) @[Conditional.scala 39:67]
    node _GEN_379 = mux(_T_99, psDataSPad_4, _GEN_333) @[Conditional.scala 39:67]
    node _GEN_380 = mux(_T_99, psDataSPad_5, _GEN_334) @[Conditional.scala 39:67]
    node _GEN_381 = mux(_T_99, psDataSPad_6, _GEN_335) @[Conditional.scala 39:67]
    node _GEN_382 = mux(_T_99, psDataSPad_7, _GEN_336) @[Conditional.scala 39:67]
    node _GEN_383 = mux(_T_99, psDataSPad_8, _GEN_337) @[Conditional.scala 39:67]
    node _GEN_384 = mux(_T_99, psDataSPad_9, _GEN_338) @[Conditional.scala 39:67]
    node _GEN_385 = mux(_T_99, psDataSPad_10, _GEN_339) @[Conditional.scala 39:67]
    node _GEN_386 = mux(_T_99, psDataSPad_11, _GEN_340) @[Conditional.scala 39:67]
    node _GEN_387 = mux(_T_99, psDataSPad_12, _GEN_341) @[Conditional.scala 39:67]
    node _GEN_388 = mux(_T_99, psDataSPad_13, _GEN_342) @[Conditional.scala 39:67]
    node _GEN_389 = mux(_T_99, psDataSPad_14, _GEN_343) @[Conditional.scala 39:67]
    node _GEN_390 = mux(_T_99, psDataSPad_15, _GEN_344) @[Conditional.scala 39:67]
    node _GEN_391 = mux(_T_99, psDataSPad_16, _GEN_345) @[Conditional.scala 39:67]
    node _GEN_392 = mux(_T_99, psDataSPad_17, _GEN_346) @[Conditional.scala 39:67]
    node _GEN_393 = mux(_T_99, psDataSPad_18, _GEN_347) @[Conditional.scala 39:67]
    node _GEN_394 = mux(_T_99, psDataSPad_19, _GEN_348) @[Conditional.scala 39:67]
    node _GEN_395 = mux(_T_99, psDataSPad_20, _GEN_349) @[Conditional.scala 39:67]
    node _GEN_396 = mux(_T_99, psDataSPad_21, _GEN_350) @[Conditional.scala 39:67]
    node _GEN_397 = mux(_T_99, psDataSPad_22, _GEN_351) @[Conditional.scala 39:67]
    node _GEN_398 = mux(_T_99, psDataSPad_23, _GEN_352) @[Conditional.scala 39:67]
    node _GEN_399 = mux(_T_99, psDataSPad_24, _GEN_353) @[Conditional.scala 39:67]
    node _GEN_400 = mux(_T_99, psDataSPad_25, _GEN_354) @[Conditional.scala 39:67]
    node _GEN_401 = mux(_T_99, psDataSPad_26, _GEN_355) @[Conditional.scala 39:67]
    node _GEN_402 = mux(_T_99, psDataSPad_27, _GEN_356) @[Conditional.scala 39:67]
    node _GEN_403 = mux(_T_99, psDataSPad_28, _GEN_357) @[Conditional.scala 39:67]
    node _GEN_404 = mux(_T_99, psDataSPad_29, _GEN_358) @[Conditional.scala 39:67]
    node _GEN_405 = mux(_T_99, psDataSPad_30, _GEN_359) @[Conditional.scala 39:67]
    node _GEN_406 = mux(_T_99, psDataSPad_31, _GEN_360) @[Conditional.scala 39:67]
    node _GEN_407 = mux(_T_96, _GEN_51, _GEN_361) @[Conditional.scala 39:67]
    node _GEN_408 = mux(_T_96, _GEN_52, _GEN_362) @[Conditional.scala 39:67]
    node _GEN_409 = mux(_T_96, _GEN_53, _GEN_363) @[Conditional.scala 39:67]
    node _GEN_410 = mux(_T_96, _GEN_54, _GEN_364) @[Conditional.scala 39:67]
    node _GEN_411 = mux(_T_96, _GEN_54, _GEN_365) @[Conditional.scala 39:67]
    node _GEN_412 = mux(_T_96, _GEN_55, _GEN_367) @[Conditional.scala 39:67]
    node _GEN_413 = mux(_T_96, _GEN_56, _GEN_368) @[Conditional.scala 39:67]
    node _GEN_414 = mux(_T_96, _GEN_57, _GEN_370) @[Conditional.scala 39:67]
    node _GEN_415 = mux(_T_96, iactDataSPadFirstReadReg, _GEN_366) @[Conditional.scala 39:67]
    node _GEN_416 = mux(_T_96, iactMatrixColumnReg, _GEN_369) @[Conditional.scala 39:67]
    node _GEN_417 = mux(_T_96, _GEN_35, _GEN_371) @[Conditional.scala 39:67]
    node _GEN_418 = mux(_T_96, productReg, _GEN_372) @[Conditional.scala 39:67]
    node _GEN_419 = mux(_T_96, _T_28, _GEN_373) @[Conditional.scala 39:67]
    node _GEN_420 = mux(_T_96, pSumSPadLoadReg, _GEN_374) @[Conditional.scala 39:67]
    node _GEN_421 = mux(_T_96, psDataSPad_0, _GEN_375) @[Conditional.scala 39:67]
    node _GEN_422 = mux(_T_96, psDataSPad_1, _GEN_376) @[Conditional.scala 39:67]
    node _GEN_423 = mux(_T_96, psDataSPad_2, _GEN_377) @[Conditional.scala 39:67]
    node _GEN_424 = mux(_T_96, psDataSPad_3, _GEN_378) @[Conditional.scala 39:67]
    node _GEN_425 = mux(_T_96, psDataSPad_4, _GEN_379) @[Conditional.scala 39:67]
    node _GEN_426 = mux(_T_96, psDataSPad_5, _GEN_380) @[Conditional.scala 39:67]
    node _GEN_427 = mux(_T_96, psDataSPad_6, _GEN_381) @[Conditional.scala 39:67]
    node _GEN_428 = mux(_T_96, psDataSPad_7, _GEN_382) @[Conditional.scala 39:67]
    node _GEN_429 = mux(_T_96, psDataSPad_8, _GEN_383) @[Conditional.scala 39:67]
    node _GEN_430 = mux(_T_96, psDataSPad_9, _GEN_384) @[Conditional.scala 39:67]
    node _GEN_431 = mux(_T_96, psDataSPad_10, _GEN_385) @[Conditional.scala 39:67]
    node _GEN_432 = mux(_T_96, psDataSPad_11, _GEN_386) @[Conditional.scala 39:67]
    node _GEN_433 = mux(_T_96, psDataSPad_12, _GEN_387) @[Conditional.scala 39:67]
    node _GEN_434 = mux(_T_96, psDataSPad_13, _GEN_388) @[Conditional.scala 39:67]
    node _GEN_435 = mux(_T_96, psDataSPad_14, _GEN_389) @[Conditional.scala 39:67]
    node _GEN_436 = mux(_T_96, psDataSPad_15, _GEN_390) @[Conditional.scala 39:67]
    node _GEN_437 = mux(_T_96, psDataSPad_16, _GEN_391) @[Conditional.scala 39:67]
    node _GEN_438 = mux(_T_96, psDataSPad_17, _GEN_392) @[Conditional.scala 39:67]
    node _GEN_439 = mux(_T_96, psDataSPad_18, _GEN_393) @[Conditional.scala 39:67]
    node _GEN_440 = mux(_T_96, psDataSPad_19, _GEN_394) @[Conditional.scala 39:67]
    node _GEN_441 = mux(_T_96, psDataSPad_20, _GEN_395) @[Conditional.scala 39:67]
    node _GEN_442 = mux(_T_96, psDataSPad_21, _GEN_396) @[Conditional.scala 39:67]
    node _GEN_443 = mux(_T_96, psDataSPad_22, _GEN_397) @[Conditional.scala 39:67]
    node _GEN_444 = mux(_T_96, psDataSPad_23, _GEN_398) @[Conditional.scala 39:67]
    node _GEN_445 = mux(_T_96, psDataSPad_24, _GEN_399) @[Conditional.scala 39:67]
    node _GEN_446 = mux(_T_96, psDataSPad_25, _GEN_400) @[Conditional.scala 39:67]
    node _GEN_447 = mux(_T_96, psDataSPad_26, _GEN_401) @[Conditional.scala 39:67]
    node _GEN_448 = mux(_T_96, psDataSPad_27, _GEN_402) @[Conditional.scala 39:67]
    node _GEN_449 = mux(_T_96, psDataSPad_28, _GEN_403) @[Conditional.scala 39:67]
    node _GEN_450 = mux(_T_96, psDataSPad_29, _GEN_404) @[Conditional.scala 39:67]
    node _GEN_451 = mux(_T_96, psDataSPad_30, _GEN_405) @[Conditional.scala 39:67]
    node _GEN_452 = mux(_T_96, psDataSPad_31, _GEN_406) @[Conditional.scala 39:67]
    node _GEN_453 = mux(_T_95, _GEN_44, _GEN_407) @[Conditional.scala 40:58]
    node _GEN_454 = mux(_T_95, _GEN_45, _GEN_408) @[Conditional.scala 40:58]
    node _GEN_455 = mux(_T_95, _GEN_46, _GEN_409) @[Conditional.scala 40:58]
    node _GEN_456 = mux(_T_95, _GEN_47, _GEN_410) @[Conditional.scala 40:58]
    node _GEN_457 = mux(_T_95, _GEN_48, _GEN_411) @[Conditional.scala 40:58]
    node _GEN_458 = mux(_T_95, _GEN_49, _GEN_412) @[Conditional.scala 40:58]
    node _GEN_459 = mux(_T_95, _GEN_50, _GEN_415) @[Conditional.scala 40:58]
    node _GEN_460 = mux(_T_95, iactSPadZeroColumnReg, _GEN_413) @[Conditional.scala 40:58]
    node _GEN_461 = mux(_T_95, iactZeroColumnNumber, _GEN_414) @[Conditional.scala 40:58]
    node _GEN_462 = mux(_T_95, iactMatrixColumnReg, _GEN_416) @[Conditional.scala 40:58]
    node _GEN_463 = mux(_T_95, _GEN_35, _GEN_417) @[Conditional.scala 40:58]
    node _GEN_464 = mux(_T_95, productReg, _GEN_418) @[Conditional.scala 40:58]
    node _GEN_465 = mux(_T_95, _T_28, _GEN_419) @[Conditional.scala 40:58]
    node _GEN_466 = mux(_T_95, pSumSPadLoadReg, _GEN_420) @[Conditional.scala 40:58]
    node _GEN_467 = mux(_T_95, psDataSPad_0, _GEN_421) @[Conditional.scala 40:58]
    node _GEN_468 = mux(_T_95, psDataSPad_1, _GEN_422) @[Conditional.scala 40:58]
    node _GEN_469 = mux(_T_95, psDataSPad_2, _GEN_423) @[Conditional.scala 40:58]
    node _GEN_470 = mux(_T_95, psDataSPad_3, _GEN_424) @[Conditional.scala 40:58]
    node _GEN_471 = mux(_T_95, psDataSPad_4, _GEN_425) @[Conditional.scala 40:58]
    node _GEN_472 = mux(_T_95, psDataSPad_5, _GEN_426) @[Conditional.scala 40:58]
    node _GEN_473 = mux(_T_95, psDataSPad_6, _GEN_427) @[Conditional.scala 40:58]
    node _GEN_474 = mux(_T_95, psDataSPad_7, _GEN_428) @[Conditional.scala 40:58]
    node _GEN_475 = mux(_T_95, psDataSPad_8, _GEN_429) @[Conditional.scala 40:58]
    node _GEN_476 = mux(_T_95, psDataSPad_9, _GEN_430) @[Conditional.scala 40:58]
    node _GEN_477 = mux(_T_95, psDataSPad_10, _GEN_431) @[Conditional.scala 40:58]
    node _GEN_478 = mux(_T_95, psDataSPad_11, _GEN_432) @[Conditional.scala 40:58]
    node _GEN_479 = mux(_T_95, psDataSPad_12, _GEN_433) @[Conditional.scala 40:58]
    node _GEN_480 = mux(_T_95, psDataSPad_13, _GEN_434) @[Conditional.scala 40:58]
    node _GEN_481 = mux(_T_95, psDataSPad_14, _GEN_435) @[Conditional.scala 40:58]
    node _GEN_482 = mux(_T_95, psDataSPad_15, _GEN_436) @[Conditional.scala 40:58]
    node _GEN_483 = mux(_T_95, psDataSPad_16, _GEN_437) @[Conditional.scala 40:58]
    node _GEN_484 = mux(_T_95, psDataSPad_17, _GEN_438) @[Conditional.scala 40:58]
    node _GEN_485 = mux(_T_95, psDataSPad_18, _GEN_439) @[Conditional.scala 40:58]
    node _GEN_486 = mux(_T_95, psDataSPad_19, _GEN_440) @[Conditional.scala 40:58]
    node _GEN_487 = mux(_T_95, psDataSPad_20, _GEN_441) @[Conditional.scala 40:58]
    node _GEN_488 = mux(_T_95, psDataSPad_21, _GEN_442) @[Conditional.scala 40:58]
    node _GEN_489 = mux(_T_95, psDataSPad_22, _GEN_443) @[Conditional.scala 40:58]
    node _GEN_490 = mux(_T_95, psDataSPad_23, _GEN_444) @[Conditional.scala 40:58]
    node _GEN_491 = mux(_T_95, psDataSPad_24, _GEN_445) @[Conditional.scala 40:58]
    node _GEN_492 = mux(_T_95, psDataSPad_25, _GEN_446) @[Conditional.scala 40:58]
    node _GEN_493 = mux(_T_95, psDataSPad_26, _GEN_447) @[Conditional.scala 40:58]
    node _GEN_494 = mux(_T_95, psDataSPad_27, _GEN_448) @[Conditional.scala 40:58]
    node _GEN_495 = mux(_T_95, psDataSPad_28, _GEN_449) @[Conditional.scala 40:58]
    node _GEN_496 = mux(_T_95, psDataSPad_29, _GEN_450) @[Conditional.scala 40:58]
    node _GEN_497 = mux(_T_95, psDataSPad_30, _GEN_451) @[Conditional.scala 40:58]
    node _GEN_498 = mux(_T_95, psDataSPad_31, _GEN_452) @[Conditional.scala 40:58]
    node _T__0 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__1 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__2 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__3 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__4 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__5 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__6 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__7 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__8 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__9 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__10 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__11 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__12 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__13 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__14 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__15 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__16 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__17 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__18 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__19 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__20 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__21 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__22 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__23 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__24 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__25 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__26 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__27 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__28 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__29 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__30 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node _T__31 = UInt<20>("h0") @[ProcessingElement.scala 122:46 ProcessingElement.scala 122:46]
    node iactAddrIndexWire = SPadSeq_0.io_commonIO_columnNum @[ProcessingElement.scala 128:37 ProcessingElement.scala 188:21]
    node iactAddrSPadIdxIncWire = _T_58 @[ProcessingElement.scala 133:42 ProcessingElement.scala 273:26]
    node iactDataSPadIdxIncWire = _T_71 @[ProcessingElement.scala 134:42 ProcessingElement.scala 275:26]
    node weightAddrIndexWire = SPadSeq_2.io_commonIO_columnNum @[ProcessingElement.scala 141:39 ProcessingElement.scala 208:23]
    node weightAddrSPadIdxIncWire = _T_61 @[ProcessingElement.scala 146:44 ProcessingElement.scala 274:28]
    node weightDataSPadIdxIncWire = _T_78 @[ProcessingElement.scala 147:44 ProcessingElement.scala 276:28]
    node weightAddrSPadReadIdxWire = _T_86 @[ProcessingElement.scala 152:45 ProcessingElement.scala 279:29]
    node weightDataIdxEnWire = _T_89 @[ProcessingElement.scala 153:39 ProcessingElement.scala 280:23]
    node weightAddrIdxEnWire = _T_80 @[ProcessingElement.scala 154:39 ProcessingElement.scala 277:23]
    io_padCtrl_fromTopIO_pSumEnqOrProduct_ready <= padEqMpy @[ProcessingElement.scala 240:47]
    io_padCtrl_fromTopIO_writeFinish <= _T_35 @[ProcessingElement.scala 250:36]
    io_padCtrl_fromTopIO_calFinish <= _T_91 @[ProcessingElement.scala 281:34]
    io_dataStream_ipsIO_ready <= _GEN_465 @[ProcessingElement.scala 229:29 ProcessingElement.scala 334:37]
    io_dataStream_opsIO_valid <= _GEN_34 @[ProcessingElement.scala 233:31 ProcessingElement.scala 236:31]
    io_dataStream_opsIO_bits <= _GEN_33 @[ProcessingElement.scala 232:30]
    io_dataStream_iactIOs_dataIOs_writeInDataIO_ready <= SPadSeq_1.io_commonIO_dataLenFinIO_writeInDataIO_ready @[ProcessingElement.scala 196:41]
    io_dataStream_iactIOs_dataIOs_writeFin <= SPadSeq_1.io_commonIO_dataLenFinIO_writeFin @[ProcessingElement.scala 196:41]
    io_dataStream_iactIOs_addrIOs_writeInDataIO_ready <= SPadSeq_0.io_commonIO_dataLenFinIO_writeInDataIO_ready @[ProcessingElement.scala 187:41]
    io_dataStream_iactIOs_addrIOs_writeFin <= SPadSeq_0.io_commonIO_dataLenFinIO_writeFin @[ProcessingElement.scala 187:41]
    io_dataStream_weightIOs_dataIOs_writeInDataIO_ready <= SPadSeq_3.io_commonIO_dataLenFinIO_writeInDataIO_ready @[ProcessingElement.scala 218:43]
    io_dataStream_weightIOs_dataIOs_writeFin <= SPadSeq_3.io_commonIO_dataLenFinIO_writeFin @[ProcessingElement.scala 218:43]
    io_dataStream_weightIOs_addrIOs_writeInDataIO_ready <= SPadSeq_2.io_commonIO_dataLenFinIO_writeInDataIO_ready @[ProcessingElement.scala 207:43]
    io_dataStream_weightIOs_addrIOs_writeFin <= SPadSeq_2.io_commonIO_dataLenFinIO_writeFin @[ProcessingElement.scala 207:43]
    io_debugIO_iactMatrixData is invalid
    io_debugIO_iactMatrixRow is invalid
    io_debugIO_iactMatrixColumn is invalid
    io_debugIO_iactAddrInc is invalid
    io_debugIO_iactDataInc is invalid
    io_debugIO_iactAddrIdx is invalid
    io_debugIO_weightAddrSPadReadOut is invalid
    io_debugIO_weightMatrixData is invalid
    io_debugIO_weightMatrixRow is invalid
    io_debugIO_productResult is invalid
    io_debugIO_pSumResult is invalid
    io_debugIO_pSumLoad is invalid
    io_debugIO_weightAddrInIdx is invalid
    io_debugIO_sPadState is invalid
    psDataSPad_0 <= mux(reset, _T__0, _GEN_467) @[ProcessingElement.scala 344:37]
    psDataSPad_1 <= mux(reset, _T__1, _GEN_468) @[ProcessingElement.scala 344:37]
    psDataSPad_2 <= mux(reset, _T__2, _GEN_469) @[ProcessingElement.scala 344:37]
    psDataSPad_3 <= mux(reset, _T__3, _GEN_470) @[ProcessingElement.scala 344:37]
    psDataSPad_4 <= mux(reset, _T__4, _GEN_471) @[ProcessingElement.scala 344:37]
    psDataSPad_5 <= mux(reset, _T__5, _GEN_472) @[ProcessingElement.scala 344:37]
    psDataSPad_6 <= mux(reset, _T__6, _GEN_473) @[ProcessingElement.scala 344:37]
    psDataSPad_7 <= mux(reset, _T__7, _GEN_474) @[ProcessingElement.scala 344:37]
    psDataSPad_8 <= mux(reset, _T__8, _GEN_475) @[ProcessingElement.scala 344:37]
    psDataSPad_9 <= mux(reset, _T__9, _GEN_476) @[ProcessingElement.scala 344:37]
    psDataSPad_10 <= mux(reset, _T__10, _GEN_477) @[ProcessingElement.scala 344:37]
    psDataSPad_11 <= mux(reset, _T__11, _GEN_478) @[ProcessingElement.scala 344:37]
    psDataSPad_12 <= mux(reset, _T__12, _GEN_479) @[ProcessingElement.scala 344:37]
    psDataSPad_13 <= mux(reset, _T__13, _GEN_480) @[ProcessingElement.scala 344:37]
    psDataSPad_14 <= mux(reset, _T__14, _GEN_481) @[ProcessingElement.scala 344:37]
    psDataSPad_15 <= mux(reset, _T__15, _GEN_482) @[ProcessingElement.scala 344:37]
    psDataSPad_16 <= mux(reset, _T__16, _GEN_483) @[ProcessingElement.scala 344:37]
    psDataSPad_17 <= mux(reset, _T__17, _GEN_484) @[ProcessingElement.scala 344:37]
    psDataSPad_18 <= mux(reset, _T__18, _GEN_485) @[ProcessingElement.scala 344:37]
    psDataSPad_19 <= mux(reset, _T__19, _GEN_486) @[ProcessingElement.scala 344:37]
    psDataSPad_20 <= mux(reset, _T__20, _GEN_487) @[ProcessingElement.scala 344:37]
    psDataSPad_21 <= mux(reset, _T__21, _GEN_488) @[ProcessingElement.scala 344:37]
    psDataSPad_22 <= mux(reset, _T__22, _GEN_489) @[ProcessingElement.scala 344:37]
    psDataSPad_23 <= mux(reset, _T__23, _GEN_490) @[ProcessingElement.scala 344:37]
    psDataSPad_24 <= mux(reset, _T__24, _GEN_491) @[ProcessingElement.scala 344:37]
    psDataSPad_25 <= mux(reset, _T__25, _GEN_492) @[ProcessingElement.scala 344:37]
    psDataSPad_26 <= mux(reset, _T__26, _GEN_493) @[ProcessingElement.scala 344:37]
    psDataSPad_27 <= mux(reset, _T__27, _GEN_494) @[ProcessingElement.scala 344:37]
    psDataSPad_28 <= mux(reset, _T__28, _GEN_495) @[ProcessingElement.scala 344:37]
    psDataSPad_29 <= mux(reset, _T__29, _GEN_496) @[ProcessingElement.scala 344:37]
    psDataSPad_30 <= mux(reset, _T__30, _GEN_497) @[ProcessingElement.scala 344:37]
    psDataSPad_31 <= mux(reset, _T__31, _GEN_498) @[ProcessingElement.scala 344:37]
    SPadSeq_0.clock <= clock
    SPadSeq_0.reset <= reset
    SPadSeq_0.io_commonIO_readEn <= iactAddrSPadReadEnReg @[ProcessingElement.scala 190:35]
    SPadSeq_0.io_commonIO_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[ProcessingElement.scala 185:37]
    SPadSeq_0.io_commonIO_dataLenFinIO_writeInDataIO_valid <= io_dataStream_iactIOs_addrIOs_writeInDataIO_valid @[ProcessingElement.scala 187:41]
    SPadSeq_0.io_commonIO_dataLenFinIO_writeInDataIO_bits_data <= io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data @[ProcessingElement.scala 187:41]
    SPadSeq_0.io_commonIO_dataLenFinIO_streamLen <= io_dataStream_iactIOs_addrIOs_streamLen @[ProcessingElement.scala 187:41]
    SPadSeq_0.io_addrIO_readInIdx is invalid
    SPadSeq_0.io_addrIO_indexInc <= iactAddrSPadIdxIncWire @[ProcessingElement.scala 191:35]
    SPadSeq_0.io_addrIO_readInIdxEn is invalid
    SPadSeq_0.io_dataIO_readInIdx is invalid
    SPadSeq_0.io_dataIO_indexInc is invalid
    SPadSeq_0.io_dataIO_readInIdxEn is invalid
    SPadSeq_1.clock <= clock
    SPadSeq_1.reset <= reset
    SPadSeq_1.io_commonIO_readEn <= iactDataSPadReadEnReg @[ProcessingElement.scala 201:35]
    SPadSeq_1.io_commonIO_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[ProcessingElement.scala 185:37]
    SPadSeq_1.io_commonIO_dataLenFinIO_writeInDataIO_valid <= io_dataStream_iactIOs_dataIOs_writeInDataIO_valid @[ProcessingElement.scala 196:41]
    SPadSeq_1.io_commonIO_dataLenFinIO_writeInDataIO_bits_data <= io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data @[ProcessingElement.scala 196:41]
    SPadSeq_1.io_commonIO_dataLenFinIO_streamLen <= io_dataStream_iactIOs_dataIOs_streamLen @[ProcessingElement.scala 196:41]
    SPadSeq_1.io_addrIO_readInIdx is invalid
    SPadSeq_1.io_addrIO_indexInc is invalid
    SPadSeq_1.io_addrIO_readInIdxEn is invalid
    SPadSeq_1.io_dataIO_readInIdx <= iactAddrDataWire @[ProcessingElement.scala 203:36]
    SPadSeq_1.io_dataIO_indexInc <= iactDataSPadIdxIncWire @[ProcessingElement.scala 202:35]
    SPadSeq_1.io_dataIO_readInIdxEn is invalid
    SPadSeq_2.clock <= clock
    SPadSeq_2.reset <= reset
    SPadSeq_2.io_commonIO_readEn <= weightAddrSPadReadEnReg @[ProcessingElement.scala 210:37]
    SPadSeq_2.io_commonIO_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[ProcessingElement.scala 185:37]
    SPadSeq_2.io_commonIO_dataLenFinIO_writeInDataIO_valid <= io_dataStream_weightIOs_addrIOs_writeInDataIO_valid @[ProcessingElement.scala 207:43]
    SPadSeq_2.io_commonIO_dataLenFinIO_writeInDataIO_bits_data <= io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data @[ProcessingElement.scala 207:43]
    SPadSeq_2.io_commonIO_dataLenFinIO_streamLen <= io_dataStream_weightIOs_addrIOs_streamLen @[ProcessingElement.scala 207:43]
    SPadSeq_2.io_addrIO_readInIdx <= weightAddrSPadReadIdxWire @[ProcessingElement.scala 211:38]
    SPadSeq_2.io_addrIO_indexInc <= weightAddrSPadIdxIncWire @[ProcessingElement.scala 214:37]
    SPadSeq_2.io_addrIO_readInIdxEn <= weightAddrIdxEnWire @[ProcessingElement.scala 215:40]
    SPadSeq_2.io_dataIO_readInIdx is invalid
    SPadSeq_2.io_dataIO_indexInc is invalid
    SPadSeq_2.io_dataIO_readInIdxEn is invalid
    SPadSeq_3.clock <= clock
    SPadSeq_3.reset <= reset
    SPadSeq_3.io_commonIO_readEn <= iactDataSPadReadEnReg @[ProcessingElement.scala 223:37]
    SPadSeq_3.io_commonIO_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[ProcessingElement.scala 185:37]
    SPadSeq_3.io_commonIO_dataLenFinIO_writeInDataIO_valid <= io_dataStream_weightIOs_dataIOs_writeInDataIO_valid @[ProcessingElement.scala 218:43]
    SPadSeq_3.io_commonIO_dataLenFinIO_writeInDataIO_bits_data <= io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data @[ProcessingElement.scala 218:43]
    SPadSeq_3.io_commonIO_dataLenFinIO_streamLen <= io_dataStream_weightIOs_dataIOs_streamLen @[ProcessingElement.scala 218:43]
    SPadSeq_3.io_addrIO_readInIdx is invalid
    SPadSeq_3.io_addrIO_indexInc is invalid
    SPadSeq_3.io_addrIO_readInIdxEn is invalid
    SPadSeq_3.io_dataIO_readInIdx <= _T_27 @[ProcessingElement.scala 224:38]
    SPadSeq_3.io_dataIO_indexInc <= weightDataSPadIdxIncWire @[ProcessingElement.scala 225:37]
    SPadSeq_3.io_dataIO_readInIdxEn <= weightDataIdxEnWire @[ProcessingElement.scala 226:40]
    iactAddrSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_454) @[ProcessingElement.scala 88:27 ProcessingElement.scala 88:27 ProcessingElement.scala 96:27 ProcessingElement.scala 104:27 ProcessingElement.scala 88:27 ProcessingElement.scala 96:27 ProcessingElement.scala 111:27 ProcessingElement.scala 117:27 ProcessingElement.scala 88:27 ProcessingElement.scala 96:27 ProcessingElement.scala 111:27]
    iactDataSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_455) @[ProcessingElement.scala 89:27 ProcessingElement.scala 89:27 ProcessingElement.scala 97:27 ProcessingElement.scala 105:27 ProcessingElement.scala 89:27 ProcessingElement.scala 97:27 ProcessingElement.scala 112:27 ProcessingElement.scala 118:27 ProcessingElement.scala 89:27 ProcessingElement.scala 97:27 ProcessingElement.scala 112:27]
    iactMatrixColumnReg <= mux(reset, UInt<4>("h0"), _GEN_462) @[ProcessingElement.scala 309:33 ProcessingElement.scala 312:33 ProcessingElement.scala 347:29 ProcessingElement.scala 355:35 ProcessingElement.scala 358:35]
    iactZeroColumnNumber <= mux(reset, UInt<4>("h0"), _GEN_461) @[ProcessingElement.scala 294:30 ProcessingElement.scala 310:34 ProcessingElement.scala 356:36]
    iactDataSPadFirstReadReg <= mux(reset, UInt<1>("h1"), _GEN_459) @[ProcessingElement.scala 287:34 ProcessingElement.scala 301:32]
    weightAddrSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_456) @[ProcessingElement.scala 90:29 ProcessingElement.scala 90:29 ProcessingElement.scala 98:29 ProcessingElement.scala 106:29 ProcessingElement.scala 90:29 ProcessingElement.scala 98:29 ProcessingElement.scala 113:29 ProcessingElement.scala 119:29 ProcessingElement.scala 90:29 ProcessingElement.scala 98:29 ProcessingElement.scala 113:29]
    weightDataSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_457) @[ProcessingElement.scala 91:29 ProcessingElement.scala 91:29 ProcessingElement.scala 99:29 ProcessingElement.scala 107:29 ProcessingElement.scala 91:29 ProcessingElement.scala 99:29 ProcessingElement.scala 114:29 ProcessingElement.scala 120:29 ProcessingElement.scala 91:29 ProcessingElement.scala 99:29 ProcessingElement.scala 114:29]
    weightDataSPadFirstRead <= mux(reset, UInt<1>("h1"), _GEN_458) @[ProcessingElement.scala 92:29 ProcessingElement.scala 92:29 ProcessingElement.scala 100:29 ProcessingElement.scala 92:29 ProcessingElement.scala 100:29 ProcessingElement.scala 92:29 ProcessingElement.scala 100:29 ProcessingElement.scala 365:35]
    productReg <= mux(reset, UInt<20>("h0"), _GEN_464) @[ProcessingElement.scala 333:22 ProcessingElement.scala 339:20]
    pSumSPadLoadReg <= mux(reset, UInt<20>("h0"), _GEN_466) @[ProcessingElement.scala 335:27 ProcessingElement.scala 340:25]
    sPad <= mux(reset, UInt<3>("h0"), _GEN_453) @[ProcessingElement.scala 87:10 ProcessingElement.scala 87:10 ProcessingElement.scala 95:10 ProcessingElement.scala 103:10 ProcessingElement.scala 87:10 ProcessingElement.scala 95:10 ProcessingElement.scala 110:10 ProcessingElement.scala 322:12 ProcessingElement.scala 325:12 ProcessingElement.scala 332:16 ProcessingElement.scala 338:14 ProcessingElement.scala 346:14 ProcessingElement.scala 87:10 ProcessingElement.scala 95:10 ProcessingElement.scala 110:10]
    value <= mux(reset, UInt<5>("h0"), _GEN_463) @[Counter.scala 38:13 Counter.scala 40:29 ProcessingElement.scala 326:33 ProcessingElement.scala 348:35]
    pSumResultIdxReg <= mux(reset, UInt<20>("h0"), pSumResultIdxReg)
    iactSPadZeroColumnReg <= mux(reset, UInt<1>("h0"), _GEN_460) @[ProcessingElement.scala 293:31 ProcessingElement.scala 308:35 ProcessingElement.scala 354:37]

  module Queue :
    input clock : Clock
    input reset : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits_data : UInt<4>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits_data : UInt<4>
    output io_count : UInt<3>
  
    mem _T_data : @[Decoupled.scala 209:24]
      data-type => UInt<4>
      depth => 4
      read-latency => 0
      write-latency => 1
      reader => _T_22
      writer => _T_12
      read-under-write => undefined
    reg _T_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_1) @[Counter.scala 29:33]
    reg _T_2 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_2) @[Counter.scala 29:33]
    reg _T_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_3) @[Decoupled.scala 212:35]
    node _T_4 = eq(_T_1, _T_2) @[Decoupled.scala 214:41]
    node _T_5 = eq(_T_3, UInt<1>("h0")) @[Decoupled.scala 215:36]
    node _T_6 = and(_T_4, _T_5) @[Decoupled.scala 215:33]
    node _T_7 = and(_T_4, _T_3) @[Decoupled.scala 216:32]
    node _T_8 = and(io_enq_ready, io_enq_valid) @[Decoupled.scala 40:37]
    node _T_10 = and(io_deq_ready, io_deq_valid) @[Decoupled.scala 40:37]
    node _T_13 = eq(_T_1, UInt<2>("h3")) @[Counter.scala 37:24]
    node _T_14 = add(_T_1, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_15 = tail(_T_14, 1) @[Counter.scala 38:22]
    node _GEN_9 = mux(io_deq_ready, UInt<1>("h0"), _T_8) @[Decoupled.scala 240:27]
    node _GEN_12 = mux(_T_6, _GEN_9, _T_8) @[Decoupled.scala 237:18]
    node _T_9 = _GEN_12 @[Decoupled.scala 240:36]
    node _GEN_0 = validif(_T_9, _T_1) @[Decoupled.scala 220:17]
    node _GEN_1 = validif(_T_9, clock) @[Decoupled.scala 220:17]
    node _GEN_2 = mux(_T_9, UInt<1>("h1"), UInt<1>("h0")) @[Decoupled.scala 220:17]
    node _GEN_3 = validif(_T_9, UInt<1>("h1")) @[Decoupled.scala 220:17]
    node _GEN_4 = validif(_T_9, io_enq_bits_data) @[Decoupled.scala 220:17]
    node _GEN_5 = mux(_T_9, _T_15, _T_1) @[Decoupled.scala 220:17]
    node _T_16 = eq(_T_2, UInt<2>("h3")) @[Counter.scala 37:24]
    node _T_17 = add(_T_2, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_18 = tail(_T_17, 1) @[Counter.scala 38:22]
    node _GEN_11 = mux(_T_6, UInt<1>("h0"), _T_10) @[Decoupled.scala 237:18]
    node _T_11 = _GEN_11 @[Decoupled.scala 239:14]
    node _GEN_6 = mux(_T_11, _T_18, _T_2) @[Decoupled.scala 224:17]
    node _T_19 = neq(_T_9, _T_11) @[Decoupled.scala 227:16]
    node _GEN_7 = mux(_T_19, _T_9, _T_3) @[Decoupled.scala 227:28]
    node _T_20 = eq(_T_6, UInt<1>("h0")) @[Decoupled.scala 231:19]
    node _T_21 = eq(_T_7, UInt<1>("h0")) @[Decoupled.scala 232:19]
    node _GEN_8 = mux(io_enq_valid, UInt<1>("h1"), _T_20) @[Decoupled.scala 236:25]
    node _GEN_10 = mux(_T_6, io_enq_bits_data, _T_data._T_22.data) @[Decoupled.scala 237:18]
    node _T_23 = sub(_T_1, _T_2) @[Decoupled.scala 248:40]
    node _T_24 = tail(_T_23, 1) @[Decoupled.scala 248:40]
    node _T_25 = and(_T_3, _T_4) @[Decoupled.scala 250:32]
    node _T_26 = mux(_T_25, UInt<3>("h4"), UInt<1>("h0")) @[Decoupled.scala 250:20]
    node _T_27 = or(_T_26, _T_24) @[Decoupled.scala 250:62]
    io_enq_ready <= _T_21 @[Decoupled.scala 232:16]
    io_deq_valid <= _GEN_8 @[Decoupled.scala 231:16 Decoupled.scala 236:40]
    io_deq_bits_data <= _GEN_10 @[Decoupled.scala 233:15 Decoupled.scala 238:19]
    io_count <= _T_27 @[Decoupled.scala 250:14]
    _T_data._T_22.addr <= _T_2 @[Decoupled.scala 233:21]
    _T_data._T_22.en <= UInt<1>("h1") @[Decoupled.scala 209:24 Decoupled.scala 233:21]
    _T_data._T_22.clk <= clock @[Decoupled.scala 233:21]
    _T_data._T_12.addr <= _GEN_0 @[Decoupled.scala 221:8]
    _T_data._T_12.en <= _GEN_2 @[Decoupled.scala 209:24 Decoupled.scala 221:8]
    _T_data._T_12.clk <= _GEN_1 @[Decoupled.scala 221:8]
    _T_data._T_12.data <= _GEN_4 @[Decoupled.scala 221:24]
    _T_data._T_12.mask <= _GEN_3 @[Decoupled.scala 221:8 Decoupled.scala 221:24]
    _T_1 <= mux(reset, UInt<2>("h0"), _GEN_5) @[Counter.scala 38:13]
    _T_2 <= mux(reset, UInt<2>("h0"), _GEN_6) @[Counter.scala 38:13]
    _T_3 <= mux(reset, UInt<1>("h0"), _GEN_7) @[Decoupled.scala 228:16]

  module Queue_1 :
    input clock : Clock
    input reset : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits_data : UInt<12>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits_data : UInt<12>
    output io_count : UInt<3>
  
    mem _T_data : @[Decoupled.scala 209:24]
      data-type => UInt<12>
      depth => 4
      read-latency => 0
      write-latency => 1
      reader => _T_22
      writer => _T_12
      read-under-write => undefined
    reg _T_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_1) @[Counter.scala 29:33]
    reg _T_2 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_2) @[Counter.scala 29:33]
    reg _T_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_3) @[Decoupled.scala 212:35]
    node _T_4 = eq(_T_1, _T_2) @[Decoupled.scala 214:41]
    node _T_5 = eq(_T_3, UInt<1>("h0")) @[Decoupled.scala 215:36]
    node _T_6 = and(_T_4, _T_5) @[Decoupled.scala 215:33]
    node _T_7 = and(_T_4, _T_3) @[Decoupled.scala 216:32]
    node _T_8 = and(io_enq_ready, io_enq_valid) @[Decoupled.scala 40:37]
    node _T_10 = and(io_deq_ready, io_deq_valid) @[Decoupled.scala 40:37]
    node _T_13 = eq(_T_1, UInt<2>("h3")) @[Counter.scala 37:24]
    node _T_14 = add(_T_1, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_15 = tail(_T_14, 1) @[Counter.scala 38:22]
    node _GEN_9 = mux(io_deq_ready, UInt<1>("h0"), _T_8) @[Decoupled.scala 240:27]
    node _GEN_12 = mux(_T_6, _GEN_9, _T_8) @[Decoupled.scala 237:18]
    node _T_9 = _GEN_12 @[Decoupled.scala 240:36]
    node _GEN_0 = validif(_T_9, _T_1) @[Decoupled.scala 220:17]
    node _GEN_1 = validif(_T_9, clock) @[Decoupled.scala 220:17]
    node _GEN_2 = mux(_T_9, UInt<1>("h1"), UInt<1>("h0")) @[Decoupled.scala 220:17]
    node _GEN_3 = validif(_T_9, UInt<1>("h1")) @[Decoupled.scala 220:17]
    node _GEN_4 = validif(_T_9, io_enq_bits_data) @[Decoupled.scala 220:17]
    node _GEN_5 = mux(_T_9, _T_15, _T_1) @[Decoupled.scala 220:17]
    node _T_16 = eq(_T_2, UInt<2>("h3")) @[Counter.scala 37:24]
    node _T_17 = add(_T_2, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_18 = tail(_T_17, 1) @[Counter.scala 38:22]
    node _GEN_11 = mux(_T_6, UInt<1>("h0"), _T_10) @[Decoupled.scala 237:18]
    node _T_11 = _GEN_11 @[Decoupled.scala 239:14]
    node _GEN_6 = mux(_T_11, _T_18, _T_2) @[Decoupled.scala 224:17]
    node _T_19 = neq(_T_9, _T_11) @[Decoupled.scala 227:16]
    node _GEN_7 = mux(_T_19, _T_9, _T_3) @[Decoupled.scala 227:28]
    node _T_20 = eq(_T_6, UInt<1>("h0")) @[Decoupled.scala 231:19]
    node _T_21 = eq(_T_7, UInt<1>("h0")) @[Decoupled.scala 232:19]
    node _GEN_8 = mux(io_enq_valid, UInt<1>("h1"), _T_20) @[Decoupled.scala 236:25]
    node _GEN_10 = mux(_T_6, io_enq_bits_data, _T_data._T_22.data) @[Decoupled.scala 237:18]
    node _T_23 = sub(_T_1, _T_2) @[Decoupled.scala 248:40]
    node _T_24 = tail(_T_23, 1) @[Decoupled.scala 248:40]
    node _T_25 = and(_T_3, _T_4) @[Decoupled.scala 250:32]
    node _T_26 = mux(_T_25, UInt<3>("h4"), UInt<1>("h0")) @[Decoupled.scala 250:20]
    node _T_27 = or(_T_26, _T_24) @[Decoupled.scala 250:62]
    io_enq_ready <= _T_21 @[Decoupled.scala 232:16]
    io_deq_valid <= _GEN_8 @[Decoupled.scala 231:16 Decoupled.scala 236:40]
    io_deq_bits_data <= _GEN_10 @[Decoupled.scala 233:15 Decoupled.scala 238:19]
    io_count <= _T_27 @[Decoupled.scala 250:14]
    _T_data._T_22.addr <= _T_2 @[Decoupled.scala 233:21]
    _T_data._T_22.en <= UInt<1>("h1") @[Decoupled.scala 209:24 Decoupled.scala 233:21]
    _T_data._T_22.clk <= clock @[Decoupled.scala 233:21]
    _T_data._T_12.addr <= _GEN_0 @[Decoupled.scala 221:8]
    _T_data._T_12.en <= _GEN_2 @[Decoupled.scala 209:24 Decoupled.scala 221:8]
    _T_data._T_12.clk <= _GEN_1 @[Decoupled.scala 221:8]
    _T_data._T_12.data <= _GEN_4 @[Decoupled.scala 221:24]
    _T_data._T_12.mask <= _GEN_3 @[Decoupled.scala 221:8 Decoupled.scala 221:24]
    _T_1 <= mux(reset, UInt<2>("h0"), _GEN_5) @[Counter.scala 38:13]
    _T_2 <= mux(reset, UInt<2>("h0"), _GEN_6) @[Counter.scala 38:13]
    _T_3 <= mux(reset, UInt<1>("h0"), _GEN_7) @[Decoupled.scala 228:16]

  module Queue_2 :
    input clock : Clock
    input reset : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits_data : UInt<7>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits_data : UInt<7>
    output io_count : UInt<3>
  
    mem _T_data : @[Decoupled.scala 209:24]
      data-type => UInt<7>
      depth => 4
      read-latency => 0
      write-latency => 1
      reader => _T_22
      writer => _T_12
      read-under-write => undefined
    reg _T_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_1) @[Counter.scala 29:33]
    reg _T_2 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_2) @[Counter.scala 29:33]
    reg _T_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_3) @[Decoupled.scala 212:35]
    node _T_4 = eq(_T_1, _T_2) @[Decoupled.scala 214:41]
    node _T_5 = eq(_T_3, UInt<1>("h0")) @[Decoupled.scala 215:36]
    node _T_6 = and(_T_4, _T_5) @[Decoupled.scala 215:33]
    node _T_7 = and(_T_4, _T_3) @[Decoupled.scala 216:32]
    node _T_8 = and(io_enq_ready, io_enq_valid) @[Decoupled.scala 40:37]
    node _T_10 = and(io_deq_ready, io_deq_valid) @[Decoupled.scala 40:37]
    node _T_13 = eq(_T_1, UInt<2>("h3")) @[Counter.scala 37:24]
    node _T_14 = add(_T_1, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_15 = tail(_T_14, 1) @[Counter.scala 38:22]
    node _GEN_9 = mux(io_deq_ready, UInt<1>("h0"), _T_8) @[Decoupled.scala 240:27]
    node _GEN_12 = mux(_T_6, _GEN_9, _T_8) @[Decoupled.scala 237:18]
    node _T_9 = _GEN_12 @[Decoupled.scala 240:36]
    node _GEN_0 = validif(_T_9, _T_1) @[Decoupled.scala 220:17]
    node _GEN_1 = validif(_T_9, clock) @[Decoupled.scala 220:17]
    node _GEN_2 = mux(_T_9, UInt<1>("h1"), UInt<1>("h0")) @[Decoupled.scala 220:17]
    node _GEN_3 = validif(_T_9, UInt<1>("h1")) @[Decoupled.scala 220:17]
    node _GEN_4 = validif(_T_9, io_enq_bits_data) @[Decoupled.scala 220:17]
    node _GEN_5 = mux(_T_9, _T_15, _T_1) @[Decoupled.scala 220:17]
    node _T_16 = eq(_T_2, UInt<2>("h3")) @[Counter.scala 37:24]
    node _T_17 = add(_T_2, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_18 = tail(_T_17, 1) @[Counter.scala 38:22]
    node _GEN_11 = mux(_T_6, UInt<1>("h0"), _T_10) @[Decoupled.scala 237:18]
    node _T_11 = _GEN_11 @[Decoupled.scala 239:14]
    node _GEN_6 = mux(_T_11, _T_18, _T_2) @[Decoupled.scala 224:17]
    node _T_19 = neq(_T_9, _T_11) @[Decoupled.scala 227:16]
    node _GEN_7 = mux(_T_19, _T_9, _T_3) @[Decoupled.scala 227:28]
    node _T_20 = eq(_T_6, UInt<1>("h0")) @[Decoupled.scala 231:19]
    node _T_21 = eq(_T_7, UInt<1>("h0")) @[Decoupled.scala 232:19]
    node _GEN_8 = mux(io_enq_valid, UInt<1>("h1"), _T_20) @[Decoupled.scala 236:25]
    node _GEN_10 = mux(_T_6, io_enq_bits_data, _T_data._T_22.data) @[Decoupled.scala 237:18]
    node _T_23 = sub(_T_1, _T_2) @[Decoupled.scala 248:40]
    node _T_24 = tail(_T_23, 1) @[Decoupled.scala 248:40]
    node _T_25 = and(_T_3, _T_4) @[Decoupled.scala 250:32]
    node _T_26 = mux(_T_25, UInt<3>("h4"), UInt<1>("h0")) @[Decoupled.scala 250:20]
    node _T_27 = or(_T_26, _T_24) @[Decoupled.scala 250:62]
    io_enq_ready <= _T_21 @[Decoupled.scala 232:16]
    io_deq_valid <= _GEN_8 @[Decoupled.scala 231:16 Decoupled.scala 236:40]
    io_deq_bits_data <= _GEN_10 @[Decoupled.scala 233:15 Decoupled.scala 238:19]
    io_count <= _T_27 @[Decoupled.scala 250:14]
    _T_data._T_22.addr <= _T_2 @[Decoupled.scala 233:21]
    _T_data._T_22.en <= UInt<1>("h1") @[Decoupled.scala 209:24 Decoupled.scala 233:21]
    _T_data._T_22.clk <= clock @[Decoupled.scala 233:21]
    _T_data._T_12.addr <= _GEN_0 @[Decoupled.scala 221:8]
    _T_data._T_12.en <= _GEN_2 @[Decoupled.scala 209:24 Decoupled.scala 221:8]
    _T_data._T_12.clk <= _GEN_1 @[Decoupled.scala 221:8]
    _T_data._T_12.data <= _GEN_4 @[Decoupled.scala 221:24]
    _T_data._T_12.mask <= _GEN_3 @[Decoupled.scala 221:8 Decoupled.scala 221:24]
    _T_1 <= mux(reset, UInt<2>("h0"), _GEN_5) @[Counter.scala 38:13]
    _T_2 <= mux(reset, UInt<2>("h0"), _GEN_6) @[Counter.scala 38:13]
    _T_3 <= mux(reset, UInt<1>("h0"), _GEN_7) @[Decoupled.scala 228:16]

  module Queue_4 :
    input clock : Clock
    input reset : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits : UInt<20>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits : UInt<20>
    output io_count : UInt<3>
  
    mem _T : @[Decoupled.scala 209:24]
      data-type => UInt<20>
      depth => 4
      read-latency => 0
      write-latency => 1
      reader => _T_22
      writer => _T_12
      read-under-write => undefined
    reg _T_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_1) @[Counter.scala 29:33]
    reg _T_2 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_2) @[Counter.scala 29:33]
    reg _T_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_3) @[Decoupled.scala 212:35]
    node _T_4 = eq(_T_1, _T_2) @[Decoupled.scala 214:41]
    node _T_5 = eq(_T_3, UInt<1>("h0")) @[Decoupled.scala 215:36]
    node _T_6 = and(_T_4, _T_5) @[Decoupled.scala 215:33]
    node _T_7 = and(_T_4, _T_3) @[Decoupled.scala 216:32]
    node _T_8 = and(io_enq_ready, io_enq_valid) @[Decoupled.scala 40:37]
    node _T_10 = and(io_deq_ready, io_deq_valid) @[Decoupled.scala 40:37]
    node _T_13 = eq(_T_1, UInt<2>("h3")) @[Counter.scala 37:24]
    node _T_14 = add(_T_1, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_15 = tail(_T_14, 1) @[Counter.scala 38:22]
    node _GEN_9 = mux(io_deq_ready, UInt<1>("h0"), _T_8) @[Decoupled.scala 240:27]
    node _GEN_12 = mux(_T_6, _GEN_9, _T_8) @[Decoupled.scala 237:18]
    node _T_9 = _GEN_12 @[Decoupled.scala 240:36]
    node _GEN_0 = validif(_T_9, _T_1) @[Decoupled.scala 220:17]
    node _GEN_1 = validif(_T_9, clock) @[Decoupled.scala 220:17]
    node _GEN_2 = mux(_T_9, UInt<1>("h1"), UInt<1>("h0")) @[Decoupled.scala 220:17]
    node _GEN_3 = validif(_T_9, UInt<1>("h1")) @[Decoupled.scala 220:17]
    node _GEN_4 = validif(_T_9, io_enq_bits) @[Decoupled.scala 220:17]
    node _GEN_5 = mux(_T_9, _T_15, _T_1) @[Decoupled.scala 220:17]
    node _T_16 = eq(_T_2, UInt<2>("h3")) @[Counter.scala 37:24]
    node _T_17 = add(_T_2, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_18 = tail(_T_17, 1) @[Counter.scala 38:22]
    node _GEN_11 = mux(_T_6, UInt<1>("h0"), _T_10) @[Decoupled.scala 237:18]
    node _T_11 = _GEN_11 @[Decoupled.scala 239:14]
    node _GEN_6 = mux(_T_11, _T_18, _T_2) @[Decoupled.scala 224:17]
    node _T_19 = neq(_T_9, _T_11) @[Decoupled.scala 227:16]
    node _GEN_7 = mux(_T_19, _T_9, _T_3) @[Decoupled.scala 227:28]
    node _T_20 = eq(_T_6, UInt<1>("h0")) @[Decoupled.scala 231:19]
    node _T_21 = eq(_T_7, UInt<1>("h0")) @[Decoupled.scala 232:19]
    node _GEN_8 = mux(io_enq_valid, UInt<1>("h1"), _T_20) @[Decoupled.scala 236:25]
    node _GEN_10 = mux(_T_6, io_enq_bits, _T._T_22.data) @[Decoupled.scala 237:18]
    node _T_23 = sub(_T_1, _T_2) @[Decoupled.scala 248:40]
    node _T_24 = tail(_T_23, 1) @[Decoupled.scala 248:40]
    node _T_25 = and(_T_3, _T_4) @[Decoupled.scala 250:32]
    node _T_26 = mux(_T_25, UInt<3>("h4"), UInt<1>("h0")) @[Decoupled.scala 250:20]
    node _T_27 = or(_T_26, _T_24) @[Decoupled.scala 250:62]
    io_enq_ready <= _T_21 @[Decoupled.scala 232:16]
    io_deq_valid <= _GEN_8 @[Decoupled.scala 231:16 Decoupled.scala 236:40]
    io_deq_bits <= _GEN_10 @[Decoupled.scala 233:15 Decoupled.scala 238:19]
    io_count <= _T_27 @[Decoupled.scala 250:14]
    _T._T_22.addr <= _T_2 @[Decoupled.scala 233:21]
    _T._T_22.en <= UInt<1>("h1") @[Decoupled.scala 209:24 Decoupled.scala 233:21]
    _T._T_22.clk <= clock @[Decoupled.scala 233:21]
    _T._T_12.addr <= _GEN_0 @[Decoupled.scala 221:8]
    _T._T_12.en <= _GEN_2 @[Decoupled.scala 209:24 Decoupled.scala 221:8]
    _T._T_12.clk <= _GEN_1 @[Decoupled.scala 221:8]
    _T._T_12.data <= _GEN_4 @[Decoupled.scala 221:24]
    _T._T_12.mask <= _GEN_3 @[Decoupled.scala 221:8 Decoupled.scala 221:24]
    _T_1 <= mux(reset, UInt<2>("h0"), _GEN_5) @[Counter.scala 38:13]
    _T_2 <= mux(reset, UInt<2>("h0"), _GEN_6) @[Counter.scala 38:13]
    _T_3 <= mux(reset, UInt<1>("h0"), _GEN_7) @[Decoupled.scala 228:16]

  module ProcessingElement :
    input clock : Clock
    input reset : UInt<1>
    output io_dataStream_ipsIO_ready : UInt<1>
    input io_dataStream_ipsIO_valid : UInt<1>
    input io_dataStream_ipsIO_bits : UInt<20>
    input io_dataStream_opsIO_ready : UInt<1>
    output io_dataStream_opsIO_valid : UInt<1>
    output io_dataStream_opsIO_bits : UInt<20>
    output io_dataStream_iactIOs_dataIOs_writeInDataIO_ready : UInt<1>
    input io_dataStream_iactIOs_dataIOs_writeInDataIO_valid : UInt<1>
    input io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data : UInt<12>
    input io_dataStream_iactIOs_dataIOs_streamLen : UInt<4>
    output io_dataStream_iactIOs_dataIOs_writeFin : UInt<1>
    output io_dataStream_iactIOs_addrIOs_writeInDataIO_ready : UInt<1>
    input io_dataStream_iactIOs_addrIOs_writeInDataIO_valid : UInt<1>
    input io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data : UInt<4>
    input io_dataStream_iactIOs_addrIOs_streamLen : UInt<4>
    output io_dataStream_iactIOs_addrIOs_writeFin : UInt<1>
    output io_dataStream_weightIOs_dataIOs_writeInDataIO_ready : UInt<1>
    input io_dataStream_weightIOs_dataIOs_writeInDataIO_valid : UInt<1>
    input io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data : UInt<12>
    input io_dataStream_weightIOs_dataIOs_streamLen : UInt<8>
    output io_dataStream_weightIOs_dataIOs_writeFin : UInt<1>
    output io_dataStream_weightIOs_addrIOs_writeInDataIO_ready : UInt<1>
    input io_dataStream_weightIOs_addrIOs_writeInDataIO_valid : UInt<1>
    input io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data : UInt<7>
    input io_dataStream_weightIOs_addrIOs_streamLen : UInt<4>
    output io_dataStream_weightIOs_addrIOs_writeFin : UInt<1>
    output io_topCtrl_pSumEnqOrProduct_ready : UInt<1>
    input io_topCtrl_pSumEnqOrProduct_valid : UInt<1>
    input io_topCtrl_pSumEnqOrProduct_bits : UInt<1>
    input io_topCtrl_doLoadEn : UInt<1>
    output io_topCtrl_writeFinish : UInt<1>
    output io_topCtrl_calFinish : UInt<1>
  
    inst peCtrl of ProcessingElementControl @[ProcessingElement.scala 11:48]
    inst pePad of ProcessingElementPad @[ProcessingElement.scala 12:43]
    inst Queue of Queue @[Decoupled.scala 287:21]
    inst Queue_1 of Queue_1 @[Decoupled.scala 287:21]
    inst Queue_2 of Queue_2 @[Decoupled.scala 287:21]
    inst Queue_3 of Queue_1 @[Decoupled.scala 287:21]
    inst Queue_4 of Queue_4 @[Decoupled.scala 287:21]
    inst Queue_5 of Queue_4 @[Decoupled.scala 287:21]
    io_dataStream_ipsIO_ready <= Queue_4.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_opsIO_valid <= Queue_5.io_deq_valid @[ProcessingElement.scala 32:23]
    io_dataStream_opsIO_bits <= Queue_5.io_deq_bits @[ProcessingElement.scala 32:23]
    io_dataStream_iactIOs_dataIOs_writeInDataIO_ready <= Queue_1.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_iactIOs_dataIOs_writeFin <= pePad.io_dataStream_iactIOs_dataIOs_writeFin @[ProcessingElement.scala 24:55]
    io_dataStream_iactIOs_addrIOs_writeInDataIO_ready <= Queue.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_iactIOs_addrIOs_writeFin <= pePad.io_dataStream_iactIOs_addrIOs_writeFin @[ProcessingElement.scala 23:55]
    io_dataStream_weightIOs_dataIOs_writeInDataIO_ready <= Queue_3.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_weightIOs_dataIOs_writeFin <= pePad.io_dataStream_weightIOs_dataIOs_writeFin @[ProcessingElement.scala 24:55]
    io_dataStream_weightIOs_addrIOs_writeInDataIO_ready <= Queue_2.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_weightIOs_addrIOs_writeFin <= pePad.io_dataStream_weightIOs_addrIOs_writeFin @[ProcessingElement.scala 23:55]
    io_topCtrl_pSumEnqOrProduct_ready <= peCtrl.io_ctrlTop_pSumEnqOrProduct_ready @[ProcessingElement.scala 30:14]
    io_topCtrl_writeFinish <= peCtrl.io_ctrlTop_writeFinish @[ProcessingElement.scala 30:14]
    io_topCtrl_calFinish <= peCtrl.io_ctrlTop_calFinish @[ProcessingElement.scala 30:14]
    peCtrl.clock <= clock
    peCtrl.reset <= reset
    peCtrl.io_ctrlPad_fromTopIO_pSumEnqOrProduct_ready <= pePad.io_padCtrl_fromTopIO_pSumEnqOrProduct_ready @[ProcessingElement.scala 29:21]
    peCtrl.io_ctrlPad_fromTopIO_writeFinish <= pePad.io_padCtrl_fromTopIO_writeFinish @[ProcessingElement.scala 29:21]
    peCtrl.io_ctrlPad_fromTopIO_calFinish <= pePad.io_padCtrl_fromTopIO_calFinish @[ProcessingElement.scala 29:21]
    peCtrl.io_ctrlTop_pSumEnqOrProduct_valid <= io_topCtrl_pSumEnqOrProduct_valid @[ProcessingElement.scala 30:14]
    peCtrl.io_ctrlTop_pSumEnqOrProduct_bits <= io_topCtrl_pSumEnqOrProduct_bits @[ProcessingElement.scala 30:14]
    peCtrl.io_ctrlTop_doLoadEn <= io_topCtrl_doLoadEn @[ProcessingElement.scala 30:14]
    pePad.clock <= clock
    pePad.reset <= reset
    pePad.io_padCtrl_doMACEn <= peCtrl.io_ctrlPad_doMACEn @[ProcessingElement.scala 29:21]
    pePad.io_padCtrl_fromTopIO_pSumEnqOrProduct_valid <= peCtrl.io_ctrlPad_fromTopIO_pSumEnqOrProduct_valid @[ProcessingElement.scala 29:21]
    pePad.io_padCtrl_fromTopIO_pSumEnqOrProduct_bits <= peCtrl.io_ctrlPad_fromTopIO_pSumEnqOrProduct_bits @[ProcessingElement.scala 29:21]
    pePad.io_padCtrl_fromTopIO_doLoadEn <= peCtrl.io_ctrlPad_fromTopIO_doLoadEn @[ProcessingElement.scala 29:21]
    pePad.io_dataStream_ipsIO_valid <= Queue_4.io_deq_valid @[ProcessingElement.scala 31:29]
    pePad.io_dataStream_ipsIO_bits <= Queue_4.io_deq_bits @[ProcessingElement.scala 31:29]
    pePad.io_dataStream_opsIO_ready <= Queue_5.io_enq_ready @[Decoupled.scala 290:17]
    pePad.io_dataStream_iactIOs_dataIOs_writeInDataIO_valid <= Queue_1.io_deq_valid @[ProcessingElement.scala 15:55]
    pePad.io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data <= Queue_1.io_deq_bits_data @[ProcessingElement.scala 15:55]
    pePad.io_dataStream_iactIOs_dataIOs_streamLen <= io_dataStream_iactIOs_dataIOs_streamLen @[ProcessingElement.scala 22:56]
    pePad.io_dataStream_iactIOs_addrIOs_writeInDataIO_valid <= Queue.io_deq_valid @[ProcessingElement.scala 14:55]
    pePad.io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data <= Queue.io_deq_bits_data @[ProcessingElement.scala 14:55]
    pePad.io_dataStream_iactIOs_addrIOs_streamLen <= io_dataStream_iactIOs_addrIOs_streamLen @[ProcessingElement.scala 21:56]
    pePad.io_dataStream_weightIOs_dataIOs_writeInDataIO_valid <= Queue_3.io_deq_valid @[ProcessingElement.scala 17:57]
    pePad.io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data <= Queue_3.io_deq_bits_data @[ProcessingElement.scala 17:57]
    pePad.io_dataStream_weightIOs_dataIOs_streamLen <= io_dataStream_weightIOs_dataIOs_streamLen @[ProcessingElement.scala 22:56]
    pePad.io_dataStream_weightIOs_addrIOs_writeInDataIO_valid <= Queue_2.io_deq_valid @[ProcessingElement.scala 16:57]
    pePad.io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data <= Queue_2.io_deq_bits_data @[ProcessingElement.scala 16:57]
    pePad.io_dataStream_weightIOs_addrIOs_streamLen <= io_dataStream_weightIOs_addrIOs_streamLen @[ProcessingElement.scala 21:56]
    Queue.clock <= clock
    Queue.reset <= reset
    Queue.io_enq_valid <= io_dataStream_iactIOs_addrIOs_writeInDataIO_valid @[Decoupled.scala 288:22]
    Queue.io_enq_bits_data <= io_dataStream_iactIOs_addrIOs_writeInDataIO_bits_data @[Decoupled.scala 289:21]
    Queue.io_deq_ready <= pePad.io_dataStream_iactIOs_addrIOs_writeInDataIO_ready @[ProcessingElement.scala 14:55]
    Queue_1.clock <= clock
    Queue_1.reset <= reset
    Queue_1.io_enq_valid <= io_dataStream_iactIOs_dataIOs_writeInDataIO_valid @[Decoupled.scala 288:22]
    Queue_1.io_enq_bits_data <= io_dataStream_iactIOs_dataIOs_writeInDataIO_bits_data @[Decoupled.scala 289:21]
    Queue_1.io_deq_ready <= pePad.io_dataStream_iactIOs_dataIOs_writeInDataIO_ready @[ProcessingElement.scala 15:55]
    Queue_2.clock <= clock
    Queue_2.reset <= reset
    Queue_2.io_enq_valid <= io_dataStream_weightIOs_addrIOs_writeInDataIO_valid @[Decoupled.scala 288:22]
    Queue_2.io_enq_bits_data <= io_dataStream_weightIOs_addrIOs_writeInDataIO_bits_data @[Decoupled.scala 289:21]
    Queue_2.io_deq_ready <= pePad.io_dataStream_weightIOs_addrIOs_writeInDataIO_ready @[ProcessingElement.scala 16:57]
    Queue_3.clock <= clock
    Queue_3.reset <= reset
    Queue_3.io_enq_valid <= io_dataStream_weightIOs_dataIOs_writeInDataIO_valid @[Decoupled.scala 288:22]
    Queue_3.io_enq_bits_data <= io_dataStream_weightIOs_dataIOs_writeInDataIO_bits_data @[Decoupled.scala 289:21]
    Queue_3.io_deq_ready <= pePad.io_dataStream_weightIOs_dataIOs_writeInDataIO_ready @[ProcessingElement.scala 17:57]
    Queue_4.clock <= clock
    Queue_4.reset <= reset
    Queue_4.io_enq_valid <= io_dataStream_ipsIO_valid @[Decoupled.scala 288:22]
    Queue_4.io_enq_bits <= io_dataStream_ipsIO_bits @[Decoupled.scala 289:21]
    Queue_4.io_deq_ready <= pePad.io_dataStream_ipsIO_ready @[ProcessingElement.scala 31:29]
    Queue_5.clock <= clock
    Queue_5.reset <= reset
    Queue_5.io_enq_valid <= pePad.io_dataStream_opsIO_valid @[Decoupled.scala 288:22]
    Queue_5.io_enq_bits <= pePad.io_dataStream_opsIO_bits @[Decoupled.scala 289:21]
    Queue_5.io_deq_ready <= io_dataStream_opsIO_ready @[ProcessingElement.scala 32:23]
