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
    output io_ctrlTop_calFinish : UInt<1>
    input io_ctrlTop_writeFinish : UInt<1>
    output io_debugIO_peState : UInt<2>
    output io_debugIO_doMACEnDebug : UInt<1>
  
    reg _T : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T) @[ProcessingElement.scala 60:39]
    node _T_1 = eq(_T, UInt<2>("h2")) @[ProcessingElement.scala 61:53]
    node _T_2 = mux(_T_1, io_ctrlPad_fromTopIO_pSumEnqOrProduct_ready, UInt<1>("h0")) @[ProcessingElement.scala 61:43]
    node _T_3 = eq(_T, UInt<2>("h2")) @[ProcessingElement.scala 62:63]
    node _T_4 = mux(_T_3, io_ctrlTop_pSumEnqOrProduct_valid, UInt<1>("h0")) @[ProcessingElement.scala 62:53]
    node _T_5 = eq(_T, UInt<2>("h2")) @[ProcessingElement.scala 65:34]
    node _T_6 = eq(UInt<2>("h0"), _T) @[Conditional.scala 37:30]
    node _GEN_0 = mux(io_ctrlTop_doLoadEn, UInt<2>("h1"), _T) @[ProcessingElement.scala 68:34]
    node _T_7 = eq(UInt<2>("h1"), _T) @[Conditional.scala 37:30]
    node _GEN_1 = mux(io_ctrlTop_writeFinish, UInt<2>("h2"), _T) @[ProcessingElement.scala 73:37]
    node _T_8 = eq(UInt<2>("h2"), _T) @[Conditional.scala 37:30]
    node _GEN_2 = mux(io_ctrlPad_fromTopIO_calFinish, UInt<2>("h0"), _T) @[ProcessingElement.scala 78:45]
    node _GEN_3 = mux(_T_8, _GEN_2, _T) @[Conditional.scala 39:67]
    node _GEN_4 = mux(_T_7, _GEN_1, _GEN_3) @[Conditional.scala 39:67]
    node _GEN_5 = mux(_T_6, _GEN_0, _GEN_4) @[Conditional.scala 40:58]
    io_ctrlPad_doMACEn <= _T_5 @[ProcessingElement.scala 65:22]
    io_ctrlPad_fromTopIO_pSumEnqOrProduct_valid <= _T_4 @[ProcessingElement.scala 62:47]
    io_ctrlPad_fromTopIO_pSumEnqOrProduct_bits <= io_ctrlTop_pSumEnqOrProduct_bits @[ProcessingElement.scala 63:46]
    io_ctrlPad_fromTopIO_doLoadEn <= io_ctrlTop_doLoadEn @[ProcessingElement.scala 64:33]
    io_ctrlTop_pSumEnqOrProduct_ready <= _T_2 @[ProcessingElement.scala 61:37]
    io_ctrlTop_calFinish <= io_ctrlPad_fromTopIO_calFinish @[ProcessingElement.scala 52:24]
    io_debugIO_peState <= _T @[ProcessingElement.scala 84:24]
    io_debugIO_doMACEnDebug <= io_ctrlPad_doMACEn @[ProcessingElement.scala 85:29]
    _T <= mux(reset, UInt<2>("h0"), _GEN_5) @[ProcessingElement.scala 69:18 ProcessingElement.scala 74:18 ProcessingElement.scala 79:18]

  module SPadAdrModule :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_columnNum : UInt<4>
    output io_dataPath_readOutData : UInt<4>
    output io_dataPath_writeInData_data_ready : UInt<1>
    input io_dataPath_writeInData_data_valid : UInt<1>
    input io_dataPath_writeInData_data_bits : UInt<4>
    input io_ctrlPath_writeEn : UInt<1>
    output io_ctrlPath_writeIdx : UInt<4>
    output io_ctrlPath_writeFin : UInt<1>
    input io_ctrlPath_readEn : UInt<1>
    input io_ctrlPath_readInIdx : UInt<4>
    input io_ctrlPath_indexInc : UInt<1>
    input io_ctrlPath_readInIdxEn : UInt<1>
  
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 76:49]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 77:48]
    node _T = eq(io_dataPath_writeInData_data_bits, UInt<1>("h0")) @[SPadModule.scala 81:41]
    node _T_1 = and(_T, io_ctrlPath_writeEn) @[SPadModule.scala 81:49]
    reg _T_8_8 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_8) @[SPadModule.scala 14:43]
    reg _T_8_7 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_7) @[SPadModule.scala 14:43]
    reg _T_8_6 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_6) @[SPadModule.scala 14:43]
    reg _T_8_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_5) @[SPadModule.scala 14:43]
    reg _T_8_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_4) @[SPadModule.scala 14:43]
    reg _T_8_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_3) @[SPadModule.scala 14:43]
    reg _T_8_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_2) @[SPadModule.scala 14:43]
    reg _T_8_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_1) @[SPadModule.scala 14:43]
    reg _T_8_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_0) @[SPadModule.scala 14:43]
    node _GEN_23 = validif(eq(UInt<1>("h0"), padReadIndexReg), _T_8_0) @[SPadModule.scala 27:12]
    node _GEN_24 = mux(eq(UInt<1>("h1"), padReadIndexReg), _T_8_1, _GEN_23) @[SPadModule.scala 27:12]
    node _GEN_25 = mux(eq(UInt<2>("h2"), padReadIndexReg), _T_8_2, _GEN_24) @[SPadModule.scala 27:12]
    node _GEN_26 = mux(eq(UInt<2>("h3"), padReadIndexReg), _T_8_3, _GEN_25) @[SPadModule.scala 27:12]
    node _GEN_27 = mux(eq(UInt<3>("h4"), padReadIndexReg), _T_8_4, _GEN_26) @[SPadModule.scala 27:12]
    node _GEN_28 = mux(eq(UInt<3>("h5"), padReadIndexReg), _T_8_5, _GEN_27) @[SPadModule.scala 27:12]
    node _GEN_29 = mux(eq(UInt<3>("h6"), padReadIndexReg), _T_8_6, _GEN_28) @[SPadModule.scala 27:12]
    node _GEN_30 = mux(eq(UInt<3>("h7"), padReadIndexReg), _T_8_7, _GEN_29) @[SPadModule.scala 27:12]
    node _GEN_31 = mux(eq(UInt<4>("h8"), padReadIndexReg), _T_8_8, _GEN_30) @[SPadModule.scala 27:12]
    node _T_8_padReadIndexReg = _GEN_31 @[SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12]
    node dataWire = _T_8_padReadIndexReg @[SPadModule.scala 75:38 SPadModule.scala 27:12]
    node _T_2 = eq(dataWire, UInt<1>("h0")) @[SPadModule.scala 82:28]
    node readIndexInc = io_ctrlPath_indexInc @[SPadModule.scala 80:42 SPadModule.scala 29:16]
    node _T_3 = and(_T_2, readIndexInc) @[SPadModule.scala 82:36]
    node _T_4 = and(io_dataPath_writeInData_data_valid, io_ctrlPath_writeEn) @[SPadModule.scala 84:31]
    node _T_5 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 86:42]
    node _T_6 = tail(_T_5, 1) @[SPadModule.scala 86:42]
    node writeWrapWire = _T_1 @[SPadModule.scala 78:43 SPadModule.scala 81:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_6) @[SPadModule.scala 87:26]
    node _GEN_1 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 84:55]
    node _GEN_2 = mux(_T_4, _GEN_0, padWriteIndexReg) @[SPadModule.scala 84:55]
    node _T_9 = and(io_dataPath_writeInData_data_valid, io_ctrlPath_writeEn) @[SPadModule.scala 16:31]
    node _T_8_padWriteIndexReg = io_dataPath_writeInData_data_bits @[SPadModule.scala 17:31 SPadModule.scala 17:31]
    node _GEN_3 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_0) @[SPadModule.scala 17:31]
    node _GEN_4 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_1) @[SPadModule.scala 17:31]
    node _GEN_5 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_2) @[SPadModule.scala 17:31]
    node _GEN_6 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_3) @[SPadModule.scala 17:31]
    node _GEN_7 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_4) @[SPadModule.scala 17:31]
    node _GEN_8 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_5) @[SPadModule.scala 17:31]
    node _GEN_9 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_6) @[SPadModule.scala 17:31]
    node _GEN_10 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_7) @[SPadModule.scala 17:31]
    node _GEN_11 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_8) @[SPadModule.scala 17:31]
    node _GEN_12 = mux(_T_9, _GEN_3, _T_8_0) @[SPadModule.scala 16:55]
    node _GEN_13 = mux(_T_9, _GEN_4, _T_8_1) @[SPadModule.scala 16:55]
    node _GEN_14 = mux(_T_9, _GEN_5, _T_8_2) @[SPadModule.scala 16:55]
    node _GEN_15 = mux(_T_9, _GEN_6, _T_8_3) @[SPadModule.scala 16:55]
    node _GEN_16 = mux(_T_9, _GEN_7, _T_8_4) @[SPadModule.scala 16:55]
    node _GEN_17 = mux(_T_9, _GEN_8, _T_8_5) @[SPadModule.scala 16:55]
    node _GEN_18 = mux(_T_9, _GEN_9, _T_8_6) @[SPadModule.scala 16:55]
    node _GEN_19 = mux(_T_9, _GEN_10, _T_8_7) @[SPadModule.scala 16:55]
    node _GEN_20 = mux(_T_9, _GEN_11, _T_8_8) @[SPadModule.scala 16:55]
    node _T_10 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 21:40]
    node _T_11 = tail(_T_10, 1) @[SPadModule.scala 21:40]
    node readWrapWire = _T_3 @[SPadModule.scala 79:42 SPadModule.scala 82:16]
    node _GEN_21 = mux(readWrapWire, UInt<1>("h0"), _T_11) @[SPadModule.scala 22:25]
    node _GEN_22 = mux(readIndexInc, _GEN_21, padReadIndexReg) @[SPadModule.scala 20:23]
    node _T_7_0 = UInt<4>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_1 = UInt<4>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_2 = UInt<4>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_3 = UInt<4>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_4 = UInt<4>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_5 = UInt<4>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_6 = UInt<4>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_7 = UInt<4>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_8 = UInt<4>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    io_dataPath_columnNum <= padReadIndexReg @[SPadModule.scala 96:25]
    io_dataPath_readOutData <= dataWire @[SPadModule.scala 28:27]
    io_dataPath_writeInData_data_ready <= _GEN_1 @[SPadModule.scala 85:27 SPadModule.scala 91:27]
    io_ctrlPath_writeIdx <= padWriteIndexReg @[SPadModule.scala 94:24]
    io_ctrlPath_writeFin <= writeWrapWire @[SPadModule.scala 95:24]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[SPadModule.scala 86:22 SPadModule.scala 88:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_22) @[SPadModule.scala 21:21 SPadModule.scala 23:23]
    _T_8_0 <= mux(reset, _T_7_0, _GEN_12) @[SPadModule.scala 17:31]
    _T_8_1 <= mux(reset, _T_7_1, _GEN_13) @[SPadModule.scala 17:31]
    _T_8_2 <= mux(reset, _T_7_2, _GEN_14) @[SPadModule.scala 17:31]
    _T_8_3 <= mux(reset, _T_7_3, _GEN_15) @[SPadModule.scala 17:31]
    _T_8_4 <= mux(reset, _T_7_4, _GEN_16) @[SPadModule.scala 17:31]
    _T_8_5 <= mux(reset, _T_7_5, _GEN_17) @[SPadModule.scala 17:31]
    _T_8_6 <= mux(reset, _T_7_6, _GEN_18) @[SPadModule.scala 17:31]
    _T_8_7 <= mux(reset, _T_7_7, _GEN_19) @[SPadModule.scala 17:31]
    _T_8_8 <= mux(reset, _T_7_8, _GEN_20) @[SPadModule.scala 17:31]

  module SPadDataModule :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_columnNum : UInt<4>
    output io_dataPath_readOutData : UInt<12>
    output io_dataPath_writeInData_data_ready : UInt<1>
    input io_dataPath_writeInData_data_valid : UInt<1>
    input io_dataPath_writeInData_data_bits : UInt<12>
    input io_ctrlPath_writeEn : UInt<1>
    output io_ctrlPath_writeIdx : UInt<4>
    output io_ctrlPath_writeFin : UInt<1>
    input io_ctrlPath_readEn : UInt<1>
    input io_ctrlPath_readInIdx : UInt<4>
    input io_ctrlPath_indexInc : UInt<1>
    input io_ctrlPath_readInIdxEn : UInt<1>
  
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 76:49]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 77:48]
    node _T = eq(io_dataPath_writeInData_data_bits, UInt<1>("h0")) @[SPadModule.scala 81:41]
    node _T_1 = and(_T, io_ctrlPath_writeEn) @[SPadModule.scala 81:49]
    reg _T_8_15 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_15) @[SPadModule.scala 54:38]
    reg _T_8_14 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_14) @[SPadModule.scala 54:38]
    reg _T_8_13 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_13) @[SPadModule.scala 54:38]
    reg _T_8_12 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_12) @[SPadModule.scala 54:38]
    reg _T_8_11 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_11) @[SPadModule.scala 54:38]
    reg _T_8_10 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_10) @[SPadModule.scala 54:38]
    reg _T_8_9 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_9) @[SPadModule.scala 54:38]
    reg _T_8_8 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_8) @[SPadModule.scala 54:38]
    reg _T_8_7 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_7) @[SPadModule.scala 54:38]
    reg _T_8_6 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_6) @[SPadModule.scala 54:38]
    reg _T_8_5 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_5) @[SPadModule.scala 54:38]
    reg _T_8_4 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_4) @[SPadModule.scala 54:38]
    reg _T_8_3 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_3) @[SPadModule.scala 54:38]
    reg _T_8_2 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_2) @[SPadModule.scala 54:38]
    reg _T_8_1 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_1) @[SPadModule.scala 54:38]
    reg _T_8_0 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_8_0) @[SPadModule.scala 54:38]
    node _GEN_37 = validif(eq(UInt<1>("h0"), padReadIndexReg), _T_8_0) @[SPadModule.scala 66:14]
    node _GEN_38 = mux(eq(UInt<1>("h1"), padReadIndexReg), _T_8_1, _GEN_37) @[SPadModule.scala 66:14]
    node _GEN_39 = mux(eq(UInt<2>("h2"), padReadIndexReg), _T_8_2, _GEN_38) @[SPadModule.scala 66:14]
    node _GEN_40 = mux(eq(UInt<2>("h3"), padReadIndexReg), _T_8_3, _GEN_39) @[SPadModule.scala 66:14]
    node _GEN_41 = mux(eq(UInt<3>("h4"), padReadIndexReg), _T_8_4, _GEN_40) @[SPadModule.scala 66:14]
    node _GEN_42 = mux(eq(UInt<3>("h5"), padReadIndexReg), _T_8_5, _GEN_41) @[SPadModule.scala 66:14]
    node _GEN_43 = mux(eq(UInt<3>("h6"), padReadIndexReg), _T_8_6, _GEN_42) @[SPadModule.scala 66:14]
    node _GEN_44 = mux(eq(UInt<3>("h7"), padReadIndexReg), _T_8_7, _GEN_43) @[SPadModule.scala 66:14]
    node _GEN_45 = mux(eq(UInt<4>("h8"), padReadIndexReg), _T_8_8, _GEN_44) @[SPadModule.scala 66:14]
    node _GEN_46 = mux(eq(UInt<4>("h9"), padReadIndexReg), _T_8_9, _GEN_45) @[SPadModule.scala 66:14]
    node _GEN_47 = mux(eq(UInt<4>("ha"), padReadIndexReg), _T_8_10, _GEN_46) @[SPadModule.scala 66:14]
    node _GEN_48 = mux(eq(UInt<4>("hb"), padReadIndexReg), _T_8_11, _GEN_47) @[SPadModule.scala 66:14]
    node _GEN_49 = mux(eq(UInt<4>("hc"), padReadIndexReg), _T_8_12, _GEN_48) @[SPadModule.scala 66:14]
    node _GEN_50 = mux(eq(UInt<4>("hd"), padReadIndexReg), _T_8_13, _GEN_49) @[SPadModule.scala 66:14]
    node _GEN_51 = mux(eq(UInt<4>("he"), padReadIndexReg), _T_8_14, _GEN_50) @[SPadModule.scala 66:14]
    node _GEN_52 = mux(eq(UInt<4>("hf"), padReadIndexReg), _T_8_15, _GEN_51) @[SPadModule.scala 66:14]
    node _T_8_padReadIndexReg = _GEN_52 @[SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14 SPadModule.scala 66:14]
    node dataWire = _T_8_padReadIndexReg @[SPadModule.scala 75:38 SPadModule.scala 66:14]
    node _T_2 = eq(dataWire, UInt<1>("h0")) @[SPadModule.scala 82:28]
    node readIndexInc = io_ctrlPath_indexInc @[SPadModule.scala 80:42 SPadModule.scala 68:16]
    node _T_3 = and(_T_2, readIndexInc) @[SPadModule.scala 82:36]
    node _T_4 = and(io_dataPath_writeInData_data_valid, io_ctrlPath_writeEn) @[SPadModule.scala 84:31]
    node _T_5 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 86:42]
    node _T_6 = tail(_T_5, 1) @[SPadModule.scala 86:42]
    node writeWrapWire = _T_1 @[SPadModule.scala 78:43 SPadModule.scala 81:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_6) @[SPadModule.scala 87:26]
    node _GEN_1 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 84:55]
    node _GEN_2 = mux(_T_4, _GEN_0, padWriteIndexReg) @[SPadModule.scala 84:55]
    node _T_9 = and(io_dataPath_writeInData_data_valid, io_ctrlPath_writeEn) @[SPadModule.scala 55:33]
    node _T_8_padWriteIndexReg = io_dataPath_writeInData_data_bits @[SPadModule.scala 56:34 SPadModule.scala 56:34]
    node _GEN_3 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_0) @[SPadModule.scala 56:34]
    node _GEN_4 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_1) @[SPadModule.scala 56:34]
    node _GEN_5 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_2) @[SPadModule.scala 56:34]
    node _GEN_6 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_3) @[SPadModule.scala 56:34]
    node _GEN_7 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_4) @[SPadModule.scala 56:34]
    node _GEN_8 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_5) @[SPadModule.scala 56:34]
    node _GEN_9 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_6) @[SPadModule.scala 56:34]
    node _GEN_10 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_7) @[SPadModule.scala 56:34]
    node _GEN_11 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_8) @[SPadModule.scala 56:34]
    node _GEN_12 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_9) @[SPadModule.scala 56:34]
    node _GEN_13 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_10) @[SPadModule.scala 56:34]
    node _GEN_14 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_11) @[SPadModule.scala 56:34]
    node _GEN_15 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_12) @[SPadModule.scala 56:34]
    node _GEN_16 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_13) @[SPadModule.scala 56:34]
    node _GEN_17 = mux(eq(UInt<4>("he"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_14) @[SPadModule.scala 56:34]
    node _GEN_18 = mux(eq(UInt<4>("hf"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_15) @[SPadModule.scala 56:34]
    node _GEN_19 = mux(_T_9, _GEN_3, _T_8_0) @[SPadModule.scala 55:57]
    node _GEN_20 = mux(_T_9, _GEN_4, _T_8_1) @[SPadModule.scala 55:57]
    node _GEN_21 = mux(_T_9, _GEN_5, _T_8_2) @[SPadModule.scala 55:57]
    node _GEN_22 = mux(_T_9, _GEN_6, _T_8_3) @[SPadModule.scala 55:57]
    node _GEN_23 = mux(_T_9, _GEN_7, _T_8_4) @[SPadModule.scala 55:57]
    node _GEN_24 = mux(_T_9, _GEN_8, _T_8_5) @[SPadModule.scala 55:57]
    node _GEN_25 = mux(_T_9, _GEN_9, _T_8_6) @[SPadModule.scala 55:57]
    node _GEN_26 = mux(_T_9, _GEN_10, _T_8_7) @[SPadModule.scala 55:57]
    node _GEN_27 = mux(_T_9, _GEN_11, _T_8_8) @[SPadModule.scala 55:57]
    node _GEN_28 = mux(_T_9, _GEN_12, _T_8_9) @[SPadModule.scala 55:57]
    node _GEN_29 = mux(_T_9, _GEN_13, _T_8_10) @[SPadModule.scala 55:57]
    node _GEN_30 = mux(_T_9, _GEN_14, _T_8_11) @[SPadModule.scala 55:57]
    node _GEN_31 = mux(_T_9, _GEN_15, _T_8_12) @[SPadModule.scala 55:57]
    node _GEN_32 = mux(_T_9, _GEN_16, _T_8_13) @[SPadModule.scala 55:57]
    node _GEN_33 = mux(_T_9, _GEN_17, _T_8_14) @[SPadModule.scala 55:57]
    node _GEN_34 = mux(_T_9, _GEN_18, _T_8_15) @[SPadModule.scala 55:57]
    node _T_10 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 60:42]
    node _T_11 = tail(_T_10, 1) @[SPadModule.scala 60:42]
    node readWrapWire = _T_3 @[SPadModule.scala 79:42 SPadModule.scala 82:16]
    node _GEN_35 = mux(readWrapWire, UInt<1>("h0"), _T_11) @[SPadModule.scala 61:27]
    node _GEN_36 = mux(readIndexInc, _GEN_35, padReadIndexReg) @[SPadModule.scala 59:25]
    node _T_7_0 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_1 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_2 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_3 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_4 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_5 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_6 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_7 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_8 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_9 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_10 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_11 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_12 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_13 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_14 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    node _T_7_15 = UInt<12>("h0") @[SPadModule.scala 54:46 SPadModule.scala 54:46]
    io_dataPath_columnNum <= padReadIndexReg @[SPadModule.scala 96:25]
    io_dataPath_readOutData <= dataWire @[SPadModule.scala 69:27]
    io_dataPath_writeInData_data_ready <= _GEN_1 @[SPadModule.scala 85:27 SPadModule.scala 91:27]
    io_ctrlPath_writeIdx <= padWriteIndexReg @[SPadModule.scala 94:24]
    io_ctrlPath_writeFin <= writeWrapWire @[SPadModule.scala 95:24]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[SPadModule.scala 86:22 SPadModule.scala 88:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_36) @[SPadModule.scala 60:23 SPadModule.scala 62:25]
    _T_8_0 <= mux(reset, _T_7_0, _GEN_19) @[SPadModule.scala 56:34]
    _T_8_1 <= mux(reset, _T_7_1, _GEN_20) @[SPadModule.scala 56:34]
    _T_8_2 <= mux(reset, _T_7_2, _GEN_21) @[SPadModule.scala 56:34]
    _T_8_3 <= mux(reset, _T_7_3, _GEN_22) @[SPadModule.scala 56:34]
    _T_8_4 <= mux(reset, _T_7_4, _GEN_23) @[SPadModule.scala 56:34]
    _T_8_5 <= mux(reset, _T_7_5, _GEN_24) @[SPadModule.scala 56:34]
    _T_8_6 <= mux(reset, _T_7_6, _GEN_25) @[SPadModule.scala 56:34]
    _T_8_7 <= mux(reset, _T_7_7, _GEN_26) @[SPadModule.scala 56:34]
    _T_8_8 <= mux(reset, _T_7_8, _GEN_27) @[SPadModule.scala 56:34]
    _T_8_9 <= mux(reset, _T_7_9, _GEN_28) @[SPadModule.scala 56:34]
    _T_8_10 <= mux(reset, _T_7_10, _GEN_29) @[SPadModule.scala 56:34]
    _T_8_11 <= mux(reset, _T_7_11, _GEN_30) @[SPadModule.scala 56:34]
    _T_8_12 <= mux(reset, _T_7_12, _GEN_31) @[SPadModule.scala 56:34]
    _T_8_13 <= mux(reset, _T_7_13, _GEN_32) @[SPadModule.scala 56:34]
    _T_8_14 <= mux(reset, _T_7_14, _GEN_33) @[SPadModule.scala 56:34]
    _T_8_15 <= mux(reset, _T_7_15, _GEN_34) @[SPadModule.scala 56:34]

  module WeightSPadAdrModule :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_columnNum : UInt<4>
    output io_dataPath_readOutData : UInt<7>
    output io_dataPath_writeInData_data_ready : UInt<1>
    input io_dataPath_writeInData_data_valid : UInt<1>
    input io_dataPath_writeInData_data_bits : UInt<7>
    input io_ctrlPath_writeEn : UInt<1>
    output io_ctrlPath_writeIdx : UInt<4>
    output io_ctrlPath_writeFin : UInt<1>
    input io_ctrlPath_readEn : UInt<1>
    input io_ctrlPath_readInIdx : UInt<4>
    input io_ctrlPath_indexInc : UInt<1>
    input io_ctrlPath_readInIdxEn : UInt<1>
  
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 76:49]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 77:48]
    node _T = eq(io_dataPath_writeInData_data_bits, UInt<1>("h0")) @[SPadModule.scala 81:41]
    node _T_1 = and(_T, io_ctrlPath_writeEn) @[SPadModule.scala 81:49]
    reg _T_8_15 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_15) @[SPadModule.scala 14:43]
    reg _T_8_14 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_14) @[SPadModule.scala 14:43]
    reg _T_8_13 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_13) @[SPadModule.scala 14:43]
    reg _T_8_12 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_12) @[SPadModule.scala 14:43]
    reg _T_8_11 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_11) @[SPadModule.scala 14:43]
    reg _T_8_10 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_10) @[SPadModule.scala 14:43]
    reg _T_8_9 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_9) @[SPadModule.scala 14:43]
    reg _T_8_8 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_8) @[SPadModule.scala 14:43]
    reg _T_8_7 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_7) @[SPadModule.scala 14:43]
    reg _T_8_6 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_6) @[SPadModule.scala 14:43]
    reg _T_8_5 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_5) @[SPadModule.scala 14:43]
    reg _T_8_4 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_4) @[SPadModule.scala 14:43]
    reg _T_8_3 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_3) @[SPadModule.scala 14:43]
    reg _T_8_2 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_2) @[SPadModule.scala 14:43]
    reg _T_8_1 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_1) @[SPadModule.scala 14:43]
    reg _T_8_0 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_8_0) @[SPadModule.scala 14:43]
    node _GEN_37 = validif(eq(UInt<1>("h0"), padReadIndexReg), _T_8_0) @[SPadModule.scala 27:12]
    node _GEN_38 = mux(eq(UInt<1>("h1"), padReadIndexReg), _T_8_1, _GEN_37) @[SPadModule.scala 27:12]
    node _GEN_39 = mux(eq(UInt<2>("h2"), padReadIndexReg), _T_8_2, _GEN_38) @[SPadModule.scala 27:12]
    node _GEN_40 = mux(eq(UInt<2>("h3"), padReadIndexReg), _T_8_3, _GEN_39) @[SPadModule.scala 27:12]
    node _GEN_41 = mux(eq(UInt<3>("h4"), padReadIndexReg), _T_8_4, _GEN_40) @[SPadModule.scala 27:12]
    node _GEN_42 = mux(eq(UInt<3>("h5"), padReadIndexReg), _T_8_5, _GEN_41) @[SPadModule.scala 27:12]
    node _GEN_43 = mux(eq(UInt<3>("h6"), padReadIndexReg), _T_8_6, _GEN_42) @[SPadModule.scala 27:12]
    node _GEN_44 = mux(eq(UInt<3>("h7"), padReadIndexReg), _T_8_7, _GEN_43) @[SPadModule.scala 27:12]
    node _GEN_45 = mux(eq(UInt<4>("h8"), padReadIndexReg), _T_8_8, _GEN_44) @[SPadModule.scala 27:12]
    node _GEN_46 = mux(eq(UInt<4>("h9"), padReadIndexReg), _T_8_9, _GEN_45) @[SPadModule.scala 27:12]
    node _GEN_47 = mux(eq(UInt<4>("ha"), padReadIndexReg), _T_8_10, _GEN_46) @[SPadModule.scala 27:12]
    node _GEN_48 = mux(eq(UInt<4>("hb"), padReadIndexReg), _T_8_11, _GEN_47) @[SPadModule.scala 27:12]
    node _GEN_49 = mux(eq(UInt<4>("hc"), padReadIndexReg), _T_8_12, _GEN_48) @[SPadModule.scala 27:12]
    node _GEN_50 = mux(eq(UInt<4>("hd"), padReadIndexReg), _T_8_13, _GEN_49) @[SPadModule.scala 27:12]
    node _GEN_51 = mux(eq(UInt<4>("he"), padReadIndexReg), _T_8_14, _GEN_50) @[SPadModule.scala 27:12]
    node _GEN_52 = mux(eq(UInt<4>("hf"), padReadIndexReg), _T_8_15, _GEN_51) @[SPadModule.scala 27:12]
    node _T_8_padReadIndexReg = _GEN_52 @[SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12 SPadModule.scala 27:12]
    node dataWire = _T_8_padReadIndexReg @[SPadModule.scala 75:38 SPadModule.scala 27:12]
    node _T_2 = eq(dataWire, UInt<1>("h0")) @[SPadModule.scala 82:28]
    node readIndexInc = io_ctrlPath_indexInc @[SPadModule.scala 80:42 SPadModule.scala 29:16]
    node _T_3 = and(_T_2, readIndexInc) @[SPadModule.scala 82:36]
    node _T_4 = and(io_dataPath_writeInData_data_valid, io_ctrlPath_writeEn) @[SPadModule.scala 84:31]
    node _T_5 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 86:42]
    node _T_6 = tail(_T_5, 1) @[SPadModule.scala 86:42]
    node writeWrapWire = _T_1 @[SPadModule.scala 78:43 SPadModule.scala 81:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_6) @[SPadModule.scala 87:26]
    node _GEN_1 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 84:55]
    node _GEN_2 = mux(_T_4, _GEN_0, padWriteIndexReg) @[SPadModule.scala 84:55]
    node _T_9 = and(io_dataPath_writeInData_data_valid, io_ctrlPath_writeEn) @[SPadModule.scala 16:31]
    node _T_8_padWriteIndexReg = io_dataPath_writeInData_data_bits @[SPadModule.scala 17:31 SPadModule.scala 17:31]
    node _GEN_3 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_0) @[SPadModule.scala 17:31]
    node _GEN_4 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_1) @[SPadModule.scala 17:31]
    node _GEN_5 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_2) @[SPadModule.scala 17:31]
    node _GEN_6 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_3) @[SPadModule.scala 17:31]
    node _GEN_7 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_4) @[SPadModule.scala 17:31]
    node _GEN_8 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_5) @[SPadModule.scala 17:31]
    node _GEN_9 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_6) @[SPadModule.scala 17:31]
    node _GEN_10 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_7) @[SPadModule.scala 17:31]
    node _GEN_11 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_8) @[SPadModule.scala 17:31]
    node _GEN_12 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_9) @[SPadModule.scala 17:31]
    node _GEN_13 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_10) @[SPadModule.scala 17:31]
    node _GEN_14 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_11) @[SPadModule.scala 17:31]
    node _GEN_15 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_12) @[SPadModule.scala 17:31]
    node _GEN_16 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_13) @[SPadModule.scala 17:31]
    node _GEN_17 = mux(eq(UInt<4>("he"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_14) @[SPadModule.scala 17:31]
    node _GEN_18 = mux(eq(UInt<4>("hf"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_15) @[SPadModule.scala 17:31]
    node _GEN_19 = mux(_T_9, _GEN_3, _T_8_0) @[SPadModule.scala 16:55]
    node _GEN_20 = mux(_T_9, _GEN_4, _T_8_1) @[SPadModule.scala 16:55]
    node _GEN_21 = mux(_T_9, _GEN_5, _T_8_2) @[SPadModule.scala 16:55]
    node _GEN_22 = mux(_T_9, _GEN_6, _T_8_3) @[SPadModule.scala 16:55]
    node _GEN_23 = mux(_T_9, _GEN_7, _T_8_4) @[SPadModule.scala 16:55]
    node _GEN_24 = mux(_T_9, _GEN_8, _T_8_5) @[SPadModule.scala 16:55]
    node _GEN_25 = mux(_T_9, _GEN_9, _T_8_6) @[SPadModule.scala 16:55]
    node _GEN_26 = mux(_T_9, _GEN_10, _T_8_7) @[SPadModule.scala 16:55]
    node _GEN_27 = mux(_T_9, _GEN_11, _T_8_8) @[SPadModule.scala 16:55]
    node _GEN_28 = mux(_T_9, _GEN_12, _T_8_9) @[SPadModule.scala 16:55]
    node _GEN_29 = mux(_T_9, _GEN_13, _T_8_10) @[SPadModule.scala 16:55]
    node _GEN_30 = mux(_T_9, _GEN_14, _T_8_11) @[SPadModule.scala 16:55]
    node _GEN_31 = mux(_T_9, _GEN_15, _T_8_12) @[SPadModule.scala 16:55]
    node _GEN_32 = mux(_T_9, _GEN_16, _T_8_13) @[SPadModule.scala 16:55]
    node _GEN_33 = mux(_T_9, _GEN_17, _T_8_14) @[SPadModule.scala 16:55]
    node _GEN_34 = mux(_T_9, _GEN_18, _T_8_15) @[SPadModule.scala 16:55]
    node _T_10 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 21:40]
    node _T_11 = tail(_T_10, 1) @[SPadModule.scala 21:40]
    node readWrapWire = _T_3 @[SPadModule.scala 79:42 SPadModule.scala 82:16]
    node _GEN_35 = mux(readWrapWire, UInt<1>("h0"), _T_11) @[SPadModule.scala 22:25]
    node _GEN_36 = mux(readIndexInc, _GEN_35, padReadIndexReg) @[SPadModule.scala 20:23]
    node _GEN_53 = mux(io_ctrlPath_readInIdxEn, io_ctrlPath_readInIdx, _GEN_36) @[SPadModule.scala 7:34]
    node _T_7_0 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_1 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_2 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_3 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_4 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_5 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_6 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_7 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_8 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_9 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_10 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_11 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_12 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_13 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_14 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    node _T_7_15 = UInt<7>("h0") @[SPadModule.scala 14:51 SPadModule.scala 14:51]
    io_dataPath_columnNum <= padReadIndexReg @[SPadModule.scala 96:25]
    io_dataPath_readOutData <= dataWire @[SPadModule.scala 28:27]
    io_dataPath_writeInData_data_ready <= _GEN_1 @[SPadModule.scala 85:27 SPadModule.scala 91:27]
    io_ctrlPath_writeIdx <= padWriteIndexReg @[SPadModule.scala 94:24]
    io_ctrlPath_writeFin <= writeWrapWire @[SPadModule.scala 95:24]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[SPadModule.scala 86:22 SPadModule.scala 88:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_53) @[SPadModule.scala 21:21 SPadModule.scala 23:23 SPadModule.scala 8:21]
    _T_8_0 <= mux(reset, _T_7_0, _GEN_19) @[SPadModule.scala 17:31]
    _T_8_1 <= mux(reset, _T_7_1, _GEN_20) @[SPadModule.scala 17:31]
    _T_8_2 <= mux(reset, _T_7_2, _GEN_21) @[SPadModule.scala 17:31]
    _T_8_3 <= mux(reset, _T_7_3, _GEN_22) @[SPadModule.scala 17:31]
    _T_8_4 <= mux(reset, _T_7_4, _GEN_23) @[SPadModule.scala 17:31]
    _T_8_5 <= mux(reset, _T_7_5, _GEN_24) @[SPadModule.scala 17:31]
    _T_8_6 <= mux(reset, _T_7_6, _GEN_25) @[SPadModule.scala 17:31]
    _T_8_7 <= mux(reset, _T_7_7, _GEN_26) @[SPadModule.scala 17:31]
    _T_8_8 <= mux(reset, _T_7_8, _GEN_27) @[SPadModule.scala 17:31]
    _T_8_9 <= mux(reset, _T_7_9, _GEN_28) @[SPadModule.scala 17:31]
    _T_8_10 <= mux(reset, _T_7_10, _GEN_29) @[SPadModule.scala 17:31]
    _T_8_11 <= mux(reset, _T_7_11, _GEN_30) @[SPadModule.scala 17:31]
    _T_8_12 <= mux(reset, _T_7_12, _GEN_31) @[SPadModule.scala 17:31]
    _T_8_13 <= mux(reset, _T_7_13, _GEN_32) @[SPadModule.scala 17:31]
    _T_8_14 <= mux(reset, _T_7_14, _GEN_33) @[SPadModule.scala 17:31]
    _T_8_15 <= mux(reset, _T_7_15, _GEN_34) @[SPadModule.scala 17:31]

  module SPadDataModule_1 :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_columnNum : UInt<8>
    output io_dataPath_readOutData : UInt<12>
    output io_dataPath_writeInData_data_ready : UInt<1>
    input io_dataPath_writeInData_data_valid : UInt<1>
    input io_dataPath_writeInData_data_bits : UInt<12>
    input io_ctrlPath_writeEn : UInt<1>
    output io_ctrlPath_writeIdx : UInt<8>
    output io_ctrlPath_writeFin : UInt<1>
    input io_ctrlPath_readEn : UInt<1>
    input io_ctrlPath_readInIdx : UInt<8>
    input io_ctrlPath_indexInc : UInt<1>
    input io_ctrlPath_readInIdxEn : UInt<1>
  
    mem _T_7 : @[SPadModule.scala 35:50]
      data-type => UInt<12>
      depth => 192
      read-latency => 1
      write-latency => 1
      reader => _T_15
      writer => _T_9
      read-under-write => undefined
    reg padWriteIndexReg : UInt<8>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 76:49]
    reg padReadIndexReg : UInt<8>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 77:48]
    node _T = eq(io_dataPath_writeInData_data_bits, UInt<1>("h0")) @[SPadModule.scala 81:41]
    node _T_1 = and(_T, io_ctrlPath_writeEn) @[SPadModule.scala 81:49]
    node dataWire = _T_7._T_15.data @[SPadModule.scala 75:38 SPadModule.scala 52:14]
    node _T_2 = eq(dataWire, UInt<1>("h0")) @[SPadModule.scala 82:28]
    node readIndexInc = io_ctrlPath_indexInc @[SPadModule.scala 80:42 SPadModule.scala 68:16]
    node _T_3 = and(_T_2, readIndexInc) @[SPadModule.scala 82:36]
    node _T_4 = and(io_dataPath_writeInData_data_valid, io_ctrlPath_writeEn) @[SPadModule.scala 84:31]
    node _T_5 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 86:42]
    node _T_6 = tail(_T_5, 1) @[SPadModule.scala 86:42]
    node writeWrapWire = _T_1 @[SPadModule.scala 78:43 SPadModule.scala 81:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_6) @[SPadModule.scala 87:26]
    node _GEN_1 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 84:55]
    node _GEN_2 = mux(_T_4, _GEN_0, padWriteIndexReg) @[SPadModule.scala 84:55]
    node _T_8 = and(io_dataPath_writeInData_data_valid, io_ctrlPath_writeEn) @[SPadModule.scala 37:33]
    node _GEN_3 = validif(_T_8, padWriteIndexReg) @[SPadModule.scala 37:57]
    node _GEN_4 = validif(_T_8, clock) @[SPadModule.scala 37:57]
    node _GEN_5 = mux(_T_8, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 37:57]
    node _GEN_6 = validif(_T_8, UInt<1>("h1")) @[SPadModule.scala 37:57]
    node _GEN_7 = validif(_T_8, io_dataPath_writeInData_data_bits) @[SPadModule.scala 37:57]
    node _T_10 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 45:44]
    node _T_11 = tail(_T_10, 1) @[SPadModule.scala 45:44]
    node readWrapWire = _T_3 @[SPadModule.scala 79:42 SPadModule.scala 82:16]
    node _GEN_8 = mux(readWrapWire, UInt<1>("h0"), _T_11) @[SPadModule.scala 46:29]
    node _GEN_9 = mux(readIndexInc, _GEN_8, padReadIndexReg) @[SPadModule.scala 44:27]
    node _GEN_10 = mux(io_ctrlPath_readInIdxEn, io_ctrlPath_readInIdx, _GEN_9) @[SPadModule.scala 41:36]
    node _GEN_11 = validif(io_ctrlPath_readEn, padReadIndexReg) @[SPadModule.scala 52:30]
    node _T_12 = _GEN_11 @[SPadModule.scala 52:30 SPadModule.scala 52:30]
    node _T_13 = or(_T_12, UInt<8>("h0")) @[SPadModule.scala 52:30]
    node _T_14 = bits(_T_13, 7, 0) @[SPadModule.scala 52:30]
    node _GEN_12 = mux(io_ctrlPath_readEn, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 52:30]
    node _GEN_13 = validif(io_ctrlPath_readEn, _T_14) @[SPadModule.scala 52:30]
    node _GEN_14 = validif(io_ctrlPath_readEn, clock) @[SPadModule.scala 52:30]
    io_dataPath_columnNum <= padReadIndexReg @[SPadModule.scala 96:25]
    io_dataPath_readOutData <= dataWire @[SPadModule.scala 69:27]
    io_dataPath_writeInData_data_ready <= _GEN_1 @[SPadModule.scala 85:27 SPadModule.scala 91:27]
    io_ctrlPath_writeIdx <= padWriteIndexReg @[SPadModule.scala 94:24]
    io_ctrlPath_writeFin <= writeWrapWire @[SPadModule.scala 95:24]
    padWriteIndexReg <= mux(reset, UInt<8>("h0"), _GEN_2) @[SPadModule.scala 86:22 SPadModule.scala 88:24]
    padReadIndexReg <= mux(reset, UInt<8>("h0"), _GEN_10) @[SPadModule.scala 42:23 SPadModule.scala 45:25 SPadModule.scala 47:27]
    _T_7._T_15.addr <= _GEN_13 @[SPadModule.scala 52:30]
    _T_7._T_15.en <= _GEN_12 @[SPadModule.scala 35:50 SPadModule.scala 52:30]
    _T_7._T_15.clk <= _GEN_14 @[SPadModule.scala 52:30]
    _T_7._T_9.addr <= _GEN_3
    _T_7._T_9.en <= _GEN_5 @[SPadModule.scala 35:50]
    _T_7._T_9.clk <= _GEN_4
    _T_7._T_9.data <= _GEN_7
    _T_7._T_9.mask <= _GEN_6

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
    output io_dataStream_inActIOs_adrIOs_data_ready : UInt<1>
    input io_dataStream_inActIOs_adrIOs_data_valid : UInt<1>
    input io_dataStream_inActIOs_adrIOs_data_bits : UInt<4>
    output io_dataStream_inActIOs_dataIOs_data_ready : UInt<1>
    input io_dataStream_inActIOs_dataIOs_data_valid : UInt<1>
    input io_dataStream_inActIOs_dataIOs_data_bits : UInt<12>
    output io_dataStream_weightIOs_adrIOs_data_ready : UInt<1>
    input io_dataStream_weightIOs_adrIOs_data_valid : UInt<1>
    input io_dataStream_weightIOs_adrIOs_data_bits : UInt<7>
    output io_dataStream_weightIOs_dataIOs_data_ready : UInt<1>
    input io_dataStream_weightIOs_dataIOs_data_valid : UInt<1>
    input io_dataStream_weightIOs_dataIOs_data_bits : UInt<12>
    output io_debugIO_inActMatrixData : UInt<8>
    output io_debugIO_inActMatrixRow : UInt<4>
    output io_debugIO_inActMatrixColumn : UInt<4>
    output io_debugIO_inActAdrInc : UInt<1>
    output io_debugIO_inActDataInc : UInt<1>
    output io_debugIO_inActAdrIdx : UInt<4>
    output io_debugIO_weightAdrSPadReadOut : UInt<7>
    output io_debugIO_weightMatrixData : UInt<8>
    output io_debugIO_weightMatrixRow : UInt<4>
    output io_debugIO_productResult : UInt<20>
    output io_debugIO_pSumResult : UInt<20>
    output io_debugIO_pSumLoad : UInt<20>
    output io_debugIO_weightAdrInIdx : UInt<4>
    output io_debugIO_sPadState : UInt<3>
    output io_padWF_inActWriteFin_adrWriteFin : UInt<1>
    output io_padWF_inActWriteFin_dataWriteFin : UInt<1>
    output io_padWF_weightWriteFin_adrWriteFin : UInt<1>
    output io_padWF_weightWriteFin_dataWriteFin : UInt<1>
  
    inst SPadAdrModule of SPadAdrModule @[ProcessingElement.scala 135:51]
    inst SPadDataModule of SPadDataModule @[ProcessingElement.scala 136:53]
    inst WeightSPadAdrModule of WeightSPadAdrModule @[ProcessingElement.scala 137:52]
    inst SPadDataModule_1 of SPadDataModule_1 @[ProcessingElement.scala 138:54]
    reg _T_1_0 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_0) @[ProcessingElement.scala 134:46]
    reg _T_1_1 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_1) @[ProcessingElement.scala 134:46]
    reg _T_1_2 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_2) @[ProcessingElement.scala 134:46]
    reg _T_1_3 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_3) @[ProcessingElement.scala 134:46]
    reg _T_1_4 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_4) @[ProcessingElement.scala 134:46]
    reg _T_1_5 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_5) @[ProcessingElement.scala 134:46]
    reg _T_1_6 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_6) @[ProcessingElement.scala 134:46]
    reg _T_1_7 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_7) @[ProcessingElement.scala 134:46]
    reg _T_1_8 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_8) @[ProcessingElement.scala 134:46]
    reg _T_1_9 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_9) @[ProcessingElement.scala 134:46]
    reg _T_1_10 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_10) @[ProcessingElement.scala 134:46]
    reg _T_1_11 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_11) @[ProcessingElement.scala 134:46]
    reg _T_1_12 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_12) @[ProcessingElement.scala 134:46]
    reg _T_1_13 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_13) @[ProcessingElement.scala 134:46]
    reg _T_1_14 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_14) @[ProcessingElement.scala 134:46]
    reg _T_1_15 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_15) @[ProcessingElement.scala 134:46]
    reg _T_1_16 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_16) @[ProcessingElement.scala 134:46]
    reg _T_1_17 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_17) @[ProcessingElement.scala 134:46]
    reg _T_1_18 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_18) @[ProcessingElement.scala 134:46]
    reg _T_1_19 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_19) @[ProcessingElement.scala 134:46]
    reg _T_1_20 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_20) @[ProcessingElement.scala 134:46]
    reg _T_1_21 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_21) @[ProcessingElement.scala 134:46]
    reg _T_1_22 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_22) @[ProcessingElement.scala 134:46]
    reg _T_1_23 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_23) @[ProcessingElement.scala 134:46]
    reg _T_1_24 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_24) @[ProcessingElement.scala 134:46]
    reg _T_1_25 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_25) @[ProcessingElement.scala 134:46]
    reg _T_1_26 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_26) @[ProcessingElement.scala 134:46]
    reg _T_1_27 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_27) @[ProcessingElement.scala 134:46]
    reg _T_1_28 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_28) @[ProcessingElement.scala 134:46]
    reg _T_1_29 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_29) @[ProcessingElement.scala 134:46]
    reg _T_1_30 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_30) @[ProcessingElement.scala 134:46]
    reg _T_1_31 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_1_31) @[ProcessingElement.scala 134:46]
    reg _T_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_5) @[ProcessingElement.scala 143:52]
    reg _T_6 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_6) @[ProcessingElement.scala 144:53]
    reg _T_9 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_9) @[ProcessingElement.scala 147:51]
    reg _T_10 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_10) @[ProcessingElement.scala 148:52]
    reg _T_11 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_11) @[ProcessingElement.scala 149:56]
    reg _T_17 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_17) @[ProcessingElement.scala 156:53]
    reg _T_18 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_18) @[ProcessingElement.scala 157:54]
    reg _T_22 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_22) @[ProcessingElement.scala 162:54]
    reg _T_28 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_28) @[ProcessingElement.scala 169:41]
    reg _T_29 : UInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_29) @[ProcessingElement.scala 170:46]
    reg _T_31 : UInt<3>, clock with :
      reset => (UInt<1>("h0"), _T_31) @[ProcessingElement.scala 182:35]
    node _T_37 = eq(_T_31, UInt<3>("h1")) @[ProcessingElement.scala 188:19]
    node _T_38 = eq(_T_31, UInt<3>("h6")) @[ProcessingElement.scala 189:20]
    node _T_39 = eq(_T_31, UInt<3>("h7")) @[ProcessingElement.scala 190:19]
    node _T_40 = eq(_T_31, UInt<3>("h3")) @[ProcessingElement.scala 191:19]
    node _T_41 = eq(_T_31, UInt<3>("h2")) @[ProcessingElement.scala 192:19]
    node _T_47 = bits(SPadDataModule.io_dataPath_readOutData, 3, 3) @[ProcessingElement.scala 211:84]
    node _T_46 = bits(SPadDataModule.io_dataPath_readOutData, 2, 2) @[ProcessingElement.scala 211:84]
    node _T_64 = cat(_T_47, _T_46) @[Cat.scala 29:58]
    node _T_45 = bits(SPadDataModule.io_dataPath_readOutData, 1, 1) @[ProcessingElement.scala 211:84]
    node _T_44 = bits(SPadDataModule.io_dataPath_readOutData, 0, 0) @[ProcessingElement.scala 211:84]
    node _T_63 = cat(_T_45, _T_44) @[Cat.scala 29:58]
    node _T_65 = cat(_T_64, _T_63) @[Cat.scala 29:58]
    node _T_12 = _T_65 @[ProcessingElement.scala 150:46 ProcessingElement.scala 213:22]
    node _T_42 = eq(_T_12, UInt<1>("h0")) @[ProcessingElement.scala 193:53]
    node _T_48 = bits(SPadDataModule.io_dataPath_readOutData, 4, 4) @[ProcessingElement.scala 211:84]
    node _T_49 = bits(SPadDataModule.io_dataPath_readOutData, 5, 5) @[ProcessingElement.scala 211:84]
    node _T_50 = bits(SPadDataModule.io_dataPath_readOutData, 6, 6) @[ProcessingElement.scala 211:84]
    node _T_51 = bits(SPadDataModule.io_dataPath_readOutData, 7, 7) @[ProcessingElement.scala 211:84]
    node _T_52 = bits(SPadDataModule.io_dataPath_readOutData, 8, 8) @[ProcessingElement.scala 211:84]
    node _T_53 = bits(SPadDataModule.io_dataPath_readOutData, 9, 9) @[ProcessingElement.scala 211:84]
    node _T_54 = bits(SPadDataModule.io_dataPath_readOutData, 10, 10) @[ProcessingElement.scala 211:84]
    node _T_55 = bits(SPadDataModule.io_dataPath_readOutData, 11, 11) @[ProcessingElement.scala 211:84]
    node _T_56 = cat(_T_49, _T_48) @[Cat.scala 29:58]
    node _T_57 = cat(_T_51, _T_50) @[Cat.scala 29:58]
    node _T_58 = cat(_T_57, _T_56) @[Cat.scala 29:58]
    node _T_59 = cat(_T_53, _T_52) @[Cat.scala 29:58]
    node _T_60 = cat(_T_55, _T_54) @[Cat.scala 29:58]
    node _T_61 = cat(_T_60, _T_59) @[Cat.scala 29:58]
    node _T_62 = cat(_T_61, _T_58) @[Cat.scala 29:58]
    node _T_66 = bits(SPadDataModule_1.io_dataPath_readOutData, 0, 0) @[ProcessingElement.scala 233:86]
    node _T_67 = bits(SPadDataModule_1.io_dataPath_readOutData, 1, 1) @[ProcessingElement.scala 233:86]
    node _T_68 = bits(SPadDataModule_1.io_dataPath_readOutData, 2, 2) @[ProcessingElement.scala 233:86]
    node _T_69 = bits(SPadDataModule_1.io_dataPath_readOutData, 3, 3) @[ProcessingElement.scala 233:86]
    node _T_70 = bits(SPadDataModule_1.io_dataPath_readOutData, 4, 4) @[ProcessingElement.scala 233:86]
    node _T_71 = bits(SPadDataModule_1.io_dataPath_readOutData, 5, 5) @[ProcessingElement.scala 233:86]
    node _T_72 = bits(SPadDataModule_1.io_dataPath_readOutData, 6, 6) @[ProcessingElement.scala 233:86]
    node _T_73 = bits(SPadDataModule_1.io_dataPath_readOutData, 7, 7) @[ProcessingElement.scala 233:86]
    node _T_74 = bits(SPadDataModule_1.io_dataPath_readOutData, 8, 8) @[ProcessingElement.scala 233:86]
    node _T_75 = bits(SPadDataModule_1.io_dataPath_readOutData, 9, 9) @[ProcessingElement.scala 233:86]
    node _T_76 = bits(SPadDataModule_1.io_dataPath_readOutData, 10, 10) @[ProcessingElement.scala 233:86]
    node _T_77 = bits(SPadDataModule_1.io_dataPath_readOutData, 11, 11) @[ProcessingElement.scala 233:86]
    node _T_78 = cat(_T_71, _T_70) @[Cat.scala 29:58]
    node _T_79 = cat(_T_73, _T_72) @[Cat.scala 29:58]
    node _T_80 = cat(_T_79, _T_78) @[Cat.scala 29:58]
    node _T_81 = cat(_T_75, _T_74) @[Cat.scala 29:58]
    node _T_82 = cat(_T_77, _T_76) @[Cat.scala 29:58]
    node _T_83 = cat(_T_82, _T_81) @[Cat.scala 29:58]
    node _T_84 = cat(_T_83, _T_80) @[Cat.scala 29:58]
    node _T_85 = cat(_T_67, _T_66) @[Cat.scala 29:58]
    node _T_86 = cat(_T_69, _T_68) @[Cat.scala 29:58]
    node _T_87 = cat(_T_86, _T_85) @[Cat.scala 29:58]
    node _T_27 = _T_42 @[ProcessingElement.scala 167:55 ProcessingElement.scala 193:31]
    node _T_15 = WeightSPadAdrModule.io_dataPath_readOutData @[ProcessingElement.scala 154:45 ProcessingElement.scala 222:21]
    node _T_88 = mux(_T_27, UInt<1>("h0"), _T_15) @[ProcessingElement.scala 238:46]
    node _T_35 = _T_38 @[ProcessingElement.scala 186:36 ProcessingElement.scala 189:12]
    node _T_89 = and(_T_35, io_padCtrl_fromTopIO_pSumEnqOrProduct_bits) @[ProcessingElement.scala 242:41]
    reg value : UInt<5>, clock with :
      reset => (UInt<1>("h0"), value) @[Counter.scala 29:33]
    node _T_90 = and(io_padCtrl_fromTopIO_doLoadEn, io_dataStream_opsIO_ready) @[ProcessingElement.scala 244:39]
    node _GEN_0 = validif(eq(UInt<1>("h0"), value), _T_1_0) @[ProcessingElement.scala 245:30]
    node _GEN_1 = mux(eq(UInt<1>("h1"), value), _T_1_1, _GEN_0) @[ProcessingElement.scala 245:30]
    node _GEN_2 = mux(eq(UInt<2>("h2"), value), _T_1_2, _GEN_1) @[ProcessingElement.scala 245:30]
    node _GEN_3 = mux(eq(UInt<2>("h3"), value), _T_1_3, _GEN_2) @[ProcessingElement.scala 245:30]
    node _GEN_4 = mux(eq(UInt<3>("h4"), value), _T_1_4, _GEN_3) @[ProcessingElement.scala 245:30]
    node _GEN_5 = mux(eq(UInt<3>("h5"), value), _T_1_5, _GEN_4) @[ProcessingElement.scala 245:30]
    node _GEN_6 = mux(eq(UInt<3>("h6"), value), _T_1_6, _GEN_5) @[ProcessingElement.scala 245:30]
    node _GEN_7 = mux(eq(UInt<3>("h7"), value), _T_1_7, _GEN_6) @[ProcessingElement.scala 245:30]
    node _GEN_8 = mux(eq(UInt<4>("h8"), value), _T_1_8, _GEN_7) @[ProcessingElement.scala 245:30]
    node _GEN_9 = mux(eq(UInt<4>("h9"), value), _T_1_9, _GEN_8) @[ProcessingElement.scala 245:30]
    node _GEN_10 = mux(eq(UInt<4>("ha"), value), _T_1_10, _GEN_9) @[ProcessingElement.scala 245:30]
    node _GEN_11 = mux(eq(UInt<4>("hb"), value), _T_1_11, _GEN_10) @[ProcessingElement.scala 245:30]
    node _GEN_12 = mux(eq(UInt<4>("hc"), value), _T_1_12, _GEN_11) @[ProcessingElement.scala 245:30]
    node _GEN_13 = mux(eq(UInt<4>("hd"), value), _T_1_13, _GEN_12) @[ProcessingElement.scala 245:30]
    node _GEN_14 = mux(eq(UInt<4>("he"), value), _T_1_14, _GEN_13) @[ProcessingElement.scala 245:30]
    node _GEN_15 = mux(eq(UInt<4>("hf"), value), _T_1_15, _GEN_14) @[ProcessingElement.scala 245:30]
    node _GEN_16 = mux(eq(UInt<5>("h10"), value), _T_1_16, _GEN_15) @[ProcessingElement.scala 245:30]
    node _GEN_17 = mux(eq(UInt<5>("h11"), value), _T_1_17, _GEN_16) @[ProcessingElement.scala 245:30]
    node _GEN_18 = mux(eq(UInt<5>("h12"), value), _T_1_18, _GEN_17) @[ProcessingElement.scala 245:30]
    node _GEN_19 = mux(eq(UInt<5>("h13"), value), _T_1_19, _GEN_18) @[ProcessingElement.scala 245:30]
    node _GEN_20 = mux(eq(UInt<5>("h14"), value), _T_1_20, _GEN_19) @[ProcessingElement.scala 245:30]
    node _GEN_21 = mux(eq(UInt<5>("h15"), value), _T_1_21, _GEN_20) @[ProcessingElement.scala 245:30]
    node _GEN_22 = mux(eq(UInt<5>("h16"), value), _T_1_22, _GEN_21) @[ProcessingElement.scala 245:30]
    node _GEN_23 = mux(eq(UInt<5>("h17"), value), _T_1_23, _GEN_22) @[ProcessingElement.scala 245:30]
    node _GEN_24 = mux(eq(UInt<5>("h18"), value), _T_1_24, _GEN_23) @[ProcessingElement.scala 245:30]
    node _GEN_25 = mux(eq(UInt<5>("h19"), value), _T_1_25, _GEN_24) @[ProcessingElement.scala 245:30]
    node _GEN_26 = mux(eq(UInt<5>("h1a"), value), _T_1_26, _GEN_25) @[ProcessingElement.scala 245:30]
    node _GEN_27 = mux(eq(UInt<5>("h1b"), value), _T_1_27, _GEN_26) @[ProcessingElement.scala 245:30]
    node _GEN_28 = mux(eq(UInt<5>("h1c"), value), _T_1_28, _GEN_27) @[ProcessingElement.scala 245:30]
    node _GEN_29 = mux(eq(UInt<5>("h1d"), value), _T_1_29, _GEN_28) @[ProcessingElement.scala 245:30]
    node _GEN_30 = mux(eq(UInt<5>("h1e"), value), _T_1_30, _GEN_29) @[ProcessingElement.scala 245:30]
    node _GEN_31 = mux(eq(UInt<5>("h1f"), value), _T_1_31, _GEN_30) @[ProcessingElement.scala 245:30]
    node _T_91 = eq(value, UInt<5>("h18")) @[Counter.scala 37:24]
    node _T_92 = add(value, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_93 = tail(_T_92, 1) @[Counter.scala 38:22]
    node _GEN_32 = mux(_T_91, UInt<1>("h0"), _T_93) @[Counter.scala 40:21]
    node _T_1_value = _GEN_31 @[ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30 ProcessingElement.scala 245:30]
    node _GEN_33 = validif(_T_90, _T_1_value) @[ProcessingElement.scala 244:69]
    node _GEN_34 = mux(_T_90, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 244:69]
    node _GEN_35 = mux(_T_90, _GEN_32, value) @[ProcessingElement.scala 244:69]
    node _T_94 = add(_T_29, _T_28) @[ProcessingElement.scala 254:50]
    node _T_95 = tail(_T_94, 1) @[ProcessingElement.scala 254:50]
    node _T_36 = _T_39 @[ProcessingElement.scala 187:35 ProcessingElement.scala 190:11]
    node _T_96 = mux(_T_36, _T_95, UInt<1>("h0")) @[ProcessingElement.scala 254:24]
    reg _T_98 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_98) @[ProcessingElement.scala 263:53]
    node _T_3 = SPadAdrModule.io_dataPath_readOutData @[ProcessingElement.scala 141:44 ProcessingElement.scala 202:20]
    node _T_105 = eq(_T_3, UInt<4>("hf")) @[ProcessingElement.scala 270:48]
    node _T_106 = eq(_T_15, UInt<7>("h7f")) @[ProcessingElement.scala 271:50]
    node _T_4 = SPadDataModule.io_dataPath_columnNum @[ProcessingElement.scala 142:46 ProcessingElement.scala 210:22]
    node _T_107 = add(_T_4, UInt<1>("h1")) @[ProcessingElement.scala 272:68]
    node _T_108 = tail(_T_107, 1) @[ProcessingElement.scala 272:68]
    node _T_109 = eq(_T_3, _T_108) @[ProcessingElement.scala 272:44]
    node _T_16 = SPadDataModule_1.io_dataPath_columnNum @[ProcessingElement.scala 155:47 ProcessingElement.scala 232:23]
    node _T_110 = add(_T_16, UInt<1>("h1")) @[ProcessingElement.scala 273:71]
    node _T_111 = tail(_T_110, 1) @[ProcessingElement.scala 273:71]
    node _T_112 = eq(_T_15, _T_111) @[ProcessingElement.scala 273:46]
    node _T_13 = _T_62 @[ProcessingElement.scala 151:47 ProcessingElement.scala 212:23]
    node _T_113 = eq(_T_13, UInt<1>("h0")) @[ProcessingElement.scala 274:47]
    node _T_114 = eq(_T_11, UInt<1>("h0")) @[ProcessingElement.scala 274:58]
    node _T_115 = and(_T_113, _T_114) @[ProcessingElement.scala 274:55]
    node _T_21 = _T_84 @[ProcessingElement.scala 160:47 ProcessingElement.scala 234:23]
    node _T_116 = eq(_T_21, UInt<1>("h0")) @[ProcessingElement.scala 275:48]
    node _T_117 = eq(_T_22, UInt<1>("h0")) @[ProcessingElement.scala 275:59]
    node _T_118 = and(_T_116, _T_117) @[ProcessingElement.scala 275:56]
    node _T_32 = _T_37 @[ProcessingElement.scala 183:35 ProcessingElement.scala 188:11]
    node _T_97 = _T_105 @[ProcessingElement.scala 262:52 ProcessingElement.scala 270:28]
    node _T_119 = and(_T_32, _T_97) @[ProcessingElement.scala 276:38]
    node _T_34 = _T_40 @[ProcessingElement.scala 185:35 ProcessingElement.scala 191:11]
    node _T_100 = _T_106 @[ProcessingElement.scala 265:53 ProcessingElement.scala 271:29]
    node _T_120 = and(_T_34, _T_100) @[ProcessingElement.scala 276:81]
    node _T_101 = _T_112 @[ProcessingElement.scala 266:49 ProcessingElement.scala 273:25]
    node _T_121 = and(_T_36, _T_101) @[ProcessingElement.scala 276:123]
    node _T_122 = or(_T_120, _T_121) @[ProcessingElement.scala 276:111]
    node _T_99 = _T_109 @[ProcessingElement.scala 264:48 ProcessingElement.scala 272:24]
    node _T_123 = and(_T_122, _T_99) @[ProcessingElement.scala 276:150]
    node _T_124 = or(_T_119, _T_123) @[ProcessingElement.scala 276:67]
    node _T_125 = eq(_T_31, UInt<3>("h4")) @[ProcessingElement.scala 277:48]
    node _T_126 = or(_T_35, _T_125) @[ProcessingElement.scala 277:40]
    node _T_127 = and(_T_126, _T_100) @[ProcessingElement.scala 277:68]
    node _T_128 = eq(_T_97, UInt<1>("h0")) @[ProcessingElement.scala 279:42]
    node _T_129 = and(_T_32, _T_128) @[ProcessingElement.scala 279:39]
    node _T_130 = eq(_T_11, UInt<1>("h0")) @[ProcessingElement.scala 279:71]
    node _T_131 = and(_T_129, _T_130) @[ProcessingElement.scala 279:68]
    node _T_132 = and(_T_34, _T_100) @[ProcessingElement.scala 279:113]
    node _T_133 = and(_T_36, _T_101) @[ProcessingElement.scala 279:155]
    node _T_134 = or(_T_132, _T_133) @[ProcessingElement.scala 279:143]
    node _T_135 = eq(_T_99, UInt<1>("h0")) @[ProcessingElement.scala 279:185]
    node _T_136 = and(_T_134, _T_135) @[ProcessingElement.scala 279:182]
    node _T_137 = or(_T_131, _T_136) @[ProcessingElement.scala 279:99]
    node _T_138 = eq(_T_100, UInt<1>("h0")) @[ProcessingElement.scala 280:43]
    node _T_139 = and(_T_34, _T_138) @[ProcessingElement.scala 280:40]
    node _T_140 = eq(_T_22, UInt<1>("h0")) @[ProcessingElement.scala 280:73]
    node _T_141 = and(_T_139, _T_140) @[ProcessingElement.scala 280:70]
    node _T_142 = eq(_T_101, UInt<1>("h0")) @[ProcessingElement.scala 280:114]
    node _T_143 = and(_T_36, _T_142) @[ProcessingElement.scala 280:111]
    node _T_144 = or(_T_141, _T_143) @[ProcessingElement.scala 280:99]
    node _T_33 = _T_41 @[ProcessingElement.scala 184:35 ProcessingElement.scala 192:11]
    node _T_145 = or(_T_33, _T_34) @[ProcessingElement.scala 281:34]
    node _T_146 = and(_T_145, _T_22) @[ProcessingElement.scala 281:46]
    node _T_147 = and(_T_33, _T_22) @[ProcessingElement.scala 282:35]
    node _T_148 = eq(_T_27, UInt<1>("h0")) @[ProcessingElement.scala 282:65]
    node _T_149 = and(_T_147, _T_148) @[ProcessingElement.scala 282:62]
    node _T_150 = sub(_T_12, UInt<1>("h1")) @[ProcessingElement.scala 283:76]
    node _T_151 = tail(_T_150, 1) @[ProcessingElement.scala 283:76]
    node _T_23 = _T_149 @[ProcessingElement.scala 163:48 ProcessingElement.scala 282:24]
    node _T_152 = mux(_T_23, _T_151, _T_12) @[ProcessingElement.scala 283:34]
    node _T_153 = and(_T_34, _T_22) @[ProcessingElement.scala 284:34]
    node _T_154 = eq(_T_100, UInt<1>("h0")) @[ProcessingElement.scala 284:64]
    node _T_155 = and(_T_153, _T_154) @[ProcessingElement.scala 284:61]
    node _T_156 = mul(_T_9, UInt<3>("h4")) @[ProcessingElement.scala 286:66]
    node _T_43 = _T_87 @[ProcessingElement.scala 194:46 ProcessingElement.scala 235:22]
    node _T_157 = add(_T_43, _T_156) @[ProcessingElement.scala 286:43]
    node _T_158 = tail(_T_157, 1) @[ProcessingElement.scala 286:43]
    node _T_159 = eq(UInt<3>("h0"), _T_31) @[Conditional.scala 37:30]
    node _GEN_36 = mux(io_padCtrl_doMACEn, UInt<3>("h1"), _T_31) @[ProcessingElement.scala 289:32]
    node _GEN_37 = mux(io_padCtrl_doMACEn, UInt<1>("h1"), _T_5) @[ProcessingElement.scala 289:32]
    node _GEN_38 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), _T_6) @[ProcessingElement.scala 289:32]
    node _GEN_39 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), _T_17) @[ProcessingElement.scala 289:32]
    node _GEN_40 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), _T_18) @[ProcessingElement.scala 289:32]
    node _GEN_41 = mux(io_padCtrl_doMACEn, UInt<1>("h1"), _T_22) @[ProcessingElement.scala 289:32]
    node _GEN_42 = mux(io_padCtrl_doMACEn, UInt<1>("h1"), _T_11) @[ProcessingElement.scala 289:32]
    node _T_160 = eq(UInt<3>("h1"), _T_31) @[Conditional.scala 37:30]
    node _T_161 = add(_T_10, UInt<1>("h1")) @[ProcessingElement.scala 298:56]
    node _T_162 = tail(_T_161, 1) @[ProcessingElement.scala 298:56]
    node _GEN_43 = mux(_T_97, UInt<3>("h1"), UInt<3>("h2")) @[ProcessingElement.scala 295:39]
    node _GEN_44 = mux(_T_97, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 295:39]
    node _GEN_45 = mux(_T_97, UInt<1>("h0"), UInt<1>("h1")) @[ProcessingElement.scala 295:39]
    node _GEN_46 = mux(_T_97, UInt<1>("h0"), UInt<1>("h0")) @[ProcessingElement.scala 295:39]
    node _GEN_47 = mux(_T_97, UInt<1>("h1"), UInt<1>("h1")) @[ProcessingElement.scala 295:39]
    node _GEN_48 = mux(_T_97, UInt<1>("h1"), _T_98) @[ProcessingElement.scala 295:39]
    node _GEN_49 = mux(_T_97, _T_162, _T_10) @[ProcessingElement.scala 295:39]
    node _T_163 = eq(UInt<3>("h2"), _T_31) @[Conditional.scala 37:30]
    node _T_102 = _T_115 @[ProcessingElement.scala 267:48 ProcessingElement.scala 274:24]
    node _GEN_50 = mux(_T_102, UInt<3>("h0"), UInt<3>("h3")) @[ProcessingElement.scala 304:35]
    node _GEN_51 = mux(_T_102, UInt<1>("h0"), _T_9) @[ProcessingElement.scala 304:35]
    node _GEN_52 = mux(_T_102, UInt<1>("h0"), _GEN_35) @[ProcessingElement.scala 304:35]
    node _GEN_53 = mux(_T_102, _T_5, UInt<1>("h0")) @[ProcessingElement.scala 304:35]
    node _GEN_54 = mux(_T_102, _T_6, UInt<1>("h0")) @[ProcessingElement.scala 304:35]
    node _GEN_55 = mux(_T_102, _T_17, UInt<1>("h1")) @[ProcessingElement.scala 304:35]
    node _GEN_56 = mux(_T_102, _T_18, UInt<1>("h0")) @[ProcessingElement.scala 304:35]
    node _GEN_57 = mux(_T_102, _T_11, UInt<1>("h0")) @[ProcessingElement.scala 304:35]
    node _T_164 = eq(UInt<3>("h3"), _T_31) @[Conditional.scala 37:30]
    node _T_165 = add(_T_9, UInt<1>("h1")) @[ProcessingElement.scala 317:58]
    node _T_166 = tail(_T_165, 1) @[ProcessingElement.scala 317:58]
    node _T_167 = add(_T_166, _T_10) @[ProcessingElement.scala 317:64]
    node _T_168 = tail(_T_167, 1) @[ProcessingElement.scala 317:64]
    node _T_169 = add(_T_9, UInt<1>("h1")) @[ProcessingElement.scala 320:58]
    node _T_170 = tail(_T_169, 1) @[ProcessingElement.scala 320:58]
    node _GEN_58 = mux(_T_98, UInt<1>("h0"), _T_98) @[ProcessingElement.scala 315:41]
    node _GEN_59 = mux(_T_98, _T_168, _T_170) @[ProcessingElement.scala 315:41]
    node _GEN_60 = mux(_T_98, UInt<1>("h0"), _T_10) @[ProcessingElement.scala 315:41]
    node _GEN_61 = mux(_T_99, UInt<3>("h1"), UInt<3>("h2")) @[ProcessingElement.scala 313:37]
    node _GEN_62 = mux(_T_99, UInt<1>("h1"), UInt<1>("h0")) @[ProcessingElement.scala 313:37]
    node _GEN_63 = mux(_T_99, UInt<1>("h0"), UInt<1>("h1")) @[ProcessingElement.scala 313:37]
    node _GEN_64 = mux(_T_99, UInt<1>("h0"), UInt<1>("h0")) @[ProcessingElement.scala 313:37]
    node _GEN_65 = mux(_T_99, UInt<1>("h1"), UInt<1>("h1")) @[ProcessingElement.scala 313:37]
    node _GEN_66 = mux(_T_99, _GEN_58, _T_98) @[ProcessingElement.scala 313:37]
    node _GEN_67 = mux(_T_99, _GEN_59, _T_9) @[ProcessingElement.scala 313:37]
    node _GEN_68 = mux(_T_99, _GEN_60, _T_10) @[ProcessingElement.scala 313:37]
    node _GEN_69 = mux(_T_100, _GEN_61, UInt<3>("h4")) @[ProcessingElement.scala 312:40]
    node _GEN_70 = mux(_T_100, _GEN_62, UInt<1>("h0")) @[ProcessingElement.scala 312:40]
    node _GEN_71 = mux(_T_100, _GEN_63, UInt<1>("h0")) @[ProcessingElement.scala 312:40]
    node _GEN_72 = mux(_T_100, _GEN_64, UInt<1>("h0")) @[ProcessingElement.scala 312:40]
    node _GEN_73 = mux(_T_100, _GEN_64, UInt<1>("h1")) @[ProcessingElement.scala 312:40]
    node _GEN_74 = mux(_T_100, _GEN_65, _T_22) @[ProcessingElement.scala 312:40]
    node _GEN_75 = mux(_T_100, _GEN_66, _T_98) @[ProcessingElement.scala 312:40]
    node _GEN_76 = mux(_T_100, _GEN_67, _T_9) @[ProcessingElement.scala 312:40]
    node _GEN_77 = mux(_T_100, _GEN_68, _T_10) @[ProcessingElement.scala 312:40]
    node _T_171 = eq(UInt<3>("h4"), _T_31) @[Conditional.scala 37:30]
    node _T_172 = eq(UInt<3>("h5"), _T_31) @[Conditional.scala 37:30]
    node _T_173 = eq(UInt<3>("h6"), _T_31) @[Conditional.scala 37:30]
    node _GEN_78 = mux(io_dataStream_ipsIO_valid, UInt<3>("h7"), _T_31) @[ProcessingElement.scala 339:42]
    node _GEN_79 = mux(io_dataStream_ipsIO_valid, io_dataStream_ipsIO_bits, _T_28) @[ProcessingElement.scala 339:42]
    node _GEN_80 = mux(io_dataStream_ipsIO_valid, UInt<1>("h1"), _T_89) @[ProcessingElement.scala 339:42]
    node _T_1_value_0 = _GEN_31 @[ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27 ProcessingElement.scala 343:27]
    node _GEN_81 = mux(io_dataStream_ipsIO_valid, _T_1_value_0, _T_29) @[ProcessingElement.scala 339:42]
    node _T_174 = mul(_T_21, _T_13) @[ProcessingElement.scala 347:44]
    node _GEN_82 = mux(io_padCtrl_fromTopIO_pSumEnqOrProduct_bits, _GEN_78, UInt<3>("h7")) @[ProcessingElement.scala 338:57]
    node _GEN_83 = mux(io_padCtrl_fromTopIO_pSumEnqOrProduct_bits, _GEN_79, _T_174) @[ProcessingElement.scala 338:57]
    node _GEN_84 = mux(io_padCtrl_fromTopIO_pSumEnqOrProduct_bits, _GEN_80, _T_89) @[ProcessingElement.scala 338:57]
    node _T_1_value_1 = _GEN_31 @[ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25 ProcessingElement.scala 348:25]
    node _GEN_85 = mux(io_padCtrl_fromTopIO_pSumEnqOrProduct_bits, _GEN_81, _T_1_value_1) @[ProcessingElement.scala 338:57]
    node _T_175 = eq(UInt<3>("h7"), _T_31) @[Conditional.scala 37:30]
    node _T_104 = bits(_T_158, 4, 0) @[ProcessingElement.scala 269:45 ProcessingElement.scala 286:21]
    node _T_30 = _T_96 @[ProcessingElement.scala 171:42 ProcessingElement.scala 254:18]
    node _T_1_T_104 = _T_30 @[ProcessingElement.scala 352:37 ProcessingElement.scala 352:37]
    node _GEN_86 = mux(eq(UInt<1>("h0"), _T_104), _T_1_T_104, _T_1_0) @[ProcessingElement.scala 352:37]
    node _GEN_87 = mux(eq(UInt<1>("h1"), _T_104), _T_1_T_104, _T_1_1) @[ProcessingElement.scala 352:37]
    node _GEN_88 = mux(eq(UInt<2>("h2"), _T_104), _T_1_T_104, _T_1_2) @[ProcessingElement.scala 352:37]
    node _GEN_89 = mux(eq(UInt<2>("h3"), _T_104), _T_1_T_104, _T_1_3) @[ProcessingElement.scala 352:37]
    node _GEN_90 = mux(eq(UInt<3>("h4"), _T_104), _T_1_T_104, _T_1_4) @[ProcessingElement.scala 352:37]
    node _GEN_91 = mux(eq(UInt<3>("h5"), _T_104), _T_1_T_104, _T_1_5) @[ProcessingElement.scala 352:37]
    node _GEN_92 = mux(eq(UInt<3>("h6"), _T_104), _T_1_T_104, _T_1_6) @[ProcessingElement.scala 352:37]
    node _GEN_93 = mux(eq(UInt<3>("h7"), _T_104), _T_1_T_104, _T_1_7) @[ProcessingElement.scala 352:37]
    node _GEN_94 = mux(eq(UInt<4>("h8"), _T_104), _T_1_T_104, _T_1_8) @[ProcessingElement.scala 352:37]
    node _GEN_95 = mux(eq(UInt<4>("h9"), _T_104), _T_1_T_104, _T_1_9) @[ProcessingElement.scala 352:37]
    node _GEN_96 = mux(eq(UInt<4>("ha"), _T_104), _T_1_T_104, _T_1_10) @[ProcessingElement.scala 352:37]
    node _GEN_97 = mux(eq(UInt<4>("hb"), _T_104), _T_1_T_104, _T_1_11) @[ProcessingElement.scala 352:37]
    node _GEN_98 = mux(eq(UInt<4>("hc"), _T_104), _T_1_T_104, _T_1_12) @[ProcessingElement.scala 352:37]
    node _GEN_99 = mux(eq(UInt<4>("hd"), _T_104), _T_1_T_104, _T_1_13) @[ProcessingElement.scala 352:37]
    node _GEN_100 = mux(eq(UInt<4>("he"), _T_104), _T_1_T_104, _T_1_14) @[ProcessingElement.scala 352:37]
    node _GEN_101 = mux(eq(UInt<4>("hf"), _T_104), _T_1_T_104, _T_1_15) @[ProcessingElement.scala 352:37]
    node _GEN_102 = mux(eq(UInt<5>("h10"), _T_104), _T_1_T_104, _T_1_16) @[ProcessingElement.scala 352:37]
    node _GEN_103 = mux(eq(UInt<5>("h11"), _T_104), _T_1_T_104, _T_1_17) @[ProcessingElement.scala 352:37]
    node _GEN_104 = mux(eq(UInt<5>("h12"), _T_104), _T_1_T_104, _T_1_18) @[ProcessingElement.scala 352:37]
    node _GEN_105 = mux(eq(UInt<5>("h13"), _T_104), _T_1_T_104, _T_1_19) @[ProcessingElement.scala 352:37]
    node _GEN_106 = mux(eq(UInt<5>("h14"), _T_104), _T_1_T_104, _T_1_20) @[ProcessingElement.scala 352:37]
    node _GEN_107 = mux(eq(UInt<5>("h15"), _T_104), _T_1_T_104, _T_1_21) @[ProcessingElement.scala 352:37]
    node _GEN_108 = mux(eq(UInt<5>("h16"), _T_104), _T_1_T_104, _T_1_22) @[ProcessingElement.scala 352:37]
    node _GEN_109 = mux(eq(UInt<5>("h17"), _T_104), _T_1_T_104, _T_1_23) @[ProcessingElement.scala 352:37]
    node _GEN_110 = mux(eq(UInt<5>("h18"), _T_104), _T_1_T_104, _T_1_24) @[ProcessingElement.scala 352:37]
    node _GEN_111 = mux(eq(UInt<5>("h19"), _T_104), _T_1_T_104, _T_1_25) @[ProcessingElement.scala 352:37]
    node _GEN_112 = mux(eq(UInt<5>("h1a"), _T_104), _T_1_T_104, _T_1_26) @[ProcessingElement.scala 352:37]
    node _GEN_113 = mux(eq(UInt<5>("h1b"), _T_104), _T_1_T_104, _T_1_27) @[ProcessingElement.scala 352:37]
    node _GEN_114 = mux(eq(UInt<5>("h1c"), _T_104), _T_1_T_104, _T_1_28) @[ProcessingElement.scala 352:37]
    node _GEN_115 = mux(eq(UInt<5>("h1d"), _T_104), _T_1_T_104, _T_1_29) @[ProcessingElement.scala 352:37]
    node _GEN_116 = mux(eq(UInt<5>("h1e"), _T_104), _T_1_T_104, _T_1_30) @[ProcessingElement.scala 352:37]
    node _GEN_117 = mux(eq(UInt<5>("h1f"), _T_104), _T_1_T_104, _T_1_31) @[ProcessingElement.scala 352:37]
    node _T_176 = add(_T_9, UInt<1>("h1")) @[ProcessingElement.scala 361:60]
    node _T_177 = tail(_T_176, 1) @[ProcessingElement.scala 361:60]
    node _T_178 = add(_T_177, _T_10) @[ProcessingElement.scala 361:66]
    node _T_179 = tail(_T_178, 1) @[ProcessingElement.scala 361:66]
    node _T_180 = add(_T_9, UInt<1>("h1")) @[ProcessingElement.scala 364:60]
    node _T_181 = tail(_T_180, 1) @[ProcessingElement.scala 364:60]
    node _GEN_118 = mux(_T_98, _T_179, _T_181) @[ProcessingElement.scala 359:43]
    node _GEN_119 = mux(_T_99, _GEN_118, _T_9) @[ProcessingElement.scala 357:39]
    node _GEN_120 = mux(_T_101, _GEN_61, UInt<3>("h4")) @[ProcessingElement.scala 356:38]
    node _GEN_121 = mux(_T_101, _GEN_62, UInt<1>("h0")) @[ProcessingElement.scala 356:38]
    node _GEN_122 = mux(_T_101, _GEN_63, UInt<1>("h0")) @[ProcessingElement.scala 356:38]
    node _GEN_123 = mux(_T_101, _GEN_64, UInt<1>("h0")) @[ProcessingElement.scala 356:38]
    node _GEN_124 = mux(_T_101, _GEN_64, UInt<1>("h1")) @[ProcessingElement.scala 356:38]
    node _GEN_125 = mux(_T_101, _GEN_65, UInt<1>("h0")) @[ProcessingElement.scala 356:38]
    node _GEN_126 = mux(_T_101, _GEN_66, _T_98) @[ProcessingElement.scala 356:38]
    node _GEN_127 = mux(_T_101, _GEN_119, _T_9) @[ProcessingElement.scala 356:38]
    node _GEN_128 = mux(_T_101, _GEN_68, _T_10) @[ProcessingElement.scala 356:38]
    node _GEN_129 = mux(_T_102, UInt<3>("h0"), _GEN_120) @[ProcessingElement.scala 353:35]
    node _GEN_130 = mux(_T_102, UInt<1>("h0"), _GEN_127) @[ProcessingElement.scala 353:35]
    node _GEN_131 = mux(_T_102, _T_5, _GEN_121) @[ProcessingElement.scala 353:35]
    node _GEN_132 = mux(_T_102, _T_6, _GEN_122) @[ProcessingElement.scala 353:35]
    node _GEN_133 = mux(_T_102, _T_17, _GEN_123) @[ProcessingElement.scala 353:35]
    node _GEN_134 = mux(_T_102, _T_18, _GEN_124) @[ProcessingElement.scala 353:35]
    node _GEN_135 = mux(_T_102, _T_22, _GEN_125) @[ProcessingElement.scala 353:35]
    node _GEN_136 = mux(_T_102, _T_98, _GEN_126) @[ProcessingElement.scala 353:35]
    node _GEN_137 = mux(_T_102, _T_10, _GEN_128) @[ProcessingElement.scala 353:35]
    node _GEN_138 = mux(_T_175, _GEN_86, _T_1_0) @[Conditional.scala 39:67]
    node _GEN_139 = mux(_T_175, _GEN_87, _T_1_1) @[Conditional.scala 39:67]
    node _GEN_140 = mux(_T_175, _GEN_88, _T_1_2) @[Conditional.scala 39:67]
    node _GEN_141 = mux(_T_175, _GEN_89, _T_1_3) @[Conditional.scala 39:67]
    node _GEN_142 = mux(_T_175, _GEN_90, _T_1_4) @[Conditional.scala 39:67]
    node _GEN_143 = mux(_T_175, _GEN_91, _T_1_5) @[Conditional.scala 39:67]
    node _GEN_144 = mux(_T_175, _GEN_92, _T_1_6) @[Conditional.scala 39:67]
    node _GEN_145 = mux(_T_175, _GEN_93, _T_1_7) @[Conditional.scala 39:67]
    node _GEN_146 = mux(_T_175, _GEN_94, _T_1_8) @[Conditional.scala 39:67]
    node _GEN_147 = mux(_T_175, _GEN_95, _T_1_9) @[Conditional.scala 39:67]
    node _GEN_148 = mux(_T_175, _GEN_96, _T_1_10) @[Conditional.scala 39:67]
    node _GEN_149 = mux(_T_175, _GEN_97, _T_1_11) @[Conditional.scala 39:67]
    node _GEN_150 = mux(_T_175, _GEN_98, _T_1_12) @[Conditional.scala 39:67]
    node _GEN_151 = mux(_T_175, _GEN_99, _T_1_13) @[Conditional.scala 39:67]
    node _GEN_152 = mux(_T_175, _GEN_100, _T_1_14) @[Conditional.scala 39:67]
    node _GEN_153 = mux(_T_175, _GEN_101, _T_1_15) @[Conditional.scala 39:67]
    node _GEN_154 = mux(_T_175, _GEN_102, _T_1_16) @[Conditional.scala 39:67]
    node _GEN_155 = mux(_T_175, _GEN_103, _T_1_17) @[Conditional.scala 39:67]
    node _GEN_156 = mux(_T_175, _GEN_104, _T_1_18) @[Conditional.scala 39:67]
    node _GEN_157 = mux(_T_175, _GEN_105, _T_1_19) @[Conditional.scala 39:67]
    node _GEN_158 = mux(_T_175, _GEN_106, _T_1_20) @[Conditional.scala 39:67]
    node _GEN_159 = mux(_T_175, _GEN_107, _T_1_21) @[Conditional.scala 39:67]
    node _GEN_160 = mux(_T_175, _GEN_108, _T_1_22) @[Conditional.scala 39:67]
    node _GEN_161 = mux(_T_175, _GEN_109, _T_1_23) @[Conditional.scala 39:67]
    node _GEN_162 = mux(_T_175, _GEN_110, _T_1_24) @[Conditional.scala 39:67]
    node _GEN_163 = mux(_T_175, _GEN_111, _T_1_25) @[Conditional.scala 39:67]
    node _GEN_164 = mux(_T_175, _GEN_112, _T_1_26) @[Conditional.scala 39:67]
    node _GEN_165 = mux(_T_175, _GEN_113, _T_1_27) @[Conditional.scala 39:67]
    node _GEN_166 = mux(_T_175, _GEN_114, _T_1_28) @[Conditional.scala 39:67]
    node _GEN_167 = mux(_T_175, _GEN_115, _T_1_29) @[Conditional.scala 39:67]
    node _GEN_168 = mux(_T_175, _GEN_116, _T_1_30) @[Conditional.scala 39:67]
    node _GEN_169 = mux(_T_175, _GEN_117, _T_1_31) @[Conditional.scala 39:67]
    node _GEN_170 = mux(_T_175, _GEN_129, _T_31) @[Conditional.scala 39:67]
    node _GEN_171 = mux(_T_175, _GEN_130, _T_9) @[Conditional.scala 39:67]
    node _GEN_172 = mux(_T_175, _GEN_52, _GEN_35) @[Conditional.scala 39:67]
    node _GEN_173 = mux(_T_175, _GEN_131, _T_5) @[Conditional.scala 39:67]
    node _GEN_174 = mux(_T_175, _GEN_132, _T_6) @[Conditional.scala 39:67]
    node _GEN_175 = mux(_T_175, _GEN_133, _T_17) @[Conditional.scala 39:67]
    node _GEN_176 = mux(_T_175, _GEN_134, _T_18) @[Conditional.scala 39:67]
    node _GEN_177 = mux(_T_175, _GEN_135, _T_22) @[Conditional.scala 39:67]
    node _GEN_178 = mux(_T_175, _GEN_136, _T_98) @[Conditional.scala 39:67]
    node _GEN_179 = mux(_T_175, _GEN_137, _T_10) @[Conditional.scala 39:67]
    node _GEN_180 = mux(_T_173, _GEN_82, _GEN_170) @[Conditional.scala 39:67]
    node _GEN_181 = mux(_T_173, _GEN_83, _T_28) @[Conditional.scala 39:67]
    node _GEN_182 = mux(_T_173, _GEN_84, _T_89) @[Conditional.scala 39:67]
    node _GEN_183 = mux(_T_173, _GEN_85, _T_29) @[Conditional.scala 39:67]
    node _GEN_184 = mux(_T_173, _T_1_0, _GEN_138) @[Conditional.scala 39:67]
    node _GEN_185 = mux(_T_173, _T_1_1, _GEN_139) @[Conditional.scala 39:67]
    node _GEN_186 = mux(_T_173, _T_1_2, _GEN_140) @[Conditional.scala 39:67]
    node _GEN_187 = mux(_T_173, _T_1_3, _GEN_141) @[Conditional.scala 39:67]
    node _GEN_188 = mux(_T_173, _T_1_4, _GEN_142) @[Conditional.scala 39:67]
    node _GEN_189 = mux(_T_173, _T_1_5, _GEN_143) @[Conditional.scala 39:67]
    node _GEN_190 = mux(_T_173, _T_1_6, _GEN_144) @[Conditional.scala 39:67]
    node _GEN_191 = mux(_T_173, _T_1_7, _GEN_145) @[Conditional.scala 39:67]
    node _GEN_192 = mux(_T_173, _T_1_8, _GEN_146) @[Conditional.scala 39:67]
    node _GEN_193 = mux(_T_173, _T_1_9, _GEN_147) @[Conditional.scala 39:67]
    node _GEN_194 = mux(_T_173, _T_1_10, _GEN_148) @[Conditional.scala 39:67]
    node _GEN_195 = mux(_T_173, _T_1_11, _GEN_149) @[Conditional.scala 39:67]
    node _GEN_196 = mux(_T_173, _T_1_12, _GEN_150) @[Conditional.scala 39:67]
    node _GEN_197 = mux(_T_173, _T_1_13, _GEN_151) @[Conditional.scala 39:67]
    node _GEN_198 = mux(_T_173, _T_1_14, _GEN_152) @[Conditional.scala 39:67]
    node _GEN_199 = mux(_T_173, _T_1_15, _GEN_153) @[Conditional.scala 39:67]
    node _GEN_200 = mux(_T_173, _T_1_16, _GEN_154) @[Conditional.scala 39:67]
    node _GEN_201 = mux(_T_173, _T_1_17, _GEN_155) @[Conditional.scala 39:67]
    node _GEN_202 = mux(_T_173, _T_1_18, _GEN_156) @[Conditional.scala 39:67]
    node _GEN_203 = mux(_T_173, _T_1_19, _GEN_157) @[Conditional.scala 39:67]
    node _GEN_204 = mux(_T_173, _T_1_20, _GEN_158) @[Conditional.scala 39:67]
    node _GEN_205 = mux(_T_173, _T_1_21, _GEN_159) @[Conditional.scala 39:67]
    node _GEN_206 = mux(_T_173, _T_1_22, _GEN_160) @[Conditional.scala 39:67]
    node _GEN_207 = mux(_T_173, _T_1_23, _GEN_161) @[Conditional.scala 39:67]
    node _GEN_208 = mux(_T_173, _T_1_24, _GEN_162) @[Conditional.scala 39:67]
    node _GEN_209 = mux(_T_173, _T_1_25, _GEN_163) @[Conditional.scala 39:67]
    node _GEN_210 = mux(_T_173, _T_1_26, _GEN_164) @[Conditional.scala 39:67]
    node _GEN_211 = mux(_T_173, _T_1_27, _GEN_165) @[Conditional.scala 39:67]
    node _GEN_212 = mux(_T_173, _T_1_28, _GEN_166) @[Conditional.scala 39:67]
    node _GEN_213 = mux(_T_173, _T_1_29, _GEN_167) @[Conditional.scala 39:67]
    node _GEN_214 = mux(_T_173, _T_1_30, _GEN_168) @[Conditional.scala 39:67]
    node _GEN_215 = mux(_T_173, _T_1_31, _GEN_169) @[Conditional.scala 39:67]
    node _GEN_216 = mux(_T_173, _T_9, _GEN_171) @[Conditional.scala 39:67]
    node _GEN_217 = mux(_T_173, _GEN_35, _GEN_172) @[Conditional.scala 39:67]
    node _GEN_218 = mux(_T_173, _T_5, _GEN_173) @[Conditional.scala 39:67]
    node _GEN_219 = mux(_T_173, _T_6, _GEN_174) @[Conditional.scala 39:67]
    node _GEN_220 = mux(_T_173, _T_17, _GEN_175) @[Conditional.scala 39:67]
    node _GEN_221 = mux(_T_173, _T_18, _GEN_176) @[Conditional.scala 39:67]
    node _GEN_222 = mux(_T_173, _T_22, _GEN_177) @[Conditional.scala 39:67]
    node _GEN_223 = mux(_T_173, _T_98, _GEN_178) @[Conditional.scala 39:67]
    node _GEN_224 = mux(_T_173, _T_10, _GEN_179) @[Conditional.scala 39:67]
    node _GEN_225 = mux(_T_172, UInt<3>("h6"), _GEN_180) @[Conditional.scala 39:67]
    node _GEN_226 = mux(_T_172, _T_104, _GEN_217) @[Conditional.scala 39:67]
    node _GEN_227 = mux(_T_172, UInt<1>("h0"), _GEN_218) @[Conditional.scala 39:67]
    node _GEN_228 = mux(_T_172, UInt<1>("h0"), _GEN_219) @[Conditional.scala 39:67]
    node _GEN_229 = mux(_T_172, UInt<1>("h0"), _GEN_220) @[Conditional.scala 39:67]
    node _GEN_230 = mux(_T_172, UInt<1>("h0"), _GEN_221) @[Conditional.scala 39:67]
    node _GEN_231 = mux(_T_172, _T_28, _GEN_181) @[Conditional.scala 39:67]
    node _GEN_232 = mux(_T_172, _T_89, _GEN_182) @[Conditional.scala 39:67]
    node _GEN_233 = mux(_T_172, _T_29, _GEN_183) @[Conditional.scala 39:67]
    node _GEN_234 = mux(_T_172, _T_1_0, _GEN_184) @[Conditional.scala 39:67]
    node _GEN_235 = mux(_T_172, _T_1_1, _GEN_185) @[Conditional.scala 39:67]
    node _GEN_236 = mux(_T_172, _T_1_2, _GEN_186) @[Conditional.scala 39:67]
    node _GEN_237 = mux(_T_172, _T_1_3, _GEN_187) @[Conditional.scala 39:67]
    node _GEN_238 = mux(_T_172, _T_1_4, _GEN_188) @[Conditional.scala 39:67]
    node _GEN_239 = mux(_T_172, _T_1_5, _GEN_189) @[Conditional.scala 39:67]
    node _GEN_240 = mux(_T_172, _T_1_6, _GEN_190) @[Conditional.scala 39:67]
    node _GEN_241 = mux(_T_172, _T_1_7, _GEN_191) @[Conditional.scala 39:67]
    node _GEN_242 = mux(_T_172, _T_1_8, _GEN_192) @[Conditional.scala 39:67]
    node _GEN_243 = mux(_T_172, _T_1_9, _GEN_193) @[Conditional.scala 39:67]
    node _GEN_244 = mux(_T_172, _T_1_10, _GEN_194) @[Conditional.scala 39:67]
    node _GEN_245 = mux(_T_172, _T_1_11, _GEN_195) @[Conditional.scala 39:67]
    node _GEN_246 = mux(_T_172, _T_1_12, _GEN_196) @[Conditional.scala 39:67]
    node _GEN_247 = mux(_T_172, _T_1_13, _GEN_197) @[Conditional.scala 39:67]
    node _GEN_248 = mux(_T_172, _T_1_14, _GEN_198) @[Conditional.scala 39:67]
    node _GEN_249 = mux(_T_172, _T_1_15, _GEN_199) @[Conditional.scala 39:67]
    node _GEN_250 = mux(_T_172, _T_1_16, _GEN_200) @[Conditional.scala 39:67]
    node _GEN_251 = mux(_T_172, _T_1_17, _GEN_201) @[Conditional.scala 39:67]
    node _GEN_252 = mux(_T_172, _T_1_18, _GEN_202) @[Conditional.scala 39:67]
    node _GEN_253 = mux(_T_172, _T_1_19, _GEN_203) @[Conditional.scala 39:67]
    node _GEN_254 = mux(_T_172, _T_1_20, _GEN_204) @[Conditional.scala 39:67]
    node _GEN_255 = mux(_T_172, _T_1_21, _GEN_205) @[Conditional.scala 39:67]
    node _GEN_256 = mux(_T_172, _T_1_22, _GEN_206) @[Conditional.scala 39:67]
    node _GEN_257 = mux(_T_172, _T_1_23, _GEN_207) @[Conditional.scala 39:67]
    node _GEN_258 = mux(_T_172, _T_1_24, _GEN_208) @[Conditional.scala 39:67]
    node _GEN_259 = mux(_T_172, _T_1_25, _GEN_209) @[Conditional.scala 39:67]
    node _GEN_260 = mux(_T_172, _T_1_26, _GEN_210) @[Conditional.scala 39:67]
    node _GEN_261 = mux(_T_172, _T_1_27, _GEN_211) @[Conditional.scala 39:67]
    node _GEN_262 = mux(_T_172, _T_1_28, _GEN_212) @[Conditional.scala 39:67]
    node _GEN_263 = mux(_T_172, _T_1_29, _GEN_213) @[Conditional.scala 39:67]
    node _GEN_264 = mux(_T_172, _T_1_30, _GEN_214) @[Conditional.scala 39:67]
    node _GEN_265 = mux(_T_172, _T_1_31, _GEN_215) @[Conditional.scala 39:67]
    node _GEN_266 = mux(_T_172, _T_9, _GEN_216) @[Conditional.scala 39:67]
    node _GEN_267 = mux(_T_172, _T_22, _GEN_222) @[Conditional.scala 39:67]
    node _GEN_268 = mux(_T_172, _T_98, _GEN_223) @[Conditional.scala 39:67]
    node _GEN_269 = mux(_T_172, _T_10, _GEN_224) @[Conditional.scala 39:67]
    node _GEN_270 = mux(_T_171, UInt<3>("h5"), _GEN_225) @[Conditional.scala 39:67]
    node _GEN_271 = mux(_T_171, _GEN_35, _GEN_226) @[Conditional.scala 39:67]
    node _GEN_272 = mux(_T_171, _T_5, _GEN_227) @[Conditional.scala 39:67]
    node _GEN_273 = mux(_T_171, _T_6, _GEN_228) @[Conditional.scala 39:67]
    node _GEN_274 = mux(_T_171, _T_17, _GEN_229) @[Conditional.scala 39:67]
    node _GEN_275 = mux(_T_171, _T_18, _GEN_230) @[Conditional.scala 39:67]
    node _GEN_276 = mux(_T_171, _T_28, _GEN_231) @[Conditional.scala 39:67]
    node _GEN_277 = mux(_T_171, _T_89, _GEN_232) @[Conditional.scala 39:67]
    node _GEN_278 = mux(_T_171, _T_29, _GEN_233) @[Conditional.scala 39:67]
    node _GEN_279 = mux(_T_171, _T_1_0, _GEN_234) @[Conditional.scala 39:67]
    node _GEN_280 = mux(_T_171, _T_1_1, _GEN_235) @[Conditional.scala 39:67]
    node _GEN_281 = mux(_T_171, _T_1_2, _GEN_236) @[Conditional.scala 39:67]
    node _GEN_282 = mux(_T_171, _T_1_3, _GEN_237) @[Conditional.scala 39:67]
    node _GEN_283 = mux(_T_171, _T_1_4, _GEN_238) @[Conditional.scala 39:67]
    node _GEN_284 = mux(_T_171, _T_1_5, _GEN_239) @[Conditional.scala 39:67]
    node _GEN_285 = mux(_T_171, _T_1_6, _GEN_240) @[Conditional.scala 39:67]
    node _GEN_286 = mux(_T_171, _T_1_7, _GEN_241) @[Conditional.scala 39:67]
    node _GEN_287 = mux(_T_171, _T_1_8, _GEN_242) @[Conditional.scala 39:67]
    node _GEN_288 = mux(_T_171, _T_1_9, _GEN_243) @[Conditional.scala 39:67]
    node _GEN_289 = mux(_T_171, _T_1_10, _GEN_244) @[Conditional.scala 39:67]
    node _GEN_290 = mux(_T_171, _T_1_11, _GEN_245) @[Conditional.scala 39:67]
    node _GEN_291 = mux(_T_171, _T_1_12, _GEN_246) @[Conditional.scala 39:67]
    node _GEN_292 = mux(_T_171, _T_1_13, _GEN_247) @[Conditional.scala 39:67]
    node _GEN_293 = mux(_T_171, _T_1_14, _GEN_248) @[Conditional.scala 39:67]
    node _GEN_294 = mux(_T_171, _T_1_15, _GEN_249) @[Conditional.scala 39:67]
    node _GEN_295 = mux(_T_171, _T_1_16, _GEN_250) @[Conditional.scala 39:67]
    node _GEN_296 = mux(_T_171, _T_1_17, _GEN_251) @[Conditional.scala 39:67]
    node _GEN_297 = mux(_T_171, _T_1_18, _GEN_252) @[Conditional.scala 39:67]
    node _GEN_298 = mux(_T_171, _T_1_19, _GEN_253) @[Conditional.scala 39:67]
    node _GEN_299 = mux(_T_171, _T_1_20, _GEN_254) @[Conditional.scala 39:67]
    node _GEN_300 = mux(_T_171, _T_1_21, _GEN_255) @[Conditional.scala 39:67]
    node _GEN_301 = mux(_T_171, _T_1_22, _GEN_256) @[Conditional.scala 39:67]
    node _GEN_302 = mux(_T_171, _T_1_23, _GEN_257) @[Conditional.scala 39:67]
    node _GEN_303 = mux(_T_171, _T_1_24, _GEN_258) @[Conditional.scala 39:67]
    node _GEN_304 = mux(_T_171, _T_1_25, _GEN_259) @[Conditional.scala 39:67]
    node _GEN_305 = mux(_T_171, _T_1_26, _GEN_260) @[Conditional.scala 39:67]
    node _GEN_306 = mux(_T_171, _T_1_27, _GEN_261) @[Conditional.scala 39:67]
    node _GEN_307 = mux(_T_171, _T_1_28, _GEN_262) @[Conditional.scala 39:67]
    node _GEN_308 = mux(_T_171, _T_1_29, _GEN_263) @[Conditional.scala 39:67]
    node _GEN_309 = mux(_T_171, _T_1_30, _GEN_264) @[Conditional.scala 39:67]
    node _GEN_310 = mux(_T_171, _T_1_31, _GEN_265) @[Conditional.scala 39:67]
    node _GEN_311 = mux(_T_171, _T_9, _GEN_266) @[Conditional.scala 39:67]
    node _GEN_312 = mux(_T_171, _T_22, _GEN_267) @[Conditional.scala 39:67]
    node _GEN_313 = mux(_T_171, _T_98, _GEN_268) @[Conditional.scala 39:67]
    node _GEN_314 = mux(_T_171, _T_10, _GEN_269) @[Conditional.scala 39:67]
    node _GEN_315 = mux(_T_164, _GEN_69, _GEN_270) @[Conditional.scala 39:67]
    node _GEN_316 = mux(_T_164, _GEN_70, _GEN_272) @[Conditional.scala 39:67]
    node _GEN_317 = mux(_T_164, _GEN_71, _GEN_273) @[Conditional.scala 39:67]
    node _GEN_318 = mux(_T_164, _GEN_72, _GEN_274) @[Conditional.scala 39:67]
    node _GEN_319 = mux(_T_164, _GEN_73, _GEN_275) @[Conditional.scala 39:67]
    node _GEN_320 = mux(_T_164, _GEN_74, _GEN_312) @[Conditional.scala 39:67]
    node _GEN_321 = mux(_T_164, _GEN_75, _GEN_313) @[Conditional.scala 39:67]
    node _GEN_322 = mux(_T_164, _GEN_76, _GEN_311) @[Conditional.scala 39:67]
    node _GEN_323 = mux(_T_164, _GEN_77, _GEN_314) @[Conditional.scala 39:67]
    node _GEN_324 = mux(_T_164, _GEN_35, _GEN_271) @[Conditional.scala 39:67]
    node _GEN_325 = mux(_T_164, _T_28, _GEN_276) @[Conditional.scala 39:67]
    node _GEN_326 = mux(_T_164, _T_89, _GEN_277) @[Conditional.scala 39:67]
    node _GEN_327 = mux(_T_164, _T_29, _GEN_278) @[Conditional.scala 39:67]
    node _GEN_328 = mux(_T_164, _T_1_0, _GEN_279) @[Conditional.scala 39:67]
    node _GEN_329 = mux(_T_164, _T_1_1, _GEN_280) @[Conditional.scala 39:67]
    node _GEN_330 = mux(_T_164, _T_1_2, _GEN_281) @[Conditional.scala 39:67]
    node _GEN_331 = mux(_T_164, _T_1_3, _GEN_282) @[Conditional.scala 39:67]
    node _GEN_332 = mux(_T_164, _T_1_4, _GEN_283) @[Conditional.scala 39:67]
    node _GEN_333 = mux(_T_164, _T_1_5, _GEN_284) @[Conditional.scala 39:67]
    node _GEN_334 = mux(_T_164, _T_1_6, _GEN_285) @[Conditional.scala 39:67]
    node _GEN_335 = mux(_T_164, _T_1_7, _GEN_286) @[Conditional.scala 39:67]
    node _GEN_336 = mux(_T_164, _T_1_8, _GEN_287) @[Conditional.scala 39:67]
    node _GEN_337 = mux(_T_164, _T_1_9, _GEN_288) @[Conditional.scala 39:67]
    node _GEN_338 = mux(_T_164, _T_1_10, _GEN_289) @[Conditional.scala 39:67]
    node _GEN_339 = mux(_T_164, _T_1_11, _GEN_290) @[Conditional.scala 39:67]
    node _GEN_340 = mux(_T_164, _T_1_12, _GEN_291) @[Conditional.scala 39:67]
    node _GEN_341 = mux(_T_164, _T_1_13, _GEN_292) @[Conditional.scala 39:67]
    node _GEN_342 = mux(_T_164, _T_1_14, _GEN_293) @[Conditional.scala 39:67]
    node _GEN_343 = mux(_T_164, _T_1_15, _GEN_294) @[Conditional.scala 39:67]
    node _GEN_344 = mux(_T_164, _T_1_16, _GEN_295) @[Conditional.scala 39:67]
    node _GEN_345 = mux(_T_164, _T_1_17, _GEN_296) @[Conditional.scala 39:67]
    node _GEN_346 = mux(_T_164, _T_1_18, _GEN_297) @[Conditional.scala 39:67]
    node _GEN_347 = mux(_T_164, _T_1_19, _GEN_298) @[Conditional.scala 39:67]
    node _GEN_348 = mux(_T_164, _T_1_20, _GEN_299) @[Conditional.scala 39:67]
    node _GEN_349 = mux(_T_164, _T_1_21, _GEN_300) @[Conditional.scala 39:67]
    node _GEN_350 = mux(_T_164, _T_1_22, _GEN_301) @[Conditional.scala 39:67]
    node _GEN_351 = mux(_T_164, _T_1_23, _GEN_302) @[Conditional.scala 39:67]
    node _GEN_352 = mux(_T_164, _T_1_24, _GEN_303) @[Conditional.scala 39:67]
    node _GEN_353 = mux(_T_164, _T_1_25, _GEN_304) @[Conditional.scala 39:67]
    node _GEN_354 = mux(_T_164, _T_1_26, _GEN_305) @[Conditional.scala 39:67]
    node _GEN_355 = mux(_T_164, _T_1_27, _GEN_306) @[Conditional.scala 39:67]
    node _GEN_356 = mux(_T_164, _T_1_28, _GEN_307) @[Conditional.scala 39:67]
    node _GEN_357 = mux(_T_164, _T_1_29, _GEN_308) @[Conditional.scala 39:67]
    node _GEN_358 = mux(_T_164, _T_1_30, _GEN_309) @[Conditional.scala 39:67]
    node _GEN_359 = mux(_T_164, _T_1_31, _GEN_310) @[Conditional.scala 39:67]
    node _GEN_360 = mux(_T_163, _GEN_50, _GEN_315) @[Conditional.scala 39:67]
    node _GEN_361 = mux(_T_163, _GEN_51, _GEN_322) @[Conditional.scala 39:67]
    node _GEN_362 = mux(_T_163, _GEN_52, _GEN_324) @[Conditional.scala 39:67]
    node _GEN_363 = mux(_T_163, _GEN_53, _GEN_316) @[Conditional.scala 39:67]
    node _GEN_364 = mux(_T_163, _GEN_54, _GEN_317) @[Conditional.scala 39:67]
    node _GEN_365 = mux(_T_163, _GEN_55, _GEN_318) @[Conditional.scala 39:67]
    node _GEN_366 = mux(_T_163, _GEN_56, _GEN_319) @[Conditional.scala 39:67]
    node _GEN_367 = mux(_T_163, _GEN_57, _T_11) @[Conditional.scala 39:67]
    node _GEN_368 = mux(_T_163, _T_22, _GEN_320) @[Conditional.scala 39:67]
    node _GEN_369 = mux(_T_163, _T_98, _GEN_321) @[Conditional.scala 39:67]
    node _GEN_370 = mux(_T_163, _T_10, _GEN_323) @[Conditional.scala 39:67]
    node _GEN_371 = mux(_T_163, _T_28, _GEN_325) @[Conditional.scala 39:67]
    node _GEN_372 = mux(_T_163, _T_89, _GEN_326) @[Conditional.scala 39:67]
    node _GEN_373 = mux(_T_163, _T_29, _GEN_327) @[Conditional.scala 39:67]
    node _GEN_374 = mux(_T_163, _T_1_0, _GEN_328) @[Conditional.scala 39:67]
    node _GEN_375 = mux(_T_163, _T_1_1, _GEN_329) @[Conditional.scala 39:67]
    node _GEN_376 = mux(_T_163, _T_1_2, _GEN_330) @[Conditional.scala 39:67]
    node _GEN_377 = mux(_T_163, _T_1_3, _GEN_331) @[Conditional.scala 39:67]
    node _GEN_378 = mux(_T_163, _T_1_4, _GEN_332) @[Conditional.scala 39:67]
    node _GEN_379 = mux(_T_163, _T_1_5, _GEN_333) @[Conditional.scala 39:67]
    node _GEN_380 = mux(_T_163, _T_1_6, _GEN_334) @[Conditional.scala 39:67]
    node _GEN_381 = mux(_T_163, _T_1_7, _GEN_335) @[Conditional.scala 39:67]
    node _GEN_382 = mux(_T_163, _T_1_8, _GEN_336) @[Conditional.scala 39:67]
    node _GEN_383 = mux(_T_163, _T_1_9, _GEN_337) @[Conditional.scala 39:67]
    node _GEN_384 = mux(_T_163, _T_1_10, _GEN_338) @[Conditional.scala 39:67]
    node _GEN_385 = mux(_T_163, _T_1_11, _GEN_339) @[Conditional.scala 39:67]
    node _GEN_386 = mux(_T_163, _T_1_12, _GEN_340) @[Conditional.scala 39:67]
    node _GEN_387 = mux(_T_163, _T_1_13, _GEN_341) @[Conditional.scala 39:67]
    node _GEN_388 = mux(_T_163, _T_1_14, _GEN_342) @[Conditional.scala 39:67]
    node _GEN_389 = mux(_T_163, _T_1_15, _GEN_343) @[Conditional.scala 39:67]
    node _GEN_390 = mux(_T_163, _T_1_16, _GEN_344) @[Conditional.scala 39:67]
    node _GEN_391 = mux(_T_163, _T_1_17, _GEN_345) @[Conditional.scala 39:67]
    node _GEN_392 = mux(_T_163, _T_1_18, _GEN_346) @[Conditional.scala 39:67]
    node _GEN_393 = mux(_T_163, _T_1_19, _GEN_347) @[Conditional.scala 39:67]
    node _GEN_394 = mux(_T_163, _T_1_20, _GEN_348) @[Conditional.scala 39:67]
    node _GEN_395 = mux(_T_163, _T_1_21, _GEN_349) @[Conditional.scala 39:67]
    node _GEN_396 = mux(_T_163, _T_1_22, _GEN_350) @[Conditional.scala 39:67]
    node _GEN_397 = mux(_T_163, _T_1_23, _GEN_351) @[Conditional.scala 39:67]
    node _GEN_398 = mux(_T_163, _T_1_24, _GEN_352) @[Conditional.scala 39:67]
    node _GEN_399 = mux(_T_163, _T_1_25, _GEN_353) @[Conditional.scala 39:67]
    node _GEN_400 = mux(_T_163, _T_1_26, _GEN_354) @[Conditional.scala 39:67]
    node _GEN_401 = mux(_T_163, _T_1_27, _GEN_355) @[Conditional.scala 39:67]
    node _GEN_402 = mux(_T_163, _T_1_28, _GEN_356) @[Conditional.scala 39:67]
    node _GEN_403 = mux(_T_163, _T_1_29, _GEN_357) @[Conditional.scala 39:67]
    node _GEN_404 = mux(_T_163, _T_1_30, _GEN_358) @[Conditional.scala 39:67]
    node _GEN_405 = mux(_T_163, _T_1_31, _GEN_359) @[Conditional.scala 39:67]
    node _GEN_406 = mux(_T_160, _GEN_43, _GEN_360) @[Conditional.scala 39:67]
    node _GEN_407 = mux(_T_160, _GEN_44, _GEN_363) @[Conditional.scala 39:67]
    node _GEN_408 = mux(_T_160, _GEN_45, _GEN_364) @[Conditional.scala 39:67]
    node _GEN_409 = mux(_T_160, _GEN_46, _GEN_365) @[Conditional.scala 39:67]
    node _GEN_410 = mux(_T_160, _GEN_46, _GEN_366) @[Conditional.scala 39:67]
    node _GEN_411 = mux(_T_160, _GEN_47, _GEN_368) @[Conditional.scala 39:67]
    node _GEN_412 = mux(_T_160, _GEN_48, _GEN_369) @[Conditional.scala 39:67]
    node _GEN_413 = mux(_T_160, _GEN_49, _GEN_370) @[Conditional.scala 39:67]
    node _GEN_414 = mux(_T_160, _T_9, _GEN_361) @[Conditional.scala 39:67]
    node _GEN_415 = mux(_T_160, _GEN_35, _GEN_362) @[Conditional.scala 39:67]
    node _GEN_416 = mux(_T_160, _T_11, _GEN_367) @[Conditional.scala 39:67]
    node _GEN_417 = mux(_T_160, _T_28, _GEN_371) @[Conditional.scala 39:67]
    node _GEN_418 = mux(_T_160, _T_89, _GEN_372) @[Conditional.scala 39:67]
    node _GEN_419 = mux(_T_160, _T_29, _GEN_373) @[Conditional.scala 39:67]
    node _GEN_420 = mux(_T_160, _T_1_0, _GEN_374) @[Conditional.scala 39:67]
    node _GEN_421 = mux(_T_160, _T_1_1, _GEN_375) @[Conditional.scala 39:67]
    node _GEN_422 = mux(_T_160, _T_1_2, _GEN_376) @[Conditional.scala 39:67]
    node _GEN_423 = mux(_T_160, _T_1_3, _GEN_377) @[Conditional.scala 39:67]
    node _GEN_424 = mux(_T_160, _T_1_4, _GEN_378) @[Conditional.scala 39:67]
    node _GEN_425 = mux(_T_160, _T_1_5, _GEN_379) @[Conditional.scala 39:67]
    node _GEN_426 = mux(_T_160, _T_1_6, _GEN_380) @[Conditional.scala 39:67]
    node _GEN_427 = mux(_T_160, _T_1_7, _GEN_381) @[Conditional.scala 39:67]
    node _GEN_428 = mux(_T_160, _T_1_8, _GEN_382) @[Conditional.scala 39:67]
    node _GEN_429 = mux(_T_160, _T_1_9, _GEN_383) @[Conditional.scala 39:67]
    node _GEN_430 = mux(_T_160, _T_1_10, _GEN_384) @[Conditional.scala 39:67]
    node _GEN_431 = mux(_T_160, _T_1_11, _GEN_385) @[Conditional.scala 39:67]
    node _GEN_432 = mux(_T_160, _T_1_12, _GEN_386) @[Conditional.scala 39:67]
    node _GEN_433 = mux(_T_160, _T_1_13, _GEN_387) @[Conditional.scala 39:67]
    node _GEN_434 = mux(_T_160, _T_1_14, _GEN_388) @[Conditional.scala 39:67]
    node _GEN_435 = mux(_T_160, _T_1_15, _GEN_389) @[Conditional.scala 39:67]
    node _GEN_436 = mux(_T_160, _T_1_16, _GEN_390) @[Conditional.scala 39:67]
    node _GEN_437 = mux(_T_160, _T_1_17, _GEN_391) @[Conditional.scala 39:67]
    node _GEN_438 = mux(_T_160, _T_1_18, _GEN_392) @[Conditional.scala 39:67]
    node _GEN_439 = mux(_T_160, _T_1_19, _GEN_393) @[Conditional.scala 39:67]
    node _GEN_440 = mux(_T_160, _T_1_20, _GEN_394) @[Conditional.scala 39:67]
    node _GEN_441 = mux(_T_160, _T_1_21, _GEN_395) @[Conditional.scala 39:67]
    node _GEN_442 = mux(_T_160, _T_1_22, _GEN_396) @[Conditional.scala 39:67]
    node _GEN_443 = mux(_T_160, _T_1_23, _GEN_397) @[Conditional.scala 39:67]
    node _GEN_444 = mux(_T_160, _T_1_24, _GEN_398) @[Conditional.scala 39:67]
    node _GEN_445 = mux(_T_160, _T_1_25, _GEN_399) @[Conditional.scala 39:67]
    node _GEN_446 = mux(_T_160, _T_1_26, _GEN_400) @[Conditional.scala 39:67]
    node _GEN_447 = mux(_T_160, _T_1_27, _GEN_401) @[Conditional.scala 39:67]
    node _GEN_448 = mux(_T_160, _T_1_28, _GEN_402) @[Conditional.scala 39:67]
    node _GEN_449 = mux(_T_160, _T_1_29, _GEN_403) @[Conditional.scala 39:67]
    node _GEN_450 = mux(_T_160, _T_1_30, _GEN_404) @[Conditional.scala 39:67]
    node _GEN_451 = mux(_T_160, _T_1_31, _GEN_405) @[Conditional.scala 39:67]
    node _GEN_452 = mux(_T_159, _GEN_36, _GEN_406) @[Conditional.scala 40:58]
    node _GEN_453 = mux(_T_159, _GEN_37, _GEN_407) @[Conditional.scala 40:58]
    node _GEN_454 = mux(_T_159, _GEN_38, _GEN_408) @[Conditional.scala 40:58]
    node _GEN_455 = mux(_T_159, _GEN_39, _GEN_409) @[Conditional.scala 40:58]
    node _GEN_456 = mux(_T_159, _GEN_40, _GEN_410) @[Conditional.scala 40:58]
    node _GEN_457 = mux(_T_159, _GEN_41, _GEN_411) @[Conditional.scala 40:58]
    node _GEN_458 = mux(_T_159, _GEN_42, _GEN_416) @[Conditional.scala 40:58]
    node _GEN_459 = mux(_T_159, _T_98, _GEN_412) @[Conditional.scala 40:58]
    node _GEN_460 = mux(_T_159, _T_10, _GEN_413) @[Conditional.scala 40:58]
    node _GEN_461 = mux(_T_159, _T_9, _GEN_414) @[Conditional.scala 40:58]
    node _GEN_462 = mux(_T_159, _GEN_35, _GEN_415) @[Conditional.scala 40:58]
    node _GEN_463 = mux(_T_159, _T_28, _GEN_417) @[Conditional.scala 40:58]
    node _GEN_464 = mux(_T_159, _T_89, _GEN_418) @[Conditional.scala 40:58]
    node _GEN_465 = mux(_T_159, _T_29, _GEN_419) @[Conditional.scala 40:58]
    node _GEN_466 = mux(_T_159, _T_1_0, _GEN_420) @[Conditional.scala 40:58]
    node _GEN_467 = mux(_T_159, _T_1_1, _GEN_421) @[Conditional.scala 40:58]
    node _GEN_468 = mux(_T_159, _T_1_2, _GEN_422) @[Conditional.scala 40:58]
    node _GEN_469 = mux(_T_159, _T_1_3, _GEN_423) @[Conditional.scala 40:58]
    node _GEN_470 = mux(_T_159, _T_1_4, _GEN_424) @[Conditional.scala 40:58]
    node _GEN_471 = mux(_T_159, _T_1_5, _GEN_425) @[Conditional.scala 40:58]
    node _GEN_472 = mux(_T_159, _T_1_6, _GEN_426) @[Conditional.scala 40:58]
    node _GEN_473 = mux(_T_159, _T_1_7, _GEN_427) @[Conditional.scala 40:58]
    node _GEN_474 = mux(_T_159, _T_1_8, _GEN_428) @[Conditional.scala 40:58]
    node _GEN_475 = mux(_T_159, _T_1_9, _GEN_429) @[Conditional.scala 40:58]
    node _GEN_476 = mux(_T_159, _T_1_10, _GEN_430) @[Conditional.scala 40:58]
    node _GEN_477 = mux(_T_159, _T_1_11, _GEN_431) @[Conditional.scala 40:58]
    node _GEN_478 = mux(_T_159, _T_1_12, _GEN_432) @[Conditional.scala 40:58]
    node _GEN_479 = mux(_T_159, _T_1_13, _GEN_433) @[Conditional.scala 40:58]
    node _GEN_480 = mux(_T_159, _T_1_14, _GEN_434) @[Conditional.scala 40:58]
    node _GEN_481 = mux(_T_159, _T_1_15, _GEN_435) @[Conditional.scala 40:58]
    node _GEN_482 = mux(_T_159, _T_1_16, _GEN_436) @[Conditional.scala 40:58]
    node _GEN_483 = mux(_T_159, _T_1_17, _GEN_437) @[Conditional.scala 40:58]
    node _GEN_484 = mux(_T_159, _T_1_18, _GEN_438) @[Conditional.scala 40:58]
    node _GEN_485 = mux(_T_159, _T_1_19, _GEN_439) @[Conditional.scala 40:58]
    node _GEN_486 = mux(_T_159, _T_1_20, _GEN_440) @[Conditional.scala 40:58]
    node _GEN_487 = mux(_T_159, _T_1_21, _GEN_441) @[Conditional.scala 40:58]
    node _GEN_488 = mux(_T_159, _T_1_22, _GEN_442) @[Conditional.scala 40:58]
    node _GEN_489 = mux(_T_159, _T_1_23, _GEN_443) @[Conditional.scala 40:58]
    node _GEN_490 = mux(_T_159, _T_1_24, _GEN_444) @[Conditional.scala 40:58]
    node _GEN_491 = mux(_T_159, _T_1_25, _GEN_445) @[Conditional.scala 40:58]
    node _GEN_492 = mux(_T_159, _T_1_26, _GEN_446) @[Conditional.scala 40:58]
    node _GEN_493 = mux(_T_159, _T_1_27, _GEN_447) @[Conditional.scala 40:58]
    node _GEN_494 = mux(_T_159, _T_1_28, _GEN_448) @[Conditional.scala 40:58]
    node _GEN_495 = mux(_T_159, _T_1_29, _GEN_449) @[Conditional.scala 40:58]
    node _GEN_496 = mux(_T_159, _T_1_30, _GEN_450) @[Conditional.scala 40:58]
    node _GEN_497 = mux(_T_159, _T_1_31, _GEN_451) @[Conditional.scala 40:58]
    node _T__0 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__1 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__2 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__3 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__4 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__5 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__6 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__7 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__8 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__9 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__10 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__11 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__12 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__13 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__14 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__15 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__16 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__17 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__18 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__19 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__20 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__21 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__22 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__23 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__24 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__25 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__26 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__27 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__28 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__29 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__30 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T__31 = UInt<20>("h0") @[ProcessingElement.scala 134:54 ProcessingElement.scala 134:54]
    node _T_2 = SPadAdrModule.io_dataPath_columnNum @[ProcessingElement.scala 140:45 ProcessingElement.scala 201:21]
    node _T_7 = _T_124 @[ProcessingElement.scala 145:50 ProcessingElement.scala 276:26]
    node _T_8 = _T_137 @[ProcessingElement.scala 146:51 ProcessingElement.scala 279:27]
    node _T_14 = WeightSPadAdrModule.io_dataPath_columnNum @[ProcessingElement.scala 153:46 ProcessingElement.scala 221:22]
    node _T_19 = _T_127 @[ProcessingElement.scala 158:51 ProcessingElement.scala 277:27]
    node _T_20 = _T_144 @[ProcessingElement.scala 159:52 ProcessingElement.scala 280:28]
    node _T_24 = _T_152 @[ProcessingElement.scala 164:52 ProcessingElement.scala 283:28]
    node _T_25 = _T_155 @[ProcessingElement.scala 165:47 ProcessingElement.scala 284:23]
    node _T_26 = _T_146 @[ProcessingElement.scala 166:46 ProcessingElement.scala 281:22]
    node _T_103 = _T_118 @[ProcessingElement.scala 268:49 ProcessingElement.scala 275:25]
    io_padCtrl_fromTopIO_pSumEnqOrProduct_ready <= _T_35 @[ProcessingElement.scala 253:47]
    io_padCtrl_fromTopIO_writeFinish is invalid
    io_padCtrl_fromTopIO_calFinish <= _T_102 @[ProcessingElement.scala 285:34]
    io_dataStream_ipsIO_ready <= _GEN_464 @[ProcessingElement.scala 242:29 ProcessingElement.scala 342:37]
    io_dataStream_opsIO_valid <= _GEN_34 @[ProcessingElement.scala 246:31 ProcessingElement.scala 249:31]
    io_dataStream_opsIO_bits <= _GEN_33 @[ProcessingElement.scala 245:30]
    io_dataStream_inActIOs_adrIOs_data_ready <= SPadAdrModule.io_dataPath_writeInData_data_ready @[ProcessingElement.scala 200:40]
    io_dataStream_inActIOs_dataIOs_data_ready <= SPadDataModule.io_dataPath_writeInData_data_ready @[ProcessingElement.scala 209:41]
    io_dataStream_weightIOs_adrIOs_data_ready <= WeightSPadAdrModule.io_dataPath_writeInData_data_ready @[ProcessingElement.scala 220:41]
    io_dataStream_weightIOs_dataIOs_data_ready <= SPadDataModule_1.io_dataPath_writeInData_data_ready @[ProcessingElement.scala 231:42]
    io_debugIO_inActMatrixData <= _T_13 @[ProcessingElement.scala 378:32]
    io_debugIO_inActMatrixRow <= _T_12 @[ProcessingElement.scala 379:31]
    io_debugIO_inActMatrixColumn <= _T_9 @[ProcessingElement.scala 377:34]
    io_debugIO_inActAdrInc <= _T_7 @[ProcessingElement.scala 381:28]
    io_debugIO_inActDataInc <= _T_8 @[ProcessingElement.scala 382:29]
    io_debugIO_inActAdrIdx <= _T_2 @[ProcessingElement.scala 380:28]
    io_debugIO_weightAdrSPadReadOut <= _T_15 @[ProcessingElement.scala 385:37]
    io_debugIO_weightMatrixData <= _T_21 @[ProcessingElement.scala 383:33]
    io_debugIO_weightMatrixRow <= _T_43 @[ProcessingElement.scala 384:32]
    io_debugIO_productResult <= _T_28 @[ProcessingElement.scala 386:30]
    io_debugIO_pSumResult <= _T_30 @[ProcessingElement.scala 387:27]
    io_debugIO_pSumLoad <= _T_29 @[ProcessingElement.scala 388:25]
    io_debugIO_weightAdrInIdx <= _T_24 @[ProcessingElement.scala 389:31]
    io_debugIO_sPadState <= _T_31 @[ProcessingElement.scala 390:26]
    io_padWF_inActWriteFin_adrWriteFin <= SPadAdrModule.io_ctrlPath_writeFin @[ProcessingElement.scala 203:38]
    io_padWF_inActWriteFin_dataWriteFin <= SPadDataModule.io_ctrlPath_writeFin @[ProcessingElement.scala 214:39]
    io_padWF_weightWriteFin_adrWriteFin <= WeightSPadAdrModule.io_ctrlPath_writeFin @[ProcessingElement.scala 223:39]
    io_padWF_weightWriteFin_dataWriteFin <= SPadDataModule_1.io_ctrlPath_writeFin @[ProcessingElement.scala 236:40]
    _T_1_0 <= mux(reset, _T__0, _GEN_466) @[ProcessingElement.scala 352:37]
    _T_1_1 <= mux(reset, _T__1, _GEN_467) @[ProcessingElement.scala 352:37]
    _T_1_2 <= mux(reset, _T__2, _GEN_468) @[ProcessingElement.scala 352:37]
    _T_1_3 <= mux(reset, _T__3, _GEN_469) @[ProcessingElement.scala 352:37]
    _T_1_4 <= mux(reset, _T__4, _GEN_470) @[ProcessingElement.scala 352:37]
    _T_1_5 <= mux(reset, _T__5, _GEN_471) @[ProcessingElement.scala 352:37]
    _T_1_6 <= mux(reset, _T__6, _GEN_472) @[ProcessingElement.scala 352:37]
    _T_1_7 <= mux(reset, _T__7, _GEN_473) @[ProcessingElement.scala 352:37]
    _T_1_8 <= mux(reset, _T__8, _GEN_474) @[ProcessingElement.scala 352:37]
    _T_1_9 <= mux(reset, _T__9, _GEN_475) @[ProcessingElement.scala 352:37]
    _T_1_10 <= mux(reset, _T__10, _GEN_476) @[ProcessingElement.scala 352:37]
    _T_1_11 <= mux(reset, _T__11, _GEN_477) @[ProcessingElement.scala 352:37]
    _T_1_12 <= mux(reset, _T__12, _GEN_478) @[ProcessingElement.scala 352:37]
    _T_1_13 <= mux(reset, _T__13, _GEN_479) @[ProcessingElement.scala 352:37]
    _T_1_14 <= mux(reset, _T__14, _GEN_480) @[ProcessingElement.scala 352:37]
    _T_1_15 <= mux(reset, _T__15, _GEN_481) @[ProcessingElement.scala 352:37]
    _T_1_16 <= mux(reset, _T__16, _GEN_482) @[ProcessingElement.scala 352:37]
    _T_1_17 <= mux(reset, _T__17, _GEN_483) @[ProcessingElement.scala 352:37]
    _T_1_18 <= mux(reset, _T__18, _GEN_484) @[ProcessingElement.scala 352:37]
    _T_1_19 <= mux(reset, _T__19, _GEN_485) @[ProcessingElement.scala 352:37]
    _T_1_20 <= mux(reset, _T__20, _GEN_486) @[ProcessingElement.scala 352:37]
    _T_1_21 <= mux(reset, _T__21, _GEN_487) @[ProcessingElement.scala 352:37]
    _T_1_22 <= mux(reset, _T__22, _GEN_488) @[ProcessingElement.scala 352:37]
    _T_1_23 <= mux(reset, _T__23, _GEN_489) @[ProcessingElement.scala 352:37]
    _T_1_24 <= mux(reset, _T__24, _GEN_490) @[ProcessingElement.scala 352:37]
    _T_1_25 <= mux(reset, _T__25, _GEN_491) @[ProcessingElement.scala 352:37]
    _T_1_26 <= mux(reset, _T__26, _GEN_492) @[ProcessingElement.scala 352:37]
    _T_1_27 <= mux(reset, _T__27, _GEN_493) @[ProcessingElement.scala 352:37]
    _T_1_28 <= mux(reset, _T__28, _GEN_494) @[ProcessingElement.scala 352:37]
    _T_1_29 <= mux(reset, _T__29, _GEN_495) @[ProcessingElement.scala 352:37]
    _T_1_30 <= mux(reset, _T__30, _GEN_496) @[ProcessingElement.scala 352:37]
    _T_1_31 <= mux(reset, _T__31, _GEN_497) @[ProcessingElement.scala 352:37]
    SPadAdrModule.clock <= clock
    SPadAdrModule.reset <= reset
    SPadAdrModule.io_dataPath_writeInData_data_valid <= io_dataStream_inActIOs_adrIOs_data_valid @[ProcessingElement.scala 200:40]
    SPadAdrModule.io_dataPath_writeInData_data_bits <= io_dataStream_inActIOs_adrIOs_data_bits @[ProcessingElement.scala 200:40]
    SPadAdrModule.io_ctrlPath_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[ProcessingElement.scala 197:37]
    SPadAdrModule.io_ctrlPath_readEn <= _T_5 @[ProcessingElement.scala 204:35]
    SPadAdrModule.io_ctrlPath_readInIdx is invalid
    SPadAdrModule.io_ctrlPath_indexInc <= _T_7 @[ProcessingElement.scala 206:37]
    SPadAdrModule.io_ctrlPath_readInIdxEn is invalid
    SPadDataModule.clock <= clock
    SPadDataModule.reset <= reset
    SPadDataModule.io_dataPath_writeInData_data_valid <= io_dataStream_inActIOs_dataIOs_data_valid @[ProcessingElement.scala 209:41]
    SPadDataModule.io_dataPath_writeInData_data_bits <= io_dataStream_inActIOs_dataIOs_data_bits @[ProcessingElement.scala 209:41]
    SPadDataModule.io_ctrlPath_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[ProcessingElement.scala 197:37]
    SPadDataModule.io_ctrlPath_readEn <= _T_6 @[ProcessingElement.scala 215:36]
    SPadDataModule.io_ctrlPath_readInIdx <= _T_3 @[ProcessingElement.scala 216:39]
    SPadDataModule.io_ctrlPath_indexInc <= _T_8 @[ProcessingElement.scala 217:38]
    SPadDataModule.io_ctrlPath_readInIdxEn is invalid
    WeightSPadAdrModule.clock <= clock
    WeightSPadAdrModule.reset <= reset
    WeightSPadAdrModule.io_dataPath_writeInData_data_valid <= io_dataStream_weightIOs_adrIOs_data_valid @[ProcessingElement.scala 220:41]
    WeightSPadAdrModule.io_dataPath_writeInData_data_bits <= io_dataStream_weightIOs_adrIOs_data_bits @[ProcessingElement.scala 220:41]
    WeightSPadAdrModule.io_ctrlPath_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[ProcessingElement.scala 197:37]
    WeightSPadAdrModule.io_ctrlPath_readEn <= _T_17 @[ProcessingElement.scala 224:36]
    WeightSPadAdrModule.io_ctrlPath_readInIdx <= _T_24 @[ProcessingElement.scala 225:39]
    WeightSPadAdrModule.io_ctrlPath_indexInc <= _T_19 @[ProcessingElement.scala 228:38]
    WeightSPadAdrModule.io_ctrlPath_readInIdxEn <= _T_26 @[ProcessingElement.scala 229:41]
    SPadDataModule_1.clock <= clock
    SPadDataModule_1.reset <= reset
    SPadDataModule_1.io_dataPath_writeInData_data_valid <= io_dataStream_weightIOs_dataIOs_data_valid @[ProcessingElement.scala 231:42]
    SPadDataModule_1.io_dataPath_writeInData_data_bits <= io_dataStream_weightIOs_dataIOs_data_bits @[ProcessingElement.scala 231:42]
    SPadDataModule_1.io_ctrlPath_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[ProcessingElement.scala 197:37]
    SPadDataModule_1.io_ctrlPath_readEn <= _T_6 @[ProcessingElement.scala 237:37]
    SPadDataModule_1.io_ctrlPath_readInIdx <= _T_88 @[ProcessingElement.scala 238:40]
    SPadDataModule_1.io_ctrlPath_indexInc <= _T_20 @[ProcessingElement.scala 239:39]
    SPadDataModule_1.io_ctrlPath_readInIdxEn <= _T_25 @[ProcessingElement.scala 240:42]
    _T_5 <= mux(reset, UInt<1>("h0"), _GEN_453) @[ProcessingElement.scala 95:27 ProcessingElement.scala 95:27 ProcessingElement.scala 103:27 ProcessingElement.scala 111:27 ProcessingElement.scala 95:27 ProcessingElement.scala 103:27 ProcessingElement.scala 118:27 ProcessingElement.scala 124:27 ProcessingElement.scala 95:27 ProcessingElement.scala 103:27 ProcessingElement.scala 118:27]
    _T_6 <= mux(reset, UInt<1>("h0"), _GEN_454) @[ProcessingElement.scala 96:28 ProcessingElement.scala 96:28 ProcessingElement.scala 104:28 ProcessingElement.scala 112:28 ProcessingElement.scala 96:28 ProcessingElement.scala 104:28 ProcessingElement.scala 119:28 ProcessingElement.scala 125:28 ProcessingElement.scala 96:28 ProcessingElement.scala 104:28 ProcessingElement.scala 119:28]
    _T_9 <= mux(reset, UInt<4>("h0"), _GEN_461) @[ProcessingElement.scala 131:26 ProcessingElement.scala 317:34 ProcessingElement.scala 320:34 ProcessingElement.scala 131:26 ProcessingElement.scala 361:36 ProcessingElement.scala 364:36]
    _T_10 <= mux(reset, UInt<4>("h0"), _GEN_460) @[ProcessingElement.scala 298:31 ProcessingElement.scala 318:35 ProcessingElement.scala 362:37]
    _T_11 <= mux(reset, UInt<1>("h1"), _GEN_458) @[ProcessingElement.scala 291:35 ProcessingElement.scala 308:35]
    _T_17 <= mux(reset, UInt<1>("h0"), _GEN_455) @[ProcessingElement.scala 97:28 ProcessingElement.scala 97:28 ProcessingElement.scala 105:28 ProcessingElement.scala 113:28 ProcessingElement.scala 97:28 ProcessingElement.scala 105:28 ProcessingElement.scala 120:28 ProcessingElement.scala 126:28 ProcessingElement.scala 97:28 ProcessingElement.scala 105:28 ProcessingElement.scala 120:28]
    _T_18 <= mux(reset, UInt<1>("h0"), _GEN_456) @[ProcessingElement.scala 98:29 ProcessingElement.scala 98:29 ProcessingElement.scala 106:29 ProcessingElement.scala 114:29 ProcessingElement.scala 98:29 ProcessingElement.scala 106:29 ProcessingElement.scala 121:29 ProcessingElement.scala 127:29 ProcessingElement.scala 98:29 ProcessingElement.scala 106:29 ProcessingElement.scala 121:29]
    _T_22 <= mux(reset, UInt<1>("h1"), _GEN_457) @[ProcessingElement.scala 99:29 ProcessingElement.scala 99:29 ProcessingElement.scala 107:29 ProcessingElement.scala 99:29 ProcessingElement.scala 107:29 ProcessingElement.scala 99:29 ProcessingElement.scala 107:29 ProcessingElement.scala 371:35]
    _T_28 <= mux(reset, UInt<20>("h0"), _GEN_463) @[ProcessingElement.scala 341:22 ProcessingElement.scala 347:20]
    _T_29 <= mux(reset, UInt<20>("h0"), _GEN_465) @[ProcessingElement.scala 343:27 ProcessingElement.scala 348:25]
    _T_31 <= mux(reset, UInt<3>("h0"), _GEN_452) @[ProcessingElement.scala 94:10 ProcessingElement.scala 94:10 ProcessingElement.scala 102:10 ProcessingElement.scala 130:10 ProcessingElement.scala 110:10 ProcessingElement.scala 94:10 ProcessingElement.scala 102:10 ProcessingElement.scala 117:10 ProcessingElement.scala 330:12 ProcessingElement.scala 333:12 ProcessingElement.scala 340:16 ProcessingElement.scala 346:14 ProcessingElement.scala 130:10 ProcessingElement.scala 94:10 ProcessingElement.scala 102:10 ProcessingElement.scala 117:10]
    value <= mux(reset, UInt<5>("h0"), _GEN_462) @[Counter.scala 38:13 Counter.scala 40:29 ProcessingElement.scala 132:31 ProcessingElement.scala 334:33 ProcessingElement.scala 132:31]
    _T_98 <= mux(reset, UInt<1>("h0"), _GEN_459) @[ProcessingElement.scala 297:32 ProcessingElement.scala 316:36 ProcessingElement.scala 360:38]

  module Queue :
    input clock : Clock
    input reset : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits : UInt<4>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits : UInt<4>
    output io_count : UInt<3>
  
    mem _T : @[Decoupled.scala 209:24]
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

  module Queue_1 :
    input clock : Clock
    input reset : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits : UInt<12>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits : UInt<12>
    output io_count : UInt<3>
  
    mem _T : @[Decoupled.scala 209:24]
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

  module Queue_2 :
    input clock : Clock
    input reset : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits : UInt<7>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits : UInt<7>
    output io_count : UInt<3>
  
    mem _T : @[Decoupled.scala 209:24]
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
    output io_dataStream_inActIOs_adrIOs_data_ready : UInt<1>
    input io_dataStream_inActIOs_adrIOs_data_valid : UInt<1>
    input io_dataStream_inActIOs_adrIOs_data_bits : UInt<4>
    output io_dataStream_inActIOs_dataIOs_data_ready : UInt<1>
    input io_dataStream_inActIOs_dataIOs_data_valid : UInt<1>
    input io_dataStream_inActIOs_dataIOs_data_bits : UInt<12>
    output io_dataStream_weightIOs_adrIOs_data_ready : UInt<1>
    input io_dataStream_weightIOs_adrIOs_data_valid : UInt<1>
    input io_dataStream_weightIOs_adrIOs_data_bits : UInt<7>
    output io_dataStream_weightIOs_dataIOs_data_ready : UInt<1>
    input io_dataStream_weightIOs_dataIOs_data_valid : UInt<1>
    input io_dataStream_weightIOs_dataIOs_data_bits : UInt<12>
    output io_padWF_inActWriteFin_adrWriteFin : UInt<1>
    output io_padWF_inActWriteFin_dataWriteFin : UInt<1>
    output io_padWF_weightWriteFin_adrWriteFin : UInt<1>
    output io_padWF_weightWriteFin_dataWriteFin : UInt<1>
    output io_topCtrl_pSumEnqOrProduct_ready : UInt<1>
    input io_topCtrl_pSumEnqOrProduct_valid : UInt<1>
    input io_topCtrl_pSumEnqOrProduct_bits : UInt<1>
    input io_topCtrl_doLoadEn : UInt<1>
    output io_topCtrl_writeFinish : UInt<1>
    output io_topCtrl_calFinish : UInt<1>
    output io_debugIO_peControlDebugIO_peState : UInt<2>
    output io_debugIO_peControlDebugIO_doMACEnDebug : UInt<1>
    output io_debugIO_peSPadDebugIO_inActMatrixData : UInt<8>
    output io_debugIO_peSPadDebugIO_inActMatrixRow : UInt<4>
    output io_debugIO_peSPadDebugIO_inActMatrixColumn : UInt<4>
    output io_debugIO_peSPadDebugIO_inActAdrInc : UInt<1>
    output io_debugIO_peSPadDebugIO_inActDataInc : UInt<1>
    output io_debugIO_peSPadDebugIO_inActAdrIdx : UInt<4>
    output io_debugIO_peSPadDebugIO_weightAdrSPadReadOut : UInt<7>
    output io_debugIO_peSPadDebugIO_weightMatrixData : UInt<8>
    output io_debugIO_peSPadDebugIO_weightMatrixRow : UInt<4>
    output io_debugIO_peSPadDebugIO_productResult : UInt<20>
    output io_debugIO_peSPadDebugIO_pSumResult : UInt<20>
    output io_debugIO_peSPadDebugIO_pSumLoad : UInt<20>
    output io_debugIO_peSPadDebugIO_weightAdrInIdx : UInt<4>
    output io_debugIO_peSPadDebugIO_sPadState : UInt<3>
  
    inst ProcessingElementControl of ProcessingElementControl @[ProcessingElement.scala 8:56]
    inst ProcessingElementPad of ProcessingElementPad @[ProcessingElement.scala 9:51]
    inst Queue of Queue @[Decoupled.scala 287:21]
    inst Queue_1 of Queue_1 @[Decoupled.scala 287:21]
    inst Queue_2 of Queue_2 @[Decoupled.scala 287:21]
    inst Queue_3 of Queue_1 @[Decoupled.scala 287:21]
    inst Queue_4 of Queue_4 @[Decoupled.scala 287:21]
    inst Queue_5 of Queue_4 @[Decoupled.scala 287:21]
    reg _T_2_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_2_0) @[ProcessingElement.scala 28:53]
    reg _T_2_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_2_1) @[ProcessingElement.scala 28:53]
    reg _T_2_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_2_2) @[ProcessingElement.scala 28:53]
    reg _T_2_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_2_3) @[ProcessingElement.scala 28:53]
    node _GEN_0 = mux(ProcessingElementPad.io_padWF_inActWriteFin_adrWriteFin, UInt<1>("h1"), _T_2_0) @[ProcessingElement.scala 30:25]
    node _GEN_1 = mux(io_topCtrl_calFinish, UInt<1>("h0"), _GEN_0) @[ProcessingElement.scala 33:33]
    node _GEN_2 = mux(ProcessingElementPad.io_padWF_inActWriteFin_dataWriteFin, UInt<1>("h1"), _T_2_1) @[ProcessingElement.scala 30:25]
    node _GEN_3 = mux(io_topCtrl_calFinish, UInt<1>("h0"), _GEN_2) @[ProcessingElement.scala 33:33]
    node _GEN_4 = mux(ProcessingElementPad.io_padWF_weightWriteFin_adrWriteFin, UInt<1>("h1"), _T_2_2) @[ProcessingElement.scala 30:25]
    node _GEN_5 = mux(io_topCtrl_calFinish, UInt<1>("h0"), _GEN_4) @[ProcessingElement.scala 33:33]
    node _GEN_6 = mux(ProcessingElementPad.io_padWF_weightWriteFin_dataWriteFin, UInt<1>("h1"), _T_2_3) @[ProcessingElement.scala 30:25]
    node _GEN_7 = mux(io_topCtrl_calFinish, UInt<1>("h0"), _GEN_6) @[ProcessingElement.scala 33:33]
    node _T_3 = and(_T_2_0, _T_2_1) @[ProcessingElement.scala 37:49]
    node _T_4 = and(_T_3, _T_2_2) @[ProcessingElement.scala 37:49]
    node _T_5 = and(_T_4, _T_2_3) @[ProcessingElement.scala 37:49]
    node _T = _T_5 @[ProcessingElement.scala 27:43 ProcessingElement.scala 37:19]
    node _T_1_0 = UInt<1>("h0") @[ProcessingElement.scala 28:61 ProcessingElement.scala 28:61]
    node _T_1_1 = UInt<1>("h0") @[ProcessingElement.scala 28:61 ProcessingElement.scala 28:61]
    node _T_1_2 = UInt<1>("h0") @[ProcessingElement.scala 28:61 ProcessingElement.scala 28:61]
    node _T_1_3 = UInt<1>("h0") @[ProcessingElement.scala 28:61 ProcessingElement.scala 28:61]
    io_dataStream_ipsIO_ready <= Queue_4.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_opsIO_valid <= Queue_5.io_deq_valid @[ProcessingElement.scala 41:23]
    io_dataStream_opsIO_bits <= Queue_5.io_deq_bits @[ProcessingElement.scala 41:23]
    io_dataStream_inActIOs_adrIOs_data_ready <= Queue.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_inActIOs_dataIOs_data_ready <= Queue_1.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_weightIOs_adrIOs_data_ready <= Queue_2.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_weightIOs_dataIOs_data_ready <= Queue_3.io_enq_ready @[Decoupled.scala 290:17]
    io_padWF_inActWriteFin_adrWriteFin <= ProcessingElementPad.io_padWF_inActWriteFin_adrWriteFin @[ProcessingElement.scala 17:74]
    io_padWF_inActWriteFin_dataWriteFin <= ProcessingElementPad.io_padWF_inActWriteFin_dataWriteFin @[ProcessingElement.scala 17:74]
    io_padWF_weightWriteFin_adrWriteFin <= ProcessingElementPad.io_padWF_weightWriteFin_adrWriteFin @[ProcessingElement.scala 17:74]
    io_padWF_weightWriteFin_dataWriteFin <= ProcessingElementPad.io_padWF_weightWriteFin_dataWriteFin @[ProcessingElement.scala 17:74]
    io_topCtrl_pSumEnqOrProduct_ready <= ProcessingElementControl.io_ctrlTop_pSumEnqOrProduct_ready @[ProcessingElement.scala 23:31]
    io_topCtrl_writeFinish <= _T @[ProcessingElement.scala 38:26]
    io_topCtrl_calFinish <= ProcessingElementControl.io_ctrlTop_calFinish @[ProcessingElement.scala 24:24]
    io_debugIO_peControlDebugIO_peState <= ProcessingElementControl.io_debugIO_peState @[ProcessingElement.scala 43:33]
    io_debugIO_peControlDebugIO_doMACEnDebug <= ProcessingElementControl.io_debugIO_doMACEnDebug @[ProcessingElement.scala 43:33]
    io_debugIO_peSPadDebugIO_inActMatrixData <= ProcessingElementPad.io_debugIO_inActMatrixData @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_inActMatrixRow <= ProcessingElementPad.io_debugIO_inActMatrixRow @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_inActMatrixColumn <= ProcessingElementPad.io_debugIO_inActMatrixColumn @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_inActAdrInc <= ProcessingElementPad.io_debugIO_inActAdrInc @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_inActDataInc <= ProcessingElementPad.io_debugIO_inActDataInc @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_inActAdrIdx <= ProcessingElementPad.io_debugIO_inActAdrIdx @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_weightAdrSPadReadOut <= ProcessingElementPad.io_debugIO_weightAdrSPadReadOut @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_weightMatrixData <= ProcessingElementPad.io_debugIO_weightMatrixData @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_weightMatrixRow <= ProcessingElementPad.io_debugIO_weightMatrixRow @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_productResult <= ProcessingElementPad.io_debugIO_productResult @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_pSumResult <= ProcessingElementPad.io_debugIO_pSumResult @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_pSumLoad <= ProcessingElementPad.io_debugIO_pSumLoad @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_weightAdrInIdx <= ProcessingElementPad.io_debugIO_weightAdrInIdx @[ProcessingElement.scala 44:30]
    io_debugIO_peSPadDebugIO_sPadState <= ProcessingElementPad.io_debugIO_sPadState @[ProcessingElement.scala 44:30]
    ProcessingElementControl.clock <= clock
    ProcessingElementControl.reset <= reset
    ProcessingElementControl.io_ctrlPad_fromTopIO_pSumEnqOrProduct_ready <= ProcessingElementPad.io_padCtrl_fromTopIO_pSumEnqOrProduct_ready @[ProcessingElement.scala 22:21]
    ProcessingElementControl.io_ctrlPad_fromTopIO_writeFinish <= ProcessingElementPad.io_padCtrl_fromTopIO_writeFinish @[ProcessingElement.scala 22:21]
    ProcessingElementControl.io_ctrlPad_fromTopIO_calFinish <= ProcessingElementPad.io_padCtrl_fromTopIO_calFinish @[ProcessingElement.scala 22:21]
    ProcessingElementControl.io_ctrlTop_pSumEnqOrProduct_valid <= io_topCtrl_pSumEnqOrProduct_valid @[ProcessingElement.scala 23:31]
    ProcessingElementControl.io_ctrlTop_pSumEnqOrProduct_bits <= io_topCtrl_pSumEnqOrProduct_bits @[ProcessingElement.scala 23:31]
    ProcessingElementControl.io_ctrlTop_doLoadEn <= io_topCtrl_doLoadEn @[ProcessingElement.scala 25:30]
    ProcessingElementControl.io_ctrlTop_writeFinish <= _T @[ProcessingElement.scala 39:33]
    ProcessingElementPad.clock <= clock
    ProcessingElementPad.reset <= reset
    ProcessingElementPad.io_padCtrl_doMACEn <= ProcessingElementControl.io_ctrlPad_doMACEn @[ProcessingElement.scala 22:21]
    ProcessingElementPad.io_padCtrl_fromTopIO_pSumEnqOrProduct_valid <= ProcessingElementControl.io_ctrlPad_fromTopIO_pSumEnqOrProduct_valid @[ProcessingElement.scala 22:21]
    ProcessingElementPad.io_padCtrl_fromTopIO_pSumEnqOrProduct_bits <= ProcessingElementControl.io_ctrlPad_fromTopIO_pSumEnqOrProduct_bits @[ProcessingElement.scala 22:21]
    ProcessingElementPad.io_padCtrl_fromTopIO_doLoadEn <= ProcessingElementControl.io_ctrlPad_fromTopIO_doLoadEn @[ProcessingElement.scala 22:21]
    ProcessingElementPad.io_dataStream_ipsIO_valid <= Queue_4.io_deq_valid @[ProcessingElement.scala 40:29]
    ProcessingElementPad.io_dataStream_ipsIO_bits <= Queue_4.io_deq_bits @[ProcessingElement.scala 40:29]
    ProcessingElementPad.io_dataStream_opsIO_ready <= Queue_5.io_enq_ready @[Decoupled.scala 290:17]
    ProcessingElementPad.io_dataStream_inActIOs_adrIOs_data_valid <= Queue.io_deq_valid @[ProcessingElement.scala 12:46]
    ProcessingElementPad.io_dataStream_inActIOs_adrIOs_data_bits <= Queue.io_deq_bits @[ProcessingElement.scala 12:46]
    ProcessingElementPad.io_dataStream_inActIOs_dataIOs_data_valid <= Queue_1.io_deq_valid @[ProcessingElement.scala 13:47]
    ProcessingElementPad.io_dataStream_inActIOs_dataIOs_data_bits <= Queue_1.io_deq_bits @[ProcessingElement.scala 13:47]
    ProcessingElementPad.io_dataStream_weightIOs_adrIOs_data_valid <= Queue_2.io_deq_valid @[ProcessingElement.scala 14:47]
    ProcessingElementPad.io_dataStream_weightIOs_adrIOs_data_bits <= Queue_2.io_deq_bits @[ProcessingElement.scala 14:47]
    ProcessingElementPad.io_dataStream_weightIOs_dataIOs_data_valid <= Queue_3.io_deq_valid @[ProcessingElement.scala 15:48]
    ProcessingElementPad.io_dataStream_weightIOs_dataIOs_data_bits <= Queue_3.io_deq_bits @[ProcessingElement.scala 15:48]
    Queue.clock <= clock
    Queue.reset <= reset
    Queue.io_enq_valid <= io_dataStream_inActIOs_adrIOs_data_valid @[Decoupled.scala 288:22]
    Queue.io_enq_bits <= io_dataStream_inActIOs_adrIOs_data_bits @[Decoupled.scala 289:21]
    Queue.io_deq_ready <= ProcessingElementPad.io_dataStream_inActIOs_adrIOs_data_ready @[ProcessingElement.scala 12:46]
    Queue_1.clock <= clock
    Queue_1.reset <= reset
    Queue_1.io_enq_valid <= io_dataStream_inActIOs_dataIOs_data_valid @[Decoupled.scala 288:22]
    Queue_1.io_enq_bits <= io_dataStream_inActIOs_dataIOs_data_bits @[Decoupled.scala 289:21]
    Queue_1.io_deq_ready <= ProcessingElementPad.io_dataStream_inActIOs_dataIOs_data_ready @[ProcessingElement.scala 13:47]
    Queue_2.clock <= clock
    Queue_2.reset <= reset
    Queue_2.io_enq_valid <= io_dataStream_weightIOs_adrIOs_data_valid @[Decoupled.scala 288:22]
    Queue_2.io_enq_bits <= io_dataStream_weightIOs_adrIOs_data_bits @[Decoupled.scala 289:21]
    Queue_2.io_deq_ready <= ProcessingElementPad.io_dataStream_weightIOs_adrIOs_data_ready @[ProcessingElement.scala 14:47]
    Queue_3.clock <= clock
    Queue_3.reset <= reset
    Queue_3.io_enq_valid <= io_dataStream_weightIOs_dataIOs_data_valid @[Decoupled.scala 288:22]
    Queue_3.io_enq_bits <= io_dataStream_weightIOs_dataIOs_data_bits @[Decoupled.scala 289:21]
    Queue_3.io_deq_ready <= ProcessingElementPad.io_dataStream_weightIOs_dataIOs_data_ready @[ProcessingElement.scala 15:48]
    _T_2_0 <= mux(reset, _T_1_0, _GEN_1) @[ProcessingElement.scala 31:28 ProcessingElement.scala 34:28]
    _T_2_1 <= mux(reset, _T_1_1, _GEN_3) @[ProcessingElement.scala 31:28 ProcessingElement.scala 34:28]
    _T_2_2 <= mux(reset, _T_1_2, _GEN_5) @[ProcessingElement.scala 31:28 ProcessingElement.scala 34:28]
    _T_2_3 <= mux(reset, _T_1_3, _GEN_7) @[ProcessingElement.scala 31:28 ProcessingElement.scala 34:28]
    Queue_4.clock <= clock
    Queue_4.reset <= reset
    Queue_4.io_enq_valid <= io_dataStream_ipsIO_valid @[Decoupled.scala 288:22]
    Queue_4.io_enq_bits <= io_dataStream_ipsIO_bits @[Decoupled.scala 289:21]
    Queue_4.io_deq_ready <= ProcessingElementPad.io_dataStream_ipsIO_ready @[ProcessingElement.scala 40:29]
    Queue_5.clock <= clock
    Queue_5.reset <= reset
    Queue_5.io_enq_valid <= ProcessingElementPad.io_dataStream_opsIO_valid @[Decoupled.scala 288:22]
    Queue_5.io_enq_bits <= ProcessingElementPad.io_dataStream_opsIO_bits @[Decoupled.scala 289:21]
    Queue_5.io_deq_ready <= io_dataStream_opsIO_ready @[ProcessingElement.scala 41:23]
