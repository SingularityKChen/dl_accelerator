circuit SimplyCombineAddrDataSPad :
  module SPadAddrModule :
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
      reset => (UInt<1>("h0"), _T_8_8) @[SPadModule.scala 14:44]
    reg _T_8_7 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_7) @[SPadModule.scala 14:44]
    reg _T_8_6 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_6) @[SPadModule.scala 14:44]
    reg _T_8_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_5) @[SPadModule.scala 14:44]
    reg _T_8_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_4) @[SPadModule.scala 14:44]
    reg _T_8_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_3) @[SPadModule.scala 14:44]
    reg _T_8_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_2) @[SPadModule.scala 14:44]
    reg _T_8_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_1) @[SPadModule.scala 14:44]
    reg _T_8_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_8_0) @[SPadModule.scala 14:44]
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
    node _T_8_padWriteIndexReg = io_dataPath_writeInData_data_bits @[SPadModule.scala 17:32 SPadModule.scala 17:32]
    node _GEN_3 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_0) @[SPadModule.scala 17:32]
    node _GEN_4 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_1) @[SPadModule.scala 17:32]
    node _GEN_5 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_2) @[SPadModule.scala 17:32]
    node _GEN_6 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_3) @[SPadModule.scala 17:32]
    node _GEN_7 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_4) @[SPadModule.scala 17:32]
    node _GEN_8 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_5) @[SPadModule.scala 17:32]
    node _GEN_9 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_6) @[SPadModule.scala 17:32]
    node _GEN_10 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_7) @[SPadModule.scala 17:32]
    node _GEN_11 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _T_8_padWriteIndexReg, _T_8_8) @[SPadModule.scala 17:32]
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
    node _T_7_0 = UInt<4>("h0") @[SPadModule.scala 14:52 SPadModule.scala 14:52]
    node _T_7_1 = UInt<4>("h0") @[SPadModule.scala 14:52 SPadModule.scala 14:52]
    node _T_7_2 = UInt<4>("h0") @[SPadModule.scala 14:52 SPadModule.scala 14:52]
    node _T_7_3 = UInt<4>("h0") @[SPadModule.scala 14:52 SPadModule.scala 14:52]
    node _T_7_4 = UInt<4>("h0") @[SPadModule.scala 14:52 SPadModule.scala 14:52]
    node _T_7_5 = UInt<4>("h0") @[SPadModule.scala 14:52 SPadModule.scala 14:52]
    node _T_7_6 = UInt<4>("h0") @[SPadModule.scala 14:52 SPadModule.scala 14:52]
    node _T_7_7 = UInt<4>("h0") @[SPadModule.scala 14:52 SPadModule.scala 14:52]
    node _T_7_8 = UInt<4>("h0") @[SPadModule.scala 14:52 SPadModule.scala 14:52]
    io_dataPath_columnNum <= padReadIndexReg @[SPadModule.scala 96:25]
    io_dataPath_readOutData <= dataWire @[SPadModule.scala 28:27]
    io_dataPath_writeInData_data_ready <= _GEN_1 @[SPadModule.scala 85:27 SPadModule.scala 91:27]
    io_ctrlPath_writeIdx <= padWriteIndexReg @[SPadModule.scala 94:24]
    io_ctrlPath_writeFin <= writeWrapWire @[SPadModule.scala 95:24]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[SPadModule.scala 86:22 SPadModule.scala 88:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_22) @[SPadModule.scala 21:21 SPadModule.scala 23:23]
    _T_8_0 <= mux(reset, _T_7_0, _GEN_12) @[SPadModule.scala 17:32]
    _T_8_1 <= mux(reset, _T_7_1, _GEN_13) @[SPadModule.scala 17:32]
    _T_8_2 <= mux(reset, _T_7_2, _GEN_14) @[SPadModule.scala 17:32]
    _T_8_3 <= mux(reset, _T_7_3, _GEN_15) @[SPadModule.scala 17:32]
    _T_8_4 <= mux(reset, _T_7_4, _GEN_16) @[SPadModule.scala 17:32]
    _T_8_5 <= mux(reset, _T_7_5, _GEN_17) @[SPadModule.scala 17:32]
    _T_8_6 <= mux(reset, _T_7_6, _GEN_18) @[SPadModule.scala 17:32]
    _T_8_7 <= mux(reset, _T_7_7, _GEN_19) @[SPadModule.scala 17:32]
    _T_8_8 <= mux(reset, _T_7_8, _GEN_20) @[SPadModule.scala 17:32]

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

  module SimplyCombineAddrDataSPad :
    input clock : Clock
    input reset : UInt<1>
    output io_iactIOs_addrIOs_data_ready : UInt<1>
    input io_iactIOs_addrIOs_data_valid : UInt<1>
    input io_iactIOs_addrIOs_data_bits : UInt<4>
    output io_iactIOs_dataIOs_data_ready : UInt<1>
    input io_iactIOs_dataIOs_data_valid : UInt<1>
    input io_iactIOs_dataIOs_data_bits : UInt<12>
    output io_iactWF_addrWriteFin : UInt<1>
    output io_iactWF_dataWriteFin : UInt<1>
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
    input io_writeEn : UInt<1>
  
    inst SPadAddrModule of SPadAddrModule @[SimplyCombineAddrDataSPad.scala 9:52]
    inst SPadDataModule of SPadDataModule @[SimplyCombineAddrDataSPad.scala 10:52]
    reg _T : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T) @[SimplyCombineAddrDataSPad.scala 12:35]
    reg _T_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_4) @[SimplyCombineAddrDataSPad.scala 16:52]
    reg _T_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_5) @[SimplyCombineAddrDataSPad.scala 17:52]
    reg _T_6 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_6) @[SimplyCombineAddrDataSPad.scala 18:52]
    reg _T_9 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_9) @[SimplyCombineAddrDataSPad.scala 21:50]
    reg _T_10 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_10) @[SimplyCombineAddrDataSPad.scala 22:51]
    reg _T_11 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_11) @[SimplyCombineAddrDataSPad.scala 23:55]
    node _T_12 = eq(_T, UInt<2>("h2")) @[SimplyCombineAddrDataSPad.scala 24:36]
    node _T_3 = SPadDataModule.io_dataPath_columnNum @[SimplyCombineAddrDataSPad.scala 15:45 SimplyCombineAddrDataSPad.scala 42:21]
    node _T_13 = add(_T_3, UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 24:97]
    node _T_14 = tail(_T_13, 1) @[SimplyCombineAddrDataSPad.scala 24:97]
    node _T_2 = SPadAddrModule.io_dataPath_readOutData @[SimplyCombineAddrDataSPad.scala 14:44 SimplyCombineAddrDataSPad.scala 30:20]
    node _T_15 = eq(_T_2, _T_14) @[SimplyCombineAddrDataSPad.scala 24:74]
    node _T_16 = and(_T_12, _T_15) @[SimplyCombineAddrDataSPad.scala 24:53]
    node _T_17 = eq(_T, UInt<2>("h1")) @[SimplyCombineAddrDataSPad.scala 24:116]
    node _T_18 = eq(_T_2, UInt<4>("hf")) @[SimplyCombineAddrDataSPad.scala 24:154]
    node _T_19 = and(_T_17, _T_18) @[SimplyCombineAddrDataSPad.scala 24:133]
    node _T_20 = or(_T_16, _T_19) @[SimplyCombineAddrDataSPad.scala 24:106]
    node _T_21 = eq(_T, UInt<2>("h1")) @[SimplyCombineAddrDataSPad.scala 25:36]
    node _T_22 = eq(_T_4, UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 25:56]
    node _T_23 = and(_T_21, _T_22) @[SimplyCombineAddrDataSPad.scala 25:53]
    node _T_24 = eq(_T_11, UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 25:82]
    node _T_25 = and(_T_23, _T_24) @[SimplyCombineAddrDataSPad.scala 25:79]
    node _T_26 = eq(_T, UInt<2>("h2")) @[SimplyCombineAddrDataSPad.scala 25:119]
    node _T_7 = _T_20 @[SimplyCombineAddrDataSPad.scala 19:50 SimplyCombineAddrDataSPad.scala 24:26]
    node _T_27 = eq(_T_7, UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 25:139]
    node _T_28 = and(_T_26, _T_27) @[SimplyCombineAddrDataSPad.scala 25:136]
    node _T_29 = or(_T_25, _T_28) @[SimplyCombineAddrDataSPad.scala 25:109]
    node _T_30 = bits(SPadDataModule.io_dataPath_readOutData, 0, 0) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_31 = bits(SPadDataModule.io_dataPath_readOutData, 1, 1) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_32 = bits(SPadDataModule.io_dataPath_readOutData, 2, 2) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_33 = bits(SPadDataModule.io_dataPath_readOutData, 3, 3) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_34 = bits(SPadDataModule.io_dataPath_readOutData, 4, 4) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_35 = bits(SPadDataModule.io_dataPath_readOutData, 5, 5) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_36 = bits(SPadDataModule.io_dataPath_readOutData, 6, 6) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_37 = bits(SPadDataModule.io_dataPath_readOutData, 7, 7) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_38 = bits(SPadDataModule.io_dataPath_readOutData, 8, 8) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_39 = bits(SPadDataModule.io_dataPath_readOutData, 9, 9) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_40 = bits(SPadDataModule.io_dataPath_readOutData, 10, 10) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_41 = bits(SPadDataModule.io_dataPath_readOutData, 11, 11) @[SimplyCombineAddrDataSPad.scala 45:82]
    node _T_42 = cat(_T_35, _T_34) @[Cat.scala 29:58]
    node _T_43 = cat(_T_37, _T_36) @[Cat.scala 29:58]
    node _T_44 = cat(_T_43, _T_42) @[Cat.scala 29:58]
    node _T_45 = cat(_T_39, _T_38) @[Cat.scala 29:58]
    node _T_46 = cat(_T_41, _T_40) @[Cat.scala 29:58]
    node _T_47 = cat(_T_46, _T_45) @[Cat.scala 29:58]
    node _T_48 = cat(_T_47, _T_44) @[Cat.scala 29:58]
    node _T_49 = cat(_T_31, _T_30) @[Cat.scala 29:58]
    node _T_50 = cat(_T_33, _T_32) @[Cat.scala 29:58]
    node _T_51 = cat(_T_50, _T_49) @[Cat.scala 29:58]
    node _T_52 = eq(UInt<2>("h0"), _T) @[Conditional.scala 37:30]
    node _GEN_0 = mux(io_iactDataReq, UInt<2>("h1"), _T) @[SimplyCombineAddrDataSPad.scala 60:28]
    node _GEN_1 = mux(io_iactDataReq, UInt<1>("h1"), _T_5) @[SimplyCombineAddrDataSPad.scala 60:28]
    node _GEN_2 = mux(io_iactDataReq, UInt<1>("h0"), _T_6) @[SimplyCombineAddrDataSPad.scala 60:28]
    node _GEN_3 = mux(io_iactDataReq, UInt<1>("h1"), _T_11) @[SimplyCombineAddrDataSPad.scala 60:28]
    node _T_53 = eq(UInt<2>("h1"), _T) @[Conditional.scala 37:30]
    node _T_54 = eq(_T_2, UInt<4>("hf")) @[SimplyCombineAddrDataSPad.scala 69:30]
    node _T_55 = add(_T_10, UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 74:54]
    node _T_56 = tail(_T_55, 1) @[SimplyCombineAddrDataSPad.scala 74:54]
    node _GEN_4 = mux(_T_54, UInt<2>("h1"), UInt<2>("h2")) @[SimplyCombineAddrDataSPad.scala 69:56]
    node _GEN_5 = mux(_T_54, UInt<1>("h1"), _T_4) @[SimplyCombineAddrDataSPad.scala 69:56]
    node _GEN_6 = mux(_T_54, UInt<1>("h1"), UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 69:56]
    node _GEN_7 = mux(_T_54, UInt<1>("h0"), UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 69:56]
    node _GEN_8 = mux(_T_54, _T_56, _T_10) @[SimplyCombineAddrDataSPad.scala 69:56]
    node _T_57 = eq(UInt<2>("h2"), _T) @[Conditional.scala 37:30]
    node _T_58 = eq(io_iactMatrixData, UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 83:31]
    node _GEN_9 = mux(_T_58, UInt<2>("h0"), _T) @[SimplyCombineAddrDataSPad.scala 83:40]
    node _GEN_10 = mux(_T_58, UInt<1>("h0"), _T_6) @[SimplyCombineAddrDataSPad.scala 83:40]
    node _GEN_11 = mux(_T_58, UInt<1>("h0"), _T_4) @[SimplyCombineAddrDataSPad.scala 83:40]
    node _T_59 = add(_T_9, UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 95:54]
    node _T_60 = tail(_T_59, 1) @[SimplyCombineAddrDataSPad.scala 95:54]
    node _T_61 = add(_T_60, _T_10) @[SimplyCombineAddrDataSPad.scala 95:60]
    node _T_62 = tail(_T_61, 1) @[SimplyCombineAddrDataSPad.scala 95:60]
    node _T_63 = add(_T_9, UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 99:54]
    node _T_64 = tail(_T_63, 1) @[SimplyCombineAddrDataSPad.scala 99:54]
    node _GEN_12 = mux(_T_4, _T_62, _T_64) @[SimplyCombineAddrDataSPad.scala 94:38]
    node _GEN_13 = mux(_T_4, UInt<1>("h0"), _T_10) @[SimplyCombineAddrDataSPad.scala 94:38]
    node _GEN_14 = mux(_T_7, UInt<2>("h1"), UInt<2>("h2")) @[SimplyCombineAddrDataSPad.scala 88:37]
    node _GEN_15 = mux(_T_7, UInt<1>("h0"), _GEN_11) @[SimplyCombineAddrDataSPad.scala 88:37]
    node _GEN_16 = mux(_T_7, UInt<1>("h1"), UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 88:37]
    node _GEN_17 = mux(_T_7, UInt<1>("h0"), UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 88:37]
    node _GEN_18 = mux(_T_7, _GEN_12, _T_9) @[SimplyCombineAddrDataSPad.scala 88:37]
    node _GEN_19 = mux(_T_7, _GEN_13, _T_10) @[SimplyCombineAddrDataSPad.scala 88:37]
    node _GEN_20 = mux(_T_57, UInt<1>("h0"), _T_11) @[Conditional.scala 39:67]
    node _GEN_21 = mux(_T_57, _GEN_14, _T) @[Conditional.scala 39:67]
    node _GEN_22 = mux(_T_57, _GEN_17, _T_6) @[Conditional.scala 39:67]
    node _GEN_23 = mux(_T_57, _GEN_15, _T_4) @[Conditional.scala 39:67]
    node _GEN_24 = mux(_T_57, _GEN_16, _T_5) @[Conditional.scala 39:67]
    node _GEN_25 = mux(_T_57, _GEN_18, _T_9) @[Conditional.scala 39:67]
    node _GEN_26 = mux(_T_57, _GEN_19, _T_10) @[Conditional.scala 39:67]
    node _GEN_27 = mux(_T_53, _GEN_4, _GEN_21) @[Conditional.scala 39:67]
    node _GEN_28 = mux(_T_53, _GEN_5, _GEN_23) @[Conditional.scala 39:67]
    node _GEN_29 = mux(_T_53, _GEN_6, _GEN_24) @[Conditional.scala 39:67]
    node _GEN_30 = mux(_T_53, _GEN_7, _GEN_22) @[Conditional.scala 39:67]
    node _GEN_31 = mux(_T_53, _GEN_8, _GEN_26) @[Conditional.scala 39:67]
    node _GEN_32 = mux(_T_53, _T_11, _GEN_20) @[Conditional.scala 39:67]
    node _GEN_33 = mux(_T_53, _T_9, _GEN_25) @[Conditional.scala 39:67]
    node _GEN_34 = mux(_T_52, _GEN_0, _GEN_27) @[Conditional.scala 40:58]
    node _GEN_35 = mux(_T_52, _GEN_1, _GEN_29) @[Conditional.scala 40:58]
    node _GEN_36 = mux(_T_52, _GEN_2, _GEN_30) @[Conditional.scala 40:58]
    node _GEN_37 = mux(_T_52, _GEN_3, _GEN_32) @[Conditional.scala 40:58]
    node _GEN_38 = mux(_T_52, _T_4, _GEN_28) @[Conditional.scala 40:58]
    node _GEN_39 = mux(_T_52, _T_10, _GEN_31) @[Conditional.scala 40:58]
    node _GEN_40 = mux(_T_52, _T_9, _GEN_33) @[Conditional.scala 40:58]
    node _T_1 = SPadAddrModule.io_dataPath_columnNum @[SimplyCombineAddrDataSPad.scala 13:45 SimplyCombineAddrDataSPad.scala 28:21]
    node _T_8 = _T_29 @[SimplyCombineAddrDataSPad.scala 20:50 SimplyCombineAddrDataSPad.scala 25:26]
    io_iactIOs_addrIOs_data_ready <= SPadAddrModule.io_dataPath_writeInData_data_ready @[SimplyCombineAddrDataSPad.scala 27:40]
    io_iactIOs_dataIOs_data_ready <= SPadDataModule.io_dataPath_writeInData_data_ready @[SimplyCombineAddrDataSPad.scala 41:40]
    io_iactWF_addrWriteFin <= SPadAddrModule.io_ctrlPath_writeFin @[SimplyCombineAddrDataSPad.scala 35:26]
    io_iactWF_dataWriteFin <= SPadDataModule.io_ctrlPath_writeFin @[SimplyCombineAddrDataSPad.scala 50:26]
    io_iactAddrWriteIdx <= SPadAddrModule.io_ctrlPath_writeIdx @[SimplyCombineAddrDataSPad.scala 34:23]
    io_iactMatrixColumn <= _T_9 @[SimplyCombineAddrDataSPad.scala 29:23]
    io_iactMatrixRow <= _T_51 @[SimplyCombineAddrDataSPad.scala 47:20]
    io_iactMatrixData <= _T_48 @[SimplyCombineAddrDataSPad.scala 46:21]
    io_iactMatrixDataBin <= SPadDataModule.io_dataPath_readOutData @[SimplyCombineAddrDataSPad.scala 44:24]
    io_iactAddrReadEn <= _T_5 @[SimplyCombineAddrDataSPad.scala 32:21]
    io_iactAddrReadData <= _T_2 @[SimplyCombineAddrDataSPad.scala 31:23]
    io_iactDataReadIndex <= _T_3 @[SimplyCombineAddrDataSPad.scala 43:24]
    io_iactDataWriteIdx <= SPadDataModule.io_ctrlPath_writeIdx @[SimplyCombineAddrDataSPad.scala 49:23]
    SPadAddrModule.clock <= clock
    SPadAddrModule.reset <= reset
    SPadAddrModule.io_dataPath_writeInData_data_valid <= io_iactIOs_addrIOs_data_valid @[SimplyCombineAddrDataSPad.scala 27:40]
    SPadAddrModule.io_dataPath_writeInData_data_bits <= io_iactIOs_addrIOs_data_bits @[SimplyCombineAddrDataSPad.scala 27:40]
    SPadAddrModule.io_ctrlPath_writeEn <= io_writeEn @[SimplyCombineAddrDataSPad.scala 33:36]
    SPadAddrModule.io_ctrlPath_readEn <= _T_5 @[SimplyCombineAddrDataSPad.scala 36:35]
    SPadAddrModule.io_ctrlPath_readInIdx is invalid
    SPadAddrModule.io_ctrlPath_indexInc <= _T_7 @[SimplyCombineAddrDataSPad.scala 38:37]
    SPadAddrModule.io_ctrlPath_readInIdxEn is invalid
    SPadDataModule.clock <= clock
    SPadDataModule.reset <= reset
    SPadDataModule.io_dataPath_writeInData_data_valid <= io_iactIOs_dataIOs_data_valid @[SimplyCombineAddrDataSPad.scala 41:40]
    SPadDataModule.io_dataPath_writeInData_data_bits <= io_iactIOs_dataIOs_data_bits @[SimplyCombineAddrDataSPad.scala 41:40]
    SPadDataModule.io_ctrlPath_writeEn <= io_writeEn @[SimplyCombineAddrDataSPad.scala 48:36]
    SPadDataModule.io_ctrlPath_readEn <= _T_6 @[SimplyCombineAddrDataSPad.scala 51:35]
    SPadDataModule.io_ctrlPath_readInIdx is invalid
    SPadDataModule.io_ctrlPath_indexInc <= _T_8 @[SimplyCombineAddrDataSPad.scala 53:37]
    SPadDataModule.io_ctrlPath_readInIdxEn is invalid
    _T <= mux(reset, UInt<2>("h0"), _GEN_34) @[SimplyCombineAddrDataSPad.scala 61:14 SimplyCombineAddrDataSPad.scala 70:14 SimplyCombineAddrDataSPad.scala 76:14 SimplyCombineAddrDataSPad.scala 84:14 SimplyCombineAddrDataSPad.scala 89:14 SimplyCombineAddrDataSPad.scala 102:14]
    _T_4 <= mux(reset, UInt<1>("h0"), _GEN_38) @[SimplyCombineAddrDataSPad.scala 71:31 SimplyCombineAddrDataSPad.scala 86:31 SimplyCombineAddrDataSPad.scala 90:31]
    _T_5 <= mux(reset, UInt<1>("h0"), _GEN_35) @[SimplyCombineAddrDataSPad.scala 62:31 SimplyCombineAddrDataSPad.scala 72:31 SimplyCombineAddrDataSPad.scala 77:31 SimplyCombineAddrDataSPad.scala 92:31 SimplyCombineAddrDataSPad.scala 103:31]
    _T_6 <= mux(reset, UInt<1>("h0"), _GEN_36) @[SimplyCombineAddrDataSPad.scala 63:31 SimplyCombineAddrDataSPad.scala 73:31 SimplyCombineAddrDataSPad.scala 78:31 SimplyCombineAddrDataSPad.scala 85:31 SimplyCombineAddrDataSPad.scala 93:31 SimplyCombineAddrDataSPad.scala 104:31]
    _T_9 <= mux(reset, UInt<4>("h0"), _GEN_40) @[SimplyCombineAddrDataSPad.scala 95:31 SimplyCombineAddrDataSPad.scala 99:31]
    _T_10 <= mux(reset, UInt<4>("h0"), _GEN_39) @[SimplyCombineAddrDataSPad.scala 74:30 SimplyCombineAddrDataSPad.scala 97:32]
    _T_11 <= mux(reset, UInt<1>("h1"), _GEN_37) @[SimplyCombineAddrDataSPad.scala 64:34 SimplyCombineAddrDataSPad.scala 82:32]
