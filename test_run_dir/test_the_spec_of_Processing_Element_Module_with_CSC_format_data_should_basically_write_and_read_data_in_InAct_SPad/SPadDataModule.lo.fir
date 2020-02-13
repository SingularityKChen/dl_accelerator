circuit SPadDataModule :
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
