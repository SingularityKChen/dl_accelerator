circuit SimplyCombineAddrDataSPad :
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
    output io_commonIO_dataLenFinIO_writeFin : UInt<1>
    input io_addrIO_readInIdx : UInt<4>
    input io_addrIO_indexInc : UInt<1>
    input io_addrIO_readInIdxEn : UInt<1>
    input io_dataIO_readInIdx : UInt<4>
    input io_dataIO_indexInc : UInt<1>
    input io_dataIO_readInIdxEn : UInt<1>
  
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 80:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 81:38]
    node _T = eq(io_commonIO_dataLenFinIO_writeInDataIO_bits_data, UInt<1>("h0")) @[SPadModule.scala 86:46]
    node _T_1 = and(_T, io_commonIO_writeEn) @[SPadModule.scala 86:54]
    reg addrSPad_8 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_8) @[SPadModule.scala 14:36]
    reg addrSPad_7 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_7) @[SPadModule.scala 14:36]
    reg addrSPad_6 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_6) @[SPadModule.scala 14:36]
    reg addrSPad_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_5) @[SPadModule.scala 14:36]
    reg addrSPad_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_4) @[SPadModule.scala 14:36]
    reg addrSPad_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_3) @[SPadModule.scala 14:36]
    reg addrSPad_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_2) @[SPadModule.scala 14:36]
    reg addrSPad_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_1) @[SPadModule.scala 14:36]
    reg addrSPad_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_0) @[SPadModule.scala 14:36]
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
    node dataWire = _addrSPad_padReadIndexReg @[SPadModule.scala 79:28 SPadModule.scala 27:12]
    node _T_2 = eq(dataWire, UInt<1>("h0")) @[SPadModule.scala 87:28]
    node readIndexInc = io_addrIO_indexInc @[SPadModule.scala 85:32 SPadModule.scala 29:16]
    node _T_3 = and(_T_2, readIndexInc) @[SPadModule.scala 87:36]
    node _T_4 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 89:31]
    node _T_5 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 91:42]
    node _T_6 = tail(_T_5, 1) @[SPadModule.scala 91:42]
    node writeWrapWire = _T_1 @[SPadModule.scala 83:33 SPadModule.scala 86:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_6) @[SPadModule.scala 92:26]
    node _GEN_1 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 89:55]
    node _GEN_2 = mux(_T_4, _GEN_0, padWriteIndexReg) @[SPadModule.scala 89:55]
    node _T_8 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 16:31]
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
    node _GEN_12 = mux(_T_8, _GEN_3, addrSPad_0) @[SPadModule.scala 16:55]
    node _GEN_13 = mux(_T_8, _GEN_4, addrSPad_1) @[SPadModule.scala 16:55]
    node _GEN_14 = mux(_T_8, _GEN_5, addrSPad_2) @[SPadModule.scala 16:55]
    node _GEN_15 = mux(_T_8, _GEN_6, addrSPad_3) @[SPadModule.scala 16:55]
    node _GEN_16 = mux(_T_8, _GEN_7, addrSPad_4) @[SPadModule.scala 16:55]
    node _GEN_17 = mux(_T_8, _GEN_8, addrSPad_5) @[SPadModule.scala 16:55]
    node _GEN_18 = mux(_T_8, _GEN_9, addrSPad_6) @[SPadModule.scala 16:55]
    node _GEN_19 = mux(_T_8, _GEN_10, addrSPad_7) @[SPadModule.scala 16:55]
    node _GEN_20 = mux(_T_8, _GEN_11, addrSPad_8) @[SPadModule.scala 16:55]
    node _T_9 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 21:40]
    node _T_10 = tail(_T_9, 1) @[SPadModule.scala 21:40]
    node readWrapWire = _T_3 @[SPadModule.scala 84:32 SPadModule.scala 87:16]
    node _GEN_21 = mux(readWrapWire, UInt<1>("h0"), _T_10) @[SPadModule.scala 22:25]
    node _GEN_22 = mux(readIndexInc, _GEN_21, padReadIndexReg) @[SPadModule.scala 20:23]
    node _T_7_0 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_7_1 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_7_2 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_7_3 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_7_4 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_7_5 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_7_6 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_7_7 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    node _T_7_8 = UInt<4>("h0") @[SPadModule.scala 14:44 SPadModule.scala 14:44]
    io_commonIO_columnNum <= padReadIndexReg @[SPadModule.scala 101:25]
    io_commonIO_readOutData <= dataWire @[SPadModule.scala 28:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[SPadModule.scala 99:24]
    io_commonIO_dataLenFinIO_writeInDataIO_ready <= _GEN_1 @[SPadModule.scala 90:27 SPadModule.scala 96:27]
    io_commonIO_dataLenFinIO_writeFin <= writeWrapWire @[SPadModule.scala 100:37]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[SPadModule.scala 91:22 SPadModule.scala 93:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_22) @[SPadModule.scala 21:21 SPadModule.scala 23:23]
    addrSPad_0 <= mux(reset, _T_7_0, _GEN_12) @[SPadModule.scala 17:32]
    addrSPad_1 <= mux(reset, _T_7_1, _GEN_13) @[SPadModule.scala 17:32]
    addrSPad_2 <= mux(reset, _T_7_2, _GEN_14) @[SPadModule.scala 17:32]
    addrSPad_3 <= mux(reset, _T_7_3, _GEN_15) @[SPadModule.scala 17:32]
    addrSPad_4 <= mux(reset, _T_7_4, _GEN_16) @[SPadModule.scala 17:32]
    addrSPad_5 <= mux(reset, _T_7_5, _GEN_17) @[SPadModule.scala 17:32]
    addrSPad_6 <= mux(reset, _T_7_6, _GEN_18) @[SPadModule.scala 17:32]
    addrSPad_7 <= mux(reset, _T_7_7, _GEN_19) @[SPadModule.scala 17:32]
    addrSPad_8 <= mux(reset, _T_7_8, _GEN_20) @[SPadModule.scala 17:32]

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
    output io_commonIO_dataLenFinIO_writeFin : UInt<1>
    input io_addrIO_readInIdx : UInt<4>
    input io_addrIO_indexInc : UInt<1>
    input io_addrIO_readInIdxEn : UInt<1>
    input io_dataIO_readInIdx : UInt<4>
    input io_dataIO_indexInc : UInt<1>
    input io_dataIO_readInIdxEn : UInt<1>
  
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 80:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 81:38]
    node _T = eq(io_commonIO_dataLenFinIO_writeInDataIO_bits_data, UInt<1>("h0")) @[SPadModule.scala 86:46]
    node _T_1 = and(_T, io_commonIO_writeEn) @[SPadModule.scala 86:54]
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
    node dataWire = _T_8_padReadIndexReg @[SPadModule.scala 79:28 SPadModule.scala 66:14]
    node _T_2 = eq(dataWire, UInt<1>("h0")) @[SPadModule.scala 87:28]
    node readIndexInc = io_dataIO_indexInc @[SPadModule.scala 85:32 SPadModule.scala 68:16]
    node _T_3 = and(_T_2, readIndexInc) @[SPadModule.scala 87:36]
    node _T_4 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 89:31]
    node _T_5 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 91:42]
    node _T_6 = tail(_T_5, 1) @[SPadModule.scala 91:42]
    node writeWrapWire = _T_1 @[SPadModule.scala 83:33 SPadModule.scala 86:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_6) @[SPadModule.scala 92:26]
    node _GEN_1 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 89:55]
    node _GEN_2 = mux(_T_4, _GEN_0, padWriteIndexReg) @[SPadModule.scala 89:55]
    node _T_9 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, io_commonIO_writeEn) @[SPadModule.scala 55:33]
    node _T_8_padWriteIndexReg = io_commonIO_dataLenFinIO_writeInDataIO_bits_data @[SPadModule.scala 56:34 SPadModule.scala 56:34]
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
    node readWrapWire = _T_3 @[SPadModule.scala 84:32 SPadModule.scala 87:16]
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
    io_commonIO_columnNum <= padReadIndexReg @[SPadModule.scala 101:25]
    io_commonIO_readOutData <= dataWire @[SPadModule.scala 69:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[SPadModule.scala 99:24]
    io_commonIO_dataLenFinIO_writeInDataIO_ready <= _GEN_1 @[SPadModule.scala 90:27 SPadModule.scala 96:27]
    io_commonIO_dataLenFinIO_writeFin <= writeWrapWire @[SPadModule.scala 100:37]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[SPadModule.scala 91:22 SPadModule.scala 93:24]
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
    output io_iactIOs_dataIOs_writeInDataIO_ready : UInt<1>
    input io_iactIOs_dataIOs_writeInDataIO_valid : UInt<1>
    input io_iactIOs_dataIOs_writeInDataIO_bits_data : UInt<12>
    output io_iactIOs_dataIOs_writeFin : UInt<1>
    output io_iactIOs_addrIOs_writeInDataIO_ready : UInt<1>
    input io_iactIOs_addrIOs_writeInDataIO_valid : UInt<1>
    input io_iactIOs_addrIOs_writeInDataIO_bits_data : UInt<4>
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
    input io_writeEn : UInt<1>
  
    inst iactAddrSPad of SPadAddrModule @[SimplyCombineAddrDataSPad.scala 24:44]
    inst iactDataSPad of SPadDataModule @[SimplyCombineAddrDataSPad.scala 25:44]
    reg sPad : UInt<2>, clock with :
      reset => (UInt<1>("h0"), sPad) @[SimplyCombineAddrDataSPad.scala 28:27]
    reg iactSPadZeroColumnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactSPadZeroColumnReg) @[SimplyCombineAddrDataSPad.scala 32:44]
    reg iactAddrSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPadReadEnReg) @[SimplyCombineAddrDataSPad.scala 33:44]
    reg iactDataSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactDataSPadReadEnReg) @[SimplyCombineAddrDataSPad.scala 34:44]
    reg iactMatrixColumnReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactMatrixColumnReg) @[SimplyCombineAddrDataSPad.scala 37:42]
    reg iactZeroColumnNumber : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactZeroColumnNumber) @[SimplyCombineAddrDataSPad.scala 38:43]
    reg iactDataSPadFirstReadReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactDataSPadFirstReadReg) @[SimplyCombineAddrDataSPad.scala 39:47]
    node _T = eq(sPad, UInt<2>("h2")) @[SimplyCombineAddrDataSPad.scala 42:36]
    node iactDataIndexWire = iactDataSPad.io_commonIO_columnNum @[SimplyCombineAddrDataSPad.scala 31:37 SimplyCombineAddrDataSPad.scala 60:21]
    node _T_1 = add(iactDataIndexWire, UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 42:97]
    node _T_2 = tail(_T_1, 1) @[SimplyCombineAddrDataSPad.scala 42:97]
    node iactAddrDataWire = iactAddrSPad.io_commonIO_readOutData @[SimplyCombineAddrDataSPad.scala 30:36 SimplyCombineAddrDataSPad.scala 51:20]
    node _T_3 = eq(iactAddrDataWire, _T_2) @[SimplyCombineAddrDataSPad.scala 42:74]
    node _T_4 = and(_T, _T_3) @[SimplyCombineAddrDataSPad.scala 42:53]
    node _T_5 = eq(sPad, UInt<2>("h1")) @[SimplyCombineAddrDataSPad.scala 42:116]
    node _T_6 = eq(iactAddrDataWire, UInt<4>("hf")) @[SimplyCombineAddrDataSPad.scala 42:154]
    node _T_7 = and(_T_5, _T_6) @[SimplyCombineAddrDataSPad.scala 42:133]
    node _T_8 = or(_T_4, _T_7) @[SimplyCombineAddrDataSPad.scala 42:106]
    node _T_9 = eq(sPad, UInt<2>("h1")) @[SimplyCombineAddrDataSPad.scala 43:36]
    node _T_10 = eq(iactSPadZeroColumnReg, UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 43:56]
    node _T_11 = and(_T_9, _T_10) @[SimplyCombineAddrDataSPad.scala 43:53]
    node _T_12 = eq(iactDataSPadFirstReadReg, UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 43:82]
    node _T_13 = and(_T_11, _T_12) @[SimplyCombineAddrDataSPad.scala 43:79]
    node _T_14 = eq(sPad, UInt<2>("h2")) @[SimplyCombineAddrDataSPad.scala 43:119]
    node iactAddrSPadIdxIncWire = _T_8 @[SimplyCombineAddrDataSPad.scala 35:42 SimplyCombineAddrDataSPad.scala 42:26]
    node _T_15 = eq(iactAddrSPadIdxIncWire, UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 43:139]
    node _T_16 = and(_T_14, _T_15) @[SimplyCombineAddrDataSPad.scala 43:136]
    node _T_17 = or(_T_13, _T_16) @[SimplyCombineAddrDataSPad.scala 43:109]
    node iactDataCountVec_0 = bits(iactDataSPad.io_commonIO_readOutData, 0, 0) @[SimplyCombineAddrDataSPad.scala 63:74]
    node iactDataCountVec_1 = bits(iactDataSPad.io_commonIO_readOutData, 1, 1) @[SimplyCombineAddrDataSPad.scala 63:74]
    node iactDataCountVec_2 = bits(iactDataSPad.io_commonIO_readOutData, 2, 2) @[SimplyCombineAddrDataSPad.scala 63:74]
    node iactDataCountVec_3 = bits(iactDataSPad.io_commonIO_readOutData, 3, 3) @[SimplyCombineAddrDataSPad.scala 63:74]
    node iactDataCountVec_4 = bits(iactDataSPad.io_commonIO_readOutData, 4, 4) @[SimplyCombineAddrDataSPad.scala 63:74]
    node iactDataCountVec_5 = bits(iactDataSPad.io_commonIO_readOutData, 5, 5) @[SimplyCombineAddrDataSPad.scala 63:74]
    node iactDataCountVec_6 = bits(iactDataSPad.io_commonIO_readOutData, 6, 6) @[SimplyCombineAddrDataSPad.scala 63:74]
    node iactDataCountVec_7 = bits(iactDataSPad.io_commonIO_readOutData, 7, 7) @[SimplyCombineAddrDataSPad.scala 63:74]
    node iactDataCountVec_8 = bits(iactDataSPad.io_commonIO_readOutData, 8, 8) @[SimplyCombineAddrDataSPad.scala 63:74]
    node iactDataCountVec_9 = bits(iactDataSPad.io_commonIO_readOutData, 9, 9) @[SimplyCombineAddrDataSPad.scala 63:74]
    node iactDataCountVec_10 = bits(iactDataSPad.io_commonIO_readOutData, 10, 10) @[SimplyCombineAddrDataSPad.scala 63:74]
    node iactDataCountVec_11 = bits(iactDataSPad.io_commonIO_readOutData, 11, 11) @[SimplyCombineAddrDataSPad.scala 63:74]
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
    node _GEN_0 = mux(io_iactDataReq, UInt<2>("h1"), sPad) @[SimplyCombineAddrDataSPad.scala 77:28]
    node _GEN_1 = mux(io_iactDataReq, UInt<1>("h1"), iactAddrSPadReadEnReg) @[SimplyCombineAddrDataSPad.scala 77:28]
    node _GEN_2 = mux(io_iactDataReq, UInt<1>("h0"), iactDataSPadReadEnReg) @[SimplyCombineAddrDataSPad.scala 77:28]
    node _GEN_3 = mux(io_iactDataReq, UInt<1>("h1"), iactDataSPadFirstReadReg) @[SimplyCombineAddrDataSPad.scala 77:28]
    node _T_29 = eq(UInt<2>("h1"), sPad) @[Conditional.scala 37:30]
    node _T_30 = eq(iactAddrDataWire, UInt<4>("hf")) @[SimplyCombineAddrDataSPad.scala 86:30]
    node _T_31 = add(iactZeroColumnNumber, UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 91:54]
    node _T_32 = tail(_T_31, 1) @[SimplyCombineAddrDataSPad.scala 91:54]
    node _GEN_4 = mux(_T_30, UInt<2>("h1"), UInt<2>("h2")) @[SimplyCombineAddrDataSPad.scala 86:56]
    node _GEN_5 = mux(_T_30, UInt<1>("h1"), iactSPadZeroColumnReg) @[SimplyCombineAddrDataSPad.scala 86:56]
    node _GEN_6 = mux(_T_30, UInt<1>("h1"), UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 86:56]
    node _GEN_7 = mux(_T_30, UInt<1>("h0"), UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 86:56]
    node _GEN_8 = mux(_T_30, _T_32, iactZeroColumnNumber) @[SimplyCombineAddrDataSPad.scala 86:56]
    node _T_33 = eq(UInt<2>("h2"), sPad) @[Conditional.scala 37:30]
    node _T_34 = eq(io_iactMatrixData, UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 100:31]
    node _GEN_9 = mux(_T_34, UInt<2>("h0"), sPad) @[SimplyCombineAddrDataSPad.scala 100:40]
    node _GEN_10 = mux(_T_34, UInt<1>("h0"), iactDataSPadReadEnReg) @[SimplyCombineAddrDataSPad.scala 100:40]
    node _GEN_11 = mux(_T_34, UInt<1>("h0"), iactSPadZeroColumnReg) @[SimplyCombineAddrDataSPad.scala 100:40]
    node _T_35 = add(iactMatrixColumnReg, UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 112:54]
    node _T_36 = tail(_T_35, 1) @[SimplyCombineAddrDataSPad.scala 112:54]
    node _T_37 = add(_T_36, iactZeroColumnNumber) @[SimplyCombineAddrDataSPad.scala 112:60]
    node _T_38 = tail(_T_37, 1) @[SimplyCombineAddrDataSPad.scala 112:60]
    node _T_39 = add(iactMatrixColumnReg, UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 116:54]
    node _T_40 = tail(_T_39, 1) @[SimplyCombineAddrDataSPad.scala 116:54]
    node _GEN_12 = mux(iactSPadZeroColumnReg, _T_38, _T_40) @[SimplyCombineAddrDataSPad.scala 111:38]
    node _GEN_13 = mux(iactSPadZeroColumnReg, UInt<1>("h0"), iactZeroColumnNumber) @[SimplyCombineAddrDataSPad.scala 111:38]
    node _GEN_14 = mux(iactAddrSPadIdxIncWire, UInt<2>("h1"), UInt<2>("h2")) @[SimplyCombineAddrDataSPad.scala 105:37]
    node _GEN_15 = mux(iactAddrSPadIdxIncWire, UInt<1>("h0"), _GEN_11) @[SimplyCombineAddrDataSPad.scala 105:37]
    node _GEN_16 = mux(iactAddrSPadIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[SimplyCombineAddrDataSPad.scala 105:37]
    node _GEN_17 = mux(iactAddrSPadIdxIncWire, UInt<1>("h0"), UInt<1>("h1")) @[SimplyCombineAddrDataSPad.scala 105:37]
    node _GEN_18 = mux(iactAddrSPadIdxIncWire, _GEN_12, iactMatrixColumnReg) @[SimplyCombineAddrDataSPad.scala 105:37]
    node _GEN_19 = mux(iactAddrSPadIdxIncWire, _GEN_13, iactZeroColumnNumber) @[SimplyCombineAddrDataSPad.scala 105:37]
    node _GEN_20 = mux(_T_33, UInt<1>("h0"), iactDataSPadFirstReadReg) @[Conditional.scala 39:67]
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
    node _GEN_32 = mux(_T_29, iactDataSPadFirstReadReg, _GEN_20) @[Conditional.scala 39:67]
    node _GEN_33 = mux(_T_29, iactMatrixColumnReg, _GEN_25) @[Conditional.scala 39:67]
    node _GEN_34 = mux(_T_28, _GEN_0, _GEN_27) @[Conditional.scala 40:58]
    node _GEN_35 = mux(_T_28, _GEN_1, _GEN_29) @[Conditional.scala 40:58]
    node _GEN_36 = mux(_T_28, _GEN_2, _GEN_30) @[Conditional.scala 40:58]
    node _GEN_37 = mux(_T_28, _GEN_3, _GEN_32) @[Conditional.scala 40:58]
    node _GEN_38 = mux(_T_28, iactSPadZeroColumnReg, _GEN_28) @[Conditional.scala 40:58]
    node _GEN_39 = mux(_T_28, iactZeroColumnNumber, _GEN_31) @[Conditional.scala 40:58]
    node _GEN_40 = mux(_T_28, iactMatrixColumnReg, _GEN_33) @[Conditional.scala 40:58]
    node iactAddrIndexWire = iactAddrSPad.io_commonIO_columnNum @[SimplyCombineAddrDataSPad.scala 29:37 SimplyCombineAddrDataSPad.scala 49:21]
    node iactDataSPadIdxIncWire = _T_17 @[SimplyCombineAddrDataSPad.scala 36:42 SimplyCombineAddrDataSPad.scala 43:26]
    io_iactIOs_dataIOs_writeInDataIO_ready <= iactDataSPad.io_commonIO_dataLenFinIO_writeInDataIO_ready @[SimplyCombineAddrDataSPad.scala 45:41]
    io_iactIOs_dataIOs_writeFin <= iactDataSPad.io_commonIO_dataLenFinIO_writeFin @[SimplyCombineAddrDataSPad.scala 45:41]
    io_iactIOs_addrIOs_writeInDataIO_ready <= iactAddrSPad.io_commonIO_dataLenFinIO_writeInDataIO_ready @[SimplyCombineAddrDataSPad.scala 44:41]
    io_iactIOs_addrIOs_writeFin <= iactAddrSPad.io_commonIO_dataLenFinIO_writeFin @[SimplyCombineAddrDataSPad.scala 44:41]
    io_iactAddrWriteIdx <= iactAddrSPad.io_commonIO_writeIdx @[SimplyCombineAddrDataSPad.scala 46:23]
    io_iactMatrixColumn <= iactMatrixColumnReg @[SimplyCombineAddrDataSPad.scala 50:23]
    io_iactMatrixRow <= _T_27 @[SimplyCombineAddrDataSPad.scala 65:20]
    io_iactMatrixData <= _T_24 @[SimplyCombineAddrDataSPad.scala 64:21]
    io_iactMatrixDataBin <= iactDataSPad.io_commonIO_readOutData @[SimplyCombineAddrDataSPad.scala 62:24]
    io_iactAddrReadEn <= iactAddrSPadReadEnReg @[SimplyCombineAddrDataSPad.scala 54:21]
    io_iactAddrReadData <= iactAddrDataWire @[SimplyCombineAddrDataSPad.scala 52:23]
    io_iactDataReadIndex <= iactDataIndexWire @[SimplyCombineAddrDataSPad.scala 61:24]
    io_iactDataWriteIdx <= iactDataSPad.io_commonIO_writeIdx @[SimplyCombineAddrDataSPad.scala 47:23]
    iactAddrSPad.clock <= clock
    iactAddrSPad.reset <= reset
    iactAddrSPad.io_commonIO_readEn <= iactAddrSPadReadEnReg @[SimplyCombineAddrDataSPad.scala 53:35]
    iactAddrSPad.io_commonIO_writeEn <= io_writeEn @[SimplyCombineAddrDataSPad.scala 55:36]
    iactAddrSPad.io_commonIO_dataLenFinIO_writeInDataIO_valid <= io_iactIOs_addrIOs_writeInDataIO_valid @[SimplyCombineAddrDataSPad.scala 44:41]
    iactAddrSPad.io_commonIO_dataLenFinIO_writeInDataIO_bits_data <= io_iactIOs_addrIOs_writeInDataIO_bits_data @[SimplyCombineAddrDataSPad.scala 44:41]
    iactAddrSPad.io_addrIO_readInIdx is invalid
    iactAddrSPad.io_addrIO_indexInc <= iactAddrSPadIdxIncWire @[SimplyCombineAddrDataSPad.scala 40:35]
    iactAddrSPad.io_addrIO_readInIdxEn is invalid
    iactAddrSPad.io_dataIO_readInIdx is invalid
    iactAddrSPad.io_dataIO_indexInc is invalid
    iactAddrSPad.io_dataIO_readInIdxEn is invalid
    iactDataSPad.clock <= clock
    iactDataSPad.reset <= reset
    iactDataSPad.io_commonIO_readEn <= iactDataSPadReadEnReg @[SimplyCombineAddrDataSPad.scala 66:35]
    iactDataSPad.io_commonIO_writeEn <= io_writeEn @[SimplyCombineAddrDataSPad.scala 67:36]
    iactDataSPad.io_commonIO_dataLenFinIO_writeInDataIO_valid <= io_iactIOs_dataIOs_writeInDataIO_valid @[SimplyCombineAddrDataSPad.scala 45:41]
    iactDataSPad.io_commonIO_dataLenFinIO_writeInDataIO_bits_data <= io_iactIOs_dataIOs_writeInDataIO_bits_data @[SimplyCombineAddrDataSPad.scala 45:41]
    iactDataSPad.io_addrIO_readInIdx is invalid
    iactDataSPad.io_addrIO_indexInc is invalid
    iactDataSPad.io_addrIO_readInIdxEn is invalid
    iactDataSPad.io_dataIO_readInIdx is invalid
    iactDataSPad.io_dataIO_indexInc <= iactDataSPadIdxIncWire @[SimplyCombineAddrDataSPad.scala 41:35]
    iactDataSPad.io_dataIO_readInIdxEn is invalid
    sPad <= mux(reset, UInt<2>("h0"), _GEN_34) @[SimplyCombineAddrDataSPad.scala 78:14 SimplyCombineAddrDataSPad.scala 87:14 SimplyCombineAddrDataSPad.scala 93:14 SimplyCombineAddrDataSPad.scala 101:14 SimplyCombineAddrDataSPad.scala 106:14 SimplyCombineAddrDataSPad.scala 119:14]
    iactSPadZeroColumnReg <= mux(reset, UInt<1>("h0"), _GEN_38) @[SimplyCombineAddrDataSPad.scala 88:31 SimplyCombineAddrDataSPad.scala 103:31 SimplyCombineAddrDataSPad.scala 107:31]
    iactAddrSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_35) @[SimplyCombineAddrDataSPad.scala 79:31 SimplyCombineAddrDataSPad.scala 89:31 SimplyCombineAddrDataSPad.scala 94:31 SimplyCombineAddrDataSPad.scala 109:31 SimplyCombineAddrDataSPad.scala 120:31]
    iactDataSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_36) @[SimplyCombineAddrDataSPad.scala 80:31 SimplyCombineAddrDataSPad.scala 90:31 SimplyCombineAddrDataSPad.scala 95:31 SimplyCombineAddrDataSPad.scala 102:31 SimplyCombineAddrDataSPad.scala 110:31 SimplyCombineAddrDataSPad.scala 121:31]
    iactMatrixColumnReg <= mux(reset, UInt<4>("h0"), _GEN_40) @[SimplyCombineAddrDataSPad.scala 112:31 SimplyCombineAddrDataSPad.scala 116:31]
    iactZeroColumnNumber <= mux(reset, UInt<4>("h0"), _GEN_39) @[SimplyCombineAddrDataSPad.scala 91:30 SimplyCombineAddrDataSPad.scala 114:32]
    iactDataSPadFirstReadReg <= mux(reset, UInt<1>("h1"), _GEN_37) @[SimplyCombineAddrDataSPad.scala 81:34 SimplyCombineAddrDataSPad.scala 99:32]
