circuit SPadAddrModule :
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
