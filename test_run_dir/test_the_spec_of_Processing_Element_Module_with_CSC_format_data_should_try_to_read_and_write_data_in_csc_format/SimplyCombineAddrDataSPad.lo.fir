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
    input io_addrIO_readInIdxEn : UInt<1>
    input io_dataIO_readInIdx : UInt<4>
    input io_dataIO_indexInc : UInt<1>
    input io_dataIO_readInIdxEn : UInt<1>
  
    reg dataLenReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[SPadModule.scala 196:33]
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 199:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 200:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 205:53]
    node _T_1 = tail(_T, 1) @[SPadModule.scala 205:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[SPadModule.scala 205:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 206:51]
    node _T_4 = tail(_T_3, 1) @[SPadModule.scala 206:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[SPadModule.scala 206:35]
    node _T_6 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 210:42]
    node _T_7 = tail(_T_6, 1) @[SPadModule.scala 210:42]
    node writeWrapWire = _T_2 @[SPadModule.scala 202:33 SPadModule.scala 205:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_7) @[SPadModule.scala 211:26]
    node _GEN_1 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 208:32]
    node _GEN_2 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_0, padWriteIndexReg) @[SPadModule.scala 208:32]
    node _T_8 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, writeWrapWire) @[SPadModule.scala 219:62]
    reg addrSPad_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_0) @[SPadModule.scala 131:36]
    reg addrSPad_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_1) @[SPadModule.scala 131:36]
    reg addrSPad_2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_2) @[SPadModule.scala 131:36]
    reg addrSPad_3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_3) @[SPadModule.scala 131:36]
    reg addrSPad_4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_4) @[SPadModule.scala 131:36]
    reg addrSPad_5 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_5) @[SPadModule.scala 131:36]
    reg addrSPad_6 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_6) @[SPadModule.scala 131:36]
    reg addrSPad_7 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_7) @[SPadModule.scala 131:36]
    reg addrSPad_8 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), addrSPad_8) @[SPadModule.scala 131:36]
    node _addrSPad_padWriteIndexReg = io_commonIO_dataLenFinIO_writeInDataIO_bits_data @[SPadModule.scala 134:32 SPadModule.scala 134:32]
    node _GEN_3 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_0) @[SPadModule.scala 134:32]
    node _GEN_4 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_1) @[SPadModule.scala 134:32]
    node _GEN_5 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_2) @[SPadModule.scala 134:32]
    node _GEN_6 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_3) @[SPadModule.scala 134:32]
    node _GEN_7 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_4) @[SPadModule.scala 134:32]
    node _GEN_8 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_5) @[SPadModule.scala 134:32]
    node _GEN_9 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_6) @[SPadModule.scala 134:32]
    node _GEN_10 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_7) @[SPadModule.scala 134:32]
    node _GEN_11 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _addrSPad_padWriteIndexReg, addrSPad_8) @[SPadModule.scala 134:32]
    node _GEN_12 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_3, addrSPad_0) @[SPadModule.scala 133:32]
    node _GEN_13 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_4, addrSPad_1) @[SPadModule.scala 133:32]
    node _GEN_14 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_5, addrSPad_2) @[SPadModule.scala 133:32]
    node _GEN_15 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_6, addrSPad_3) @[SPadModule.scala 133:32]
    node _GEN_16 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_7, addrSPad_4) @[SPadModule.scala 133:32]
    node _GEN_17 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_8, addrSPad_5) @[SPadModule.scala 133:32]
    node _GEN_18 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_9, addrSPad_6) @[SPadModule.scala 133:32]
    node _GEN_19 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_10, addrSPad_7) @[SPadModule.scala 133:32]
    node _GEN_20 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_11, addrSPad_8) @[SPadModule.scala 133:32]
    node _T_10 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 138:40]
    node _T_11 = tail(_T_10, 1) @[SPadModule.scala 138:40]
    node readWrapWire = _T_5 @[SPadModule.scala 203:32 SPadModule.scala 206:16]
    node _GEN_21 = mux(readWrapWire, UInt<1>("h0"), _T_11) @[SPadModule.scala 139:25]
    node readIndexInc = io_addrIO_indexInc @[SPadModule.scala 204:32 SPadModule.scala 146:16]
    node _GEN_22 = mux(readIndexInc, _GEN_21, padReadIndexReg) @[SPadModule.scala 137:23]
    node _GEN_23 = validif(eq(UInt<1>("h0"), padReadIndexReg), addrSPad_0) @[SPadModule.scala 144:12]
    node _GEN_24 = mux(eq(UInt<1>("h1"), padReadIndexReg), addrSPad_1, _GEN_23) @[SPadModule.scala 144:12]
    node _GEN_25 = mux(eq(UInt<2>("h2"), padReadIndexReg), addrSPad_2, _GEN_24) @[SPadModule.scala 144:12]
    node _GEN_26 = mux(eq(UInt<2>("h3"), padReadIndexReg), addrSPad_3, _GEN_25) @[SPadModule.scala 144:12]
    node _GEN_27 = mux(eq(UInt<3>("h4"), padReadIndexReg), addrSPad_4, _GEN_26) @[SPadModule.scala 144:12]
    node _GEN_28 = mux(eq(UInt<3>("h5"), padReadIndexReg), addrSPad_5, _GEN_27) @[SPadModule.scala 144:12]
    node _GEN_29 = mux(eq(UInt<3>("h6"), padReadIndexReg), addrSPad_6, _GEN_28) @[SPadModule.scala 144:12]
    node _GEN_30 = mux(eq(UInt<3>("h7"), padReadIndexReg), addrSPad_7, _GEN_29) @[SPadModule.scala 144:12]
    node _GEN_31 = mux(eq(UInt<4>("h8"), padReadIndexReg), addrSPad_8, _GEN_30) @[SPadModule.scala 144:12]
    node _addrSPad_padReadIndexReg = _GEN_31 @[SPadModule.scala 144:12 SPadModule.scala 144:12 SPadModule.scala 144:12 SPadModule.scala 144:12 SPadModule.scala 144:12 SPadModule.scala 144:12 SPadModule.scala 144:12 SPadModule.scala 144:12 SPadModule.scala 144:12 SPadModule.scala 144:12]
    node dataWire = _addrSPad_padReadIndexReg @[SPadModule.scala 197:28 SPadModule.scala 144:12]
    node _T_9_0 = UInt<4>("h0") @[SPadModule.scala 131:44 SPadModule.scala 131:44]
    node _T_9_1 = UInt<4>("h0") @[SPadModule.scala 131:44 SPadModule.scala 131:44]
    node _T_9_2 = UInt<4>("h0") @[SPadModule.scala 131:44 SPadModule.scala 131:44]
    node _T_9_3 = UInt<4>("h0") @[SPadModule.scala 131:44 SPadModule.scala 131:44]
    node _T_9_4 = UInt<4>("h0") @[SPadModule.scala 131:44 SPadModule.scala 131:44]
    node _T_9_5 = UInt<4>("h0") @[SPadModule.scala 131:44 SPadModule.scala 131:44]
    node _T_9_6 = UInt<4>("h0") @[SPadModule.scala 131:44 SPadModule.scala 131:44]
    node _T_9_7 = UInt<4>("h0") @[SPadModule.scala 131:44 SPadModule.scala 131:44]
    node _T_9_8 = UInt<4>("h0") @[SPadModule.scala 131:44 SPadModule.scala 131:44]
    io_commonIO_columnNum <= padReadIndexReg @[SPadModule.scala 220:25]
    io_commonIO_readOutData <= dataWire @[SPadModule.scala 145:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[SPadModule.scala 218:24]
    io_commonIO_dataLenFinIO_writeInDataIO_ready <= _GEN_1 @[SPadModule.scala 209:27 SPadModule.scala 215:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_8 @[SPadModule.scala 219:37]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[SPadModule.scala 198:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[SPadModule.scala 210:22 SPadModule.scala 212:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_22) @[SPadModule.scala 138:21 SPadModule.scala 140:23]
    addrSPad_0 <= mux(reset, _T_9_0, _GEN_12) @[SPadModule.scala 134:32]
    addrSPad_1 <= mux(reset, _T_9_1, _GEN_13) @[SPadModule.scala 134:32]
    addrSPad_2 <= mux(reset, _T_9_2, _GEN_14) @[SPadModule.scala 134:32]
    addrSPad_3 <= mux(reset, _T_9_3, _GEN_15) @[SPadModule.scala 134:32]
    addrSPad_4 <= mux(reset, _T_9_4, _GEN_16) @[SPadModule.scala 134:32]
    addrSPad_5 <= mux(reset, _T_9_5, _GEN_17) @[SPadModule.scala 134:32]
    addrSPad_6 <= mux(reset, _T_9_6, _GEN_18) @[SPadModule.scala 134:32]
    addrSPad_7 <= mux(reset, _T_9_7, _GEN_19) @[SPadModule.scala 134:32]
    addrSPad_8 <= mux(reset, _T_9_8, _GEN_20) @[SPadModule.scala 134:32]

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
    input io_addrIO_readInIdxEn : UInt<1>
    input io_dataIO_readInIdx : UInt<4>
    input io_dataIO_indexInc : UInt<1>
    input io_dataIO_readInIdxEn : UInt<1>
  
    reg dataLenReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), dataLenReg) @[SPadModule.scala 196:33]
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 199:39]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 200:38]
    node _T = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 205:53]
    node _T_1 = tail(_T, 1) @[SPadModule.scala 205:53]
    node _T_2 = eq(padWriteIndexReg, _T_1) @[SPadModule.scala 205:37]
    node _T_3 = sub(dataLenReg, UInt<1>("h1")) @[SPadModule.scala 206:51]
    node _T_4 = tail(_T_3, 1) @[SPadModule.scala 206:51]
    node _T_5 = eq(padReadIndexReg, _T_4) @[SPadModule.scala 206:35]
    node _T_6 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 210:42]
    node _T_7 = tail(_T_6, 1) @[SPadModule.scala 210:42]
    node writeWrapWire = _T_2 @[SPadModule.scala 202:33 SPadModule.scala 205:17]
    node _GEN_0 = mux(writeWrapWire, UInt<1>("h0"), _T_7) @[SPadModule.scala 211:26]
    node _GEN_1 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 208:32]
    node _GEN_2 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_0, padWriteIndexReg) @[SPadModule.scala 208:32]
    node _T_8 = and(io_commonIO_dataLenFinIO_writeInDataIO_valid, writeWrapWire) @[SPadModule.scala 219:62]
    reg _T_10_0 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_0) @[SPadModule.scala 171:38]
    reg _T_10_1 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_1) @[SPadModule.scala 171:38]
    reg _T_10_2 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_2) @[SPadModule.scala 171:38]
    reg _T_10_3 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_3) @[SPadModule.scala 171:38]
    reg _T_10_4 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_4) @[SPadModule.scala 171:38]
    reg _T_10_5 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_5) @[SPadModule.scala 171:38]
    reg _T_10_6 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_6) @[SPadModule.scala 171:38]
    reg _T_10_7 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_7) @[SPadModule.scala 171:38]
    reg _T_10_8 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_8) @[SPadModule.scala 171:38]
    reg _T_10_9 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_9) @[SPadModule.scala 171:38]
    reg _T_10_10 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_10) @[SPadModule.scala 171:38]
    reg _T_10_11 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_11) @[SPadModule.scala 171:38]
    reg _T_10_12 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_12) @[SPadModule.scala 171:38]
    reg _T_10_13 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_13) @[SPadModule.scala 171:38]
    reg _T_10_14 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_14) @[SPadModule.scala 171:38]
    reg _T_10_15 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_10_15) @[SPadModule.scala 171:38]
    node _T_10_padWriteIndexReg = io_commonIO_dataLenFinIO_writeInDataIO_bits_data @[SPadModule.scala 173:34 SPadModule.scala 173:34]
    node _GEN_3 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_0) @[SPadModule.scala 173:34]
    node _GEN_4 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_1) @[SPadModule.scala 173:34]
    node _GEN_5 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_2) @[SPadModule.scala 173:34]
    node _GEN_6 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_3) @[SPadModule.scala 173:34]
    node _GEN_7 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_4) @[SPadModule.scala 173:34]
    node _GEN_8 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_5) @[SPadModule.scala 173:34]
    node _GEN_9 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_6) @[SPadModule.scala 173:34]
    node _GEN_10 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_7) @[SPadModule.scala 173:34]
    node _GEN_11 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_8) @[SPadModule.scala 173:34]
    node _GEN_12 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_9) @[SPadModule.scala 173:34]
    node _GEN_13 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_10) @[SPadModule.scala 173:34]
    node _GEN_14 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_11) @[SPadModule.scala 173:34]
    node _GEN_15 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_12) @[SPadModule.scala 173:34]
    node _GEN_16 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_13) @[SPadModule.scala 173:34]
    node _GEN_17 = mux(eq(UInt<4>("he"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_14) @[SPadModule.scala 173:34]
    node _GEN_18 = mux(eq(UInt<4>("hf"), padWriteIndexReg), _T_10_padWriteIndexReg, _T_10_15) @[SPadModule.scala 173:34]
    node _GEN_19 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_3, _T_10_0) @[SPadModule.scala 172:34]
    node _GEN_20 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_4, _T_10_1) @[SPadModule.scala 172:34]
    node _GEN_21 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_5, _T_10_2) @[SPadModule.scala 172:34]
    node _GEN_22 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_6, _T_10_3) @[SPadModule.scala 172:34]
    node _GEN_23 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_7, _T_10_4) @[SPadModule.scala 172:34]
    node _GEN_24 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_8, _T_10_5) @[SPadModule.scala 172:34]
    node _GEN_25 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_9, _T_10_6) @[SPadModule.scala 172:34]
    node _GEN_26 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_10, _T_10_7) @[SPadModule.scala 172:34]
    node _GEN_27 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_11, _T_10_8) @[SPadModule.scala 172:34]
    node _GEN_28 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_12, _T_10_9) @[SPadModule.scala 172:34]
    node _GEN_29 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_13, _T_10_10) @[SPadModule.scala 172:34]
    node _GEN_30 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_14, _T_10_11) @[SPadModule.scala 172:34]
    node _GEN_31 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_15, _T_10_12) @[SPadModule.scala 172:34]
    node _GEN_32 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_16, _T_10_13) @[SPadModule.scala 172:34]
    node _GEN_33 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_17, _T_10_14) @[SPadModule.scala 172:34]
    node _GEN_34 = mux(io_commonIO_dataLenFinIO_writeInDataIO_valid, _GEN_18, _T_10_15) @[SPadModule.scala 172:34]
    node _T_11 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 177:42]
    node _T_12 = tail(_T_11, 1) @[SPadModule.scala 177:42]
    node readWrapWire = _T_5 @[SPadModule.scala 203:32 SPadModule.scala 206:16]
    node _GEN_35 = mux(readWrapWire, UInt<1>("h0"), _T_12) @[SPadModule.scala 178:27]
    node readIndexInc = io_dataIO_indexInc @[SPadModule.scala 204:32 SPadModule.scala 185:16]
    node _GEN_36 = mux(readIndexInc, _GEN_35, padReadIndexReg) @[SPadModule.scala 176:25]
    node _GEN_37 = validif(eq(UInt<1>("h0"), padReadIndexReg), _T_10_0) @[SPadModule.scala 183:14]
    node _GEN_38 = mux(eq(UInt<1>("h1"), padReadIndexReg), _T_10_1, _GEN_37) @[SPadModule.scala 183:14]
    node _GEN_39 = mux(eq(UInt<2>("h2"), padReadIndexReg), _T_10_2, _GEN_38) @[SPadModule.scala 183:14]
    node _GEN_40 = mux(eq(UInt<2>("h3"), padReadIndexReg), _T_10_3, _GEN_39) @[SPadModule.scala 183:14]
    node _GEN_41 = mux(eq(UInt<3>("h4"), padReadIndexReg), _T_10_4, _GEN_40) @[SPadModule.scala 183:14]
    node _GEN_42 = mux(eq(UInt<3>("h5"), padReadIndexReg), _T_10_5, _GEN_41) @[SPadModule.scala 183:14]
    node _GEN_43 = mux(eq(UInt<3>("h6"), padReadIndexReg), _T_10_6, _GEN_42) @[SPadModule.scala 183:14]
    node _GEN_44 = mux(eq(UInt<3>("h7"), padReadIndexReg), _T_10_7, _GEN_43) @[SPadModule.scala 183:14]
    node _GEN_45 = mux(eq(UInt<4>("h8"), padReadIndexReg), _T_10_8, _GEN_44) @[SPadModule.scala 183:14]
    node _GEN_46 = mux(eq(UInt<4>("h9"), padReadIndexReg), _T_10_9, _GEN_45) @[SPadModule.scala 183:14]
    node _GEN_47 = mux(eq(UInt<4>("ha"), padReadIndexReg), _T_10_10, _GEN_46) @[SPadModule.scala 183:14]
    node _GEN_48 = mux(eq(UInt<4>("hb"), padReadIndexReg), _T_10_11, _GEN_47) @[SPadModule.scala 183:14]
    node _GEN_49 = mux(eq(UInt<4>("hc"), padReadIndexReg), _T_10_12, _GEN_48) @[SPadModule.scala 183:14]
    node _GEN_50 = mux(eq(UInt<4>("hd"), padReadIndexReg), _T_10_13, _GEN_49) @[SPadModule.scala 183:14]
    node _GEN_51 = mux(eq(UInt<4>("he"), padReadIndexReg), _T_10_14, _GEN_50) @[SPadModule.scala 183:14]
    node _GEN_52 = mux(eq(UInt<4>("hf"), padReadIndexReg), _T_10_15, _GEN_51) @[SPadModule.scala 183:14]
    node _T_10_padReadIndexReg = _GEN_52 @[SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14 SPadModule.scala 183:14]
    node dataWire = _T_10_padReadIndexReg @[SPadModule.scala 197:28 SPadModule.scala 183:14]
    node _T_9_0 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_1 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_2 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_3 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_4 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_5 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_6 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_7 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_8 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_9 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_10 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_11 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_12 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_13 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_14 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    node _T_9_15 = UInt<12>("h0") @[SPadModule.scala 171:46 SPadModule.scala 171:46]
    io_commonIO_columnNum <= padReadIndexReg @[SPadModule.scala 220:25]
    io_commonIO_readOutData <= dataWire @[SPadModule.scala 186:27]
    io_commonIO_writeIdx <= padWriteIndexReg @[SPadModule.scala 218:24]
    io_commonIO_dataLenFinIO_writeInDataIO_ready <= _GEN_1 @[SPadModule.scala 209:27 SPadModule.scala 215:27]
    io_commonIO_dataLenFinIO_writeFin <= _T_8 @[SPadModule.scala 219:37]
    dataLenReg <= mux(reset, UInt<4>("h9"), io_commonIO_dataLenFinIO_streamLen) @[SPadModule.scala 198:14]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _GEN_2) @[SPadModule.scala 210:22 SPadModule.scala 212:24]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_36) @[SPadModule.scala 177:23 SPadModule.scala 179:25]
    _T_10_0 <= mux(reset, _T_9_0, _GEN_19) @[SPadModule.scala 173:34]
    _T_10_1 <= mux(reset, _T_9_1, _GEN_20) @[SPadModule.scala 173:34]
    _T_10_2 <= mux(reset, _T_9_2, _GEN_21) @[SPadModule.scala 173:34]
    _T_10_3 <= mux(reset, _T_9_3, _GEN_22) @[SPadModule.scala 173:34]
    _T_10_4 <= mux(reset, _T_9_4, _GEN_23) @[SPadModule.scala 173:34]
    _T_10_5 <= mux(reset, _T_9_5, _GEN_24) @[SPadModule.scala 173:34]
    _T_10_6 <= mux(reset, _T_9_6, _GEN_25) @[SPadModule.scala 173:34]
    _T_10_7 <= mux(reset, _T_9_7, _GEN_26) @[SPadModule.scala 173:34]
    _T_10_8 <= mux(reset, _T_9_8, _GEN_27) @[SPadModule.scala 173:34]
    _T_10_9 <= mux(reset, _T_9_9, _GEN_28) @[SPadModule.scala 173:34]
    _T_10_10 <= mux(reset, _T_9_10, _GEN_29) @[SPadModule.scala 173:34]
    _T_10_11 <= mux(reset, _T_9_11, _GEN_30) @[SPadModule.scala 173:34]
    _T_10_12 <= mux(reset, _T_9_12, _GEN_31) @[SPadModule.scala 173:34]
    _T_10_13 <= mux(reset, _T_9_13, _GEN_32) @[SPadModule.scala 173:34]
    _T_10_14 <= mux(reset, _T_9_14, _GEN_33) @[SPadModule.scala 173:34]
    _T_10_15 <= mux(reset, _T_9_15, _GEN_34) @[SPadModule.scala 173:34]

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
  
    inst iactAddrSPad of SPadAddrModule @[SPadModule.scala 22:44]
    inst iactDataSPad of SPadDataModule @[SPadModule.scala 23:44]
    reg sPad : UInt<2>, clock with :
      reset => (UInt<1>("h0"), sPad) @[SPadModule.scala 26:27]
    reg iactSPadZeroColumnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactSPadZeroColumnReg) @[SPadModule.scala 30:44]
    reg iactAddrSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactAddrSPadReadEnReg) @[SPadModule.scala 31:44]
    reg iactDataSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactDataSPadReadEnReg) @[SPadModule.scala 32:44]
    reg iactMatrixColumnReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactMatrixColumnReg) @[SPadModule.scala 35:42]
    reg iactZeroColumnNumber : UInt<4>, clock with :
      reset => (UInt<1>("h0"), iactZeroColumnNumber) @[SPadModule.scala 36:43]
    reg iactDataSPadFirstReadReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), iactDataSPadFirstReadReg) @[SPadModule.scala 37:47]
    node _T = eq(sPad, UInt<2>("h2")) @[SPadModule.scala 40:36]
    node iactDataIndexWire = iactDataSPad.io_commonIO_columnNum @[SPadModule.scala 29:37 SPadModule.scala 57:21]
    node _T_1 = add(iactDataIndexWire, UInt<1>("h1")) @[SPadModule.scala 40:97]
    node _T_2 = tail(_T_1, 1) @[SPadModule.scala 40:97]
    node iactAddrDataWire = iactAddrSPad.io_commonIO_readOutData @[SPadModule.scala 28:36 SPadModule.scala 49:20]
    node _T_3 = eq(iactAddrDataWire, _T_2) @[SPadModule.scala 40:74]
    node _T_4 = and(_T, _T_3) @[SPadModule.scala 40:53]
    node _T_5 = eq(sPad, UInt<2>("h1")) @[SPadModule.scala 40:116]
    node _T_6 = eq(iactAddrDataWire, UInt<4>("hf")) @[SPadModule.scala 40:154]
    node _T_7 = and(_T_5, _T_6) @[SPadModule.scala 40:133]
    node _T_8 = or(_T_4, _T_7) @[SPadModule.scala 40:106]
    node _T_9 = eq(sPad, UInt<2>("h1")) @[SPadModule.scala 41:36]
    node _T_10 = eq(iactSPadZeroColumnReg, UInt<1>("h0")) @[SPadModule.scala 41:56]
    node _T_11 = and(_T_9, _T_10) @[SPadModule.scala 41:53]
    node _T_12 = eq(iactDataSPadFirstReadReg, UInt<1>("h0")) @[SPadModule.scala 41:82]
    node _T_13 = and(_T_11, _T_12) @[SPadModule.scala 41:79]
    node _T_14 = eq(sPad, UInt<2>("h2")) @[SPadModule.scala 41:119]
    node iactAddrSPadIdxIncWire = _T_8 @[SPadModule.scala 33:42 SPadModule.scala 40:26]
    node _T_15 = eq(iactAddrSPadIdxIncWire, UInt<1>("h0")) @[SPadModule.scala 41:139]
    node _T_16 = and(_T_14, _T_15) @[SPadModule.scala 41:136]
    node _T_17 = or(_T_13, _T_16) @[SPadModule.scala 41:109]
    node iactDataCountVec_0 = bits(iactDataSPad.io_commonIO_readOutData, 0, 0) @[SPadModule.scala 60:74]
    node iactDataCountVec_1 = bits(iactDataSPad.io_commonIO_readOutData, 1, 1) @[SPadModule.scala 60:74]
    node iactDataCountVec_2 = bits(iactDataSPad.io_commonIO_readOutData, 2, 2) @[SPadModule.scala 60:74]
    node iactDataCountVec_3 = bits(iactDataSPad.io_commonIO_readOutData, 3, 3) @[SPadModule.scala 60:74]
    node iactDataCountVec_4 = bits(iactDataSPad.io_commonIO_readOutData, 4, 4) @[SPadModule.scala 60:74]
    node iactDataCountVec_5 = bits(iactDataSPad.io_commonIO_readOutData, 5, 5) @[SPadModule.scala 60:74]
    node iactDataCountVec_6 = bits(iactDataSPad.io_commonIO_readOutData, 6, 6) @[SPadModule.scala 60:74]
    node iactDataCountVec_7 = bits(iactDataSPad.io_commonIO_readOutData, 7, 7) @[SPadModule.scala 60:74]
    node iactDataCountVec_8 = bits(iactDataSPad.io_commonIO_readOutData, 8, 8) @[SPadModule.scala 60:74]
    node iactDataCountVec_9 = bits(iactDataSPad.io_commonIO_readOutData, 9, 9) @[SPadModule.scala 60:74]
    node iactDataCountVec_10 = bits(iactDataSPad.io_commonIO_readOutData, 10, 10) @[SPadModule.scala 60:74]
    node iactDataCountVec_11 = bits(iactDataSPad.io_commonIO_readOutData, 11, 11) @[SPadModule.scala 60:74]
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
    node _GEN_0 = mux(io_iactDataReq, UInt<2>("h1"), sPad) @[SPadModule.scala 73:28]
    node _GEN_1 = mux(io_iactDataReq, UInt<1>("h1"), iactAddrSPadReadEnReg) @[SPadModule.scala 73:28]
    node _GEN_2 = mux(io_iactDataReq, UInt<1>("h0"), iactDataSPadReadEnReg) @[SPadModule.scala 73:28]
    node _GEN_3 = mux(io_iactDataReq, UInt<1>("h1"), iactDataSPadFirstReadReg) @[SPadModule.scala 73:28]
    node _T_29 = eq(UInt<2>("h1"), sPad) @[Conditional.scala 37:30]
    node _T_30 = eq(iactAddrDataWire, UInt<4>("hf")) @[SPadModule.scala 82:30]
    node _T_31 = add(iactZeroColumnNumber, UInt<1>("h1")) @[SPadModule.scala 87:54]
    node _T_32 = tail(_T_31, 1) @[SPadModule.scala 87:54]
    node _GEN_4 = mux(_T_30, UInt<2>("h1"), UInt<2>("h2")) @[SPadModule.scala 82:56]
    node _GEN_5 = mux(_T_30, UInt<1>("h1"), iactSPadZeroColumnReg) @[SPadModule.scala 82:56]
    node _GEN_6 = mux(_T_30, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 82:56]
    node _GEN_7 = mux(_T_30, UInt<1>("h0"), UInt<1>("h1")) @[SPadModule.scala 82:56]
    node _GEN_8 = mux(_T_30, _T_32, iactZeroColumnNumber) @[SPadModule.scala 82:56]
    node _T_33 = eq(UInt<2>("h2"), sPad) @[Conditional.scala 37:30]
    node _T_34 = sub(io_iactIOs_dataIOs_streamLen, UInt<1>("h1")) @[SPadModule.scala 96:64]
    node _T_35 = tail(_T_34, 1) @[SPadModule.scala 96:64]
    node _T_36 = eq(iactDataIndexWire, _T_35) @[SPadModule.scala 96:31]
    node _GEN_9 = mux(_T_36, UInt<2>("h0"), sPad) @[SPadModule.scala 96:71]
    node _GEN_10 = mux(_T_36, UInt<1>("h0"), iactDataSPadReadEnReg) @[SPadModule.scala 96:71]
    node _GEN_11 = mux(_T_36, UInt<1>("h0"), iactSPadZeroColumnReg) @[SPadModule.scala 96:71]
    node _T_37 = add(iactMatrixColumnReg, UInt<1>("h1")) @[SPadModule.scala 108:54]
    node _T_38 = tail(_T_37, 1) @[SPadModule.scala 108:54]
    node _T_39 = add(_T_38, iactZeroColumnNumber) @[SPadModule.scala 108:60]
    node _T_40 = tail(_T_39, 1) @[SPadModule.scala 108:60]
    node _T_41 = add(iactMatrixColumnReg, UInt<1>("h1")) @[SPadModule.scala 112:54]
    node _T_42 = tail(_T_41, 1) @[SPadModule.scala 112:54]
    node _GEN_12 = mux(iactSPadZeroColumnReg, _T_40, _T_42) @[SPadModule.scala 107:38]
    node _GEN_13 = mux(iactSPadZeroColumnReg, UInt<1>("h0"), iactZeroColumnNumber) @[SPadModule.scala 107:38]
    node _GEN_14 = mux(iactAddrSPadIdxIncWire, UInt<2>("h1"), UInt<2>("h2")) @[SPadModule.scala 101:37]
    node _GEN_15 = mux(iactAddrSPadIdxIncWire, UInt<1>("h0"), _GEN_11) @[SPadModule.scala 101:37]
    node _GEN_16 = mux(iactAddrSPadIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 101:37]
    node _GEN_17 = mux(iactAddrSPadIdxIncWire, UInt<1>("h0"), UInt<1>("h1")) @[SPadModule.scala 101:37]
    node _GEN_18 = mux(iactAddrSPadIdxIncWire, _GEN_12, iactMatrixColumnReg) @[SPadModule.scala 101:37]
    node _GEN_19 = mux(iactAddrSPadIdxIncWire, _GEN_13, iactZeroColumnNumber) @[SPadModule.scala 101:37]
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
    node iactAddrIndexWire = iactAddrSPad.io_commonIO_columnNum @[SPadModule.scala 27:37 SPadModule.scala 47:21]
    node iactDataSPadIdxIncWire = _T_17 @[SPadModule.scala 34:42 SPadModule.scala 41:26]
    io_iactIOs_dataIOs_writeInDataIO_ready <= iactDataSPad.io_commonIO_dataLenFinIO_writeInDataIO_ready @[SPadModule.scala 43:41]
    io_iactIOs_dataIOs_writeFin <= iactDataSPad.io_commonIO_dataLenFinIO_writeFin @[SPadModule.scala 43:41]
    io_iactIOs_addrIOs_writeInDataIO_ready <= iactAddrSPad.io_commonIO_dataLenFinIO_writeInDataIO_ready @[SPadModule.scala 42:41]
    io_iactIOs_addrIOs_writeFin <= iactAddrSPad.io_commonIO_dataLenFinIO_writeFin @[SPadModule.scala 42:41]
    io_iactAddrWriteIdx <= iactAddrSPad.io_commonIO_writeIdx @[SPadModule.scala 44:23]
    io_iactMatrixColumn <= iactMatrixColumnReg @[SPadModule.scala 48:23]
    io_iactMatrixRow <= _T_27 @[SPadModule.scala 62:20]
    io_iactMatrixData <= _T_24 @[SPadModule.scala 61:21]
    io_iactMatrixDataBin <= iactDataSPad.io_commonIO_readOutData @[SPadModule.scala 59:24]
    io_iactAddrReadEn <= iactAddrSPadReadEnReg @[SPadModule.scala 52:21]
    io_iactAddrReadData <= iactAddrDataWire @[SPadModule.scala 50:23]
    io_iactDataReadIndex <= iactDataIndexWire @[SPadModule.scala 58:24]
    io_iactDataWriteIdx <= iactDataSPad.io_commonIO_writeIdx @[SPadModule.scala 45:23]
    iactAddrSPad.clock <= clock
    iactAddrSPad.reset <= reset
    iactAddrSPad.io_commonIO_readEn <= iactAddrSPadReadEnReg @[SPadModule.scala 51:35]
    iactAddrSPad.io_commonIO_dataLenFinIO_writeInDataIO_valid <= io_iactIOs_addrIOs_writeInDataIO_valid @[SPadModule.scala 42:41]
    iactAddrSPad.io_commonIO_dataLenFinIO_writeInDataIO_bits_data <= io_iactIOs_addrIOs_writeInDataIO_bits_data @[SPadModule.scala 42:41]
    iactAddrSPad.io_commonIO_dataLenFinIO_streamLen <= io_iactIOs_addrIOs_streamLen @[SPadModule.scala 42:41]
    iactAddrSPad.io_addrIO_readInIdx is invalid
    iactAddrSPad.io_addrIO_indexInc <= iactAddrSPadIdxIncWire @[SPadModule.scala 38:35]
    iactAddrSPad.io_addrIO_readInIdxEn is invalid
    iactAddrSPad.io_dataIO_readInIdx is invalid
    iactAddrSPad.io_dataIO_indexInc is invalid
    iactAddrSPad.io_dataIO_readInIdxEn is invalid
    iactDataSPad.clock <= clock
    iactDataSPad.reset <= reset
    iactDataSPad.io_commonIO_readEn <= iactDataSPadReadEnReg @[SPadModule.scala 63:35]
    iactDataSPad.io_commonIO_dataLenFinIO_writeInDataIO_valid <= io_iactIOs_dataIOs_writeInDataIO_valid @[SPadModule.scala 43:41]
    iactDataSPad.io_commonIO_dataLenFinIO_writeInDataIO_bits_data <= io_iactIOs_dataIOs_writeInDataIO_bits_data @[SPadModule.scala 43:41]
    iactDataSPad.io_commonIO_dataLenFinIO_streamLen <= io_iactIOs_dataIOs_streamLen @[SPadModule.scala 43:41]
    iactDataSPad.io_addrIO_readInIdx is invalid
    iactDataSPad.io_addrIO_indexInc is invalid
    iactDataSPad.io_addrIO_readInIdxEn is invalid
    iactDataSPad.io_dataIO_readInIdx is invalid
    iactDataSPad.io_dataIO_indexInc <= iactDataSPadIdxIncWire @[SPadModule.scala 39:35]
    iactDataSPad.io_dataIO_readInIdxEn is invalid
    sPad <= mux(reset, UInt<2>("h0"), _GEN_34) @[SPadModule.scala 74:14 SPadModule.scala 83:14 SPadModule.scala 89:14 SPadModule.scala 97:14 SPadModule.scala 102:14 SPadModule.scala 115:14]
    iactSPadZeroColumnReg <= mux(reset, UInt<1>("h0"), _GEN_38) @[SPadModule.scala 84:31 SPadModule.scala 99:31 SPadModule.scala 103:31]
    iactAddrSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_35) @[SPadModule.scala 75:31 SPadModule.scala 85:31 SPadModule.scala 90:31 SPadModule.scala 105:31 SPadModule.scala 116:31]
    iactDataSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_36) @[SPadModule.scala 76:31 SPadModule.scala 86:31 SPadModule.scala 91:31 SPadModule.scala 98:31 SPadModule.scala 106:31 SPadModule.scala 117:31]
    iactMatrixColumnReg <= mux(reset, UInt<4>("h0"), _GEN_40) @[SPadModule.scala 108:31 SPadModule.scala 112:31]
    iactZeroColumnNumber <= mux(reset, UInt<4>("h0"), _GEN_39) @[SPadModule.scala 87:30 SPadModule.scala 110:32]
    iactDataSPadFirstReadReg <= mux(reset, UInt<1>("h1"), _GEN_37) @[SPadModule.scala 77:34 SPadModule.scala 95:32]
