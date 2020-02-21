circuit InActSRAMCommon :
  module InActSRAMCommon :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_inIOs_data_ready : UInt<1>
    input io_dataPath_inIOs_data_valid : UInt<1>
    input io_dataPath_inIOs_data_bits : UInt<4>
    input io_dataPath_outIOs_data_ready : UInt<1>
    output io_dataPath_outIOs_data_valid : UInt<1>
    output io_dataPath_outIOs_data_bits : UInt<4>
    input io_ctrlPath_writeOrRead : UInt<1>
    input io_ctrlPath_doEn : UInt<1>
    output io_ctrlPath_done : UInt<1>
    output io_debugIO_idx : UInt<9>
    output io_debugIO_idxInc : UInt<1>
    output io_debugIO_idxCount : UInt<9>
    output io_debugIO_currentData : UInt<4>
    output io_debugIO_meetOneZero : UInt<1>
    output io_debugIO_theState : UInt<2>
  
    mem theSRAM : @[GLBCluster.scala 55:57]
      data-type => UInt<4>
      depth => 486
      read-latency => 1
      write-latency => 1
      reader => _T_31
      writer => _T_19
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 67:44]
    reg nextValidReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nextValidReg) @[GLBCluster.scala 70:45]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 71:42]
    node _T_25 = and(io_dataPath_outIOs_data_ready, nextValidReg) @[GLBCluster.scala 77:35]
    node doReadWire = _T_25 @[GLBCluster.scala 61:40 GLBCluster.scala 77:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 72:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 57:41 GLBCluster.scala 113:15]
    node _T_11 = eq(writeOrRead, UInt<1>("h0")) @[GLBCluster.scala 116:16]
    node _T_12 = eq(doDoneReg, UInt<1>("h0")) @[GLBCluster.scala 116:32]
    node _T_13 = or(_T_11, _T_12) @[GLBCluster.scala 116:29]
    node _T_14 = and(_T_13, io_ctrlPath_doEn) @[GLBCluster.scala 116:44]
    node doEnWire = _T_14 @[GLBCluster.scala 60:38 GLBCluster.scala 116:12]
    node _T_18 = and(doEnWire, io_dataPath_inIOs_data_valid) @[GLBCluster.scala 84:29]
    node doWriteWire = _T_18 @[GLBCluster.scala 62:41 GLBCluster.scala 84:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 72:22]
    reg _T_6 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_6) @[GLBCluster.scala 103:39]
    node writeInData = io_dataPath_inIOs_data_bits @[GLBCluster.scala 63:41 GLBCluster.scala 83:17]
    node readOutData = theSRAM._T_31.data @[GLBCluster.scala 64:41 GLBCluster.scala 78:17]
    node _T_15 = mux(writeOrRead, writeInData, readOutData) @[GLBCluster.scala 117:21]
    node _T_16 = add(_T_6, UInt<1>("h1")) @[GLBCluster.scala 120:26]
    node _T_17 = tail(_T_16, 1) @[GLBCluster.scala 120:26]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 68:42 GLBCluster.scala 72:16]
    node _GEN_0 = mux(doIdxIncWire, _T_17, _T_6) @[GLBCluster.scala 119:23]
    reg _T_33 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_33) @[GLBCluster.scala 131:34]
    node _T_42 = eq(_T_33, UInt<2>("h2")) @[GLBCluster.scala 153:33]
    node _T_9 = _T_42 @[GLBCluster.scala 110:38 GLBCluster.scala 153:20]
    node _T_10 = _T_9 @[GLBCluster.scala 111:29 GLBCluster.scala 157:11]
    node _GEN_1 = mux(_T_10, UInt<1>("h0"), _GEN_0) @[GLBCluster.scala 122:18]
    node _GEN_2 = validif(doIdxIncWire, _T_6) @[GLBCluster.scala 86:25]
    node _GEN_3 = validif(doIdxIncWire, clock) @[GLBCluster.scala 86:25]
    node _GEN_4 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 86:25]
    node _GEN_5 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 86:25]
    node _GEN_6 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 86:25]
    node _T_20 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 74:56]
    node _T_21 = and(doEnWire, _T_20) @[GLBCluster.scala 74:53]
    node _T_22 = mux(writeOrRead, UInt<1>("h0"), _T_21) @[GLBCluster.scala 74:21]
    node _T_23 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 75:50]
    node _T_24 = mux(writeOrRead, waitForRead, _T_23) @[GLBCluster.scala 75:23]
    node _T_26 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 78:54]
    node _T_27 = and(doReadWire, _T_26) @[GLBCluster.scala 78:51]
    node _GEN_7 = validif(_T_27, _T_6) @[GLBCluster.scala 78:32]
    node _T_28 = _GEN_7 @[GLBCluster.scala 78:32 GLBCluster.scala 78:32]
    node _T_29 = or(_T_28, UInt<9>("h0")) @[GLBCluster.scala 78:32]
    node _T_30 = bits(_T_29, 8, 0) @[GLBCluster.scala 78:32]
    node _GEN_8 = mux(_T_27, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 78:32]
    node _GEN_9 = validif(_T_27, _T_30) @[GLBCluster.scala 78:32]
    node _GEN_10 = validif(_T_27, clock) @[GLBCluster.scala 78:32]
    node _T_32 = mux(waitForRead, readOutData, UInt<1>("h0")) @[GLBCluster.scala 79:26]
    node _T_7 = _T_15 @[GLBCluster.scala 107:33 GLBCluster.scala 117:15]
    node _T_35 = eq(_T_7, UInt<1>("h0")) @[GLBCluster.scala 133:31]
    node _T_34 = _T_35 @[GLBCluster.scala 132:34 GLBCluster.scala 133:16]
    node _T_36 = and(_T_34, waitForRead) @[GLBCluster.scala 134:65]
    node _T_37 = mux(writeOrRead, _T_34, _T_36) @[GLBCluster.scala 134:24]
    node _T_2 = _T_37 @[GLBCluster.scala 99:36 GLBCluster.scala 134:18]
    node _T_38 = and(_T_2, doEnWire) @[GLBCluster.scala 135:41]
    node _T_39 = eq(UInt<2>("h0"), _T_33) @[Conditional.scala 37:30]
    node _T_3 = _T_38 @[GLBCluster.scala 100:41 GLBCluster.scala 135:23]
    node _GEN_11 = mux(_T_3, UInt<2>("h1"), _T_33) @[GLBCluster.scala 138:34]
    node _T_40 = eq(UInt<2>("h1"), _T_33) @[Conditional.scala 37:30]
    node _GEN_12 = mux(_T_3, UInt<2>("h2"), UInt<2>("h0")) @[GLBCluster.scala 143:34]
    node _T_41 = eq(UInt<2>("h2"), _T_33) @[Conditional.scala 37:30]
    node _GEN_13 = mux(_T_41, UInt<2>("h0"), _T_33) @[Conditional.scala 39:67]
    node _GEN_14 = mux(_T_40, _GEN_12, _GEN_13) @[Conditional.scala 39:67]
    node _GEN_15 = mux(_T_39, _GEN_11, _GEN_14) @[Conditional.scala 40:58]
    node _T_43 = eq(_T_33, UInt<2>("h1")) @[GLBCluster.scala 154:29]
    node _T_4 = _T_9 @[GLBCluster.scala 101:35 GLBCluster.scala 155:17]
    node _T_5 = _T_43 @[GLBCluster.scala 102:34 GLBCluster.scala 154:16]
    node _T_44 = mux(writeOrRead, _T_4, _T_5) @[GLBCluster.scala 156:20]
    node nextValid = _T_22 @[GLBCluster.scala 69:39 GLBCluster.scala 74:15]
    node _T_8 = _T_44 @[GLBCluster.scala 108:32 GLBCluster.scala 156:14]
    io_dataPath_inIOs_data_ready <= doWriteWire @[GLBCluster.scala 85:21]
    io_dataPath_outIOs_data_valid <= nextValidReg @[GLBCluster.scala 76:21]
    io_dataPath_outIOs_data_bits <= _T_32 @[GLBCluster.scala 79:20]
    io_ctrlPath_done <= _T_8 @[GLBCluster.scala 114:20]
    io_debugIO_idx <= _T_6 @[GLBCluster.scala 92:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 93:20]
    io_debugIO_idxCount is invalid
    io_debugIO_currentData <= _T_7 @[GLBCluster.scala 161:28]
    io_debugIO_meetOneZero <= _T_5 @[GLBCluster.scala 162:28]
    io_debugIO_theState <= _T_33 @[GLBCluster.scala 163:25]
    theSRAM._T_31.addr <= _GEN_9 @[GLBCluster.scala 78:32]
    theSRAM._T_31.en <= _GEN_8 @[GLBCluster.scala 55:57 GLBCluster.scala 78:32]
    theSRAM._T_31.clk <= _GEN_10 @[GLBCluster.scala 78:32]
    theSRAM._T_19.addr <= _GEN_2
    theSRAM._T_19.en <= _GEN_4 @[GLBCluster.scala 55:57]
    theSRAM._T_19.clk <= _GEN_3
    theSRAM._T_19.data <= _GEN_6
    theSRAM._T_19.mask <= _GEN_5
    waitForRead <= mux(reset, UInt<1>("h0"), _T_24) @[GLBCluster.scala 75:17]
    nextValidReg <= nextValid @[GLBCluster.scala 70:45]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_8) @[GLBCluster.scala 115:13]
    _T_6 <= mux(reset, UInt<9>("h0"), _GEN_1) @[GLBCluster.scala 120:14 GLBCluster.scala 123:14]
    _T_33 <= mux(reset, UInt<2>("h0"), _GEN_15) @[GLBCluster.scala 139:19 GLBCluster.scala 144:19 GLBCluster.scala 146:19 GLBCluster.scala 150:17]
