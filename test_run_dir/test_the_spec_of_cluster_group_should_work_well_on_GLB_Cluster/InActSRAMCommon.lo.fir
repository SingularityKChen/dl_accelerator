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
  
    mem theSRAM : @[GLBCluster.scala 54:57]
      data-type => UInt<4>
      depth => 486
      read-latency => 1
      write-latency => 1
      reader => _T_32
      writer => _T_20
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 66:44]
    reg nextValidReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nextValidReg) @[GLBCluster.scala 69:45]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 70:42]
    node _T_26 = and(io_dataPath_outIOs_data_ready, nextValidReg) @[GLBCluster.scala 76:35]
    node doReadWire = _T_26 @[GLBCluster.scala 60:40 GLBCluster.scala 76:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 71:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 56:41 GLBCluster.scala 113:15]
    node _T_12 = eq(writeOrRead, UInt<1>("h0")) @[GLBCluster.scala 116:16]
    node _T_13 = eq(doDoneReg, UInt<1>("h0")) @[GLBCluster.scala 116:32]
    node _T_14 = or(_T_12, _T_13) @[GLBCluster.scala 116:29]
    node _T_15 = and(_T_14, io_ctrlPath_doEn) @[GLBCluster.scala 116:44]
    node doEnWire = _T_15 @[GLBCluster.scala 59:38 GLBCluster.scala 116:12]
    node _T_19 = and(doEnWire, io_dataPath_inIOs_data_valid) @[GLBCluster.scala 83:29]
    node doWriteWire = _T_19 @[GLBCluster.scala 61:41 GLBCluster.scala 83:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 71:22]
    reg _T_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_3) @[GLBCluster.scala 99:43]
    reg _T_7 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_7) @[GLBCluster.scala 103:39]
    node writeInData = io_dataPath_inIOs_data_bits @[GLBCluster.scala 62:41 GLBCluster.scala 82:17]
    node readOutData = theSRAM._T_32.data @[GLBCluster.scala 63:41 GLBCluster.scala 77:17]
    node _T_16 = mux(writeOrRead, writeInData, readOutData) @[GLBCluster.scala 117:21]
    node _T_17 = add(_T_7, UInt<1>("h1")) @[GLBCluster.scala 120:26]
    node _T_18 = tail(_T_17, 1) @[GLBCluster.scala 120:26]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 67:42 GLBCluster.scala 71:16]
    node _GEN_0 = mux(doIdxIncWire, _T_18, _T_7) @[GLBCluster.scala 119:23]
    node _T_8 = pad(_T_16, 9) @[GLBCluster.scala 107:33 GLBCluster.scala 117:15]
    node _T_35 = eq(_T_8, UInt<1>("h0")) @[GLBCluster.scala 132:31]
    node _T_34 = _T_35 @[GLBCluster.scala 131:34 GLBCluster.scala 132:16]
    node _T_36 = and(_T_34, waitForRead) @[GLBCluster.scala 133:65]
    node _T_37 = mux(writeOrRead, _T_34, _T_36) @[GLBCluster.scala 133:24]
    node _T_2 = _T_37 @[GLBCluster.scala 98:36 GLBCluster.scala 133:18]
    node _T_39 = and(_T_2, doEnWire) @[GLBCluster.scala 136:38]
    node _T_40 = and(_T_39, _T_3) @[GLBCluster.scala 136:50]
    node _T_10 = _T_40 @[GLBCluster.scala 110:38 GLBCluster.scala 136:20]
    node _T_11 = _T_10 @[GLBCluster.scala 111:29 GLBCluster.scala 140:11]
    node _GEN_1 = mux(_T_11, UInt<1>("h0"), _GEN_0) @[GLBCluster.scala 122:18]
    node _GEN_2 = mux(_T_11, UInt<1>("h0"), _T_3) @[GLBCluster.scala 122:18]
    node _GEN_3 = validif(doIdxIncWire, _T_7) @[GLBCluster.scala 85:25]
    node _GEN_4 = validif(doIdxIncWire, clock) @[GLBCluster.scala 85:25]
    node _GEN_5 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 85:25]
    node _GEN_6 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 85:25]
    node _GEN_7 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 85:25]
    node _T_21 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 73:56]
    node _T_22 = and(doEnWire, _T_21) @[GLBCluster.scala 73:53]
    node _T_23 = mux(writeOrRead, UInt<1>("h0"), _T_22) @[GLBCluster.scala 73:21]
    node _T_24 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 74:50]
    node _T_25 = mux(writeOrRead, waitForRead, _T_24) @[GLBCluster.scala 74:23]
    node _T_27 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 77:54]
    node _T_28 = and(doReadWire, _T_27) @[GLBCluster.scala 77:51]
    node _GEN_8 = validif(_T_28, _T_7) @[GLBCluster.scala 77:32]
    node _T_29 = _GEN_8 @[GLBCluster.scala 77:32 GLBCluster.scala 77:32]
    node _T_30 = or(_T_29, UInt<9>("h0")) @[GLBCluster.scala 77:32]
    node _T_31 = bits(_T_30, 8, 0) @[GLBCluster.scala 77:32]
    node _GEN_9 = mux(_T_28, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 77:32]
    node _GEN_10 = validif(_T_28, _T_31) @[GLBCluster.scala 77:32]
    node _GEN_11 = validif(_T_28, clock) @[GLBCluster.scala 77:32]
    node _T_33 = mux(waitForRead, readOutData, UInt<1>("h0")) @[GLBCluster.scala 78:26]
    node _T_38 = and(_T_2, doEnWire) @[GLBCluster.scala 134:41]
    node _T_5 = _T_10 @[GLBCluster.scala 101:35 GLBCluster.scala 137:17]
    node _T_4 = _T_38 @[GLBCluster.scala 100:41 GLBCluster.scala 134:23]
    node _T_6 = _T_4 @[GLBCluster.scala 102:34 GLBCluster.scala 138:16]
    node _T_41 = mux(writeOrRead, _T_5, _T_6) @[GLBCluster.scala 139:20]
    node nextValid = _T_23 @[GLBCluster.scala 68:39 GLBCluster.scala 73:15]
    node _T_9 = _T_41 @[GLBCluster.scala 108:32 GLBCluster.scala 139:14]
    io_dataPath_inIOs_data_ready <= doWriteWire @[GLBCluster.scala 84:21]
    io_dataPath_outIOs_data_valid <= nextValidReg @[GLBCluster.scala 75:21]
    io_dataPath_outIOs_data_bits <= _T_33 @[GLBCluster.scala 78:20]
    io_ctrlPath_done <= _T_9 @[GLBCluster.scala 114:20]
    io_debugIO_idx <= _T_7 @[GLBCluster.scala 91:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 92:20]
    io_debugIO_idxCount is invalid
    io_debugIO_currentData <= bits(_T_8, 3, 0) @[GLBCluster.scala 144:28]
    io_debugIO_meetOneZero <= _T_3 @[GLBCluster.scala 145:28]
    theSRAM._T_32.addr <= _GEN_10 @[GLBCluster.scala 77:32]
    theSRAM._T_32.en <= _GEN_9 @[GLBCluster.scala 54:57 GLBCluster.scala 77:32]
    theSRAM._T_32.clk <= _GEN_11 @[GLBCluster.scala 77:32]
    theSRAM._T_20.addr <= _GEN_3
    theSRAM._T_20.en <= _GEN_5 @[GLBCluster.scala 54:57]
    theSRAM._T_20.clk <= _GEN_4
    theSRAM._T_20.data <= _GEN_7
    theSRAM._T_20.mask <= _GEN_6
    waitForRead <= mux(reset, UInt<1>("h0"), _T_25) @[GLBCluster.scala 74:17]
    nextValidReg <= nextValid @[GLBCluster.scala 69:45]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_9) @[GLBCluster.scala 115:13]
    _T_3 <= mux(reset, UInt<1>("h0"), _T_4) @[GLBCluster.scala 124:24 GLBCluster.scala 135:22]
    _T_7 <= mux(reset, UInt<9>("h0"), _GEN_1) @[GLBCluster.scala 120:14 GLBCluster.scala 123:14]
