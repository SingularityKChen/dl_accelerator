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
  
    mem theSRAM : @[GLBCluster.scala 52:57]
      data-type => UInt<4>
      depth => 486
      read-latency => 1
      write-latency => 1
      reader => _T_25
      writer => _T_16
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 62:44]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 65:42]
    node _T_9 = eq(doDoneReg, UInt<1>("h0")) @[GLBCluster.scala 107:15]
    node _T_10 = and(_T_9, io_ctrlPath_doEn) @[GLBCluster.scala 107:26]
    node doEnWire = _T_10 @[GLBCluster.scala 57:38 GLBCluster.scala 107:12]
    node nextValid = doEnWire @[GLBCluster.scala 64:39 GLBCluster.scala 68:15]
    node _T_19 = and(io_dataPath_outIOs_data_ready, nextValid) @[GLBCluster.scala 71:35]
    node doReadWire = _T_19 @[GLBCluster.scala 58:40 GLBCluster.scala 71:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 66:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 54:41 GLBCluster.scala 104:15]
    node _T_15 = and(doEnWire, io_dataPath_inIOs_data_valid) @[GLBCluster.scala 78:29]
    node doWriteWire = _T_15 @[GLBCluster.scala 59:41 GLBCluster.scala 78:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 66:22]
    reg _T_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_3) @[GLBCluster.scala 94:43]
    reg _T_6 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_6) @[GLBCluster.scala 97:39]
    node _T_14 = mux(writeOrRead, io_dataPath_inIOs_data_bits, UInt<1>("h0")) @[GLBCluster.scala 77:23]
    node writeInData = _T_14 @[GLBCluster.scala 60:41 GLBCluster.scala 77:17]
    node readOutData = theSRAM._T_25.data @[GLBCluster.scala 61:41 GLBCluster.scala 72:17]
    node _T_11 = mux(writeOrRead, writeInData, readOutData) @[GLBCluster.scala 108:21]
    node _T_12 = add(_T_6, UInt<1>("h1")) @[GLBCluster.scala 110:26]
    node _T_13 = tail(_T_12, 1) @[GLBCluster.scala 110:26]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 63:42 GLBCluster.scala 66:16]
    node _GEN_0 = mux(doIdxIncWire, _T_13, _T_6) @[GLBCluster.scala 109:23]
    node _GEN_1 = validif(doIdxIncWire, _T_6) @[GLBCluster.scala 80:25]
    node _GEN_2 = validif(doIdxIncWire, clock) @[GLBCluster.scala 80:25]
    node _GEN_3 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 80:25]
    node _GEN_4 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 80:25]
    node _GEN_5 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 80:25]
    node _T_17 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 69:20]
    reg _T_18 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_18) @[GLBCluster.scala 70:31]
    node _T_20 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 72:54]
    node _T_21 = and(doReadWire, _T_20) @[GLBCluster.scala 72:51]
    node _GEN_6 = validif(_T_21, _T_6) @[GLBCluster.scala 72:32]
    node _T_22 = _GEN_6 @[GLBCluster.scala 72:32 GLBCluster.scala 72:32]
    node _T_23 = or(_T_22, UInt<9>("h0")) @[GLBCluster.scala 72:32]
    node _T_24 = bits(_T_23, 8, 0) @[GLBCluster.scala 72:32]
    node _GEN_7 = mux(_T_21, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 72:32]
    node _GEN_8 = validif(_T_21, _T_24) @[GLBCluster.scala 72:32]
    node _GEN_9 = validif(_T_21, clock) @[GLBCluster.scala 72:32]
    node _T_26 = mux(writeOrRead, UInt<1>("h0"), readOutData) @[GLBCluster.scala 73:26]
    node _T_7 = pad(_T_11, 9) @[GLBCluster.scala 101:33 GLBCluster.scala 108:15]
    node _T_27 = eq(_T_7, UInt<1>("h0")) @[GLBCluster.scala 117:33]
    node _T_2 = _T_27 @[GLBCluster.scala 93:36 GLBCluster.scala 117:18]
    node _T_28 = and(_T_2, doEnWire) @[GLBCluster.scala 118:40]
    node _T_29 = and(_T_2, doEnWire) @[GLBCluster.scala 120:35]
    node _T_30 = and(_T_29, _T_3) @[GLBCluster.scala 120:47]
    node _T_4 = _T_30 @[GLBCluster.scala 95:35 GLBCluster.scala 120:17]
    node _T_5 = _T_3 @[GLBCluster.scala 96:34 GLBCluster.scala 121:16]
    node _T_31 = mux(writeOrRead, _T_4, _T_5) @[GLBCluster.scala 122:20]
    node _T_8 = _T_31 @[GLBCluster.scala 102:32 GLBCluster.scala 122:14]
    io_dataPath_inIOs_data_ready <= doWriteWire @[GLBCluster.scala 79:21]
    io_dataPath_outIOs_data_valid <= _T_18 @[GLBCluster.scala 70:21]
    io_dataPath_outIOs_data_bits <= _T_26 @[GLBCluster.scala 73:20]
    io_ctrlPath_done <= _T_8 @[GLBCluster.scala 105:20]
    io_debugIO_idx <= _T_6 @[GLBCluster.scala 86:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 87:20]
    io_debugIO_idxCount is invalid
    theSRAM._T_25.addr <= _GEN_8 @[GLBCluster.scala 72:32]
    theSRAM._T_25.en <= _GEN_7 @[GLBCluster.scala 52:57 GLBCluster.scala 72:32]
    theSRAM._T_25.clk <= _GEN_9 @[GLBCluster.scala 72:32]
    theSRAM._T_16.addr <= _GEN_1
    theSRAM._T_16.en <= _GEN_3 @[GLBCluster.scala 52:57]
    theSRAM._T_16.clk <= _GEN_2
    theSRAM._T_16.data <= _GEN_5
    theSRAM._T_16.mask <= _GEN_4
    waitForRead <= mux(reset, UInt<1>("h1"), _T_17) @[GLBCluster.scala 69:17]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_8) @[GLBCluster.scala 106:13]
    _T_3 <= mux(reset, UInt<1>("h0"), _T_28) @[GLBCluster.scala 118:22]
    _T_6 <= mux(reset, UInt<9>("h0"), _GEN_0) @[GLBCluster.scala 110:14]
    _T_18 <= nextValid @[GLBCluster.scala 70:31]
