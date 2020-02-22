circuit PSumSRAMBank :
  module PSumSRAMBank :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_inIOs_ready : UInt<1>
    input io_dataPath_inIOs_valid : UInt<1>
    input io_dataPath_inIOs_bits : UInt<20>
    input io_dataPath_outIOs_ready : UInt<1>
    output io_dataPath_outIOs_valid : UInt<1>
    output io_dataPath_outIOs_bits : UInt<20>
    input io_ctrlPath_writeOrRead : UInt<1>
    input io_ctrlPath_doEn : UInt<1>
    output io_ctrlPath_done : UInt<1>
    input io_ctrlPath_startIdx : UInt<10>
    output io_debugIO_idx : UInt<10>
    output io_debugIO_idxInc : UInt<1>
    output io_debugIO_idxCount : UInt<10>
    output io_debugIO_currentData : UInt<20>
    output io_debugIO_meetOneZero : UInt<1>
    output io_debugIO_theState : UInt<2>
    output io_debugIO_waitForRead : UInt<1>
    output io_debugIO_doReadWire : UInt<1>
  
    mem theSRAM : @[GLBCluster.scala 115:57]
      data-type => UInt<20>
      depth => 768
      read-latency => 1
      write-latency => 1
      reader => _T_29
      writer => _T_12
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 127:44]
    reg nextValidReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nextValidReg) @[GLBCluster.scala 130:45]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 131:42]
    node _T_23 = and(io_dataPath_outIOs_ready, nextValidReg) @[GLBCluster.scala 141:35]
    node doReadWire = _T_23 @[GLBCluster.scala 121:40 GLBCluster.scala 141:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 132:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 117:41 GLBCluster.scala 92:15]
    node doEnWire = io_ctrlPath_doEn @[GLBCluster.scala 120:38 GLBCluster.scala 96:12]
    node _T_11 = and(doEnWire, io_dataPath_inIOs_valid) @[GLBCluster.scala 148:29]
    node doWriteWire = _T_11 @[GLBCluster.scala 122:41 GLBCluster.scala 148:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 132:22]
    reg _T_4 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), _T_4) @[Counter.scala 29:33]
    node _T_5 = eq(_T_4, UInt<5>("h17")) @[Counter.scala 37:24]
    node _T_6 = add(_T_4, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_7 = tail(_T_6, 1) @[Counter.scala 38:22]
    node _GEN_0 = mux(_T_5, UInt<1>("h0"), _T_7) @[Counter.scala 40:21]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 128:42 GLBCluster.scala 132:16]
    node _GEN_1 = mux(doIdxIncWire, _GEN_0, _T_4) @[Counter.scala 71:17]
    node _T_8 = and(doIdxIncWire, _T_5) @[Counter.scala 72:20]
    node _T_2 = io_ctrlPath_startIdx @[GLBCluster.scala 85:30 GLBCluster.scala 91:12]
    node _T_9 = add(_T_2, _T_4) @[GLBCluster.scala 97:25]
    node _T_10 = tail(_T_9, 1) @[GLBCluster.scala 97:25]
    node _T_3 = _T_10 @[GLBCluster.scala 86:37 GLBCluster.scala 97:13]
    node _GEN_2 = validif(doIdxIncWire, _T_3) @[GLBCluster.scala 150:24]
    node _GEN_3 = validif(doIdxIncWire, clock) @[GLBCluster.scala 150:24]
    node _GEN_4 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 150:24]
    node _GEN_5 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 150:24]
    node writeInData = io_dataPath_inIOs_bits @[GLBCluster.scala 123:41 GLBCluster.scala 147:17]
    node _GEN_6 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 150:24]
    node _T_13 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 135:56]
    node _T_14 = and(doEnWire, _T_13) @[GLBCluster.scala 135:53]
    node _T_15 = mux(writeOrRead, UInt<1>("h0"), _T_14) @[GLBCluster.scala 135:21]
    node nextValid = _T_15 @[GLBCluster.scala 129:39 GLBCluster.scala 135:15]
    node _T_16 = and(nextValid, io_dataPath_outIOs_ready) @[GLBCluster.scala 136:31]
    node _T_17 = eq(_T_8, UInt<1>("h0")) @[GLBCluster.scala 139:24]
    node _T_18 = eq(doEnWire, UInt<1>("h0")) @[GLBCluster.scala 139:56]
    node _T_19 = or(writeOrRead, _T_18) @[GLBCluster.scala 139:53]
    node _T_20 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 139:80]
    node _T_21 = mux(_T_19, waitForRead, _T_20) @[GLBCluster.scala 139:40]
    node _T_22 = mux(_T_17, _T_21, UInt<1>("h0")) @[GLBCluster.scala 139:23]
    node _T_24 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 142:54]
    node _T_25 = and(doReadWire, _T_24) @[GLBCluster.scala 142:51]
    node _GEN_7 = validif(_T_25, _T_3) @[GLBCluster.scala 142:32]
    node _T_26 = _GEN_7 @[GLBCluster.scala 142:32 GLBCluster.scala 142:32]
    node _T_27 = or(_T_26, UInt<10>("h0")) @[GLBCluster.scala 142:32]
    node _T_28 = bits(_T_27, 9, 0) @[GLBCluster.scala 142:32]
    node _GEN_8 = mux(_T_25, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 142:32]
    node _GEN_9 = validif(_T_25, _T_28) @[GLBCluster.scala 142:32]
    node _GEN_10 = validif(_T_25, clock) @[GLBCluster.scala 142:32]
    node readOutData = theSRAM._T_29.data @[GLBCluster.scala 124:41 GLBCluster.scala 142:17]
    node _T_30 = mux(waitForRead, readOutData, UInt<1>("h0")) @[GLBCluster.scala 143:26]
    io_dataPath_inIOs_ready <= doWriteWire @[GLBCluster.scala 149:21]
    io_dataPath_outIOs_valid <= nextValidReg @[GLBCluster.scala 140:21]
    io_dataPath_outIOs_bits <= _T_30 @[GLBCluster.scala 143:20]
    io_ctrlPath_done <= doDoneReg @[GLBCluster.scala 93:20]
    io_debugIO_idx <= _T_3 @[GLBCluster.scala 156:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 157:20]
    io_debugIO_idxCount <= _T_4 @[GLBCluster.scala 105:25]
    io_debugIO_currentData is invalid
    io_debugIO_meetOneZero is invalid
    io_debugIO_theState is invalid
    io_debugIO_waitForRead <= waitForRead @[GLBCluster.scala 158:25]
    io_debugIO_doReadWire <= doReadWire @[GLBCluster.scala 159:24]
    theSRAM._T_29.addr <= _GEN_9 @[GLBCluster.scala 142:32]
    theSRAM._T_29.en <= _GEN_8 @[GLBCluster.scala 115:57 GLBCluster.scala 142:32]
    theSRAM._T_29.clk <= _GEN_10 @[GLBCluster.scala 142:32]
    theSRAM._T_12.addr <= _GEN_2
    theSRAM._T_12.en <= _GEN_4 @[GLBCluster.scala 115:57]
    theSRAM._T_12.clk <= _GEN_3
    theSRAM._T_12.data <= _GEN_6
    theSRAM._T_12.mask <= _GEN_5
    waitForRead <= mux(reset, UInt<1>("h0"), _T_22) @[GLBCluster.scala 139:17]
    nextValidReg <= mux(reset, UInt<1>("h0"), _T_16) @[GLBCluster.scala 136:18]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_8) @[GLBCluster.scala 94:13]
    _T_4 <= mux(reset, UInt<5>("h0"), _GEN_1) @[Counter.scala 38:13 Counter.scala 40:29]
