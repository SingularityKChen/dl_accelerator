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
  
    mem onePSumSRAMBank : @[GLBCluster.scala 131:57]
      data-type => UInt<20>
      depth => 768
      read-latency => 1
      write-latency => 1
      reader => _T_29
      writer => _T_12
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 143:44]
    reg nextValidReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nextValidReg) @[GLBCluster.scala 146:45]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 147:42]
    node _T_23 = and(io_dataPath_outIOs_ready, nextValidReg) @[GLBCluster.scala 157:35]
    node doReadWire = _T_23 @[GLBCluster.scala 137:40 GLBCluster.scala 157:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 148:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 133:41 GLBCluster.scala 108:15]
    node doEnWire = io_ctrlPath_doEn @[GLBCluster.scala 136:38 GLBCluster.scala 112:12]
    node _T_11 = and(doEnWire, io_dataPath_inIOs_valid) @[GLBCluster.scala 164:29]
    node doWriteWire = _T_11 @[GLBCluster.scala 138:41 GLBCluster.scala 164:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 148:22]
    reg _T_4 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), _T_4) @[Counter.scala 29:33]
    node _T_5 = eq(_T_4, UInt<5>("h17")) @[Counter.scala 37:24]
    node _T_6 = add(_T_4, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_7 = tail(_T_6, 1) @[Counter.scala 38:22]
    node _GEN_0 = mux(_T_5, UInt<1>("h0"), _T_7) @[Counter.scala 40:21]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 144:42 GLBCluster.scala 148:16]
    node _GEN_1 = mux(doIdxIncWire, _GEN_0, _T_4) @[Counter.scala 71:17]
    node _T_8 = and(doIdxIncWire, _T_5) @[Counter.scala 72:20]
    node _T_2 = io_ctrlPath_startIdx @[GLBCluster.scala 101:30 GLBCluster.scala 107:12]
    node _T_9 = add(_T_2, _T_4) @[GLBCluster.scala 113:25]
    node _T_10 = tail(_T_9, 1) @[GLBCluster.scala 113:25]
    node _T_3 = _T_10 @[GLBCluster.scala 102:37 GLBCluster.scala 113:13]
    node _GEN_2 = validif(doIdxIncWire, _T_3) @[GLBCluster.scala 166:24]
    node _GEN_3 = validif(doIdxIncWire, clock) @[GLBCluster.scala 166:24]
    node _GEN_4 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 166:24]
    node _GEN_5 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 166:24]
    node writeInData = io_dataPath_inIOs_bits @[GLBCluster.scala 139:41 GLBCluster.scala 163:17]
    node _GEN_6 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 166:24]
    node _T_13 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 151:56]
    node _T_14 = and(doEnWire, _T_13) @[GLBCluster.scala 151:53]
    node _T_15 = mux(writeOrRead, UInt<1>("h0"), _T_14) @[GLBCluster.scala 151:21]
    node nextValid = _T_15 @[GLBCluster.scala 145:39 GLBCluster.scala 151:15]
    node _T_16 = and(nextValid, io_dataPath_outIOs_ready) @[GLBCluster.scala 152:31]
    node _T_17 = eq(_T_8, UInt<1>("h0")) @[GLBCluster.scala 155:24]
    node _T_18 = eq(doEnWire, UInt<1>("h0")) @[GLBCluster.scala 155:56]
    node _T_19 = or(writeOrRead, _T_18) @[GLBCluster.scala 155:53]
    node _T_20 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 155:80]
    node _T_21 = mux(_T_19, waitForRead, _T_20) @[GLBCluster.scala 155:40]
    node _T_22 = mux(_T_17, _T_21, UInt<1>("h0")) @[GLBCluster.scala 155:23]
    node _T_24 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 158:54]
    node _T_25 = and(doReadWire, _T_24) @[GLBCluster.scala 158:51]
    node _GEN_7 = validif(_T_25, _T_3) @[GLBCluster.scala 158:32]
    node _T_26 = _GEN_7 @[GLBCluster.scala 158:32 GLBCluster.scala 158:32]
    node _T_27 = or(_T_26, UInt<10>("h0")) @[GLBCluster.scala 158:32]
    node _T_28 = bits(_T_27, 9, 0) @[GLBCluster.scala 158:32]
    node _GEN_8 = mux(_T_25, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 158:32]
    node _GEN_9 = validif(_T_25, _T_28) @[GLBCluster.scala 158:32]
    node _GEN_10 = validif(_T_25, clock) @[GLBCluster.scala 158:32]
    node readOutData = onePSumSRAMBank._T_29.data @[GLBCluster.scala 140:41 GLBCluster.scala 158:17]
    node _T_30 = mux(waitForRead, readOutData, UInt<1>("h0")) @[GLBCluster.scala 159:26]
    io_dataPath_inIOs_ready <= doWriteWire @[GLBCluster.scala 165:21]
    io_dataPath_outIOs_valid <= nextValidReg @[GLBCluster.scala 156:21]
    io_dataPath_outIOs_bits <= _T_30 @[GLBCluster.scala 159:20]
    io_ctrlPath_done <= doDoneReg @[GLBCluster.scala 109:20]
    io_debugIO_idx <= _T_3 @[GLBCluster.scala 172:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 173:20]
    io_debugIO_idxCount <= _T_4 @[GLBCluster.scala 121:25]
    io_debugIO_currentData is invalid
    io_debugIO_meetOneZero is invalid
    io_debugIO_theState is invalid
    io_debugIO_waitForRead <= waitForRead @[GLBCluster.scala 174:25]
    io_debugIO_doReadWire <= doReadWire @[GLBCluster.scala 175:24]
    onePSumSRAMBank._T_29.addr <= _GEN_9 @[GLBCluster.scala 158:32]
    onePSumSRAMBank._T_29.en <= _GEN_8 @[GLBCluster.scala 131:57 GLBCluster.scala 158:32]
    onePSumSRAMBank._T_29.clk <= _GEN_10 @[GLBCluster.scala 158:32]
    onePSumSRAMBank._T_12.addr <= _GEN_2
    onePSumSRAMBank._T_12.en <= _GEN_4 @[GLBCluster.scala 131:57]
    onePSumSRAMBank._T_12.clk <= _GEN_3
    onePSumSRAMBank._T_12.data <= _GEN_6
    onePSumSRAMBank._T_12.mask <= _GEN_5
    waitForRead <= mux(reset, UInt<1>("h0"), _T_22) @[GLBCluster.scala 155:17]
    nextValidReg <= mux(reset, UInt<1>("h0"), _T_16) @[GLBCluster.scala 152:18]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_8) @[GLBCluster.scala 110:13]
    _T_4 <= mux(reset, UInt<5>("h0"), _GEN_1) @[Counter.scala 38:13 Counter.scala 40:29]
