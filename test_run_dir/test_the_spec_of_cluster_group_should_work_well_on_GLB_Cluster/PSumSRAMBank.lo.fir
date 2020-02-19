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
  
    mem theSRAM : @[GLBCluster.scala 52:57]
      data-type => UInt<20>
      depth => 768
      read-latency => 1
      write-latency => 1
      reader => _T_22
      writer => _T_13
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 62:44]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 65:42]
    node doEnWire = io_ctrlPath_doEn @[GLBCluster.scala 57:38 GLBCluster.scala 36:12]
    node nextValid = doEnWire @[GLBCluster.scala 64:39 GLBCluster.scala 68:15]
    node _T_16 = and(io_dataPath_outIOs_ready, nextValid) @[GLBCluster.scala 71:35]
    node doReadWire = _T_16 @[GLBCluster.scala 58:40 GLBCluster.scala 71:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 66:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 54:41 GLBCluster.scala 32:15]
    node _T_12 = and(doEnWire, io_dataPath_inIOs_valid) @[GLBCluster.scala 78:29]
    node doWriteWire = _T_12 @[GLBCluster.scala 59:41 GLBCluster.scala 78:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 66:22]
    reg _T_4 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), _T_4) @[Counter.scala 29:33]
    node _T_5 = eq(_T_4, UInt<5>("h17")) @[Counter.scala 37:24]
    node _T_6 = add(_T_4, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_7 = tail(_T_6, 1) @[Counter.scala 38:22]
    node _GEN_0 = mux(_T_5, UInt<1>("h0"), _T_7) @[Counter.scala 40:21]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 63:42 GLBCluster.scala 66:16]
    node _GEN_1 = mux(doIdxIncWire, _GEN_0, _T_4) @[Counter.scala 71:17]
    node _T_8 = and(doIdxIncWire, _T_5) @[Counter.scala 72:20]
    node _T_2 = io_ctrlPath_startIdx @[GLBCluster.scala 25:30 GLBCluster.scala 31:12]
    node _T_9 = add(_T_2, _T_4) @[GLBCluster.scala 37:25]
    node _T_10 = tail(_T_9, 1) @[GLBCluster.scala 37:25]
    node _T_11 = mux(writeOrRead, io_dataPath_inIOs_bits, UInt<1>("h0")) @[GLBCluster.scala 77:23]
    node _T_3 = _T_10 @[GLBCluster.scala 26:37 GLBCluster.scala 37:13]
    node _GEN_2 = validif(doIdxIncWire, _T_3) @[GLBCluster.scala 80:25]
    node _GEN_3 = validif(doIdxIncWire, clock) @[GLBCluster.scala 80:25]
    node _GEN_4 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 80:25]
    node _GEN_5 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 80:25]
    node writeInData = _T_11 @[GLBCluster.scala 60:41 GLBCluster.scala 77:17]
    node _GEN_6 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 80:25]
    node _T_14 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 69:20]
    reg _T_15 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_15) @[GLBCluster.scala 70:31]
    node _T_17 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 72:54]
    node _T_18 = and(doReadWire, _T_17) @[GLBCluster.scala 72:51]
    node _GEN_7 = validif(_T_18, _T_3) @[GLBCluster.scala 72:32]
    node _T_19 = _GEN_7 @[GLBCluster.scala 72:32 GLBCluster.scala 72:32]
    node _T_20 = or(_T_19, UInt<10>("h0")) @[GLBCluster.scala 72:32]
    node _T_21 = bits(_T_20, 9, 0) @[GLBCluster.scala 72:32]
    node _GEN_8 = mux(_T_18, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 72:32]
    node _GEN_9 = validif(_T_18, _T_21) @[GLBCluster.scala 72:32]
    node _GEN_10 = validif(_T_18, clock) @[GLBCluster.scala 72:32]
    node readOutData = theSRAM._T_22.data @[GLBCluster.scala 61:41 GLBCluster.scala 72:17]
    node _T_23 = mux(writeOrRead, UInt<1>("h0"), readOutData) @[GLBCluster.scala 73:26]
    io_dataPath_inIOs_ready <= doWriteWire @[GLBCluster.scala 79:21]
    io_dataPath_outIOs_valid <= _T_15 @[GLBCluster.scala 70:21]
    io_dataPath_outIOs_bits <= _T_23 @[GLBCluster.scala 73:20]
    io_ctrlPath_done <= _T_8 @[GLBCluster.scala 33:20]
    io_debugIO_idx <= _T_3 @[GLBCluster.scala 86:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 87:20]
    io_debugIO_idxCount <= _T_4 @[GLBCluster.scala 45:25]
    theSRAM._T_22.addr <= _GEN_9 @[GLBCluster.scala 72:32]
    theSRAM._T_22.en <= _GEN_8 @[GLBCluster.scala 52:57 GLBCluster.scala 72:32]
    theSRAM._T_22.clk <= _GEN_10 @[GLBCluster.scala 72:32]
    theSRAM._T_13.addr <= _GEN_2
    theSRAM._T_13.en <= _GEN_4 @[GLBCluster.scala 52:57]
    theSRAM._T_13.clk <= _GEN_3
    theSRAM._T_13.data <= _GEN_6
    theSRAM._T_13.mask <= _GEN_5
    waitForRead <= mux(reset, UInt<1>("h1"), _T_14) @[GLBCluster.scala 69:17]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_8) @[GLBCluster.scala 34:13]
    _T_4 <= mux(reset, UInt<5>("h0"), _GEN_1) @[Counter.scala 38:13 Counter.scala 40:29]
    _T_15 <= nextValid @[GLBCluster.scala 70:31]
