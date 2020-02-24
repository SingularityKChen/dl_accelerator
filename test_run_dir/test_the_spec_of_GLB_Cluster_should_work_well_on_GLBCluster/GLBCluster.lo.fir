circuit GLBCluster :
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
    output io_debugIO_waitForRead : UInt<1>
    output io_debugIO_doReadWire : UInt<1>
  
    mem oneInActSRAM : @[GLBCluster.scala 119:57]
      data-type => UInt<4>
      depth => 486
      read-latency => 1
      write-latency => 1
      reader => _T_36
      writer => _T_19
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 131:44]
    reg nextValidReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nextValidReg) @[GLBCluster.scala 134:45]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 135:42]
    node _T_30 = and(io_dataPath_outIOs_data_ready, nextValidReg) @[GLBCluster.scala 145:35]
    node doReadWire = _T_30 @[GLBCluster.scala 125:40 GLBCluster.scala 145:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 136:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 121:41 GLBCluster.scala 184:15]
    node _T_11 = eq(writeOrRead, UInt<1>("h0")) @[GLBCluster.scala 186:16]
    node _T_12 = eq(doDoneReg, UInt<1>("h0")) @[GLBCluster.scala 186:32]
    node _T_13 = or(_T_11, _T_12) @[GLBCluster.scala 186:29]
    node _T_14 = and(_T_13, io_ctrlPath_doEn) @[GLBCluster.scala 186:44]
    node doEnWire = _T_14 @[GLBCluster.scala 124:38 GLBCluster.scala 186:12]
    node _T_18 = and(doEnWire, io_dataPath_inIOs_data_valid) @[GLBCluster.scala 152:29]
    node doWriteWire = _T_18 @[GLBCluster.scala 126:41 GLBCluster.scala 152:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 136:22]
    reg _T_6 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_6) @[GLBCluster.scala 174:39]
    node writeInData = io_dataPath_inIOs_data_bits @[GLBCluster.scala 127:41 GLBCluster.scala 151:17]
    node readOutData = oneInActSRAM._T_36.data @[GLBCluster.scala 128:41 GLBCluster.scala 146:17]
    node _T_15 = mux(writeOrRead, writeInData, readOutData) @[GLBCluster.scala 187:21]
    node _T_16 = add(_T_6, UInt<1>("h1")) @[GLBCluster.scala 190:26]
    node _T_17 = tail(_T_16, 1) @[GLBCluster.scala 190:26]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 132:42 GLBCluster.scala 136:16]
    node _GEN_0 = mux(doIdxIncWire, _T_17, _T_6) @[GLBCluster.scala 189:23]
    reg _T_38 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_38) @[GLBCluster.scala 201:34]
    node _T_47 = eq(_T_38, UInt<2>("h2")) @[GLBCluster.scala 224:33]
    node _T_9 = _T_47 @[GLBCluster.scala 181:38 GLBCluster.scala 224:20]
    node _T_10 = _T_9 @[GLBCluster.scala 182:29 GLBCluster.scala 229:11]
    node _GEN_1 = mux(_T_10, UInt<1>("h0"), _GEN_0) @[GLBCluster.scala 192:18]
    node _GEN_2 = validif(doIdxIncWire, _T_6) @[GLBCluster.scala 154:24]
    node _GEN_3 = validif(doIdxIncWire, clock) @[GLBCluster.scala 154:24]
    node _GEN_4 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 154:24]
    node _GEN_5 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 154:24]
    node _GEN_6 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 154:24]
    node _T_20 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 139:56]
    node _T_21 = and(doEnWire, _T_20) @[GLBCluster.scala 139:53]
    node _T_22 = mux(writeOrRead, UInt<1>("h0"), _T_21) @[GLBCluster.scala 139:21]
    node nextValid = _T_22 @[GLBCluster.scala 133:39 GLBCluster.scala 139:15]
    node _T_23 = and(nextValid, io_dataPath_outIOs_data_ready) @[GLBCluster.scala 140:31]
    node _T_4 = _T_9 @[GLBCluster.scala 172:35 GLBCluster.scala 226:17]
    node _T_48 = eq(_T_38, UInt<2>("h1")) @[GLBCluster.scala 225:29]
    node _T_5 = _T_48 @[GLBCluster.scala 173:34 GLBCluster.scala 225:16]
    node _T_49 = mux(writeOrRead, _T_4, _T_5) @[GLBCluster.scala 227:20]
    node _T_8 = _T_49 @[GLBCluster.scala 179:32 GLBCluster.scala 227:14]
    node _T_24 = eq(_T_8, UInt<1>("h0")) @[GLBCluster.scala 143:24]
    node _T_25 = eq(doEnWire, UInt<1>("h0")) @[GLBCluster.scala 143:56]
    node _T_26 = or(writeOrRead, _T_25) @[GLBCluster.scala 143:53]
    node _T_27 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 143:80]
    node _T_28 = mux(_T_26, waitForRead, _T_27) @[GLBCluster.scala 143:40]
    node _T_29 = mux(_T_24, _T_28, UInt<1>("h0")) @[GLBCluster.scala 143:23]
    node _T_31 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 146:54]
    node _T_32 = and(doReadWire, _T_31) @[GLBCluster.scala 146:51]
    node _GEN_7 = validif(_T_32, _T_6) @[GLBCluster.scala 146:32]
    node _T_33 = _GEN_7 @[GLBCluster.scala 146:32 GLBCluster.scala 146:32]
    node _T_34 = or(_T_33, UInt<9>("h0")) @[GLBCluster.scala 146:32]
    node _T_35 = bits(_T_34, 8, 0) @[GLBCluster.scala 146:32]
    node _GEN_8 = mux(_T_32, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 146:32]
    node _GEN_9 = validif(_T_32, _T_35) @[GLBCluster.scala 146:32]
    node _GEN_10 = validif(_T_32, clock) @[GLBCluster.scala 146:32]
    node _T_37 = mux(waitForRead, readOutData, UInt<1>("h0")) @[GLBCluster.scala 147:26]
    node _T_7 = _T_15 @[GLBCluster.scala 178:33 GLBCluster.scala 187:15]
    node _T_40 = eq(_T_7, UInt<1>("h0")) @[GLBCluster.scala 203:31]
    node _T_39 = _T_40 @[GLBCluster.scala 202:34 GLBCluster.scala 203:16]
    node _T_41 = and(_T_39, waitForRead) @[GLBCluster.scala 204:65]
    node _T_42 = mux(writeOrRead, _T_39, _T_41) @[GLBCluster.scala 204:24]
    node _T_2 = _T_42 @[GLBCluster.scala 170:36 GLBCluster.scala 204:18]
    node _T_43 = and(_T_2, doEnWire) @[GLBCluster.scala 205:41]
    node _T_44 = eq(UInt<2>("h0"), _T_38) @[Conditional.scala 37:30]
    node _T_3 = _T_43 @[GLBCluster.scala 171:41 GLBCluster.scala 205:23]
    node _GEN_11 = mux(_T_3, UInt<2>("h1"), _T_38) @[GLBCluster.scala 209:34]
    node _T_45 = eq(UInt<2>("h1"), _T_38) @[Conditional.scala 37:30]
    node _GEN_12 = mux(_T_3, UInt<2>("h2"), UInt<2>("h0")) @[GLBCluster.scala 214:34]
    node _T_46 = eq(UInt<2>("h2"), _T_38) @[Conditional.scala 37:30]
    node _GEN_13 = mux(_T_46, UInt<2>("h0"), _T_38) @[Conditional.scala 39:67]
    node _GEN_14 = mux(_T_45, _GEN_12, _GEN_13) @[Conditional.scala 39:67]
    node _GEN_15 = mux(_T_44, UInt<1>("h0"), doDoneReg) @[Conditional.scala 40:58]
    node _GEN_16 = mux(_T_44, _GEN_11, _GEN_14) @[Conditional.scala 40:58]
    io_dataPath_inIOs_data_ready <= doWriteWire @[GLBCluster.scala 153:21]
    io_dataPath_outIOs_data_valid <= nextValidReg @[GLBCluster.scala 144:21]
    io_dataPath_outIOs_data_bits <= _T_37 @[GLBCluster.scala 147:20]
    io_ctrlPath_done <= _T_8 @[GLBCluster.scala 185:20]
    io_debugIO_idx <= _T_6 @[GLBCluster.scala 160:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 161:20]
    io_debugIO_idxCount is invalid
    io_debugIO_currentData <= _T_7 @[GLBCluster.scala 233:28]
    io_debugIO_meetOneZero <= _T_5 @[GLBCluster.scala 234:28]
    io_debugIO_theState <= _T_38 @[GLBCluster.scala 235:25]
    io_debugIO_waitForRead <= waitForRead @[GLBCluster.scala 162:25]
    io_debugIO_doReadWire <= doReadWire @[GLBCluster.scala 163:24]
    oneInActSRAM._T_36.addr <= _GEN_9 @[GLBCluster.scala 146:32]
    oneInActSRAM._T_36.en <= _GEN_8 @[GLBCluster.scala 119:57 GLBCluster.scala 146:32]
    oneInActSRAM._T_36.clk <= _GEN_10 @[GLBCluster.scala 146:32]
    oneInActSRAM._T_19.addr <= _GEN_2
    oneInActSRAM._T_19.en <= _GEN_4 @[GLBCluster.scala 119:57]
    oneInActSRAM._T_19.clk <= _GEN_3
    oneInActSRAM._T_19.data <= _GEN_6
    oneInActSRAM._T_19.mask <= _GEN_5
    waitForRead <= mux(reset, UInt<1>("h0"), _T_29) @[GLBCluster.scala 143:17]
    nextValidReg <= mux(reset, UInt<1>("h0"), _T_23) @[GLBCluster.scala 140:18]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_8) @[GLBCluster.scala 208:17 GLBCluster.scala 228:13]
    _T_6 <= mux(reset, UInt<9>("h0"), _GEN_1) @[GLBCluster.scala 190:14 GLBCluster.scala 193:14]
    _T_38 <= mux(reset, UInt<2>("h0"), _GEN_16) @[GLBCluster.scala 210:19 GLBCluster.scala 215:19 GLBCluster.scala 217:19 GLBCluster.scala 221:17]

  module InActSRAMCommon_1 :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_inIOs_data_ready : UInt<1>
    input io_dataPath_inIOs_data_valid : UInt<1>
    input io_dataPath_inIOs_data_bits : UInt<12>
    input io_dataPath_outIOs_data_ready : UInt<1>
    output io_dataPath_outIOs_data_valid : UInt<1>
    output io_dataPath_outIOs_data_bits : UInt<12>
    input io_ctrlPath_writeOrRead : UInt<1>
    input io_ctrlPath_doEn : UInt<1>
    output io_ctrlPath_done : UInt<1>
    output io_debugIO_idx : UInt<10>
    output io_debugIO_idxInc : UInt<1>
    output io_debugIO_idxCount : UInt<10>
    output io_debugIO_currentData : UInt<12>
    output io_debugIO_meetOneZero : UInt<1>
    output io_debugIO_theState : UInt<2>
    output io_debugIO_waitForRead : UInt<1>
    output io_debugIO_doReadWire : UInt<1>
  
    mem oneInActSRAM : @[GLBCluster.scala 119:57]
      data-type => UInt<12>
      depth => 862
      read-latency => 1
      write-latency => 1
      reader => _T_36
      writer => _T_19
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 131:44]
    reg nextValidReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nextValidReg) @[GLBCluster.scala 134:45]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 135:42]
    node _T_30 = and(io_dataPath_outIOs_data_ready, nextValidReg) @[GLBCluster.scala 145:35]
    node doReadWire = _T_30 @[GLBCluster.scala 125:40 GLBCluster.scala 145:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 136:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 121:41 GLBCluster.scala 184:15]
    node _T_11 = eq(writeOrRead, UInt<1>("h0")) @[GLBCluster.scala 186:16]
    node _T_12 = eq(doDoneReg, UInt<1>("h0")) @[GLBCluster.scala 186:32]
    node _T_13 = or(_T_11, _T_12) @[GLBCluster.scala 186:29]
    node _T_14 = and(_T_13, io_ctrlPath_doEn) @[GLBCluster.scala 186:44]
    node doEnWire = _T_14 @[GLBCluster.scala 124:38 GLBCluster.scala 186:12]
    node _T_18 = and(doEnWire, io_dataPath_inIOs_data_valid) @[GLBCluster.scala 152:29]
    node doWriteWire = _T_18 @[GLBCluster.scala 126:41 GLBCluster.scala 152:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 136:22]
    reg _T_6 : UInt<10>, clock with :
      reset => (UInt<1>("h0"), _T_6) @[GLBCluster.scala 174:39]
    node writeInData = io_dataPath_inIOs_data_bits @[GLBCluster.scala 127:41 GLBCluster.scala 151:17]
    node readOutData = oneInActSRAM._T_36.data @[GLBCluster.scala 128:41 GLBCluster.scala 146:17]
    node _T_15 = mux(writeOrRead, writeInData, readOutData) @[GLBCluster.scala 187:21]
    node _T_16 = add(_T_6, UInt<1>("h1")) @[GLBCluster.scala 190:26]
    node _T_17 = tail(_T_16, 1) @[GLBCluster.scala 190:26]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 132:42 GLBCluster.scala 136:16]
    node _GEN_0 = mux(doIdxIncWire, _T_17, _T_6) @[GLBCluster.scala 189:23]
    reg _T_38 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_38) @[GLBCluster.scala 201:34]
    node _T_47 = eq(_T_38, UInt<2>("h2")) @[GLBCluster.scala 224:33]
    node _T_9 = _T_47 @[GLBCluster.scala 181:38 GLBCluster.scala 224:20]
    node _T_10 = _T_9 @[GLBCluster.scala 182:29 GLBCluster.scala 229:11]
    node _GEN_1 = mux(_T_10, UInt<1>("h0"), _GEN_0) @[GLBCluster.scala 192:18]
    node _GEN_2 = validif(doIdxIncWire, _T_6) @[GLBCluster.scala 154:24]
    node _GEN_3 = validif(doIdxIncWire, clock) @[GLBCluster.scala 154:24]
    node _GEN_4 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 154:24]
    node _GEN_5 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 154:24]
    node _GEN_6 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 154:24]
    node _T_20 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 139:56]
    node _T_21 = and(doEnWire, _T_20) @[GLBCluster.scala 139:53]
    node _T_22 = mux(writeOrRead, UInt<1>("h0"), _T_21) @[GLBCluster.scala 139:21]
    node nextValid = _T_22 @[GLBCluster.scala 133:39 GLBCluster.scala 139:15]
    node _T_23 = and(nextValid, io_dataPath_outIOs_data_ready) @[GLBCluster.scala 140:31]
    node _T_4 = _T_9 @[GLBCluster.scala 172:35 GLBCluster.scala 226:17]
    node _T_48 = eq(_T_38, UInt<2>("h1")) @[GLBCluster.scala 225:29]
    node _T_5 = _T_48 @[GLBCluster.scala 173:34 GLBCluster.scala 225:16]
    node _T_49 = mux(writeOrRead, _T_4, _T_5) @[GLBCluster.scala 227:20]
    node _T_8 = _T_49 @[GLBCluster.scala 179:32 GLBCluster.scala 227:14]
    node _T_24 = eq(_T_8, UInt<1>("h0")) @[GLBCluster.scala 143:24]
    node _T_25 = eq(doEnWire, UInt<1>("h0")) @[GLBCluster.scala 143:56]
    node _T_26 = or(writeOrRead, _T_25) @[GLBCluster.scala 143:53]
    node _T_27 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 143:80]
    node _T_28 = mux(_T_26, waitForRead, _T_27) @[GLBCluster.scala 143:40]
    node _T_29 = mux(_T_24, _T_28, UInt<1>("h0")) @[GLBCluster.scala 143:23]
    node _T_31 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 146:54]
    node _T_32 = and(doReadWire, _T_31) @[GLBCluster.scala 146:51]
    node _GEN_7 = validif(_T_32, _T_6) @[GLBCluster.scala 146:32]
    node _T_33 = _GEN_7 @[GLBCluster.scala 146:32 GLBCluster.scala 146:32]
    node _T_34 = or(_T_33, UInt<10>("h0")) @[GLBCluster.scala 146:32]
    node _T_35 = bits(_T_34, 9, 0) @[GLBCluster.scala 146:32]
    node _GEN_8 = mux(_T_32, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 146:32]
    node _GEN_9 = validif(_T_32, _T_35) @[GLBCluster.scala 146:32]
    node _GEN_10 = validif(_T_32, clock) @[GLBCluster.scala 146:32]
    node _T_37 = mux(waitForRead, readOutData, UInt<1>("h0")) @[GLBCluster.scala 147:26]
    node _T_7 = _T_15 @[GLBCluster.scala 178:33 GLBCluster.scala 187:15]
    node _T_40 = eq(_T_7, UInt<1>("h0")) @[GLBCluster.scala 203:31]
    node _T_39 = _T_40 @[GLBCluster.scala 202:34 GLBCluster.scala 203:16]
    node _T_41 = and(_T_39, waitForRead) @[GLBCluster.scala 204:65]
    node _T_42 = mux(writeOrRead, _T_39, _T_41) @[GLBCluster.scala 204:24]
    node _T_2 = _T_42 @[GLBCluster.scala 170:36 GLBCluster.scala 204:18]
    node _T_43 = and(_T_2, doEnWire) @[GLBCluster.scala 205:41]
    node _T_44 = eq(UInt<2>("h0"), _T_38) @[Conditional.scala 37:30]
    node _T_3 = _T_43 @[GLBCluster.scala 171:41 GLBCluster.scala 205:23]
    node _GEN_11 = mux(_T_3, UInt<2>("h1"), _T_38) @[GLBCluster.scala 209:34]
    node _T_45 = eq(UInt<2>("h1"), _T_38) @[Conditional.scala 37:30]
    node _GEN_12 = mux(_T_3, UInt<2>("h2"), UInt<2>("h0")) @[GLBCluster.scala 214:34]
    node _T_46 = eq(UInt<2>("h2"), _T_38) @[Conditional.scala 37:30]
    node _GEN_13 = mux(_T_46, UInt<2>("h0"), _T_38) @[Conditional.scala 39:67]
    node _GEN_14 = mux(_T_45, _GEN_12, _GEN_13) @[Conditional.scala 39:67]
    node _GEN_15 = mux(_T_44, UInt<1>("h0"), doDoneReg) @[Conditional.scala 40:58]
    node _GEN_16 = mux(_T_44, _GEN_11, _GEN_14) @[Conditional.scala 40:58]
    io_dataPath_inIOs_data_ready <= doWriteWire @[GLBCluster.scala 153:21]
    io_dataPath_outIOs_data_valid <= nextValidReg @[GLBCluster.scala 144:21]
    io_dataPath_outIOs_data_bits <= _T_37 @[GLBCluster.scala 147:20]
    io_ctrlPath_done <= _T_8 @[GLBCluster.scala 185:20]
    io_debugIO_idx <= _T_6 @[GLBCluster.scala 160:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 161:20]
    io_debugIO_idxCount is invalid
    io_debugIO_currentData <= _T_7 @[GLBCluster.scala 233:28]
    io_debugIO_meetOneZero <= _T_5 @[GLBCluster.scala 234:28]
    io_debugIO_theState <= _T_38 @[GLBCluster.scala 235:25]
    io_debugIO_waitForRead <= waitForRead @[GLBCluster.scala 162:25]
    io_debugIO_doReadWire <= doReadWire @[GLBCluster.scala 163:24]
    oneInActSRAM._T_36.addr <= _GEN_9 @[GLBCluster.scala 146:32]
    oneInActSRAM._T_36.en <= _GEN_8 @[GLBCluster.scala 119:57 GLBCluster.scala 146:32]
    oneInActSRAM._T_36.clk <= _GEN_10 @[GLBCluster.scala 146:32]
    oneInActSRAM._T_19.addr <= _GEN_2
    oneInActSRAM._T_19.en <= _GEN_4 @[GLBCluster.scala 119:57]
    oneInActSRAM._T_19.clk <= _GEN_3
    oneInActSRAM._T_19.data <= _GEN_6
    oneInActSRAM._T_19.mask <= _GEN_5
    waitForRead <= mux(reset, UInt<1>("h0"), _T_29) @[GLBCluster.scala 143:17]
    nextValidReg <= mux(reset, UInt<1>("h0"), _T_23) @[GLBCluster.scala 140:18]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_8) @[GLBCluster.scala 208:17 GLBCluster.scala 228:13]
    _T_6 <= mux(reset, UInt<10>("h0"), _GEN_1) @[GLBCluster.scala 190:14 GLBCluster.scala 193:14]
    _T_38 <= mux(reset, UInt<2>("h0"), _GEN_16) @[GLBCluster.scala 210:19 GLBCluster.scala 215:19 GLBCluster.scala 217:19 GLBCluster.scala 221:17]

  module InActSRAMBank :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_inIOs_adrIOs_data_ready : UInt<1>
    input io_dataPath_inIOs_adrIOs_data_valid : UInt<1>
    input io_dataPath_inIOs_adrIOs_data_bits : UInt<4>
    output io_dataPath_inIOs_dataIOs_data_ready : UInt<1>
    input io_dataPath_inIOs_dataIOs_data_valid : UInt<1>
    input io_dataPath_inIOs_dataIOs_data_bits : UInt<12>
    input io_dataPath_outIOs_adrIOs_data_ready : UInt<1>
    output io_dataPath_outIOs_adrIOs_data_valid : UInt<1>
    output io_dataPath_outIOs_adrIOs_data_bits : UInt<4>
    input io_dataPath_outIOs_dataIOs_data_ready : UInt<1>
    output io_dataPath_outIOs_dataIOs_data_valid : UInt<1>
    output io_dataPath_outIOs_dataIOs_data_bits : UInt<12>
    input io_ctrlPath_writeOrRead : UInt<1>
    input io_ctrlPath_doEn : UInt<1>
    output io_ctrlPath_done : UInt<1>
    output io_debugIO_theState : UInt<2>
    output io_debugIO_adrDebug_commonDebug_idx : UInt<9>
    output io_debugIO_adrDebug_commonDebug_idxInc : UInt<1>
    output io_debugIO_adrDebug_commonDebug_idxCount : UInt<9>
    output io_debugIO_adrDebug_commonDebug_currentData : UInt<4>
    output io_debugIO_adrDebug_commonDebug_meetOneZero : UInt<1>
    output io_debugIO_adrDebug_commonDebug_theState : UInt<2>
    output io_debugIO_adrDebug_commonDebug_waitForRead : UInt<1>
    output io_debugIO_adrDebug_commonDebug_doReadWire : UInt<1>
    output io_debugIO_adrDebug_subDone : UInt<1>
    output io_debugIO_dataDebug_commonDebug_idx : UInt<10>
    output io_debugIO_dataDebug_commonDebug_idxInc : UInt<1>
    output io_debugIO_dataDebug_commonDebug_idxCount : UInt<10>
    output io_debugIO_dataDebug_commonDebug_currentData : UInt<12>
    output io_debugIO_dataDebug_commonDebug_meetOneZero : UInt<1>
    output io_debugIO_dataDebug_commonDebug_theState : UInt<2>
    output io_debugIO_dataDebug_commonDebug_waitForRead : UInt<1>
    output io_debugIO_dataDebug_commonDebug_doReadWire : UInt<1>
    output io_debugIO_dataDebug_subDone : UInt<1>
  
    inst adrSRAM of InActSRAMCommon @[GLBCluster.scala 244:31]
    inst dataSRAM of InActSRAMCommon_1 @[GLBCluster.scala 246:32]
    reg _T_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_1) @[GLBCluster.scala 250:35]
    node _T_5 = eq(_T_1, UInt<2>("h1")) @[GLBCluster.scala 254:34]
    node _T_6 = eq(_T_1, UInt<2>("h2")) @[GLBCluster.scala 257:63]
    node _T_2 = _T_5 @[GLBCluster.scala 251:38 GLBCluster.scala 254:20]
    node _T_7 = or(_T_2, _T_6) @[GLBCluster.scala 257:48]
    node _T_8 = eq(_T_1, UInt<2>("h3")) @[GLBCluster.scala 258:64]
    node _T_9 = or(_T_2, _T_8) @[GLBCluster.scala 258:49]
    node _T_10 = eq(_T_1, UInt<2>("h3")) @[GLBCluster.scala 259:32]
    node _T_4 = dataSRAM.io_ctrlPath_done @[GLBCluster.scala 253:34 GLBCluster.scala 256:16]
    node _T_11 = and(_T_10, _T_4) @[GLBCluster.scala 259:51]
    node _T_12 = eq(_T_1, UInt<2>("h2")) @[GLBCluster.scala 259:84]
    node _T_3 = adrSRAM.io_ctrlPath_done @[GLBCluster.scala 252:33 GLBCluster.scala 255:15]
    node _T_13 = and(_T_12, _T_3) @[GLBCluster.scala 259:102]
    node _T_14 = or(_T_11, _T_13) @[GLBCluster.scala 259:68]
    node _T_15 = eq(UInt<2>("h0"), _T_1) @[Conditional.scala 37:30]
    node _GEN_0 = mux(io_ctrlPath_doEn, UInt<2>("h1"), _T_1) @[GLBCluster.scala 262:31]
    node _T_16 = eq(UInt<2>("h1"), _T_1) @[Conditional.scala 37:30]
    node _GEN_1 = mux(_T_4, UInt<2>("h2"), UInt<2>("h1")) @[GLBCluster.scala 269:34]
    node _GEN_2 = mux(_T_3, UInt<2>("h3"), _GEN_1) @[GLBCluster.scala 267:26]
    node _T_17 = eq(UInt<2>("h3"), _T_1) @[Conditional.scala 37:30]
    node _GEN_3 = mux(_T_4, UInt<2>("h0"), _T_1) @[GLBCluster.scala 276:27]
    node _T_18 = eq(UInt<2>("h2"), _T_1) @[Conditional.scala 37:30]
    node _GEN_4 = mux(_T_3, UInt<2>("h0"), _T_1) @[GLBCluster.scala 281:26]
    node _GEN_5 = mux(_T_18, _GEN_4, _T_1) @[Conditional.scala 39:67]
    node _GEN_6 = mux(_T_17, _GEN_3, _GEN_5) @[Conditional.scala 39:67]
    node _GEN_7 = mux(_T_16, _GEN_2, _GEN_6) @[Conditional.scala 39:67]
    node _GEN_8 = mux(_T_15, _GEN_0, _GEN_7) @[Conditional.scala 40:58]
    node _T = _T_14 @[GLBCluster.scala 248:34 GLBCluster.scala 259:16]
    io_dataPath_inIOs_adrIOs_data_ready <= adrSRAM.io_dataPath_inIOs_data_ready @[GLBCluster.scala 287:29]
    io_dataPath_inIOs_dataIOs_data_ready <= dataSRAM.io_dataPath_inIOs_data_ready @[GLBCluster.scala 289:30]
    io_dataPath_outIOs_adrIOs_data_valid <= adrSRAM.io_dataPath_outIOs_data_valid @[GLBCluster.scala 288:30]
    io_dataPath_outIOs_adrIOs_data_bits <= adrSRAM.io_dataPath_outIOs_data_bits @[GLBCluster.scala 288:30]
    io_dataPath_outIOs_dataIOs_data_valid <= dataSRAM.io_dataPath_outIOs_data_valid @[GLBCluster.scala 290:31]
    io_dataPath_outIOs_dataIOs_data_bits <= dataSRAM.io_dataPath_outIOs_data_bits @[GLBCluster.scala 290:31]
    io_ctrlPath_done <= _T @[GLBCluster.scala 292:20]
    io_debugIO_theState <= _T_1 @[GLBCluster.scala 297:25]
    io_debugIO_adrDebug_commonDebug_idx <= adrSRAM.io_debugIO_idx @[GLBCluster.scala 300:25]
    io_debugIO_adrDebug_commonDebug_idxInc <= adrSRAM.io_debugIO_idxInc @[GLBCluster.scala 300:25]
    io_debugIO_adrDebug_commonDebug_idxCount <= adrSRAM.io_debugIO_idxCount @[GLBCluster.scala 300:25]
    io_debugIO_adrDebug_commonDebug_currentData <= adrSRAM.io_debugIO_currentData @[GLBCluster.scala 300:25]
    io_debugIO_adrDebug_commonDebug_meetOneZero <= adrSRAM.io_debugIO_meetOneZero @[GLBCluster.scala 300:25]
    io_debugIO_adrDebug_commonDebug_theState <= adrSRAM.io_debugIO_theState @[GLBCluster.scala 300:25]
    io_debugIO_adrDebug_commonDebug_waitForRead <= adrSRAM.io_debugIO_waitForRead @[GLBCluster.scala 300:25]
    io_debugIO_adrDebug_commonDebug_doReadWire <= adrSRAM.io_debugIO_doReadWire @[GLBCluster.scala 300:25]
    io_debugIO_adrDebug_subDone <= adrSRAM.io_ctrlPath_done @[GLBCluster.scala 299:21]
    io_debugIO_dataDebug_commonDebug_idx <= dataSRAM.io_debugIO_idx @[GLBCluster.scala 300:25]
    io_debugIO_dataDebug_commonDebug_idxInc <= dataSRAM.io_debugIO_idxInc @[GLBCluster.scala 300:25]
    io_debugIO_dataDebug_commonDebug_idxCount <= dataSRAM.io_debugIO_idxCount @[GLBCluster.scala 300:25]
    io_debugIO_dataDebug_commonDebug_currentData <= dataSRAM.io_debugIO_currentData @[GLBCluster.scala 300:25]
    io_debugIO_dataDebug_commonDebug_meetOneZero <= dataSRAM.io_debugIO_meetOneZero @[GLBCluster.scala 300:25]
    io_debugIO_dataDebug_commonDebug_theState <= dataSRAM.io_debugIO_theState @[GLBCluster.scala 300:25]
    io_debugIO_dataDebug_commonDebug_waitForRead <= dataSRAM.io_debugIO_waitForRead @[GLBCluster.scala 300:25]
    io_debugIO_dataDebug_commonDebug_doReadWire <= dataSRAM.io_debugIO_doReadWire @[GLBCluster.scala 300:25]
    io_debugIO_dataDebug_subDone <= dataSRAM.io_ctrlPath_done @[GLBCluster.scala 299:21]
    adrSRAM.clock <= clock
    adrSRAM.reset <= reset
    adrSRAM.io_dataPath_inIOs_data_valid <= io_dataPath_inIOs_adrIOs_data_valid @[GLBCluster.scala 287:29]
    adrSRAM.io_dataPath_inIOs_data_bits <= io_dataPath_inIOs_adrIOs_data_bits @[GLBCluster.scala 287:29]
    adrSRAM.io_dataPath_outIOs_data_ready <= io_dataPath_outIOs_adrIOs_data_ready @[GLBCluster.scala 288:30]
    adrSRAM.io_ctrlPath_writeOrRead <= io_ctrlPath_writeOrRead @[GLBCluster.scala 294:19]
    adrSRAM.io_ctrlPath_doEn <= _T_7 @[GLBCluster.scala 257:28]
    dataSRAM.clock <= clock
    dataSRAM.reset <= reset
    dataSRAM.io_dataPath_inIOs_data_valid <= io_dataPath_inIOs_dataIOs_data_valid @[GLBCluster.scala 289:30]
    dataSRAM.io_dataPath_inIOs_data_bits <= io_dataPath_inIOs_dataIOs_data_bits @[GLBCluster.scala 289:30]
    dataSRAM.io_dataPath_outIOs_data_ready <= io_dataPath_outIOs_dataIOs_data_ready @[GLBCluster.scala 290:31]
    dataSRAM.io_ctrlPath_writeOrRead <= io_ctrlPath_writeOrRead @[GLBCluster.scala 294:19]
    dataSRAM.io_ctrlPath_doEn <= _T_9 @[GLBCluster.scala 258:29]
    _T_1 <= mux(reset, UInt<2>("h0"), _GEN_8) @[GLBCluster.scala 263:20 GLBCluster.scala 268:20 GLBCluster.scala 270:20 GLBCluster.scala 272:20 GLBCluster.scala 277:20 GLBCluster.scala 282:20]

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
  
    mem onePSumSRAMBank : @[GLBCluster.scala 119:57]
      data-type => UInt<20>
      depth => 768
      read-latency => 1
      write-latency => 1
      reader => _T_29
      writer => _T_12
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 131:44]
    reg nextValidReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nextValidReg) @[GLBCluster.scala 134:45]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 135:42]
    node _T_23 = and(io_dataPath_outIOs_ready, nextValidReg) @[GLBCluster.scala 145:35]
    node doReadWire = _T_23 @[GLBCluster.scala 125:40 GLBCluster.scala 145:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 136:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 121:41 GLBCluster.scala 96:15]
    node doEnWire = io_ctrlPath_doEn @[GLBCluster.scala 124:38 GLBCluster.scala 100:12]
    node _T_11 = and(doEnWire, io_dataPath_inIOs_valid) @[GLBCluster.scala 152:29]
    node doWriteWire = _T_11 @[GLBCluster.scala 126:41 GLBCluster.scala 152:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 136:22]
    reg _T_4 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), _T_4) @[Counter.scala 29:33]
    node _T_5 = eq(_T_4, UInt<5>("h17")) @[Counter.scala 37:24]
    node _T_6 = add(_T_4, UInt<1>("h1")) @[Counter.scala 38:22]
    node _T_7 = tail(_T_6, 1) @[Counter.scala 38:22]
    node _GEN_0 = mux(_T_5, UInt<1>("h0"), _T_7) @[Counter.scala 40:21]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 132:42 GLBCluster.scala 136:16]
    node _GEN_1 = mux(doIdxIncWire, _GEN_0, _T_4) @[Counter.scala 71:17]
    node _T_8 = and(doIdxIncWire, _T_5) @[Counter.scala 72:20]
    node _T_2 = io_ctrlPath_startIdx @[GLBCluster.scala 89:30 GLBCluster.scala 95:12]
    node _T_9 = add(_T_2, _T_4) @[GLBCluster.scala 101:25]
    node _T_10 = tail(_T_9, 1) @[GLBCluster.scala 101:25]
    node _T_3 = _T_10 @[GLBCluster.scala 90:37 GLBCluster.scala 101:13]
    node _GEN_2 = validif(doIdxIncWire, _T_3) @[GLBCluster.scala 154:24]
    node _GEN_3 = validif(doIdxIncWire, clock) @[GLBCluster.scala 154:24]
    node _GEN_4 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 154:24]
    node _GEN_5 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 154:24]
    node writeInData = io_dataPath_inIOs_bits @[GLBCluster.scala 127:41 GLBCluster.scala 151:17]
    node _GEN_6 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 154:24]
    node _T_13 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 139:56]
    node _T_14 = and(doEnWire, _T_13) @[GLBCluster.scala 139:53]
    node _T_15 = mux(writeOrRead, UInt<1>("h0"), _T_14) @[GLBCluster.scala 139:21]
    node nextValid = _T_15 @[GLBCluster.scala 133:39 GLBCluster.scala 139:15]
    node _T_16 = and(nextValid, io_dataPath_outIOs_ready) @[GLBCluster.scala 140:31]
    node _T_17 = eq(_T_8, UInt<1>("h0")) @[GLBCluster.scala 143:24]
    node _T_18 = eq(doEnWire, UInt<1>("h0")) @[GLBCluster.scala 143:56]
    node _T_19 = or(writeOrRead, _T_18) @[GLBCluster.scala 143:53]
    node _T_20 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 143:80]
    node _T_21 = mux(_T_19, waitForRead, _T_20) @[GLBCluster.scala 143:40]
    node _T_22 = mux(_T_17, _T_21, UInt<1>("h0")) @[GLBCluster.scala 143:23]
    node _T_24 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 146:54]
    node _T_25 = and(doReadWire, _T_24) @[GLBCluster.scala 146:51]
    node _GEN_7 = validif(_T_25, _T_3) @[GLBCluster.scala 146:32]
    node _T_26 = _GEN_7 @[GLBCluster.scala 146:32 GLBCluster.scala 146:32]
    node _T_27 = or(_T_26, UInt<10>("h0")) @[GLBCluster.scala 146:32]
    node _T_28 = bits(_T_27, 9, 0) @[GLBCluster.scala 146:32]
    node _GEN_8 = mux(_T_25, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 146:32]
    node _GEN_9 = validif(_T_25, _T_28) @[GLBCluster.scala 146:32]
    node _GEN_10 = validif(_T_25, clock) @[GLBCluster.scala 146:32]
    node readOutData = onePSumSRAMBank._T_29.data @[GLBCluster.scala 128:41 GLBCluster.scala 146:17]
    node _T_30 = mux(waitForRead, readOutData, UInt<1>("h0")) @[GLBCluster.scala 147:26]
    io_dataPath_inIOs_ready <= doWriteWire @[GLBCluster.scala 153:21]
    io_dataPath_outIOs_valid <= nextValidReg @[GLBCluster.scala 144:21]
    io_dataPath_outIOs_bits <= _T_30 @[GLBCluster.scala 147:20]
    io_ctrlPath_done <= doDoneReg @[GLBCluster.scala 97:20]
    io_debugIO_idx <= _T_3 @[GLBCluster.scala 160:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 161:20]
    io_debugIO_idxCount <= _T_4 @[GLBCluster.scala 109:25]
    io_debugIO_currentData is invalid
    io_debugIO_meetOneZero is invalid
    io_debugIO_theState is invalid
    io_debugIO_waitForRead <= waitForRead @[GLBCluster.scala 162:25]
    io_debugIO_doReadWire <= doReadWire @[GLBCluster.scala 163:24]
    onePSumSRAMBank._T_29.addr <= _GEN_9 @[GLBCluster.scala 146:32]
    onePSumSRAMBank._T_29.en <= _GEN_8 @[GLBCluster.scala 119:57 GLBCluster.scala 146:32]
    onePSumSRAMBank._T_29.clk <= _GEN_10 @[GLBCluster.scala 146:32]
    onePSumSRAMBank._T_12.addr <= _GEN_2
    onePSumSRAMBank._T_12.en <= _GEN_4 @[GLBCluster.scala 119:57]
    onePSumSRAMBank._T_12.clk <= _GEN_3
    onePSumSRAMBank._T_12.data <= _GEN_6
    onePSumSRAMBank._T_12.mask <= _GEN_5
    waitForRead <= mux(reset, UInt<1>("h0"), _T_22) @[GLBCluster.scala 143:17]
    nextValidReg <= mux(reset, UInt<1>("h0"), _T_16) @[GLBCluster.scala 140:18]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_8) @[GLBCluster.scala 98:13]
    _T_4 <= mux(reset, UInt<5>("h0"), _GEN_1) @[Counter.scala 38:13 Counter.scala 40:29]

  module GLBCluster :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_inActIO_0_inIOs_adrIOs_data_ready : UInt<1>
    input io_dataPath_inActIO_0_inIOs_adrIOs_data_valid : UInt<1>
    input io_dataPath_inActIO_0_inIOs_adrIOs_data_bits : UInt<4>
    output io_dataPath_inActIO_0_inIOs_dataIOs_data_ready : UInt<1>
    input io_dataPath_inActIO_0_inIOs_dataIOs_data_valid : UInt<1>
    input io_dataPath_inActIO_0_inIOs_dataIOs_data_bits : UInt<12>
    input io_dataPath_inActIO_0_outIOs_adrIOs_data_ready : UInt<1>
    output io_dataPath_inActIO_0_outIOs_adrIOs_data_valid : UInt<1>
    output io_dataPath_inActIO_0_outIOs_adrIOs_data_bits : UInt<4>
    input io_dataPath_inActIO_0_outIOs_dataIOs_data_ready : UInt<1>
    output io_dataPath_inActIO_0_outIOs_dataIOs_data_valid : UInt<1>
    output io_dataPath_inActIO_0_outIOs_dataIOs_data_bits : UInt<12>
    output io_dataPath_inActIO_1_inIOs_adrIOs_data_ready : UInt<1>
    input io_dataPath_inActIO_1_inIOs_adrIOs_data_valid : UInt<1>
    input io_dataPath_inActIO_1_inIOs_adrIOs_data_bits : UInt<4>
    output io_dataPath_inActIO_1_inIOs_dataIOs_data_ready : UInt<1>
    input io_dataPath_inActIO_1_inIOs_dataIOs_data_valid : UInt<1>
    input io_dataPath_inActIO_1_inIOs_dataIOs_data_bits : UInt<12>
    input io_dataPath_inActIO_1_outIOs_adrIOs_data_ready : UInt<1>
    output io_dataPath_inActIO_1_outIOs_adrIOs_data_valid : UInt<1>
    output io_dataPath_inActIO_1_outIOs_adrIOs_data_bits : UInt<4>
    input io_dataPath_inActIO_1_outIOs_dataIOs_data_ready : UInt<1>
    output io_dataPath_inActIO_1_outIOs_dataIOs_data_valid : UInt<1>
    output io_dataPath_inActIO_1_outIOs_dataIOs_data_bits : UInt<12>
    output io_dataPath_inActIO_2_inIOs_adrIOs_data_ready : UInt<1>
    input io_dataPath_inActIO_2_inIOs_adrIOs_data_valid : UInt<1>
    input io_dataPath_inActIO_2_inIOs_adrIOs_data_bits : UInt<4>
    output io_dataPath_inActIO_2_inIOs_dataIOs_data_ready : UInt<1>
    input io_dataPath_inActIO_2_inIOs_dataIOs_data_valid : UInt<1>
    input io_dataPath_inActIO_2_inIOs_dataIOs_data_bits : UInt<12>
    input io_dataPath_inActIO_2_outIOs_adrIOs_data_ready : UInt<1>
    output io_dataPath_inActIO_2_outIOs_adrIOs_data_valid : UInt<1>
    output io_dataPath_inActIO_2_outIOs_adrIOs_data_bits : UInt<4>
    input io_dataPath_inActIO_2_outIOs_dataIOs_data_ready : UInt<1>
    output io_dataPath_inActIO_2_outIOs_dataIOs_data_valid : UInt<1>
    output io_dataPath_inActIO_2_outIOs_dataIOs_data_bits : UInt<12>
    output io_dataPath_weightIO_0_inIOs_adrIOs_data_ready : UInt<1>
    input io_dataPath_weightIO_0_inIOs_adrIOs_data_valid : UInt<1>
    input io_dataPath_weightIO_0_inIOs_adrIOs_data_bits : UInt<7>
    output io_dataPath_weightIO_0_inIOs_dataIOs_data_ready : UInt<1>
    input io_dataPath_weightIO_0_inIOs_dataIOs_data_valid : UInt<1>
    input io_dataPath_weightIO_0_inIOs_dataIOs_data_bits : UInt<12>
    input io_dataPath_weightIO_0_outIOs_adrIOs_data_ready : UInt<1>
    output io_dataPath_weightIO_0_outIOs_adrIOs_data_valid : UInt<1>
    output io_dataPath_weightIO_0_outIOs_adrIOs_data_bits : UInt<7>
    input io_dataPath_weightIO_0_outIOs_dataIOs_data_ready : UInt<1>
    output io_dataPath_weightIO_0_outIOs_dataIOs_data_valid : UInt<1>
    output io_dataPath_weightIO_0_outIOs_dataIOs_data_bits : UInt<12>
    output io_dataPath_weightIO_1_inIOs_adrIOs_data_ready : UInt<1>
    input io_dataPath_weightIO_1_inIOs_adrIOs_data_valid : UInt<1>
    input io_dataPath_weightIO_1_inIOs_adrIOs_data_bits : UInt<7>
    output io_dataPath_weightIO_1_inIOs_dataIOs_data_ready : UInt<1>
    input io_dataPath_weightIO_1_inIOs_dataIOs_data_valid : UInt<1>
    input io_dataPath_weightIO_1_inIOs_dataIOs_data_bits : UInt<12>
    input io_dataPath_weightIO_1_outIOs_adrIOs_data_ready : UInt<1>
    output io_dataPath_weightIO_1_outIOs_adrIOs_data_valid : UInt<1>
    output io_dataPath_weightIO_1_outIOs_adrIOs_data_bits : UInt<7>
    input io_dataPath_weightIO_1_outIOs_dataIOs_data_ready : UInt<1>
    output io_dataPath_weightIO_1_outIOs_dataIOs_data_valid : UInt<1>
    output io_dataPath_weightIO_1_outIOs_dataIOs_data_bits : UInt<12>
    output io_dataPath_weightIO_2_inIOs_adrIOs_data_ready : UInt<1>
    input io_dataPath_weightIO_2_inIOs_adrIOs_data_valid : UInt<1>
    input io_dataPath_weightIO_2_inIOs_adrIOs_data_bits : UInt<7>
    output io_dataPath_weightIO_2_inIOs_dataIOs_data_ready : UInt<1>
    input io_dataPath_weightIO_2_inIOs_dataIOs_data_valid : UInt<1>
    input io_dataPath_weightIO_2_inIOs_dataIOs_data_bits : UInt<12>
    input io_dataPath_weightIO_2_outIOs_adrIOs_data_ready : UInt<1>
    output io_dataPath_weightIO_2_outIOs_adrIOs_data_valid : UInt<1>
    output io_dataPath_weightIO_2_outIOs_adrIOs_data_bits : UInt<7>
    input io_dataPath_weightIO_2_outIOs_dataIOs_data_ready : UInt<1>
    output io_dataPath_weightIO_2_outIOs_dataIOs_data_valid : UInt<1>
    output io_dataPath_weightIO_2_outIOs_dataIOs_data_bits : UInt<12>
    output io_dataPath_pSumIO_0_inIOs_ready : UInt<1>
    input io_dataPath_pSumIO_0_inIOs_valid : UInt<1>
    input io_dataPath_pSumIO_0_inIOs_bits : UInt<20>
    input io_dataPath_pSumIO_0_outIOs_ready : UInt<1>
    output io_dataPath_pSumIO_0_outIOs_valid : UInt<1>
    output io_dataPath_pSumIO_0_outIOs_bits : UInt<20>
    output io_dataPath_pSumIO_1_inIOs_ready : UInt<1>
    input io_dataPath_pSumIO_1_inIOs_valid : UInt<1>
    input io_dataPath_pSumIO_1_inIOs_bits : UInt<20>
    input io_dataPath_pSumIO_1_outIOs_ready : UInt<1>
    output io_dataPath_pSumIO_1_outIOs_valid : UInt<1>
    output io_dataPath_pSumIO_1_outIOs_bits : UInt<20>
    output io_dataPath_pSumIO_2_inIOs_ready : UInt<1>
    input io_dataPath_pSumIO_2_inIOs_valid : UInt<1>
    input io_dataPath_pSumIO_2_inIOs_bits : UInt<20>
    input io_dataPath_pSumIO_2_outIOs_ready : UInt<1>
    output io_dataPath_pSumIO_2_outIOs_valid : UInt<1>
    output io_dataPath_pSumIO_2_outIOs_bits : UInt<20>
    output io_dataPath_pSumIO_3_inIOs_ready : UInt<1>
    input io_dataPath_pSumIO_3_inIOs_valid : UInt<1>
    input io_dataPath_pSumIO_3_inIOs_bits : UInt<20>
    input io_dataPath_pSumIO_3_outIOs_ready : UInt<1>
    output io_dataPath_pSumIO_3_outIOs_valid : UInt<1>
    output io_dataPath_pSumIO_3_outIOs_bits : UInt<20>
    input io_ctrlPath_inActIO_writeOrRead : UInt<1>
    input io_ctrlPath_inActIO_doEn : UInt<1>
    output io_ctrlPath_inActIO_done : UInt<1>
    output io_ctrlPath_inActIO_busy : UInt<1>
    input io_ctrlPath_pSumIO_writeOrRead : UInt<1>
    input io_ctrlPath_pSumIO_doEn : UInt<1>
    output io_ctrlPath_pSumIO_done : UInt<1>
    output io_ctrlPath_pSumIO_busy : UInt<1>
    output io_debugIO_theState_0 : UInt<1>
    output io_debugIO_theState_1 : UInt<1>
    output io_debugIO_inActDebugIO_0_theState : UInt<2>
    output io_debugIO_inActDebugIO_0_adrDebug_commonDebug_idx : UInt<9>
    output io_debugIO_inActDebugIO_0_adrDebug_commonDebug_idxInc : UInt<1>
    output io_debugIO_inActDebugIO_0_adrDebug_commonDebug_idxCount : UInt<9>
    output io_debugIO_inActDebugIO_0_adrDebug_commonDebug_currentData : UInt<4>
    output io_debugIO_inActDebugIO_0_adrDebug_commonDebug_meetOneZero : UInt<1>
    output io_debugIO_inActDebugIO_0_adrDebug_commonDebug_theState : UInt<2>
    output io_debugIO_inActDebugIO_0_adrDebug_commonDebug_waitForRead : UInt<1>
    output io_debugIO_inActDebugIO_0_adrDebug_commonDebug_doReadWire : UInt<1>
    output io_debugIO_inActDebugIO_0_adrDebug_subDone : UInt<1>
    output io_debugIO_inActDebugIO_0_dataDebug_commonDebug_idx : UInt<10>
    output io_debugIO_inActDebugIO_0_dataDebug_commonDebug_idxInc : UInt<1>
    output io_debugIO_inActDebugIO_0_dataDebug_commonDebug_idxCount : UInt<10>
    output io_debugIO_inActDebugIO_0_dataDebug_commonDebug_currentData : UInt<12>
    output io_debugIO_inActDebugIO_0_dataDebug_commonDebug_meetOneZero : UInt<1>
    output io_debugIO_inActDebugIO_0_dataDebug_commonDebug_theState : UInt<2>
    output io_debugIO_inActDebugIO_0_dataDebug_commonDebug_waitForRead : UInt<1>
    output io_debugIO_inActDebugIO_0_dataDebug_commonDebug_doReadWire : UInt<1>
    output io_debugIO_inActDebugIO_0_dataDebug_subDone : UInt<1>
    output io_debugIO_inActDebugIO_1_theState : UInt<2>
    output io_debugIO_inActDebugIO_1_adrDebug_commonDebug_idx : UInt<9>
    output io_debugIO_inActDebugIO_1_adrDebug_commonDebug_idxInc : UInt<1>
    output io_debugIO_inActDebugIO_1_adrDebug_commonDebug_idxCount : UInt<9>
    output io_debugIO_inActDebugIO_1_adrDebug_commonDebug_currentData : UInt<4>
    output io_debugIO_inActDebugIO_1_adrDebug_commonDebug_meetOneZero : UInt<1>
    output io_debugIO_inActDebugIO_1_adrDebug_commonDebug_theState : UInt<2>
    output io_debugIO_inActDebugIO_1_adrDebug_commonDebug_waitForRead : UInt<1>
    output io_debugIO_inActDebugIO_1_adrDebug_commonDebug_doReadWire : UInt<1>
    output io_debugIO_inActDebugIO_1_adrDebug_subDone : UInt<1>
    output io_debugIO_inActDebugIO_1_dataDebug_commonDebug_idx : UInt<10>
    output io_debugIO_inActDebugIO_1_dataDebug_commonDebug_idxInc : UInt<1>
    output io_debugIO_inActDebugIO_1_dataDebug_commonDebug_idxCount : UInt<10>
    output io_debugIO_inActDebugIO_1_dataDebug_commonDebug_currentData : UInt<12>
    output io_debugIO_inActDebugIO_1_dataDebug_commonDebug_meetOneZero : UInt<1>
    output io_debugIO_inActDebugIO_1_dataDebug_commonDebug_theState : UInt<2>
    output io_debugIO_inActDebugIO_1_dataDebug_commonDebug_waitForRead : UInt<1>
    output io_debugIO_inActDebugIO_1_dataDebug_commonDebug_doReadWire : UInt<1>
    output io_debugIO_inActDebugIO_1_dataDebug_subDone : UInt<1>
    output io_debugIO_inActDebugIO_2_theState : UInt<2>
    output io_debugIO_inActDebugIO_2_adrDebug_commonDebug_idx : UInt<9>
    output io_debugIO_inActDebugIO_2_adrDebug_commonDebug_idxInc : UInt<1>
    output io_debugIO_inActDebugIO_2_adrDebug_commonDebug_idxCount : UInt<9>
    output io_debugIO_inActDebugIO_2_adrDebug_commonDebug_currentData : UInt<4>
    output io_debugIO_inActDebugIO_2_adrDebug_commonDebug_meetOneZero : UInt<1>
    output io_debugIO_inActDebugIO_2_adrDebug_commonDebug_theState : UInt<2>
    output io_debugIO_inActDebugIO_2_adrDebug_commonDebug_waitForRead : UInt<1>
    output io_debugIO_inActDebugIO_2_adrDebug_commonDebug_doReadWire : UInt<1>
    output io_debugIO_inActDebugIO_2_adrDebug_subDone : UInt<1>
    output io_debugIO_inActDebugIO_2_dataDebug_commonDebug_idx : UInt<10>
    output io_debugIO_inActDebugIO_2_dataDebug_commonDebug_idxInc : UInt<1>
    output io_debugIO_inActDebugIO_2_dataDebug_commonDebug_idxCount : UInt<10>
    output io_debugIO_inActDebugIO_2_dataDebug_commonDebug_currentData : UInt<12>
    output io_debugIO_inActDebugIO_2_dataDebug_commonDebug_meetOneZero : UInt<1>
    output io_debugIO_inActDebugIO_2_dataDebug_commonDebug_theState : UInt<2>
    output io_debugIO_inActDebugIO_2_dataDebug_commonDebug_waitForRead : UInt<1>
    output io_debugIO_inActDebugIO_2_dataDebug_commonDebug_doReadWire : UInt<1>
    output io_debugIO_inActDebugIO_2_dataDebug_subDone : UInt<1>
    output io_debugIO_pSumDebugIO_0_idx : UInt<10>
    output io_debugIO_pSumDebugIO_0_idxInc : UInt<1>
    output io_debugIO_pSumDebugIO_0_idxCount : UInt<10>
    output io_debugIO_pSumDebugIO_0_currentData : UInt<20>
    output io_debugIO_pSumDebugIO_0_meetOneZero : UInt<1>
    output io_debugIO_pSumDebugIO_0_theState : UInt<2>
    output io_debugIO_pSumDebugIO_0_waitForRead : UInt<1>
    output io_debugIO_pSumDebugIO_0_doReadWire : UInt<1>
    output io_debugIO_pSumDebugIO_1_idx : UInt<10>
    output io_debugIO_pSumDebugIO_1_idxInc : UInt<1>
    output io_debugIO_pSumDebugIO_1_idxCount : UInt<10>
    output io_debugIO_pSumDebugIO_1_currentData : UInt<20>
    output io_debugIO_pSumDebugIO_1_meetOneZero : UInt<1>
    output io_debugIO_pSumDebugIO_1_theState : UInt<2>
    output io_debugIO_pSumDebugIO_1_waitForRead : UInt<1>
    output io_debugIO_pSumDebugIO_1_doReadWire : UInt<1>
    output io_debugIO_pSumDebugIO_2_idx : UInt<10>
    output io_debugIO_pSumDebugIO_2_idxInc : UInt<1>
    output io_debugIO_pSumDebugIO_2_idxCount : UInt<10>
    output io_debugIO_pSumDebugIO_2_currentData : UInt<20>
    output io_debugIO_pSumDebugIO_2_meetOneZero : UInt<1>
    output io_debugIO_pSumDebugIO_2_theState : UInt<2>
    output io_debugIO_pSumDebugIO_2_waitForRead : UInt<1>
    output io_debugIO_pSumDebugIO_2_doReadWire : UInt<1>
    output io_debugIO_pSumDebugIO_3_idx : UInt<10>
    output io_debugIO_pSumDebugIO_3_idxInc : UInt<1>
    output io_debugIO_pSumDebugIO_3_idxCount : UInt<10>
    output io_debugIO_pSumDebugIO_3_currentData : UInt<20>
    output io_debugIO_pSumDebugIO_3_meetOneZero : UInt<1>
    output io_debugIO_pSumDebugIO_3_theState : UInt<2>
    output io_debugIO_pSumDebugIO_3_waitForRead : UInt<1>
    output io_debugIO_pSumDebugIO_3_doReadWire : UInt<1>
  
    inst InActSRAMBank of InActSRAMBank @[GLBCluster.scala 9:61]
    inst InActSRAMBank_1 of InActSRAMBank @[GLBCluster.scala 9:61]
    inst InActSRAMBank_2 of InActSRAMBank @[GLBCluster.scala 9:61]
    inst PSumSRAMBank of PSumSRAMBank @[GLBCluster.scala 11:60]
    inst PSumSRAMBank_1 of PSumSRAMBank @[GLBCluster.scala 11:60]
    inst PSumSRAMBank_2 of PSumSRAMBank @[GLBCluster.scala 11:60]
    inst PSumSRAMBank_3 of PSumSRAMBank @[GLBCluster.scala 11:60]
    reg theSRAMsState_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), theSRAMsState_0) @[GLBCluster.scala 19:38]
    reg theSRAMsState_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), theSRAMsState_1) @[GLBCluster.scala 19:38]
    reg inActSRAMDoneRegVec_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), inActSRAMDoneRegVec_0) @[GLBCluster.scala 21:47]
    reg inActSRAMDoneRegVec_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), inActSRAMDoneRegVec_1) @[GLBCluster.scala 21:47]
    reg inActSRAMDoneRegVec_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), inActSRAMDoneRegVec_2) @[GLBCluster.scala 21:47]
    reg pSumSRAMDoneRegVec_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), pSumSRAMDoneRegVec_0) @[GLBCluster.scala 21:98]
    reg pSumSRAMDoneRegVec_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), pSumSRAMDoneRegVec_1) @[GLBCluster.scala 21:98]
    reg pSumSRAMDoneRegVec_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), pSumSRAMDoneRegVec_2) @[GLBCluster.scala 21:98]
    reg pSumSRAMDoneRegVec_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), pSumSRAMDoneRegVec_3) @[GLBCluster.scala 21:98]
    reg _T_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_3) @[GLBCluster.scala 28:69]
    reg _T_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_4) @[GLBCluster.scala 28:69]
    reg _T_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_5) @[GLBCluster.scala 28:69]
    reg _T_6 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_6) @[GLBCluster.scala 28:69]
    node _T_8 = and(inActSRAMDoneRegVec_0, inActSRAMDoneRegVec_1) @[GLBCluster.scala 50:62]
    node _T_9 = and(_T_8, inActSRAMDoneRegVec_2) @[GLBCluster.scala 50:62]
    node _T_10 = eq(theSRAMsState_0, UInt<1>("h1")) @[GLBCluster.scala 51:46]
    node _T_11 = eq(inActSRAMDoneRegVec_0, UInt<1>("h0")) @[GLBCluster.scala 54:58]
    node theSRAMsDoingWire_0 = _T_10 @[GLBCluster.scala 26:39 GLBCluster.scala 51:26]
    node _T_12 = and(theSRAMsDoingWire_0, _T_11) @[GLBCluster.scala 54:55]
    node _T_13 = eq(theSRAMsDoingWire_0, UInt<1>("h0")) @[GLBCluster.scala 57:39]
    node _T_14 = eq(inActSRAMDoneRegVec_0, UInt<1>("h0")) @[GLBCluster.scala 57:91]
    node inActSRAMs_0_ctrlPath_done = InActSRAMBank.io_ctrlPath_done @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node _T_15 = mux(inActSRAMs_0_ctrlPath_done, _T_14, inActSRAMDoneRegVec_0) @[GLBCluster.scala 57:65]
    node _T_16 = mux(_T_13, _T_15, UInt<1>("h0")) @[GLBCluster.scala 57:38]
    node _T_17 = eq(inActSRAMDoneRegVec_1, UInt<1>("h0")) @[GLBCluster.scala 54:58]
    node _T_18 = and(theSRAMsDoingWire_0, _T_17) @[GLBCluster.scala 54:55]
    node _T_19 = eq(theSRAMsDoingWire_0, UInt<1>("h0")) @[GLBCluster.scala 57:39]
    node _T_20 = eq(inActSRAMDoneRegVec_1, UInt<1>("h0")) @[GLBCluster.scala 57:91]
    node inActSRAMs_1_ctrlPath_done = InActSRAMBank_1.io_ctrlPath_done @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node _T_21 = mux(inActSRAMs_1_ctrlPath_done, _T_20, inActSRAMDoneRegVec_1) @[GLBCluster.scala 57:65]
    node _T_22 = mux(_T_19, _T_21, UInt<1>("h0")) @[GLBCluster.scala 57:38]
    node _T_23 = eq(inActSRAMDoneRegVec_2, UInt<1>("h0")) @[GLBCluster.scala 54:58]
    node _T_24 = and(theSRAMsDoingWire_0, _T_23) @[GLBCluster.scala 54:55]
    node _T_25 = eq(theSRAMsDoingWire_0, UInt<1>("h0")) @[GLBCluster.scala 57:39]
    node _T_26 = eq(inActSRAMDoneRegVec_2, UInt<1>("h0")) @[GLBCluster.scala 57:91]
    node inActSRAMs_2_ctrlPath_done = InActSRAMBank_2.io_ctrlPath_done @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node _T_27 = mux(inActSRAMs_2_ctrlPath_done, _T_26, inActSRAMDoneRegVec_2) @[GLBCluster.scala 57:65]
    node _T_28 = mux(_T_25, _T_27, UInt<1>("h0")) @[GLBCluster.scala 57:38]
    node _T_29 = eq(UInt<1>("h0"), theSRAMsState_0) @[Conditional.scala 37:30]
    node theSRAMsEnWire_0 = io_ctrlPath_inActIO_doEn @[GLBCluster.scala 17:36 GLBCluster.scala 48:23]
    node _GEN_0 = mux(theSRAMsEnWire_0, UInt<1>("h1"), theSRAMsState_0) @[GLBCluster.scala 32:23]
    node _T_30 = eq(UInt<1>("h1"), theSRAMsState_0) @[Conditional.scala 37:30]
    node theSRAMsAllDoneWire_0 = _T_9 @[GLBCluster.scala 24:41 GLBCluster.scala 50:28]
    node _GEN_1 = mux(theSRAMsAllDoneWire_0, UInt<1>("h0"), theSRAMsState_0) @[GLBCluster.scala 37:21]
    node _GEN_2 = mux(_T_30, _GEN_1, theSRAMsState_0) @[Conditional.scala 39:67]
    node _GEN_3 = mux(_T_29, _GEN_0, _GEN_2) @[Conditional.scala 40:58]
    node _T_31 = and(pSumSRAMDoneRegVec_0, pSumSRAMDoneRegVec_1) @[GLBCluster.scala 50:62]
    node _T_32 = and(_T_31, pSumSRAMDoneRegVec_2) @[GLBCluster.scala 50:62]
    node _T_33 = and(_T_32, pSumSRAMDoneRegVec_3) @[GLBCluster.scala 50:62]
    node _T_34 = eq(theSRAMsState_1, UInt<1>("h1")) @[GLBCluster.scala 51:46]
    node _T_35 = eq(pSumSRAMDoneRegVec_0, UInt<1>("h0")) @[GLBCluster.scala 54:58]
    node theSRAMsDoingWire_1 = _T_34 @[GLBCluster.scala 26:39 GLBCluster.scala 51:26]
    node _T_36 = and(theSRAMsDoingWire_1, _T_35) @[GLBCluster.scala 54:55]
    node _T_37 = eq(theSRAMsDoingWire_1, UInt<1>("h0")) @[GLBCluster.scala 57:39]
    node _T_38 = eq(pSumSRAMDoneRegVec_0, UInt<1>("h0")) @[GLBCluster.scala 57:91]
    node pSumSRAMs_0_ctrlPath_done = PSumSRAMBank.io_ctrlPath_done @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node _T_39 = mux(pSumSRAMs_0_ctrlPath_done, _T_38, pSumSRAMDoneRegVec_0) @[GLBCluster.scala 57:65]
    node _T_40 = mux(_T_37, _T_39, UInt<1>("h0")) @[GLBCluster.scala 57:38]
    node _T_41 = eq(pSumSRAMDoneRegVec_1, UInt<1>("h0")) @[GLBCluster.scala 54:58]
    node _T_42 = and(theSRAMsDoingWire_1, _T_41) @[GLBCluster.scala 54:55]
    node _T_43 = eq(theSRAMsDoingWire_1, UInt<1>("h0")) @[GLBCluster.scala 57:39]
    node _T_44 = eq(pSumSRAMDoneRegVec_1, UInt<1>("h0")) @[GLBCluster.scala 57:91]
    node pSumSRAMs_1_ctrlPath_done = PSumSRAMBank_1.io_ctrlPath_done @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node _T_45 = mux(pSumSRAMs_1_ctrlPath_done, _T_44, pSumSRAMDoneRegVec_1) @[GLBCluster.scala 57:65]
    node _T_46 = mux(_T_43, _T_45, UInt<1>("h0")) @[GLBCluster.scala 57:38]
    node _T_47 = eq(pSumSRAMDoneRegVec_2, UInt<1>("h0")) @[GLBCluster.scala 54:58]
    node _T_48 = and(theSRAMsDoingWire_1, _T_47) @[GLBCluster.scala 54:55]
    node _T_49 = eq(theSRAMsDoingWire_1, UInt<1>("h0")) @[GLBCluster.scala 57:39]
    node _T_50 = eq(pSumSRAMDoneRegVec_2, UInt<1>("h0")) @[GLBCluster.scala 57:91]
    node pSumSRAMs_2_ctrlPath_done = PSumSRAMBank_2.io_ctrlPath_done @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node _T_51 = mux(pSumSRAMs_2_ctrlPath_done, _T_50, pSumSRAMDoneRegVec_2) @[GLBCluster.scala 57:65]
    node _T_52 = mux(_T_49, _T_51, UInt<1>("h0")) @[GLBCluster.scala 57:38]
    node _T_53 = eq(pSumSRAMDoneRegVec_3, UInt<1>("h0")) @[GLBCluster.scala 54:58]
    node _T_54 = and(theSRAMsDoingWire_1, _T_53) @[GLBCluster.scala 54:55]
    node _T_55 = eq(theSRAMsDoingWire_1, UInt<1>("h0")) @[GLBCluster.scala 57:39]
    node _T_56 = eq(pSumSRAMDoneRegVec_3, UInt<1>("h0")) @[GLBCluster.scala 57:91]
    node pSumSRAMs_3_ctrlPath_done = PSumSRAMBank_3.io_ctrlPath_done @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node _T_57 = mux(pSumSRAMs_3_ctrlPath_done, _T_56, pSumSRAMDoneRegVec_3) @[GLBCluster.scala 57:65]
    node _T_58 = mux(_T_55, _T_57, UInt<1>("h0")) @[GLBCluster.scala 57:38]
    node _T_59 = eq(UInt<1>("h0"), theSRAMsState_1) @[Conditional.scala 37:30]
    node theSRAMsEnWire_1 = io_ctrlPath_pSumIO_doEn @[GLBCluster.scala 17:36 GLBCluster.scala 48:23]
    node _GEN_4 = mux(theSRAMsEnWire_1, UInt<1>("h1"), theSRAMsState_1) @[GLBCluster.scala 32:23]
    node _T_60 = eq(UInt<1>("h1"), theSRAMsState_1) @[Conditional.scala 37:30]
    node theSRAMsAllDoneWire_1 = _T_33 @[GLBCluster.scala 24:41 GLBCluster.scala 50:28]
    node _GEN_5 = mux(theSRAMsAllDoneWire_1, UInt<1>("h0"), theSRAMsState_1) @[GLBCluster.scala 37:21]
    node _GEN_6 = mux(_T_60, _GEN_5, theSRAMsState_1) @[Conditional.scala 39:67]
    node _GEN_7 = mux(_T_59, _GEN_4, _GEN_6) @[Conditional.scala 40:58]
    node inActSRAMs_0_dataPath_inIOs_adrIOs_data_ready = InActSRAMBank.io_dataPath_inIOs_adrIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_dataPath_inIOs_adrIOs_data_valid = io_dataPath_inActIO_0_inIOs_adrIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_0_dataPath_inIOs_adrIOs_data_bits = io_dataPath_inActIO_0_inIOs_adrIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_0_dataPath_inIOs_dataIOs_data_ready = InActSRAMBank.io_dataPath_inIOs_dataIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_dataPath_inIOs_dataIOs_data_valid = io_dataPath_inActIO_0_inIOs_dataIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_0_dataPath_inIOs_dataIOs_data_bits = io_dataPath_inActIO_0_inIOs_dataIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_0_dataPath_outIOs_adrIOs_data_ready = io_dataPath_inActIO_0_outIOs_adrIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_0_dataPath_outIOs_adrIOs_data_valid = InActSRAMBank.io_dataPath_outIOs_adrIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_dataPath_outIOs_adrIOs_data_bits = InActSRAMBank.io_dataPath_outIOs_adrIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_dataPath_outIOs_dataIOs_data_ready = io_dataPath_inActIO_0_outIOs_dataIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_0_dataPath_outIOs_dataIOs_data_valid = InActSRAMBank.io_dataPath_outIOs_dataIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_dataPath_outIOs_dataIOs_data_bits = InActSRAMBank.io_dataPath_outIOs_dataIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_ctrlPath_writeOrRead = io_ctrlPath_inActIO_writeOrRead @[GLBCluster.scala 9:31 GLBCluster.scala 58:38]
    node inActSRAMs_0_ctrlPath_doEn = _T_12 @[GLBCluster.scala 9:31 GLBCluster.scala 54:31]
    node inActSRAMs_0_debugIO_theState = InActSRAMBank.io_debugIO_theState @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_adrDebug_commonDebug_idx = InActSRAMBank.io_debugIO_adrDebug_commonDebug_idx @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_adrDebug_commonDebug_idxInc = InActSRAMBank.io_debugIO_adrDebug_commonDebug_idxInc @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_adrDebug_commonDebug_idxCount = InActSRAMBank.io_debugIO_adrDebug_commonDebug_idxCount @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_adrDebug_commonDebug_currentData = InActSRAMBank.io_debugIO_adrDebug_commonDebug_currentData @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_adrDebug_commonDebug_meetOneZero = InActSRAMBank.io_debugIO_adrDebug_commonDebug_meetOneZero @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_adrDebug_commonDebug_theState = InActSRAMBank.io_debugIO_adrDebug_commonDebug_theState @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_adrDebug_commonDebug_waitForRead = InActSRAMBank.io_debugIO_adrDebug_commonDebug_waitForRead @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_adrDebug_commonDebug_doReadWire = InActSRAMBank.io_debugIO_adrDebug_commonDebug_doReadWire @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_adrDebug_subDone = InActSRAMBank.io_debugIO_adrDebug_subDone @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_dataDebug_commonDebug_idx = InActSRAMBank.io_debugIO_dataDebug_commonDebug_idx @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_dataDebug_commonDebug_idxInc = InActSRAMBank.io_debugIO_dataDebug_commonDebug_idxInc @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_dataDebug_commonDebug_idxCount = InActSRAMBank.io_debugIO_dataDebug_commonDebug_idxCount @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_dataDebug_commonDebug_currentData = InActSRAMBank.io_debugIO_dataDebug_commonDebug_currentData @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_dataDebug_commonDebug_meetOneZero = InActSRAMBank.io_debugIO_dataDebug_commonDebug_meetOneZero @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_dataDebug_commonDebug_theState = InActSRAMBank.io_debugIO_dataDebug_commonDebug_theState @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_dataDebug_commonDebug_waitForRead = InActSRAMBank.io_debugIO_dataDebug_commonDebug_waitForRead @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_dataDebug_commonDebug_doReadWire = InActSRAMBank.io_debugIO_dataDebug_commonDebug_doReadWire @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_0_debugIO_dataDebug_subDone = InActSRAMBank.io_debugIO_dataDebug_subDone @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_dataPath_inIOs_adrIOs_data_ready = InActSRAMBank_1.io_dataPath_inIOs_adrIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_dataPath_inIOs_adrIOs_data_valid = io_dataPath_inActIO_1_inIOs_adrIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_1_dataPath_inIOs_adrIOs_data_bits = io_dataPath_inActIO_1_inIOs_adrIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_1_dataPath_inIOs_dataIOs_data_ready = InActSRAMBank_1.io_dataPath_inIOs_dataIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_dataPath_inIOs_dataIOs_data_valid = io_dataPath_inActIO_1_inIOs_dataIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_1_dataPath_inIOs_dataIOs_data_bits = io_dataPath_inActIO_1_inIOs_dataIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_1_dataPath_outIOs_adrIOs_data_ready = io_dataPath_inActIO_1_outIOs_adrIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_1_dataPath_outIOs_adrIOs_data_valid = InActSRAMBank_1.io_dataPath_outIOs_adrIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_dataPath_outIOs_adrIOs_data_bits = InActSRAMBank_1.io_dataPath_outIOs_adrIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_dataPath_outIOs_dataIOs_data_ready = io_dataPath_inActIO_1_outIOs_dataIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_1_dataPath_outIOs_dataIOs_data_valid = InActSRAMBank_1.io_dataPath_outIOs_dataIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_dataPath_outIOs_dataIOs_data_bits = InActSRAMBank_1.io_dataPath_outIOs_dataIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_ctrlPath_writeOrRead = io_ctrlPath_inActIO_writeOrRead @[GLBCluster.scala 9:31 GLBCluster.scala 58:38]
    node inActSRAMs_1_ctrlPath_doEn = _T_18 @[GLBCluster.scala 9:31 GLBCluster.scala 54:31]
    node inActSRAMs_1_debugIO_theState = InActSRAMBank_1.io_debugIO_theState @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_adrDebug_commonDebug_idx = InActSRAMBank_1.io_debugIO_adrDebug_commonDebug_idx @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_adrDebug_commonDebug_idxInc = InActSRAMBank_1.io_debugIO_adrDebug_commonDebug_idxInc @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_adrDebug_commonDebug_idxCount = InActSRAMBank_1.io_debugIO_adrDebug_commonDebug_idxCount @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_adrDebug_commonDebug_currentData = InActSRAMBank_1.io_debugIO_adrDebug_commonDebug_currentData @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_adrDebug_commonDebug_meetOneZero = InActSRAMBank_1.io_debugIO_adrDebug_commonDebug_meetOneZero @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_adrDebug_commonDebug_theState = InActSRAMBank_1.io_debugIO_adrDebug_commonDebug_theState @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_adrDebug_commonDebug_waitForRead = InActSRAMBank_1.io_debugIO_adrDebug_commonDebug_waitForRead @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_adrDebug_commonDebug_doReadWire = InActSRAMBank_1.io_debugIO_adrDebug_commonDebug_doReadWire @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_adrDebug_subDone = InActSRAMBank_1.io_debugIO_adrDebug_subDone @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_dataDebug_commonDebug_idx = InActSRAMBank_1.io_debugIO_dataDebug_commonDebug_idx @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_dataDebug_commonDebug_idxInc = InActSRAMBank_1.io_debugIO_dataDebug_commonDebug_idxInc @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_dataDebug_commonDebug_idxCount = InActSRAMBank_1.io_debugIO_dataDebug_commonDebug_idxCount @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_dataDebug_commonDebug_currentData = InActSRAMBank_1.io_debugIO_dataDebug_commonDebug_currentData @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_dataDebug_commonDebug_meetOneZero = InActSRAMBank_1.io_debugIO_dataDebug_commonDebug_meetOneZero @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_dataDebug_commonDebug_theState = InActSRAMBank_1.io_debugIO_dataDebug_commonDebug_theState @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_dataDebug_commonDebug_waitForRead = InActSRAMBank_1.io_debugIO_dataDebug_commonDebug_waitForRead @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_dataDebug_commonDebug_doReadWire = InActSRAMBank_1.io_debugIO_dataDebug_commonDebug_doReadWire @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_1_debugIO_dataDebug_subDone = InActSRAMBank_1.io_debugIO_dataDebug_subDone @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_dataPath_inIOs_adrIOs_data_ready = InActSRAMBank_2.io_dataPath_inIOs_adrIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_dataPath_inIOs_adrIOs_data_valid = io_dataPath_inActIO_2_inIOs_adrIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_2_dataPath_inIOs_adrIOs_data_bits = io_dataPath_inActIO_2_inIOs_adrIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_2_dataPath_inIOs_dataIOs_data_ready = InActSRAMBank_2.io_dataPath_inIOs_dataIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_dataPath_inIOs_dataIOs_data_valid = io_dataPath_inActIO_2_inIOs_dataIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_2_dataPath_inIOs_dataIOs_data_bits = io_dataPath_inActIO_2_inIOs_dataIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_2_dataPath_outIOs_adrIOs_data_ready = io_dataPath_inActIO_2_outIOs_adrIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_2_dataPath_outIOs_adrIOs_data_valid = InActSRAMBank_2.io_dataPath_outIOs_adrIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_dataPath_outIOs_adrIOs_data_bits = InActSRAMBank_2.io_dataPath_outIOs_adrIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_dataPath_outIOs_dataIOs_data_ready = io_dataPath_inActIO_2_outIOs_dataIOs_data_ready @[GLBCluster.scala 9:31 GLBCluster.scala 64:77]
    node inActSRAMs_2_dataPath_outIOs_dataIOs_data_valid = InActSRAMBank_2.io_dataPath_outIOs_dataIOs_data_valid @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_dataPath_outIOs_dataIOs_data_bits = InActSRAMBank_2.io_dataPath_outIOs_dataIOs_data_bits @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_ctrlPath_writeOrRead = io_ctrlPath_inActIO_writeOrRead @[GLBCluster.scala 9:31 GLBCluster.scala 58:38]
    node inActSRAMs_2_ctrlPath_doEn = _T_24 @[GLBCluster.scala 9:31 GLBCluster.scala 54:31]
    node inActSRAMs_2_debugIO_theState = InActSRAMBank_2.io_debugIO_theState @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_adrDebug_commonDebug_idx = InActSRAMBank_2.io_debugIO_adrDebug_commonDebug_idx @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_adrDebug_commonDebug_idxInc = InActSRAMBank_2.io_debugIO_adrDebug_commonDebug_idxInc @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_adrDebug_commonDebug_idxCount = InActSRAMBank_2.io_debugIO_adrDebug_commonDebug_idxCount @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_adrDebug_commonDebug_currentData = InActSRAMBank_2.io_debugIO_adrDebug_commonDebug_currentData @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_adrDebug_commonDebug_meetOneZero = InActSRAMBank_2.io_debugIO_adrDebug_commonDebug_meetOneZero @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_adrDebug_commonDebug_theState = InActSRAMBank_2.io_debugIO_adrDebug_commonDebug_theState @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_adrDebug_commonDebug_waitForRead = InActSRAMBank_2.io_debugIO_adrDebug_commonDebug_waitForRead @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_adrDebug_commonDebug_doReadWire = InActSRAMBank_2.io_debugIO_adrDebug_commonDebug_doReadWire @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_adrDebug_subDone = InActSRAMBank_2.io_debugIO_adrDebug_subDone @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_dataDebug_commonDebug_idx = InActSRAMBank_2.io_debugIO_dataDebug_commonDebug_idx @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_dataDebug_commonDebug_idxInc = InActSRAMBank_2.io_debugIO_dataDebug_commonDebug_idxInc @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_dataDebug_commonDebug_idxCount = InActSRAMBank_2.io_debugIO_dataDebug_commonDebug_idxCount @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_dataDebug_commonDebug_currentData = InActSRAMBank_2.io_debugIO_dataDebug_commonDebug_currentData @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_dataDebug_commonDebug_meetOneZero = InActSRAMBank_2.io_debugIO_dataDebug_commonDebug_meetOneZero @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_dataDebug_commonDebug_theState = InActSRAMBank_2.io_debugIO_dataDebug_commonDebug_theState @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_dataDebug_commonDebug_waitForRead = InActSRAMBank_2.io_debugIO_dataDebug_commonDebug_waitForRead @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_dataDebug_commonDebug_doReadWire = InActSRAMBank_2.io_debugIO_dataDebug_commonDebug_doReadWire @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node inActSRAMs_2_debugIO_dataDebug_subDone = InActSRAMBank_2.io_debugIO_dataDebug_subDone @[GLBCluster.scala 9:31 GLBCluster.scala 9:31]
    node pSumSRAMs_0_dataPath_inIOs_ready = PSumSRAMBank.io_dataPath_inIOs_ready @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_0_dataPath_inIOs_valid = io_dataPath_pSumIO_0_inIOs_valid @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_0_dataPath_inIOs_bits = io_dataPath_pSumIO_0_inIOs_bits @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_0_dataPath_outIOs_ready = io_dataPath_pSumIO_0_outIOs_ready @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_0_dataPath_outIOs_valid = PSumSRAMBank.io_dataPath_outIOs_valid @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_0_dataPath_outIOs_bits = PSumSRAMBank.io_dataPath_outIOs_bits @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_0_ctrlPath_writeOrRead = io_ctrlPath_pSumIO_writeOrRead @[GLBCluster.scala 11:31 GLBCluster.scala 58:38]
    node pSumSRAMs_0_ctrlPath_doEn = _T_36 @[GLBCluster.scala 11:31 GLBCluster.scala 54:31]
    node _T_7_0 = _T_3 @[GLBCluster.scala 28:39 GLBCluster.scala 28:39]
    node pSumSRAMs_0_ctrlPath_startIdx = pad(_T_7_0, 10) @[GLBCluster.scala 11:31 GLBCluster.scala 68:30]
    node pSumSRAMs_0_debugIO_idx = PSumSRAMBank.io_debugIO_idx @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_0_debugIO_idxInc = PSumSRAMBank.io_debugIO_idxInc @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_0_debugIO_idxCount = PSumSRAMBank.io_debugIO_idxCount @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_0_debugIO_currentData = PSumSRAMBank.io_debugIO_currentData @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_0_debugIO_meetOneZero = PSumSRAMBank.io_debugIO_meetOneZero @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_0_debugIO_theState = PSumSRAMBank.io_debugIO_theState @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_0_debugIO_waitForRead = PSumSRAMBank.io_debugIO_waitForRead @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_0_debugIO_doReadWire = PSumSRAMBank.io_debugIO_doReadWire @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_1_dataPath_inIOs_ready = PSumSRAMBank_1.io_dataPath_inIOs_ready @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_1_dataPath_inIOs_valid = io_dataPath_pSumIO_1_inIOs_valid @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_1_dataPath_inIOs_bits = io_dataPath_pSumIO_1_inIOs_bits @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_1_dataPath_outIOs_ready = io_dataPath_pSumIO_1_outIOs_ready @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_1_dataPath_outIOs_valid = PSumSRAMBank_1.io_dataPath_outIOs_valid @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_1_dataPath_outIOs_bits = PSumSRAMBank_1.io_dataPath_outIOs_bits @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_1_ctrlPath_writeOrRead = io_ctrlPath_pSumIO_writeOrRead @[GLBCluster.scala 11:31 GLBCluster.scala 58:38]
    node pSumSRAMs_1_ctrlPath_doEn = _T_42 @[GLBCluster.scala 11:31 GLBCluster.scala 54:31]
    node _T_7_1 = _T_4 @[GLBCluster.scala 28:39 GLBCluster.scala 28:39]
    node pSumSRAMs_1_ctrlPath_startIdx = pad(_T_7_1, 10) @[GLBCluster.scala 11:31 GLBCluster.scala 68:30]
    node pSumSRAMs_1_debugIO_idx = PSumSRAMBank_1.io_debugIO_idx @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_1_debugIO_idxInc = PSumSRAMBank_1.io_debugIO_idxInc @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_1_debugIO_idxCount = PSumSRAMBank_1.io_debugIO_idxCount @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_1_debugIO_currentData = PSumSRAMBank_1.io_debugIO_currentData @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_1_debugIO_meetOneZero = PSumSRAMBank_1.io_debugIO_meetOneZero @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_1_debugIO_theState = PSumSRAMBank_1.io_debugIO_theState @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_1_debugIO_waitForRead = PSumSRAMBank_1.io_debugIO_waitForRead @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_1_debugIO_doReadWire = PSumSRAMBank_1.io_debugIO_doReadWire @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_2_dataPath_inIOs_ready = PSumSRAMBank_2.io_dataPath_inIOs_ready @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_2_dataPath_inIOs_valid = io_dataPath_pSumIO_2_inIOs_valid @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_2_dataPath_inIOs_bits = io_dataPath_pSumIO_2_inIOs_bits @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_2_dataPath_outIOs_ready = io_dataPath_pSumIO_2_outIOs_ready @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_2_dataPath_outIOs_valid = PSumSRAMBank_2.io_dataPath_outIOs_valid @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_2_dataPath_outIOs_bits = PSumSRAMBank_2.io_dataPath_outIOs_bits @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_2_ctrlPath_writeOrRead = io_ctrlPath_pSumIO_writeOrRead @[GLBCluster.scala 11:31 GLBCluster.scala 58:38]
    node pSumSRAMs_2_ctrlPath_doEn = _T_48 @[GLBCluster.scala 11:31 GLBCluster.scala 54:31]
    node _T_7_2 = _T_5 @[GLBCluster.scala 28:39 GLBCluster.scala 28:39]
    node pSumSRAMs_2_ctrlPath_startIdx = pad(_T_7_2, 10) @[GLBCluster.scala 11:31 GLBCluster.scala 68:30]
    node pSumSRAMs_2_debugIO_idx = PSumSRAMBank_2.io_debugIO_idx @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_2_debugIO_idxInc = PSumSRAMBank_2.io_debugIO_idxInc @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_2_debugIO_idxCount = PSumSRAMBank_2.io_debugIO_idxCount @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_2_debugIO_currentData = PSumSRAMBank_2.io_debugIO_currentData @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_2_debugIO_meetOneZero = PSumSRAMBank_2.io_debugIO_meetOneZero @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_2_debugIO_theState = PSumSRAMBank_2.io_debugIO_theState @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_2_debugIO_waitForRead = PSumSRAMBank_2.io_debugIO_waitForRead @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_2_debugIO_doReadWire = PSumSRAMBank_2.io_debugIO_doReadWire @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_3_dataPath_inIOs_ready = PSumSRAMBank_3.io_dataPath_inIOs_ready @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_3_dataPath_inIOs_valid = io_dataPath_pSumIO_3_inIOs_valid @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_3_dataPath_inIOs_bits = io_dataPath_pSumIO_3_inIOs_bits @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_3_dataPath_outIOs_ready = io_dataPath_pSumIO_3_outIOs_ready @[GLBCluster.scala 11:31 GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    node pSumSRAMs_3_dataPath_outIOs_valid = PSumSRAMBank_3.io_dataPath_outIOs_valid @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_3_dataPath_outIOs_bits = PSumSRAMBank_3.io_dataPath_outIOs_bits @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_3_ctrlPath_writeOrRead = io_ctrlPath_pSumIO_writeOrRead @[GLBCluster.scala 11:31 GLBCluster.scala 58:38]
    node pSumSRAMs_3_ctrlPath_doEn = _T_54 @[GLBCluster.scala 11:31 GLBCluster.scala 54:31]
    node _T_7_3 = _T_6 @[GLBCluster.scala 28:39 GLBCluster.scala 28:39]
    node pSumSRAMs_3_ctrlPath_startIdx = pad(_T_7_3, 10) @[GLBCluster.scala 11:31 GLBCluster.scala 68:30]
    node pSumSRAMs_3_debugIO_idx = PSumSRAMBank_3.io_debugIO_idx @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_3_debugIO_idxInc = PSumSRAMBank_3.io_debugIO_idxInc @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_3_debugIO_idxCount = PSumSRAMBank_3.io_debugIO_idxCount @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_3_debugIO_currentData = PSumSRAMBank_3.io_debugIO_currentData @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_3_debugIO_meetOneZero = PSumSRAMBank_3.io_debugIO_meetOneZero @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_3_debugIO_theState = PSumSRAMBank_3.io_debugIO_theState @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_3_debugIO_waitForRead = PSumSRAMBank_3.io_debugIO_waitForRead @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node pSumSRAMs_3_debugIO_doReadWire = PSumSRAMBank_3.io_debugIO_doReadWire @[GLBCluster.scala 11:31 GLBCluster.scala 11:31]
    node _T__0 = UInt<1>("h0") @[GLBCluster.scala 19:46 GLBCluster.scala 19:46]
    node _T__1 = UInt<1>("h0") @[GLBCluster.scala 19:46 GLBCluster.scala 19:46]
    node _T_1_0 = UInt<1>("h0") @[GLBCluster.scala 21:55 GLBCluster.scala 21:55]
    node _T_1_1 = UInt<1>("h0") @[GLBCluster.scala 21:55 GLBCluster.scala 21:55]
    node _T_1_2 = UInt<1>("h0") @[GLBCluster.scala 21:55 GLBCluster.scala 21:55]
    node _T_2_0 = UInt<1>("h0") @[GLBCluster.scala 21:106 GLBCluster.scala 21:106]
    node _T_2_1 = UInt<1>("h0") @[GLBCluster.scala 21:106 GLBCluster.scala 21:106]
    node _T_2_2 = UInt<1>("h0") @[GLBCluster.scala 21:106 GLBCluster.scala 21:106]
    node _T_2_3 = UInt<1>("h0") @[GLBCluster.scala 21:106 GLBCluster.scala 21:106]
    io_dataPath_inActIO_0_inIOs_adrIOs_data_ready <= inActSRAMs_0_dataPath_inIOs_adrIOs_data_ready @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_0_inIOs_dataIOs_data_ready <= inActSRAMs_0_dataPath_inIOs_dataIOs_data_ready @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_0_outIOs_adrIOs_data_valid <= inActSRAMs_0_dataPath_outIOs_adrIOs_data_valid @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_0_outIOs_adrIOs_data_bits <= inActSRAMs_0_dataPath_outIOs_adrIOs_data_bits @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_0_outIOs_dataIOs_data_valid <= inActSRAMs_0_dataPath_outIOs_dataIOs_data_valid @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_0_outIOs_dataIOs_data_bits <= inActSRAMs_0_dataPath_outIOs_dataIOs_data_bits @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_1_inIOs_adrIOs_data_ready <= inActSRAMs_1_dataPath_inIOs_adrIOs_data_ready @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_1_inIOs_dataIOs_data_ready <= inActSRAMs_1_dataPath_inIOs_dataIOs_data_ready @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_1_outIOs_adrIOs_data_valid <= inActSRAMs_1_dataPath_outIOs_adrIOs_data_valid @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_1_outIOs_adrIOs_data_bits <= inActSRAMs_1_dataPath_outIOs_adrIOs_data_bits @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_1_outIOs_dataIOs_data_valid <= inActSRAMs_1_dataPath_outIOs_dataIOs_data_valid @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_1_outIOs_dataIOs_data_bits <= inActSRAMs_1_dataPath_outIOs_dataIOs_data_bits @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_2_inIOs_adrIOs_data_ready <= inActSRAMs_2_dataPath_inIOs_adrIOs_data_ready @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_2_inIOs_dataIOs_data_ready <= inActSRAMs_2_dataPath_inIOs_dataIOs_data_ready @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_2_outIOs_adrIOs_data_valid <= inActSRAMs_2_dataPath_outIOs_adrIOs_data_valid @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_2_outIOs_adrIOs_data_bits <= inActSRAMs_2_dataPath_outIOs_adrIOs_data_bits @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_2_outIOs_dataIOs_data_valid <= inActSRAMs_2_dataPath_outIOs_dataIOs_data_valid @[GLBCluster.scala 64:77]
    io_dataPath_inActIO_2_outIOs_dataIOs_data_bits <= inActSRAMs_2_dataPath_outIOs_dataIOs_data_bits @[GLBCluster.scala 64:77]
    io_dataPath_weightIO_0_inIOs_adrIOs_data_ready <= io_dataPath_weightIO_0_outIOs_adrIOs_data_ready @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_0_inIOs_dataIOs_data_ready <= io_dataPath_weightIO_0_outIOs_dataIOs_data_ready @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_0_outIOs_adrIOs_data_valid <= io_dataPath_weightIO_0_inIOs_adrIOs_data_valid @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_0_outIOs_adrIOs_data_bits <= io_dataPath_weightIO_0_inIOs_adrIOs_data_bits @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_0_outIOs_dataIOs_data_valid <= io_dataPath_weightIO_0_inIOs_dataIOs_data_valid @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_0_outIOs_dataIOs_data_bits <= io_dataPath_weightIO_0_inIOs_dataIOs_data_bits @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_1_inIOs_adrIOs_data_ready <= io_dataPath_weightIO_1_outIOs_adrIOs_data_ready @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_1_inIOs_dataIOs_data_ready <= io_dataPath_weightIO_1_outIOs_dataIOs_data_ready @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_1_outIOs_adrIOs_data_valid <= io_dataPath_weightIO_1_inIOs_adrIOs_data_valid @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_1_outIOs_adrIOs_data_bits <= io_dataPath_weightIO_1_inIOs_adrIOs_data_bits @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_1_outIOs_dataIOs_data_valid <= io_dataPath_weightIO_1_inIOs_dataIOs_data_valid @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_1_outIOs_dataIOs_data_bits <= io_dataPath_weightIO_1_inIOs_dataIOs_data_bits @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_2_inIOs_adrIOs_data_ready <= io_dataPath_weightIO_2_outIOs_adrIOs_data_ready @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_2_inIOs_dataIOs_data_ready <= io_dataPath_weightIO_2_outIOs_dataIOs_data_ready @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_2_outIOs_adrIOs_data_valid <= io_dataPath_weightIO_2_inIOs_adrIOs_data_valid @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_2_outIOs_adrIOs_data_bits <= io_dataPath_weightIO_2_inIOs_adrIOs_data_bits @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_2_outIOs_dataIOs_data_valid <= io_dataPath_weightIO_2_inIOs_dataIOs_data_valid @[GLBCluster.scala 63:45]
    io_dataPath_weightIO_2_outIOs_dataIOs_data_bits <= io_dataPath_weightIO_2_inIOs_dataIOs_data_bits @[GLBCluster.scala 63:45]
    io_dataPath_pSumIO_0_inIOs_ready <= pSumSRAMs_0_dataPath_inIOs_ready @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_dataPath_pSumIO_0_outIOs_valid <= pSumSRAMs_0_dataPath_outIOs_valid @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_dataPath_pSumIO_0_outIOs_bits <= pSumSRAMs_0_dataPath_outIOs_bits @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_dataPath_pSumIO_1_inIOs_ready <= pSumSRAMs_1_dataPath_inIOs_ready @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_dataPath_pSumIO_1_outIOs_valid <= pSumSRAMs_1_dataPath_outIOs_valid @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_dataPath_pSumIO_1_outIOs_bits <= pSumSRAMs_1_dataPath_outIOs_bits @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_dataPath_pSumIO_2_inIOs_ready <= pSumSRAMs_2_dataPath_inIOs_ready @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_dataPath_pSumIO_2_outIOs_valid <= pSumSRAMs_2_dataPath_outIOs_valid @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_dataPath_pSumIO_2_outIOs_bits <= pSumSRAMs_2_dataPath_outIOs_bits @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_dataPath_pSumIO_3_inIOs_ready <= pSumSRAMs_3_dataPath_inIOs_ready @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_dataPath_pSumIO_3_outIOs_valid <= pSumSRAMs_3_dataPath_outIOs_valid @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_dataPath_pSumIO_3_outIOs_bits <= pSumSRAMs_3_dataPath_outIOs_bits @[GLBCluster.scala 65:76 GLBCluster.scala 67:21]
    io_ctrlPath_inActIO_done <= theSRAMsAllDoneWire_0 @[GLBCluster.scala 46:25]
    io_ctrlPath_inActIO_busy <= theSRAMsDoingWire_0 @[GLBCluster.scala 47:25]
    io_ctrlPath_pSumIO_done <= theSRAMsAllDoneWire_1 @[GLBCluster.scala 46:25]
    io_ctrlPath_pSumIO_busy <= theSRAMsDoingWire_1 @[GLBCluster.scala 47:25]
    io_debugIO_theState_0 <= theSRAMsState_0 @[GLBCluster.scala 74:25]
    io_debugIO_theState_1 <= theSRAMsState_1 @[GLBCluster.scala 74:25]
    io_debugIO_inActDebugIO_0_theState <= inActSRAMs_0_debugIO_theState @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_adrDebug_commonDebug_idx <= inActSRAMs_0_debugIO_adrDebug_commonDebug_idx @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_adrDebug_commonDebug_idxInc <= inActSRAMs_0_debugIO_adrDebug_commonDebug_idxInc @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_adrDebug_commonDebug_idxCount <= inActSRAMs_0_debugIO_adrDebug_commonDebug_idxCount @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_adrDebug_commonDebug_currentData <= inActSRAMs_0_debugIO_adrDebug_commonDebug_currentData @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_adrDebug_commonDebug_meetOneZero <= inActSRAMs_0_debugIO_adrDebug_commonDebug_meetOneZero @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_adrDebug_commonDebug_theState <= inActSRAMs_0_debugIO_adrDebug_commonDebug_theState @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_adrDebug_commonDebug_waitForRead <= inActSRAMs_0_debugIO_adrDebug_commonDebug_waitForRead @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_adrDebug_commonDebug_doReadWire <= inActSRAMs_0_debugIO_adrDebug_commonDebug_doReadWire @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_adrDebug_subDone <= inActSRAMs_0_debugIO_adrDebug_subDone @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_dataDebug_commonDebug_idx <= inActSRAMs_0_debugIO_dataDebug_commonDebug_idx @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_dataDebug_commonDebug_idxInc <= inActSRAMs_0_debugIO_dataDebug_commonDebug_idxInc @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_dataDebug_commonDebug_idxCount <= inActSRAMs_0_debugIO_dataDebug_commonDebug_idxCount @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_dataDebug_commonDebug_currentData <= inActSRAMs_0_debugIO_dataDebug_commonDebug_currentData @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_dataDebug_commonDebug_meetOneZero <= inActSRAMs_0_debugIO_dataDebug_commonDebug_meetOneZero @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_dataDebug_commonDebug_theState <= inActSRAMs_0_debugIO_dataDebug_commonDebug_theState @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_dataDebug_commonDebug_waitForRead <= inActSRAMs_0_debugIO_dataDebug_commonDebug_waitForRead @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_dataDebug_commonDebug_doReadWire <= inActSRAMs_0_debugIO_dataDebug_commonDebug_doReadWire @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_0_dataDebug_subDone <= inActSRAMs_0_debugIO_dataDebug_subDone @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_theState <= inActSRAMs_1_debugIO_theState @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_adrDebug_commonDebug_idx <= inActSRAMs_1_debugIO_adrDebug_commonDebug_idx @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_adrDebug_commonDebug_idxInc <= inActSRAMs_1_debugIO_adrDebug_commonDebug_idxInc @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_adrDebug_commonDebug_idxCount <= inActSRAMs_1_debugIO_adrDebug_commonDebug_idxCount @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_adrDebug_commonDebug_currentData <= inActSRAMs_1_debugIO_adrDebug_commonDebug_currentData @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_adrDebug_commonDebug_meetOneZero <= inActSRAMs_1_debugIO_adrDebug_commonDebug_meetOneZero @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_adrDebug_commonDebug_theState <= inActSRAMs_1_debugIO_adrDebug_commonDebug_theState @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_adrDebug_commonDebug_waitForRead <= inActSRAMs_1_debugIO_adrDebug_commonDebug_waitForRead @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_adrDebug_commonDebug_doReadWire <= inActSRAMs_1_debugIO_adrDebug_commonDebug_doReadWire @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_adrDebug_subDone <= inActSRAMs_1_debugIO_adrDebug_subDone @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_dataDebug_commonDebug_idx <= inActSRAMs_1_debugIO_dataDebug_commonDebug_idx @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_dataDebug_commonDebug_idxInc <= inActSRAMs_1_debugIO_dataDebug_commonDebug_idxInc @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_dataDebug_commonDebug_idxCount <= inActSRAMs_1_debugIO_dataDebug_commonDebug_idxCount @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_dataDebug_commonDebug_currentData <= inActSRAMs_1_debugIO_dataDebug_commonDebug_currentData @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_dataDebug_commonDebug_meetOneZero <= inActSRAMs_1_debugIO_dataDebug_commonDebug_meetOneZero @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_dataDebug_commonDebug_theState <= inActSRAMs_1_debugIO_dataDebug_commonDebug_theState @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_dataDebug_commonDebug_waitForRead <= inActSRAMs_1_debugIO_dataDebug_commonDebug_waitForRead @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_dataDebug_commonDebug_doReadWire <= inActSRAMs_1_debugIO_dataDebug_commonDebug_doReadWire @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_1_dataDebug_subDone <= inActSRAMs_1_debugIO_dataDebug_subDone @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_theState <= inActSRAMs_2_debugIO_theState @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_adrDebug_commonDebug_idx <= inActSRAMs_2_debugIO_adrDebug_commonDebug_idx @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_adrDebug_commonDebug_idxInc <= inActSRAMs_2_debugIO_adrDebug_commonDebug_idxInc @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_adrDebug_commonDebug_idxCount <= inActSRAMs_2_debugIO_adrDebug_commonDebug_idxCount @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_adrDebug_commonDebug_currentData <= inActSRAMs_2_debugIO_adrDebug_commonDebug_currentData @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_adrDebug_commonDebug_meetOneZero <= inActSRAMs_2_debugIO_adrDebug_commonDebug_meetOneZero @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_adrDebug_commonDebug_theState <= inActSRAMs_2_debugIO_adrDebug_commonDebug_theState @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_adrDebug_commonDebug_waitForRead <= inActSRAMs_2_debugIO_adrDebug_commonDebug_waitForRead @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_adrDebug_commonDebug_doReadWire <= inActSRAMs_2_debugIO_adrDebug_commonDebug_doReadWire @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_adrDebug_subDone <= inActSRAMs_2_debugIO_adrDebug_subDone @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_dataDebug_commonDebug_idx <= inActSRAMs_2_debugIO_dataDebug_commonDebug_idx @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_dataDebug_commonDebug_idxInc <= inActSRAMs_2_debugIO_dataDebug_commonDebug_idxInc @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_dataDebug_commonDebug_idxCount <= inActSRAMs_2_debugIO_dataDebug_commonDebug_idxCount @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_dataDebug_commonDebug_currentData <= inActSRAMs_2_debugIO_dataDebug_commonDebug_currentData @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_dataDebug_commonDebug_meetOneZero <= inActSRAMs_2_debugIO_dataDebug_commonDebug_meetOneZero @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_dataDebug_commonDebug_theState <= inActSRAMs_2_debugIO_dataDebug_commonDebug_theState @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_dataDebug_commonDebug_waitForRead <= inActSRAMs_2_debugIO_dataDebug_commonDebug_waitForRead @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_dataDebug_commonDebug_doReadWire <= inActSRAMs_2_debugIO_dataDebug_commonDebug_doReadWire @[GLBCluster.scala 72:85]
    io_debugIO_inActDebugIO_2_dataDebug_subDone <= inActSRAMs_2_debugIO_dataDebug_subDone @[GLBCluster.scala 72:85]
    io_debugIO_pSumDebugIO_0_idx <= pSumSRAMs_0_debugIO_idx @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_0_idxInc <= pSumSRAMs_0_debugIO_idxInc @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_0_idxCount <= pSumSRAMs_0_debugIO_idxCount @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_0_currentData <= pSumSRAMs_0_debugIO_currentData @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_0_meetOneZero <= pSumSRAMs_0_debugIO_meetOneZero @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_0_theState <= pSumSRAMs_0_debugIO_theState @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_0_waitForRead <= pSumSRAMs_0_debugIO_waitForRead @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_0_doReadWire <= pSumSRAMs_0_debugIO_doReadWire @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_1_idx <= pSumSRAMs_1_debugIO_idx @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_1_idxInc <= pSumSRAMs_1_debugIO_idxInc @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_1_idxCount <= pSumSRAMs_1_debugIO_idxCount @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_1_currentData <= pSumSRAMs_1_debugIO_currentData @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_1_meetOneZero <= pSumSRAMs_1_debugIO_meetOneZero @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_1_theState <= pSumSRAMs_1_debugIO_theState @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_1_waitForRead <= pSumSRAMs_1_debugIO_waitForRead @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_1_doReadWire <= pSumSRAMs_1_debugIO_doReadWire @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_2_idx <= pSumSRAMs_2_debugIO_idx @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_2_idxInc <= pSumSRAMs_2_debugIO_idxInc @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_2_idxCount <= pSumSRAMs_2_debugIO_idxCount @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_2_currentData <= pSumSRAMs_2_debugIO_currentData @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_2_meetOneZero <= pSumSRAMs_2_debugIO_meetOneZero @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_2_theState <= pSumSRAMs_2_debugIO_theState @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_2_waitForRead <= pSumSRAMs_2_debugIO_waitForRead @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_2_doReadWire <= pSumSRAMs_2_debugIO_doReadWire @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_3_idx <= pSumSRAMs_3_debugIO_idx @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_3_idxInc <= pSumSRAMs_3_debugIO_idxInc @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_3_idxCount <= pSumSRAMs_3_debugIO_idxCount @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_3_currentData <= pSumSRAMs_3_debugIO_currentData @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_3_meetOneZero <= pSumSRAMs_3_debugIO_meetOneZero @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_3_theState <= pSumSRAMs_3_debugIO_theState @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_3_waitForRead <= pSumSRAMs_3_debugIO_waitForRead @[GLBCluster.scala 73:84]
    io_debugIO_pSumDebugIO_3_doReadWire <= pSumSRAMs_3_debugIO_doReadWire @[GLBCluster.scala 73:84]
    InActSRAMBank.clock <= clock
    InActSRAMBank.reset <= reset
    InActSRAMBank.io_dataPath_inIOs_adrIOs_data_valid <= inActSRAMs_0_dataPath_inIOs_adrIOs_data_valid @[GLBCluster.scala 9:31]
    InActSRAMBank.io_dataPath_inIOs_adrIOs_data_bits <= inActSRAMs_0_dataPath_inIOs_adrIOs_data_bits @[GLBCluster.scala 9:31]
    InActSRAMBank.io_dataPath_inIOs_dataIOs_data_valid <= inActSRAMs_0_dataPath_inIOs_dataIOs_data_valid @[GLBCluster.scala 9:31]
    InActSRAMBank.io_dataPath_inIOs_dataIOs_data_bits <= inActSRAMs_0_dataPath_inIOs_dataIOs_data_bits @[GLBCluster.scala 9:31]
    InActSRAMBank.io_dataPath_outIOs_adrIOs_data_ready <= inActSRAMs_0_dataPath_outIOs_adrIOs_data_ready @[GLBCluster.scala 9:31]
    InActSRAMBank.io_dataPath_outIOs_dataIOs_data_ready <= inActSRAMs_0_dataPath_outIOs_dataIOs_data_ready @[GLBCluster.scala 9:31]
    InActSRAMBank.io_ctrlPath_writeOrRead <= inActSRAMs_0_ctrlPath_writeOrRead @[GLBCluster.scala 9:31]
    InActSRAMBank.io_ctrlPath_doEn <= inActSRAMs_0_ctrlPath_doEn @[GLBCluster.scala 9:31]
    InActSRAMBank_1.clock <= clock
    InActSRAMBank_1.reset <= reset
    InActSRAMBank_1.io_dataPath_inIOs_adrIOs_data_valid <= inActSRAMs_1_dataPath_inIOs_adrIOs_data_valid @[GLBCluster.scala 9:31]
    InActSRAMBank_1.io_dataPath_inIOs_adrIOs_data_bits <= inActSRAMs_1_dataPath_inIOs_adrIOs_data_bits @[GLBCluster.scala 9:31]
    InActSRAMBank_1.io_dataPath_inIOs_dataIOs_data_valid <= inActSRAMs_1_dataPath_inIOs_dataIOs_data_valid @[GLBCluster.scala 9:31]
    InActSRAMBank_1.io_dataPath_inIOs_dataIOs_data_bits <= inActSRAMs_1_dataPath_inIOs_dataIOs_data_bits @[GLBCluster.scala 9:31]
    InActSRAMBank_1.io_dataPath_outIOs_adrIOs_data_ready <= inActSRAMs_1_dataPath_outIOs_adrIOs_data_ready @[GLBCluster.scala 9:31]
    InActSRAMBank_1.io_dataPath_outIOs_dataIOs_data_ready <= inActSRAMs_1_dataPath_outIOs_dataIOs_data_ready @[GLBCluster.scala 9:31]
    InActSRAMBank_1.io_ctrlPath_writeOrRead <= inActSRAMs_1_ctrlPath_writeOrRead @[GLBCluster.scala 9:31]
    InActSRAMBank_1.io_ctrlPath_doEn <= inActSRAMs_1_ctrlPath_doEn @[GLBCluster.scala 9:31]
    InActSRAMBank_2.clock <= clock
    InActSRAMBank_2.reset <= reset
    InActSRAMBank_2.io_dataPath_inIOs_adrIOs_data_valid <= inActSRAMs_2_dataPath_inIOs_adrIOs_data_valid @[GLBCluster.scala 9:31]
    InActSRAMBank_2.io_dataPath_inIOs_adrIOs_data_bits <= inActSRAMs_2_dataPath_inIOs_adrIOs_data_bits @[GLBCluster.scala 9:31]
    InActSRAMBank_2.io_dataPath_inIOs_dataIOs_data_valid <= inActSRAMs_2_dataPath_inIOs_dataIOs_data_valid @[GLBCluster.scala 9:31]
    InActSRAMBank_2.io_dataPath_inIOs_dataIOs_data_bits <= inActSRAMs_2_dataPath_inIOs_dataIOs_data_bits @[GLBCluster.scala 9:31]
    InActSRAMBank_2.io_dataPath_outIOs_adrIOs_data_ready <= inActSRAMs_2_dataPath_outIOs_adrIOs_data_ready @[GLBCluster.scala 9:31]
    InActSRAMBank_2.io_dataPath_outIOs_dataIOs_data_ready <= inActSRAMs_2_dataPath_outIOs_dataIOs_data_ready @[GLBCluster.scala 9:31]
    InActSRAMBank_2.io_ctrlPath_writeOrRead <= inActSRAMs_2_ctrlPath_writeOrRead @[GLBCluster.scala 9:31]
    InActSRAMBank_2.io_ctrlPath_doEn <= inActSRAMs_2_ctrlPath_doEn @[GLBCluster.scala 9:31]
    PSumSRAMBank.clock <= clock
    PSumSRAMBank.reset <= reset
    PSumSRAMBank.io_dataPath_inIOs_valid <= pSumSRAMs_0_dataPath_inIOs_valid @[GLBCluster.scala 11:31]
    PSumSRAMBank.io_dataPath_inIOs_bits <= pSumSRAMs_0_dataPath_inIOs_bits @[GLBCluster.scala 11:31]
    PSumSRAMBank.io_dataPath_outIOs_ready <= pSumSRAMs_0_dataPath_outIOs_ready @[GLBCluster.scala 11:31]
    PSumSRAMBank.io_ctrlPath_writeOrRead <= pSumSRAMs_0_ctrlPath_writeOrRead @[GLBCluster.scala 11:31]
    PSumSRAMBank.io_ctrlPath_doEn <= pSumSRAMs_0_ctrlPath_doEn @[GLBCluster.scala 11:31]
    PSumSRAMBank.io_ctrlPath_startIdx <= pSumSRAMs_0_ctrlPath_startIdx @[GLBCluster.scala 11:31]
    PSumSRAMBank_1.clock <= clock
    PSumSRAMBank_1.reset <= reset
    PSumSRAMBank_1.io_dataPath_inIOs_valid <= pSumSRAMs_1_dataPath_inIOs_valid @[GLBCluster.scala 11:31]
    PSumSRAMBank_1.io_dataPath_inIOs_bits <= pSumSRAMs_1_dataPath_inIOs_bits @[GLBCluster.scala 11:31]
    PSumSRAMBank_1.io_dataPath_outIOs_ready <= pSumSRAMs_1_dataPath_outIOs_ready @[GLBCluster.scala 11:31]
    PSumSRAMBank_1.io_ctrlPath_writeOrRead <= pSumSRAMs_1_ctrlPath_writeOrRead @[GLBCluster.scala 11:31]
    PSumSRAMBank_1.io_ctrlPath_doEn <= pSumSRAMs_1_ctrlPath_doEn @[GLBCluster.scala 11:31]
    PSumSRAMBank_1.io_ctrlPath_startIdx <= pSumSRAMs_1_ctrlPath_startIdx @[GLBCluster.scala 11:31]
    PSumSRAMBank_2.clock <= clock
    PSumSRAMBank_2.reset <= reset
    PSumSRAMBank_2.io_dataPath_inIOs_valid <= pSumSRAMs_2_dataPath_inIOs_valid @[GLBCluster.scala 11:31]
    PSumSRAMBank_2.io_dataPath_inIOs_bits <= pSumSRAMs_2_dataPath_inIOs_bits @[GLBCluster.scala 11:31]
    PSumSRAMBank_2.io_dataPath_outIOs_ready <= pSumSRAMs_2_dataPath_outIOs_ready @[GLBCluster.scala 11:31]
    PSumSRAMBank_2.io_ctrlPath_writeOrRead <= pSumSRAMs_2_ctrlPath_writeOrRead @[GLBCluster.scala 11:31]
    PSumSRAMBank_2.io_ctrlPath_doEn <= pSumSRAMs_2_ctrlPath_doEn @[GLBCluster.scala 11:31]
    PSumSRAMBank_2.io_ctrlPath_startIdx <= pSumSRAMs_2_ctrlPath_startIdx @[GLBCluster.scala 11:31]
    PSumSRAMBank_3.clock <= clock
    PSumSRAMBank_3.reset <= reset
    PSumSRAMBank_3.io_dataPath_inIOs_valid <= pSumSRAMs_3_dataPath_inIOs_valid @[GLBCluster.scala 11:31]
    PSumSRAMBank_3.io_dataPath_inIOs_bits <= pSumSRAMs_3_dataPath_inIOs_bits @[GLBCluster.scala 11:31]
    PSumSRAMBank_3.io_dataPath_outIOs_ready <= pSumSRAMs_3_dataPath_outIOs_ready @[GLBCluster.scala 11:31]
    PSumSRAMBank_3.io_ctrlPath_writeOrRead <= pSumSRAMs_3_ctrlPath_writeOrRead @[GLBCluster.scala 11:31]
    PSumSRAMBank_3.io_ctrlPath_doEn <= pSumSRAMs_3_ctrlPath_doEn @[GLBCluster.scala 11:31]
    PSumSRAMBank_3.io_ctrlPath_startIdx <= pSumSRAMs_3_ctrlPath_startIdx @[GLBCluster.scala 11:31]
    theSRAMsState_0 <= mux(reset, _T__0, _GEN_3) @[GLBCluster.scala 33:20 GLBCluster.scala 38:20]
    theSRAMsState_1 <= mux(reset, _T__1, _GEN_7) @[GLBCluster.scala 33:20 GLBCluster.scala 38:20]
    inActSRAMDoneRegVec_0 <= mux(reset, _T_1_0, _T_16) @[GLBCluster.scala 57:32]
    inActSRAMDoneRegVec_1 <= mux(reset, _T_1_1, _T_22) @[GLBCluster.scala 57:32]
    inActSRAMDoneRegVec_2 <= mux(reset, _T_1_2, _T_28) @[GLBCluster.scala 57:32]
    pSumSRAMDoneRegVec_0 <= mux(reset, _T_2_0, _T_40) @[GLBCluster.scala 57:32]
    pSumSRAMDoneRegVec_1 <= mux(reset, _T_2_1, _T_46) @[GLBCluster.scala 57:32]
    pSumSRAMDoneRegVec_2 <= mux(reset, _T_2_2, _T_52) @[GLBCluster.scala 57:32]
    pSumSRAMDoneRegVec_3 <= mux(reset, _T_2_3, _T_58) @[GLBCluster.scala 57:32]
    _T_3 <= mux(reset, UInt<1>("h0"), _T_3)
    _T_4 <= mux(reset, UInt<1>("h0"), _T_4)
    _T_5 <= mux(reset, UInt<1>("h0"), _T_5)
    _T_6 <= mux(reset, UInt<1>("h0"), _T_6)
