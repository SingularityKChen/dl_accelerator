circuit InActSRAMBank :
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
  
    mem oneInActSRAM : @[GLBCluster.scala 131:57]
      data-type => UInt<4>
      depth => 486
      read-latency => 1
      write-latency => 1
      reader => _T_36
      writer => _T_19
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 143:44]
    reg nextValidReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nextValidReg) @[GLBCluster.scala 146:45]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 147:42]
    node _T_30 = and(io_dataPath_outIOs_data_ready, nextValidReg) @[GLBCluster.scala 157:35]
    node doReadWire = _T_30 @[GLBCluster.scala 137:40 GLBCluster.scala 157:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 148:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 133:41 GLBCluster.scala 196:15]
    node _T_11 = eq(writeOrRead, UInt<1>("h0")) @[GLBCluster.scala 198:16]
    node _T_12 = eq(doDoneReg, UInt<1>("h0")) @[GLBCluster.scala 198:32]
    node _T_13 = or(_T_11, _T_12) @[GLBCluster.scala 198:29]
    node _T_14 = and(_T_13, io_ctrlPath_doEn) @[GLBCluster.scala 198:44]
    node doEnWire = _T_14 @[GLBCluster.scala 136:38 GLBCluster.scala 198:12]
    node _T_18 = and(doEnWire, io_dataPath_inIOs_data_valid) @[GLBCluster.scala 164:29]
    node doWriteWire = _T_18 @[GLBCluster.scala 138:41 GLBCluster.scala 164:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 148:22]
    reg _T_6 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_6) @[GLBCluster.scala 186:39]
    node writeInData = io_dataPath_inIOs_data_bits @[GLBCluster.scala 139:41 GLBCluster.scala 163:17]
    node readOutData = oneInActSRAM._T_36.data @[GLBCluster.scala 140:41 GLBCluster.scala 158:17]
    node _T_15 = mux(writeOrRead, writeInData, readOutData) @[GLBCluster.scala 199:21]
    node _T_16 = add(_T_6, UInt<1>("h1")) @[GLBCluster.scala 202:26]
    node _T_17 = tail(_T_16, 1) @[GLBCluster.scala 202:26]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 144:42 GLBCluster.scala 148:16]
    node _GEN_0 = mux(doIdxIncWire, _T_17, _T_6) @[GLBCluster.scala 201:23]
    reg _T_38 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_38) @[GLBCluster.scala 213:34]
    node _T_47 = eq(_T_38, UInt<2>("h2")) @[GLBCluster.scala 236:33]
    node _T_9 = _T_47 @[GLBCluster.scala 193:38 GLBCluster.scala 236:20]
    node _T_10 = _T_9 @[GLBCluster.scala 194:29 GLBCluster.scala 241:11]
    node _GEN_1 = mux(_T_10, UInt<1>("h0"), _GEN_0) @[GLBCluster.scala 204:18]
    node _GEN_2 = validif(doIdxIncWire, _T_6) @[GLBCluster.scala 166:24]
    node _GEN_3 = validif(doIdxIncWire, clock) @[GLBCluster.scala 166:24]
    node _GEN_4 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 166:24]
    node _GEN_5 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 166:24]
    node _GEN_6 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 166:24]
    node _T_20 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 151:56]
    node _T_21 = and(doEnWire, _T_20) @[GLBCluster.scala 151:53]
    node _T_22 = mux(writeOrRead, UInt<1>("h0"), _T_21) @[GLBCluster.scala 151:21]
    node nextValid = _T_22 @[GLBCluster.scala 145:39 GLBCluster.scala 151:15]
    node _T_23 = and(nextValid, io_dataPath_outIOs_data_ready) @[GLBCluster.scala 152:31]
    node _T_4 = _T_9 @[GLBCluster.scala 184:35 GLBCluster.scala 238:17]
    node _T_48 = eq(_T_38, UInt<2>("h1")) @[GLBCluster.scala 237:29]
    node _T_5 = _T_48 @[GLBCluster.scala 185:34 GLBCluster.scala 237:16]
    node _T_49 = mux(writeOrRead, _T_4, _T_5) @[GLBCluster.scala 239:20]
    node _T_8 = _T_49 @[GLBCluster.scala 191:32 GLBCluster.scala 239:14]
    node _T_24 = eq(_T_8, UInt<1>("h0")) @[GLBCluster.scala 155:24]
    node _T_25 = eq(doEnWire, UInt<1>("h0")) @[GLBCluster.scala 155:56]
    node _T_26 = or(writeOrRead, _T_25) @[GLBCluster.scala 155:53]
    node _T_27 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 155:80]
    node _T_28 = mux(_T_26, waitForRead, _T_27) @[GLBCluster.scala 155:40]
    node _T_29 = mux(_T_24, _T_28, UInt<1>("h0")) @[GLBCluster.scala 155:23]
    node _T_31 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 158:54]
    node _T_32 = and(doReadWire, _T_31) @[GLBCluster.scala 158:51]
    node _GEN_7 = validif(_T_32, _T_6) @[GLBCluster.scala 158:32]
    node _T_33 = _GEN_7 @[GLBCluster.scala 158:32 GLBCluster.scala 158:32]
    node _T_34 = or(_T_33, UInt<9>("h0")) @[GLBCluster.scala 158:32]
    node _T_35 = bits(_T_34, 8, 0) @[GLBCluster.scala 158:32]
    node _GEN_8 = mux(_T_32, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 158:32]
    node _GEN_9 = validif(_T_32, _T_35) @[GLBCluster.scala 158:32]
    node _GEN_10 = validif(_T_32, clock) @[GLBCluster.scala 158:32]
    node _T_37 = mux(waitForRead, readOutData, UInt<1>("h0")) @[GLBCluster.scala 159:26]
    node _T_7 = _T_15 @[GLBCluster.scala 190:33 GLBCluster.scala 199:15]
    node _T_40 = eq(_T_7, UInt<1>("h0")) @[GLBCluster.scala 215:31]
    node _T_39 = _T_40 @[GLBCluster.scala 214:34 GLBCluster.scala 215:16]
    node _T_41 = and(_T_39, waitForRead) @[GLBCluster.scala 216:65]
    node _T_42 = mux(writeOrRead, _T_39, _T_41) @[GLBCluster.scala 216:24]
    node _T_2 = _T_42 @[GLBCluster.scala 182:36 GLBCluster.scala 216:18]
    node _T_43 = and(_T_2, doEnWire) @[GLBCluster.scala 217:41]
    node _T_44 = eq(UInt<2>("h0"), _T_38) @[Conditional.scala 37:30]
    node _T_3 = _T_43 @[GLBCluster.scala 183:41 GLBCluster.scala 217:23]
    node _GEN_11 = mux(_T_3, UInt<2>("h1"), _T_38) @[GLBCluster.scala 221:34]
    node _T_45 = eq(UInt<2>("h1"), _T_38) @[Conditional.scala 37:30]
    node _GEN_12 = mux(_T_3, UInt<2>("h2"), UInt<2>("h0")) @[GLBCluster.scala 226:34]
    node _T_46 = eq(UInt<2>("h2"), _T_38) @[Conditional.scala 37:30]
    node _GEN_13 = mux(_T_46, UInt<2>("h0"), _T_38) @[Conditional.scala 39:67]
    node _GEN_14 = mux(_T_45, _GEN_12, _GEN_13) @[Conditional.scala 39:67]
    node _GEN_15 = mux(_T_44, UInt<1>("h0"), doDoneReg) @[Conditional.scala 40:58]
    node _GEN_16 = mux(_T_44, _GEN_11, _GEN_14) @[Conditional.scala 40:58]
    io_dataPath_inIOs_data_ready <= doWriteWire @[GLBCluster.scala 165:21]
    io_dataPath_outIOs_data_valid <= nextValidReg @[GLBCluster.scala 156:21]
    io_dataPath_outIOs_data_bits <= _T_37 @[GLBCluster.scala 159:20]
    io_ctrlPath_done <= _T_8 @[GLBCluster.scala 197:20]
    io_debugIO_idx <= _T_6 @[GLBCluster.scala 172:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 173:20]
    io_debugIO_idxCount is invalid
    io_debugIO_currentData <= _T_7 @[GLBCluster.scala 245:28]
    io_debugIO_meetOneZero <= _T_5 @[GLBCluster.scala 246:28]
    io_debugIO_theState <= _T_38 @[GLBCluster.scala 247:25]
    io_debugIO_waitForRead <= waitForRead @[GLBCluster.scala 174:25]
    io_debugIO_doReadWire <= doReadWire @[GLBCluster.scala 175:24]
    oneInActSRAM._T_36.addr <= _GEN_9 @[GLBCluster.scala 158:32]
    oneInActSRAM._T_36.en <= _GEN_8 @[GLBCluster.scala 131:57 GLBCluster.scala 158:32]
    oneInActSRAM._T_36.clk <= _GEN_10 @[GLBCluster.scala 158:32]
    oneInActSRAM._T_19.addr <= _GEN_2
    oneInActSRAM._T_19.en <= _GEN_4 @[GLBCluster.scala 131:57]
    oneInActSRAM._T_19.clk <= _GEN_3
    oneInActSRAM._T_19.data <= _GEN_6
    oneInActSRAM._T_19.mask <= _GEN_5
    waitForRead <= mux(reset, UInt<1>("h0"), _T_29) @[GLBCluster.scala 155:17]
    nextValidReg <= mux(reset, UInt<1>("h0"), _T_23) @[GLBCluster.scala 152:18]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_8) @[GLBCluster.scala 220:17 GLBCluster.scala 240:13]
    _T_6 <= mux(reset, UInt<9>("h0"), _GEN_1) @[GLBCluster.scala 202:14 GLBCluster.scala 205:14]
    _T_38 <= mux(reset, UInt<2>("h0"), _GEN_16) @[GLBCluster.scala 222:19 GLBCluster.scala 227:19 GLBCluster.scala 229:19 GLBCluster.scala 233:17]

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
  
    mem oneInActSRAM : @[GLBCluster.scala 131:57]
      data-type => UInt<12>
      depth => 862
      read-latency => 1
      write-latency => 1
      reader => _T_36
      writer => _T_19
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 143:44]
    reg nextValidReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nextValidReg) @[GLBCluster.scala 146:45]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 147:42]
    node _T_30 = and(io_dataPath_outIOs_data_ready, nextValidReg) @[GLBCluster.scala 157:35]
    node doReadWire = _T_30 @[GLBCluster.scala 137:40 GLBCluster.scala 157:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 148:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 133:41 GLBCluster.scala 196:15]
    node _T_11 = eq(writeOrRead, UInt<1>("h0")) @[GLBCluster.scala 198:16]
    node _T_12 = eq(doDoneReg, UInt<1>("h0")) @[GLBCluster.scala 198:32]
    node _T_13 = or(_T_11, _T_12) @[GLBCluster.scala 198:29]
    node _T_14 = and(_T_13, io_ctrlPath_doEn) @[GLBCluster.scala 198:44]
    node doEnWire = _T_14 @[GLBCluster.scala 136:38 GLBCluster.scala 198:12]
    node _T_18 = and(doEnWire, io_dataPath_inIOs_data_valid) @[GLBCluster.scala 164:29]
    node doWriteWire = _T_18 @[GLBCluster.scala 138:41 GLBCluster.scala 164:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 148:22]
    reg _T_6 : UInt<10>, clock with :
      reset => (UInt<1>("h0"), _T_6) @[GLBCluster.scala 186:39]
    node writeInData = io_dataPath_inIOs_data_bits @[GLBCluster.scala 139:41 GLBCluster.scala 163:17]
    node readOutData = oneInActSRAM._T_36.data @[GLBCluster.scala 140:41 GLBCluster.scala 158:17]
    node _T_15 = mux(writeOrRead, writeInData, readOutData) @[GLBCluster.scala 199:21]
    node _T_16 = add(_T_6, UInt<1>("h1")) @[GLBCluster.scala 202:26]
    node _T_17 = tail(_T_16, 1) @[GLBCluster.scala 202:26]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 144:42 GLBCluster.scala 148:16]
    node _GEN_0 = mux(doIdxIncWire, _T_17, _T_6) @[GLBCluster.scala 201:23]
    reg _T_38 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_38) @[GLBCluster.scala 213:34]
    node _T_47 = eq(_T_38, UInt<2>("h2")) @[GLBCluster.scala 236:33]
    node _T_9 = _T_47 @[GLBCluster.scala 193:38 GLBCluster.scala 236:20]
    node _T_10 = _T_9 @[GLBCluster.scala 194:29 GLBCluster.scala 241:11]
    node _GEN_1 = mux(_T_10, UInt<1>("h0"), _GEN_0) @[GLBCluster.scala 204:18]
    node _GEN_2 = validif(doIdxIncWire, _T_6) @[GLBCluster.scala 166:24]
    node _GEN_3 = validif(doIdxIncWire, clock) @[GLBCluster.scala 166:24]
    node _GEN_4 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 166:24]
    node _GEN_5 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 166:24]
    node _GEN_6 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 166:24]
    node _T_20 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 151:56]
    node _T_21 = and(doEnWire, _T_20) @[GLBCluster.scala 151:53]
    node _T_22 = mux(writeOrRead, UInt<1>("h0"), _T_21) @[GLBCluster.scala 151:21]
    node nextValid = _T_22 @[GLBCluster.scala 145:39 GLBCluster.scala 151:15]
    node _T_23 = and(nextValid, io_dataPath_outIOs_data_ready) @[GLBCluster.scala 152:31]
    node _T_4 = _T_9 @[GLBCluster.scala 184:35 GLBCluster.scala 238:17]
    node _T_48 = eq(_T_38, UInt<2>("h1")) @[GLBCluster.scala 237:29]
    node _T_5 = _T_48 @[GLBCluster.scala 185:34 GLBCluster.scala 237:16]
    node _T_49 = mux(writeOrRead, _T_4, _T_5) @[GLBCluster.scala 239:20]
    node _T_8 = _T_49 @[GLBCluster.scala 191:32 GLBCluster.scala 239:14]
    node _T_24 = eq(_T_8, UInt<1>("h0")) @[GLBCluster.scala 155:24]
    node _T_25 = eq(doEnWire, UInt<1>("h0")) @[GLBCluster.scala 155:56]
    node _T_26 = or(writeOrRead, _T_25) @[GLBCluster.scala 155:53]
    node _T_27 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 155:80]
    node _T_28 = mux(_T_26, waitForRead, _T_27) @[GLBCluster.scala 155:40]
    node _T_29 = mux(_T_24, _T_28, UInt<1>("h0")) @[GLBCluster.scala 155:23]
    node _T_31 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 158:54]
    node _T_32 = and(doReadWire, _T_31) @[GLBCluster.scala 158:51]
    node _GEN_7 = validif(_T_32, _T_6) @[GLBCluster.scala 158:32]
    node _T_33 = _GEN_7 @[GLBCluster.scala 158:32 GLBCluster.scala 158:32]
    node _T_34 = or(_T_33, UInt<10>("h0")) @[GLBCluster.scala 158:32]
    node _T_35 = bits(_T_34, 9, 0) @[GLBCluster.scala 158:32]
    node _GEN_8 = mux(_T_32, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 158:32]
    node _GEN_9 = validif(_T_32, _T_35) @[GLBCluster.scala 158:32]
    node _GEN_10 = validif(_T_32, clock) @[GLBCluster.scala 158:32]
    node _T_37 = mux(waitForRead, readOutData, UInt<1>("h0")) @[GLBCluster.scala 159:26]
    node _T_7 = _T_15 @[GLBCluster.scala 190:33 GLBCluster.scala 199:15]
    node _T_40 = eq(_T_7, UInt<1>("h0")) @[GLBCluster.scala 215:31]
    node _T_39 = _T_40 @[GLBCluster.scala 214:34 GLBCluster.scala 215:16]
    node _T_41 = and(_T_39, waitForRead) @[GLBCluster.scala 216:65]
    node _T_42 = mux(writeOrRead, _T_39, _T_41) @[GLBCluster.scala 216:24]
    node _T_2 = _T_42 @[GLBCluster.scala 182:36 GLBCluster.scala 216:18]
    node _T_43 = and(_T_2, doEnWire) @[GLBCluster.scala 217:41]
    node _T_44 = eq(UInt<2>("h0"), _T_38) @[Conditional.scala 37:30]
    node _T_3 = _T_43 @[GLBCluster.scala 183:41 GLBCluster.scala 217:23]
    node _GEN_11 = mux(_T_3, UInt<2>("h1"), _T_38) @[GLBCluster.scala 221:34]
    node _T_45 = eq(UInt<2>("h1"), _T_38) @[Conditional.scala 37:30]
    node _GEN_12 = mux(_T_3, UInt<2>("h2"), UInt<2>("h0")) @[GLBCluster.scala 226:34]
    node _T_46 = eq(UInt<2>("h2"), _T_38) @[Conditional.scala 37:30]
    node _GEN_13 = mux(_T_46, UInt<2>("h0"), _T_38) @[Conditional.scala 39:67]
    node _GEN_14 = mux(_T_45, _GEN_12, _GEN_13) @[Conditional.scala 39:67]
    node _GEN_15 = mux(_T_44, UInt<1>("h0"), doDoneReg) @[Conditional.scala 40:58]
    node _GEN_16 = mux(_T_44, _GEN_11, _GEN_14) @[Conditional.scala 40:58]
    io_dataPath_inIOs_data_ready <= doWriteWire @[GLBCluster.scala 165:21]
    io_dataPath_outIOs_data_valid <= nextValidReg @[GLBCluster.scala 156:21]
    io_dataPath_outIOs_data_bits <= _T_37 @[GLBCluster.scala 159:20]
    io_ctrlPath_done <= _T_8 @[GLBCluster.scala 197:20]
    io_debugIO_idx <= _T_6 @[GLBCluster.scala 172:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 173:20]
    io_debugIO_idxCount is invalid
    io_debugIO_currentData <= _T_7 @[GLBCluster.scala 245:28]
    io_debugIO_meetOneZero <= _T_5 @[GLBCluster.scala 246:28]
    io_debugIO_theState <= _T_38 @[GLBCluster.scala 247:25]
    io_debugIO_waitForRead <= waitForRead @[GLBCluster.scala 174:25]
    io_debugIO_doReadWire <= doReadWire @[GLBCluster.scala 175:24]
    oneInActSRAM._T_36.addr <= _GEN_9 @[GLBCluster.scala 158:32]
    oneInActSRAM._T_36.en <= _GEN_8 @[GLBCluster.scala 131:57 GLBCluster.scala 158:32]
    oneInActSRAM._T_36.clk <= _GEN_10 @[GLBCluster.scala 158:32]
    oneInActSRAM._T_19.addr <= _GEN_2
    oneInActSRAM._T_19.en <= _GEN_4 @[GLBCluster.scala 131:57]
    oneInActSRAM._T_19.clk <= _GEN_3
    oneInActSRAM._T_19.data <= _GEN_6
    oneInActSRAM._T_19.mask <= _GEN_5
    waitForRead <= mux(reset, UInt<1>("h0"), _T_29) @[GLBCluster.scala 155:17]
    nextValidReg <= mux(reset, UInt<1>("h0"), _T_23) @[GLBCluster.scala 152:18]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_8) @[GLBCluster.scala 220:17 GLBCluster.scala 240:13]
    _T_6 <= mux(reset, UInt<10>("h0"), _GEN_1) @[GLBCluster.scala 202:14 GLBCluster.scala 205:14]
    _T_38 <= mux(reset, UInt<2>("h0"), _GEN_16) @[GLBCluster.scala 222:19 GLBCluster.scala 227:19 GLBCluster.scala 229:19 GLBCluster.scala 233:17]

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
  
    inst adrSRAM of InActSRAMCommon @[GLBCluster.scala 256:31]
    inst dataSRAM of InActSRAMCommon_1 @[GLBCluster.scala 258:32]
    reg _T_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_1) @[GLBCluster.scala 262:35]
    node _T_5 = eq(_T_1, UInt<2>("h1")) @[GLBCluster.scala 266:34]
    node _T_6 = eq(_T_1, UInt<2>("h2")) @[GLBCluster.scala 269:63]
    node _T_2 = _T_5 @[GLBCluster.scala 263:38 GLBCluster.scala 266:20]
    node _T_7 = or(_T_2, _T_6) @[GLBCluster.scala 269:48]
    node _T_8 = eq(_T_1, UInt<2>("h3")) @[GLBCluster.scala 270:64]
    node _T_9 = or(_T_2, _T_8) @[GLBCluster.scala 270:49]
    node _T_10 = eq(_T_1, UInt<2>("h3")) @[GLBCluster.scala 271:32]
    node _T_4 = dataSRAM.io_ctrlPath_done @[GLBCluster.scala 265:34 GLBCluster.scala 268:16]
    node _T_11 = and(_T_10, _T_4) @[GLBCluster.scala 271:51]
    node _T_12 = eq(_T_1, UInt<2>("h2")) @[GLBCluster.scala 271:84]
    node _T_3 = adrSRAM.io_ctrlPath_done @[GLBCluster.scala 264:33 GLBCluster.scala 267:15]
    node _T_13 = and(_T_12, _T_3) @[GLBCluster.scala 271:102]
    node _T_14 = or(_T_11, _T_13) @[GLBCluster.scala 271:68]
    node _T_15 = eq(UInt<2>("h0"), _T_1) @[Conditional.scala 37:30]
    node _GEN_0 = mux(io_ctrlPath_doEn, UInt<2>("h1"), _T_1) @[GLBCluster.scala 274:31]
    node _T_16 = eq(UInt<2>("h1"), _T_1) @[Conditional.scala 37:30]
    node _GEN_1 = mux(_T_4, UInt<2>("h2"), UInt<2>("h1")) @[GLBCluster.scala 281:34]
    node _GEN_2 = mux(_T_3, UInt<2>("h3"), _GEN_1) @[GLBCluster.scala 279:26]
    node _T_17 = eq(UInt<2>("h3"), _T_1) @[Conditional.scala 37:30]
    node _GEN_3 = mux(_T_4, UInt<2>("h0"), _T_1) @[GLBCluster.scala 288:27]
    node _T_18 = eq(UInt<2>("h2"), _T_1) @[Conditional.scala 37:30]
    node _GEN_4 = mux(_T_3, UInt<2>("h0"), _T_1) @[GLBCluster.scala 293:26]
    node _GEN_5 = mux(_T_18, _GEN_4, _T_1) @[Conditional.scala 39:67]
    node _GEN_6 = mux(_T_17, _GEN_3, _GEN_5) @[Conditional.scala 39:67]
    node _GEN_7 = mux(_T_16, _GEN_2, _GEN_6) @[Conditional.scala 39:67]
    node _GEN_8 = mux(_T_15, _GEN_0, _GEN_7) @[Conditional.scala 40:58]
    node _T = _T_14 @[GLBCluster.scala 260:34 GLBCluster.scala 271:16]
    io_dataPath_inIOs_adrIOs_data_ready <= adrSRAM.io_dataPath_inIOs_data_ready @[GLBCluster.scala 299:29]
    io_dataPath_inIOs_dataIOs_data_ready <= dataSRAM.io_dataPath_inIOs_data_ready @[GLBCluster.scala 301:30]
    io_dataPath_outIOs_adrIOs_data_valid <= adrSRAM.io_dataPath_outIOs_data_valid @[GLBCluster.scala 300:30]
    io_dataPath_outIOs_adrIOs_data_bits <= adrSRAM.io_dataPath_outIOs_data_bits @[GLBCluster.scala 300:30]
    io_dataPath_outIOs_dataIOs_data_valid <= dataSRAM.io_dataPath_outIOs_data_valid @[GLBCluster.scala 302:31]
    io_dataPath_outIOs_dataIOs_data_bits <= dataSRAM.io_dataPath_outIOs_data_bits @[GLBCluster.scala 302:31]
    io_ctrlPath_done <= _T @[GLBCluster.scala 304:20]
    io_debugIO_theState <= _T_1 @[GLBCluster.scala 309:25]
    io_debugIO_adrDebug_commonDebug_idx <= adrSRAM.io_debugIO_idx @[GLBCluster.scala 312:25]
    io_debugIO_adrDebug_commonDebug_idxInc <= adrSRAM.io_debugIO_idxInc @[GLBCluster.scala 312:25]
    io_debugIO_adrDebug_commonDebug_idxCount <= adrSRAM.io_debugIO_idxCount @[GLBCluster.scala 312:25]
    io_debugIO_adrDebug_commonDebug_currentData <= adrSRAM.io_debugIO_currentData @[GLBCluster.scala 312:25]
    io_debugIO_adrDebug_commonDebug_meetOneZero <= adrSRAM.io_debugIO_meetOneZero @[GLBCluster.scala 312:25]
    io_debugIO_adrDebug_commonDebug_theState <= adrSRAM.io_debugIO_theState @[GLBCluster.scala 312:25]
    io_debugIO_adrDebug_commonDebug_waitForRead <= adrSRAM.io_debugIO_waitForRead @[GLBCluster.scala 312:25]
    io_debugIO_adrDebug_commonDebug_doReadWire <= adrSRAM.io_debugIO_doReadWire @[GLBCluster.scala 312:25]
    io_debugIO_adrDebug_subDone <= adrSRAM.io_ctrlPath_done @[GLBCluster.scala 311:21]
    io_debugIO_dataDebug_commonDebug_idx <= dataSRAM.io_debugIO_idx @[GLBCluster.scala 312:25]
    io_debugIO_dataDebug_commonDebug_idxInc <= dataSRAM.io_debugIO_idxInc @[GLBCluster.scala 312:25]
    io_debugIO_dataDebug_commonDebug_idxCount <= dataSRAM.io_debugIO_idxCount @[GLBCluster.scala 312:25]
    io_debugIO_dataDebug_commonDebug_currentData <= dataSRAM.io_debugIO_currentData @[GLBCluster.scala 312:25]
    io_debugIO_dataDebug_commonDebug_meetOneZero <= dataSRAM.io_debugIO_meetOneZero @[GLBCluster.scala 312:25]
    io_debugIO_dataDebug_commonDebug_theState <= dataSRAM.io_debugIO_theState @[GLBCluster.scala 312:25]
    io_debugIO_dataDebug_commonDebug_waitForRead <= dataSRAM.io_debugIO_waitForRead @[GLBCluster.scala 312:25]
    io_debugIO_dataDebug_commonDebug_doReadWire <= dataSRAM.io_debugIO_doReadWire @[GLBCluster.scala 312:25]
    io_debugIO_dataDebug_subDone <= dataSRAM.io_ctrlPath_done @[GLBCluster.scala 311:21]
    adrSRAM.clock <= clock
    adrSRAM.reset <= reset
    adrSRAM.io_dataPath_inIOs_data_valid <= io_dataPath_inIOs_adrIOs_data_valid @[GLBCluster.scala 299:29]
    adrSRAM.io_dataPath_inIOs_data_bits <= io_dataPath_inIOs_adrIOs_data_bits @[GLBCluster.scala 299:29]
    adrSRAM.io_dataPath_outIOs_data_ready <= io_dataPath_outIOs_adrIOs_data_ready @[GLBCluster.scala 300:30]
    adrSRAM.io_ctrlPath_writeOrRead <= io_ctrlPath_writeOrRead @[GLBCluster.scala 306:19]
    adrSRAM.io_ctrlPath_doEn <= _T_7 @[GLBCluster.scala 269:28]
    dataSRAM.clock <= clock
    dataSRAM.reset <= reset
    dataSRAM.io_dataPath_inIOs_data_valid <= io_dataPath_inIOs_dataIOs_data_valid @[GLBCluster.scala 301:30]
    dataSRAM.io_dataPath_inIOs_data_bits <= io_dataPath_inIOs_dataIOs_data_bits @[GLBCluster.scala 301:30]
    dataSRAM.io_dataPath_outIOs_data_ready <= io_dataPath_outIOs_dataIOs_data_ready @[GLBCluster.scala 302:31]
    dataSRAM.io_ctrlPath_writeOrRead <= io_ctrlPath_writeOrRead @[GLBCluster.scala 306:19]
    dataSRAM.io_ctrlPath_doEn <= _T_9 @[GLBCluster.scala 270:29]
    _T_1 <= mux(reset, UInt<2>("h0"), _GEN_8) @[GLBCluster.scala 275:20 GLBCluster.scala 280:20 GLBCluster.scala 282:20 GLBCluster.scala 284:20 GLBCluster.scala 289:20 GLBCluster.scala 294:20]
