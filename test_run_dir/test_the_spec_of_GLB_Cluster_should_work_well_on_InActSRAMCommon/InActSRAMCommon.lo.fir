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
    output io_debugIO_waitForRead : UInt<1>
    output io_debugIO_doReadWire : UInt<1>
  
    mem theSRAM : @[GLBCluster.scala 115:57]
      data-type => UInt<4>
      depth => 486
      read-latency => 1
      write-latency => 1
      reader => _T_36
      writer => _T_19
      read-under-write => undefined
    reg waitForRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), waitForRead) @[GLBCluster.scala 127:44]
    reg nextValidReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nextValidReg) @[GLBCluster.scala 130:45]
    reg doDoneReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), doDoneReg) @[GLBCluster.scala 131:42]
    node _T_30 = and(io_dataPath_outIOs_data_ready, nextValidReg) @[GLBCluster.scala 141:35]
    node doReadWire = _T_30 @[GLBCluster.scala 121:40 GLBCluster.scala 141:16]
    node _T = and(doReadWire, waitForRead) @[GLBCluster.scala 132:60]
    node writeOrRead = io_ctrlPath_writeOrRead @[GLBCluster.scala 117:41 GLBCluster.scala 179:15]
    node _T_11 = eq(writeOrRead, UInt<1>("h0")) @[GLBCluster.scala 181:16]
    node _T_12 = eq(doDoneReg, UInt<1>("h0")) @[GLBCluster.scala 181:32]
    node _T_13 = or(_T_11, _T_12) @[GLBCluster.scala 181:29]
    node _T_14 = and(_T_13, io_ctrlPath_doEn) @[GLBCluster.scala 181:44]
    node doEnWire = _T_14 @[GLBCluster.scala 120:38 GLBCluster.scala 181:12]
    node _T_18 = and(doEnWire, io_dataPath_inIOs_data_valid) @[GLBCluster.scala 148:29]
    node doWriteWire = _T_18 @[GLBCluster.scala 122:41 GLBCluster.scala 148:17]
    node _T_1 = mux(writeOrRead, doWriteWire, _T) @[GLBCluster.scala 132:22]
    reg _T_6 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_6) @[GLBCluster.scala 169:39]
    node writeInData = io_dataPath_inIOs_data_bits @[GLBCluster.scala 123:41 GLBCluster.scala 147:17]
    node readOutData = theSRAM._T_36.data @[GLBCluster.scala 124:41 GLBCluster.scala 142:17]
    node _T_15 = mux(writeOrRead, writeInData, readOutData) @[GLBCluster.scala 182:21]
    node _T_16 = add(_T_6, UInt<1>("h1")) @[GLBCluster.scala 185:26]
    node _T_17 = tail(_T_16, 1) @[GLBCluster.scala 185:26]
    node doIdxIncWire = _T_1 @[GLBCluster.scala 128:42 GLBCluster.scala 132:16]
    node _GEN_0 = mux(doIdxIncWire, _T_17, _T_6) @[GLBCluster.scala 184:23]
    reg _T_38 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_38) @[GLBCluster.scala 196:34]
    node _T_47 = eq(_T_38, UInt<2>("h2")) @[GLBCluster.scala 219:33]
    node _T_9 = _T_47 @[GLBCluster.scala 176:38 GLBCluster.scala 219:20]
    node _T_10 = _T_9 @[GLBCluster.scala 177:29 GLBCluster.scala 224:11]
    node _GEN_1 = mux(_T_10, UInt<1>("h0"), _GEN_0) @[GLBCluster.scala 187:18]
    node _GEN_2 = validif(doIdxIncWire, _T_6) @[GLBCluster.scala 150:24]
    node _GEN_3 = validif(doIdxIncWire, clock) @[GLBCluster.scala 150:24]
    node _GEN_4 = mux(doIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 150:24]
    node _GEN_5 = validif(doIdxIncWire, UInt<1>("h1")) @[GLBCluster.scala 150:24]
    node _GEN_6 = validif(doIdxIncWire, writeInData) @[GLBCluster.scala 150:24]
    node _T_20 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 135:56]
    node _T_21 = and(doEnWire, _T_20) @[GLBCluster.scala 135:53]
    node _T_22 = mux(writeOrRead, UInt<1>("h0"), _T_21) @[GLBCluster.scala 135:21]
    node nextValid = _T_22 @[GLBCluster.scala 129:39 GLBCluster.scala 135:15]
    node _T_23 = and(nextValid, io_dataPath_outIOs_data_ready) @[GLBCluster.scala 136:31]
    node _T_4 = _T_9 @[GLBCluster.scala 167:35 GLBCluster.scala 221:17]
    node _T_48 = eq(_T_38, UInt<2>("h1")) @[GLBCluster.scala 220:29]
    node _T_5 = _T_48 @[GLBCluster.scala 168:34 GLBCluster.scala 220:16]
    node _T_49 = mux(writeOrRead, _T_4, _T_5) @[GLBCluster.scala 222:20]
    node _T_8 = _T_49 @[GLBCluster.scala 174:32 GLBCluster.scala 222:14]
    node _T_24 = eq(_T_8, UInt<1>("h0")) @[GLBCluster.scala 139:24]
    node _T_25 = eq(doEnWire, UInt<1>("h0")) @[GLBCluster.scala 139:56]
    node _T_26 = or(writeOrRead, _T_25) @[GLBCluster.scala 139:53]
    node _T_27 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 139:80]
    node _T_28 = mux(_T_26, waitForRead, _T_27) @[GLBCluster.scala 139:40]
    node _T_29 = mux(_T_24, _T_28, UInt<1>("h0")) @[GLBCluster.scala 139:23]
    node _T_31 = eq(waitForRead, UInt<1>("h0")) @[GLBCluster.scala 142:54]
    node _T_32 = and(doReadWire, _T_31) @[GLBCluster.scala 142:51]
    node _GEN_7 = validif(_T_32, _T_6) @[GLBCluster.scala 142:32]
    node _T_33 = _GEN_7 @[GLBCluster.scala 142:32 GLBCluster.scala 142:32]
    node _T_34 = or(_T_33, UInt<9>("h0")) @[GLBCluster.scala 142:32]
    node _T_35 = bits(_T_34, 8, 0) @[GLBCluster.scala 142:32]
    node _GEN_8 = mux(_T_32, UInt<1>("h1"), UInt<1>("h0")) @[GLBCluster.scala 142:32]
    node _GEN_9 = validif(_T_32, _T_35) @[GLBCluster.scala 142:32]
    node _GEN_10 = validif(_T_32, clock) @[GLBCluster.scala 142:32]
    node _T_37 = mux(waitForRead, readOutData, UInt<1>("h0")) @[GLBCluster.scala 143:26]
    node _T_7 = _T_15 @[GLBCluster.scala 173:33 GLBCluster.scala 182:15]
    node _T_40 = eq(_T_7, UInt<1>("h0")) @[GLBCluster.scala 198:31]
    node _T_39 = _T_40 @[GLBCluster.scala 197:34 GLBCluster.scala 198:16]
    node _T_41 = and(_T_39, waitForRead) @[GLBCluster.scala 199:65]
    node _T_42 = mux(writeOrRead, _T_39, _T_41) @[GLBCluster.scala 199:24]
    node _T_2 = _T_42 @[GLBCluster.scala 165:36 GLBCluster.scala 199:18]
    node _T_43 = and(_T_2, doEnWire) @[GLBCluster.scala 200:41]
    node _T_44 = eq(UInt<2>("h0"), _T_38) @[Conditional.scala 37:30]
    node _T_3 = _T_43 @[GLBCluster.scala 166:41 GLBCluster.scala 200:23]
    node _GEN_11 = mux(_T_3, UInt<2>("h1"), _T_38) @[GLBCluster.scala 204:34]
    node _T_45 = eq(UInt<2>("h1"), _T_38) @[Conditional.scala 37:30]
    node _GEN_12 = mux(_T_3, UInt<2>("h2"), UInt<2>("h0")) @[GLBCluster.scala 209:34]
    node _T_46 = eq(UInt<2>("h2"), _T_38) @[Conditional.scala 37:30]
    node _GEN_13 = mux(_T_46, UInt<2>("h0"), _T_38) @[Conditional.scala 39:67]
    node _GEN_14 = mux(_T_45, _GEN_12, _GEN_13) @[Conditional.scala 39:67]
    node _GEN_15 = mux(_T_44, UInt<1>("h0"), doDoneReg) @[Conditional.scala 40:58]
    node _GEN_16 = mux(_T_44, _GEN_11, _GEN_14) @[Conditional.scala 40:58]
    io_dataPath_inIOs_data_ready <= doWriteWire @[GLBCluster.scala 149:21]
    io_dataPath_outIOs_data_valid <= nextValidReg @[GLBCluster.scala 140:21]
    io_dataPath_outIOs_data_bits <= _T_37 @[GLBCluster.scala 143:20]
    io_ctrlPath_done <= _T_8 @[GLBCluster.scala 180:20]
    io_debugIO_idx <= _T_6 @[GLBCluster.scala 156:17]
    io_debugIO_idxInc <= doIdxIncWire @[GLBCluster.scala 157:20]
    io_debugIO_idxCount is invalid
    io_debugIO_currentData <= _T_7 @[GLBCluster.scala 228:28]
    io_debugIO_meetOneZero <= _T_5 @[GLBCluster.scala 229:28]
    io_debugIO_theState <= _T_38 @[GLBCluster.scala 230:25]
    io_debugIO_waitForRead <= waitForRead @[GLBCluster.scala 158:25]
    io_debugIO_doReadWire <= doReadWire @[GLBCluster.scala 159:24]
    theSRAM._T_36.addr <= _GEN_9 @[GLBCluster.scala 142:32]
    theSRAM._T_36.en <= _GEN_8 @[GLBCluster.scala 115:57 GLBCluster.scala 142:32]
    theSRAM._T_36.clk <= _GEN_10 @[GLBCluster.scala 142:32]
    theSRAM._T_19.addr <= _GEN_2
    theSRAM._T_19.en <= _GEN_4 @[GLBCluster.scala 115:57]
    theSRAM._T_19.clk <= _GEN_3
    theSRAM._T_19.data <= _GEN_6
    theSRAM._T_19.mask <= _GEN_5
    waitForRead <= mux(reset, UInt<1>("h0"), _T_29) @[GLBCluster.scala 139:17]
    nextValidReg <= mux(reset, UInt<1>("h0"), _T_23) @[GLBCluster.scala 136:18]
    doDoneReg <= mux(reset, UInt<1>("h0"), _T_8) @[GLBCluster.scala 203:17 GLBCluster.scala 223:13]
    _T_6 <= mux(reset, UInt<9>("h0"), _GEN_1) @[GLBCluster.scala 185:14 GLBCluster.scala 188:14]
    _T_38 <= mux(reset, UInt<2>("h0"), _GEN_16) @[GLBCluster.scala 205:19 GLBCluster.scala 210:19 GLBCluster.scala 212:19 GLBCluster.scala 216:17]
