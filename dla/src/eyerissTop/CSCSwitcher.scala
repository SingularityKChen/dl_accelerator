package dla.eyerissTop

import chisel3._
import chisel3.util._
import dla.cluster.GNMFCS2Config
import dla.pe.{CSCStreamIO, MCRENFConfig, PESizeConfig}

class CSCSwitcherDebugIO extends Bundle with PESizeConfig {
  val firstNoneZero: Bool = Output(Bool())
  val zeroColReg: Bool = Output(Bool())
  val cscAdrReg: UInt = Output(UInt(5.W))
  val columnCounter: UInt = Output(UInt(5.W))
  val endFlag: Bool = Output(Bool())
  val oneVecFin: Bool = Output(Bool())
  val allVecFin: Bool = Output(Bool())
  val oneColFin: Bool = Output(Bool())
  val currentRow: UInt = Output(UInt(cscCountWidth.W))
  val currentStreamNum: UInt = Output(UInt(5.W))
}

class CSCSwitcherIO(private val adrWidth: Int) extends Bundle
  with PESizeConfig with GNMFCS2Config with MCRENFConfig {
  private val lgVectorNum = if (adrWidth == inActAdrWidth) log2Ceil(inActStreamNum) else log2Ceil(weightStreamNum)
  val inData: DecoupledIO[UInt] = Flipped(Decoupled(UInt(cscDataWidth.W)))
  val outData = new CSCStreamIO(adrWidth = adrWidth, dataWidth = cscDataWidth + cscCountWidth)
  /** use matrix height and width to increase and wrap csc address and count reg */
  val matrixHeight: UInt = Input(UInt(5.W)) // TODO: check the width
  val matrixWidth: UInt = Input(UInt(5.W))
  require(scala.math.max(inActMatrixHeight, weightMatrixHeight) <= scala.math.pow(2, 5))
  require(scala.math.max(inActMatrixWidth, weightMatrixWidth) <= scala.math.pow(2, 5))
  val vectorNum: UInt = Input(UInt(lgVectorNum.W))
  val debugIO = new CSCSwitcherDebugIO
}

class CSCSwitcher(private val adrWidth: Int, debug: Boolean) extends Module
  with PESizeConfig with GNMFCS2Config {
  val io: CSCSwitcherIO = IO(new CSCSwitcherIO(adrWidth = adrWidth))
  private val dataWidth = cscDataWidth + cscCountWidth
  private val zeroCode = if (adrWidth == inActAdrWidth) inActZeroColumnCode else weightZeroColumnCode
  private val lgVectorNum = if (adrWidth == inActAdrWidth) log2Ceil(inActStreamNum) else log2Ceil(weightStreamNum)
  // TODO: generate SIMD csc for weight
  private val inData = Queue(io.inData, fifoSize, flow = true, pipe = true)
  private val outAdr = Wire(Decoupled(UInt(adrWidth.W)))
  private val outData = Wire(Decoupled(UInt(dataWidth.W)))
  private val cscCountReg = RegInit(0.U(cscCountWidth.W))
  private val cscCountPlusOne = cscCountReg + 1.U
  private val cscAdrReg = RegInit(0.U(adrWidth.W))
  private val cscAdrPlusOne = cscAdrReg + 1.U
  private val columnCounter = RegInit(0.U(5.W))
  private val columnCounterPlusOne = columnCounter + 1.U
  private val zeroColReg = RegInit(true.B) // true when current column contains zero only
  /** [[vectorNumCounter]] will count current padNumber.
    * TODO: change [[lgVectorNum]] to SRAMSize/padSize */
  private val vectorNumCounter = RegInit(0.U(lgVectorNum.W))
  private val vectorNumPlusOne = vectorNumCounter + 1.U
  private val meetNoneZeroWire = Wire(Bool())
  private val oneColFinWire = Wire(Bool())
  private val oneMatrixFinWire = Wire(Bool())
  private val oneVectorFinRegNext = RegNext(oneColFinWire && oneMatrixFinWire) // true when process one pad data
  private val oneStreamFinRegNext = RegNext(oneVectorFinRegNext && (io.vectorNum === vectorNumPlusOne))
  /** when cscCountReg equals to the height of matrix, then current column finishes */
  oneColFinWire := io.matrixHeight === cscCountPlusOne
  oneMatrixFinWire := io.matrixWidth === columnCounterPlusOne
  /** meetNoneZeroWire will be true when current bits is not zero*/
  meetNoneZeroWire := inData.bits =/= 0.U
  /** when meet none a zero element, zeroColReg will be assigned to false, otherwise keep its value
    * After every column, it will be reset */
  zeroColReg := Mux(oneColFinWire, true.B, Mux(meetNoneZeroWire, false.B, zeroColReg))
  private val currentZeroColumn = oneColFinWire && !meetNoneZeroWire && zeroColReg
  // true then its the first none zero element in current column
  private val firstNoneZeroValue = meetNoneZeroWire && zeroColReg
  private val outDataShouldValid = meetNoneZeroWire && inData.valid
  // TODO: remove `cscAdrReg =/= 0.U` for zero column
  /** address vector will emmit one element at the beginning of each column */
  private val outAdrShouldValid = (currentZeroColumn || firstNoneZeroValue) && inData.valid && cscAdrReg =/= 0.U
  private val endFlag = oneStreamFinRegNext || oneVectorFinRegNext
  /** when its the last element of one Pad or the whole stream, then ready will be false to stop deq from in queue
    * when any of the out queues is full (out queue.ready is false) then stop deq from in queue
    * but when out queue is full but current data is zero, then we can deq it from in queue*/
  inData.ready := !endFlag && ((outData.ready && outAdr.ready) || !meetNoneZeroWire)
  /** and both csc data and address will be zero when endFlag is true */
  outAdr.bits := Mux(endFlag, 0.U, Mux(currentZeroColumn, zeroCode.U, cscAdrReg))
  outData.bits := Mux(endFlag, 0.U, Cat(inData.bits | 0.U(cscDataWidth.W), cscCountReg | 0.U(cscCountWidth.W)))
  /** when [[oneVectorFinRegNext]] equals to true, then pad number should add one */
  vectorNumCounter := Mux(oneStreamFinRegNext, 0.U, Mux(oneVectorFinRegNext, vectorNumPlusOne, vectorNumCounter))
  outData.valid := Mux(endFlag, true.B, outDataShouldValid)
  outAdr.valid := Mux(endFlag, true.B, outAdrShouldValid)
  cscCountReg := Mux(inData.fire(), Mux(oneColFinWire, 0.U, cscCountPlusOne), cscCountReg)
  /** if it's a zero column, then adr will keep its value */
  cscAdrReg := Mux(endFlag, 0.U, Mux(outDataShouldValid, cscAdrPlusOne, cscAdrReg))
  columnCounter := Mux(endFlag, 0.U, Mux(oneColFinWire, columnCounterPlusOne, columnCounter))
  io.outData.adrIOs.data <> Queue(outAdr, fifoSize, pipe = true, flow = true)
  io.outData.dataIOs.data <> Queue(outData, fifoSize, pipe = true, flow = true)
  if (debug) {
    io.debugIO.firstNoneZero := firstNoneZeroValue
    io.debugIO.zeroColReg := zeroColReg
    io.debugIO.cscAdrReg := cscAdrReg
    io.debugIO.columnCounter := columnCounter
    io.debugIO.endFlag := endFlag
    io.debugIO.oneVecFin := oneVectorFinRegNext
    io.debugIO.allVecFin := oneStreamFinRegNext
    io.debugIO.oneColFin := oneColFinWire
    io.debugIO.currentRow := cscCountReg
    io.debugIO.currentStreamNum := vectorNumCounter
  } else {
    io.debugIO <> DontCare
  }
}
