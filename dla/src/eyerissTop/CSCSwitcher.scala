package dla.eyerissTop

import chisel3._
import chisel3.util._
import dla.pe.{CSCStreamIO, PESizeConfig}

class CSCSwitcherIO(private val adrWidth: Int) extends Bundle with PESizeConfig {
  val inData: DecoupledIO[UInt] = Decoupled(UInt(cscDataWidth.W))
  val outData = new CSCStreamIO(adrWidth = adrWidth, dataWidth = cscDataWidth + cscCountWidth)
  /** use matrix height and width to increase and wrap csc address and count reg */
  val matrixHeight: UInt = Input(UInt(5.W)) // TODO: check the width
  val matrixWidth: UInt = Input(UInt(5.W))
  val padNum: UInt = Input(UInt(5.W))
}

class CSCSwitcher(private val adrWidth: Int) extends Module with PESizeConfig {
  val io: CSCSwitcherIO = IO(new CSCSwitcherIO(adrWidth = adrWidth))
  private val dataWidth = cscDataWidth + cscCountWidth
  private val zeroCode = if (adrWidth == inActAdrWidth) inActZeroColumnCode else weightZeroColumnCode
  // TODO: generate SIMD csc for weight
  private val inData = Queue(io.inData, fifoSize, flow = true, pipe = true)
  private val outAdr = Wire(Decoupled(UInt(adrWidth.W)))
  private val outData = Wire(Decoupled(UInt(dataWidth.W)))
  private val cscCountReg = RegInit(0.U(cscCountWidth.W))
  private val cscCountPlusOne = cscCountReg + 1.U
  private val cscAdrReg = RegInit(0.U(adrWidth.W))
  private val cscAdrPlusOne = cscAdrReg + 1.U
  private val zeroColReg = RegInit(true.B) // true when current column contains zero only
  private val padNumCounter = RegInit(0.U) // counter for padNumbers
  private val padNumPlusOne = padNumCounter + 1.U
  private val meetNoneZeroWire = Wire(Bool())
  private val oneColFinWire = Wire(Bool())
  private val oneRowFinWire = Wire(Bool())
  // TODO: check the logic when inData.fire is false
  private val onePadFinRegNext = RegNext(inData.fire() && oneColFinWire && oneRowFinWire) // true when process one pad data
  private val oneStreamFinRegNext = RegNext(inData.fire() && onePadFinRegNext && (padNumPlusOne === io.padNum))
  /** meetNoneZeroWire will be true when current bits is not zero*/
  meetNoneZeroWire := inData.fire() && inData.bits =/= 0.U
  private val currentZeroColumn = inData.fire() && oneColFinWire && zeroColReg
  private val outShouldValid = meetNoneZeroWire || currentZeroColumn
  /** when cscCountReg equals to the height of matrix, then current column finishes */
  oneColFinWire := io.matrixHeight === cscCountPlusOne
  oneRowFinWire := io.matrixWidth === cscAdrPlusOne
  private val currentShouldBeZero = oneStreamFinRegNext || onePadFinRegNext
  /** when its the last element of one Pad or the whole stream, then ready will be false to stop deq from queue */
  inData.ready := !currentShouldBeZero
  /** and both csc data and address will be zero when currentShouldBeZero is true */
  outAdr.bits := Mux(currentShouldBeZero, 0.U, cscAdrReg)
  outData.bits := Mux(currentShouldBeZero, 0.U, Mux(currentZeroColumn, zeroCode.U, Cat(inData.bits, cscCountReg)))
  /** when [[onePadFinRegNext]] equals to true, then pad number should add one */
  padNumCounter := Mux(onePadFinRegNext, padNumPlusOne, padNumCounter)
  when (oneColFinWire && zeroColReg) {
    /** if current column only contains zero elements */
  }
  outData.valid := Mux(currentShouldBeZero, true.B, outShouldValid)
  // TODO: remove `cscAdrReg =/= 0.U` for zero column
  outAdr.valid := Mux(currentShouldBeZero, true.B, outShouldValid && cscAdrReg =/= 0.U)
  /** when meet none a zero element, zeroColReg will be assigned to false, otherwise keep its value */
  zeroColReg := Mux(meetNoneZeroWire, false.B, zeroColReg)
  when (inData.fire()) {
    when (oneColFinWire) {
      cscCountReg := 0.U
      when (oneRowFinWire) {
        cscAdrReg := 0.U
      } .otherwise {
        cscAdrReg := cscAdrPlusOne
      }
    } .otherwise {
      cscCountReg := cscCountPlusOne
    }
  }
  io.outData.adrIOs.data <> Queue(outAdr, fifoSize, pipe = true, flow = true)
  io.outData.dataIOs.data <> Queue(outData, fifoSize, pipe = true, flow = true)
}
