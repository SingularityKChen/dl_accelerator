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
}

class CSCSwitcher(private val adrWidth: Int) extends Module with PESizeConfig {
  val io: CSCSwitcherIO = IO(new CSCSwitcherIO(adrWidth = adrWidth))
  private val dataWidth = cscDataWidth + cscCountWidth
  // TODO: generate SIMD csc for weight
  private val inData = Queue(io.inData, fifoSize, flow = true, pipe = true)
  private val outAdr = Wire(Decoupled(UInt(adrWidth.W)))
  private val outData = Wire(Decoupled(UInt(dataWidth.W)))
  private val cscCountReg = RegInit(0.U(cscCountWidth.W))
  private val cscAdrReg = RegInit(0.U(adrWidth.W))
  private val zeroColReg = RegInit(true.B) // true when current column contains zero only
  private val sramCounter = RegInit(0.U) // counter for one stream
  private val meetNoneZeroWire = Wire(Bool())
  private val oneColFinWire = Wire(Bool())
  private val oneRowFinWire = Wire(Bool())
  private val onePadFinRegNext = RegNext(oneColFinWire && oneRowFinWire) // true when process one pad data
  meetNoneZeroWire := inData.fire() && inData.bits =/= 0.U
  /** when cscCountReg equals to the height of matrix, then current column finishes */
  oneColFinWire := io.matrixHeight === cscCountReg
  oneRowFinWire := io.matrixWidth === cscAdrReg
  /** when its the last element of one Pad, then ready will be false to stop deq from queue */
  inData.ready := !onePadFinRegNext
  /** and both csc data and address will be zero */
  when (onePadFinRegNext) {
    outData.bits := 0.U
    outAdr.bits := 0.U
  } .otherwise {
    outData.bits := Cat(inData.bits, cscCountReg) // combine them together
    outAdr.bits := cscAdrReg
  }
  when (oneColFinWire && zeroColReg) {
    /** if current column only contains zero elements */
    // TODO: should valid for both address and data; should assign data with zeroCode
  }
  outData.valid := Mux(onePadFinRegNext, true.B, meetNoneZeroWire)
  outAdr.valid := Mux(onePadFinRegNext, true.B, meetNoneZeroWire && cscAdrReg =/= 0.U)
  /** when meet none a zero element, zeroColReg will be assigned to false, otherwise keep its value */
  zeroColReg := Mux(meetNoneZeroWire, false.B, zeroColReg)
  when (inData.fire()) {
    when (oneColFinWire) {
      cscCountReg := 0.U
      when (oneRowFinWire) {
        cscAdrReg := 0.U
        zeroColReg := true.B
      } .otherwise {
        cscAdrReg := 1.U + cscAdrReg
      }
    } .otherwise {
      cscCountReg := 1.U + cscCountReg
    }
  }
  io.outData.adrIOs.data <> Queue(outAdr, fifoSize, pipe = true, flow = true)
}
