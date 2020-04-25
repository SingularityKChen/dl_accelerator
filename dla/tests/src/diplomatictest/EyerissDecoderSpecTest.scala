package dla.tests.diplomatictest

import chisel3._
import chisel3.tester._
import dla.cluster.{GNMFCS1Config, GNMFCS2Config}
import dla.diplomatic.EyerissDecoder
import dla.pe.MCRENFConfig
import org.scalatest._

import scala.util.Random

object DecoderSequencer extends MCRENFConfig with GNMFCS2Config with GNMFCS1Config {
  private def toBinary(i: Int, digits: Int = 1): String =
  s"%${digits}s".format(i.toBinaryString).replaceAllLiterally(" ", "0")
  private def getImm(datas: Seq[Int]): String = {
    datas.map(x => toBinary(x, digits = 3)).reduce(_ + _)
  }
  private val G2N2M2F2 = getImm(Seq(G2, N2, M2, F2))
  private val C2S2G1N1 = getImm(Seq(C2, S2, G1, N1))
  private val M1F1C1S1 = getImm(Seq(M1, F1, C1, S1))
  private val F0N0C0M0 = getImm(Seq(F0, N0, C0, M0))
  object inActAdr {
    val hex = 0x02
    val b: String = toBinary(hex, 5)
  }
  object weightAdr {
    val hex = 0x0b
    val b: String = toBinary(hex, 5)
  }
  object pSumAdr {
    val hex = 0x1c
    val b: String = toBinary(hex, 5)
  }
  private val opcode: String = "0101011"
  private val zeroAddress = "00000"
  val loadPart0: Int = BigInt(s"$G2N2M2F2${inActAdr.b}000${weightAdr.b}$opcode", 2).toInt
  val loadPart1: Int = BigInt(s"$C2S2G1N1${zeroAddress}001$zeroAddress$opcode", 2).toInt
  val loadPart2: Int = BigInt(s"$M1F1C1S1${zeroAddress}010$zeroAddress$opcode", 2).toInt
  val loadPart3: Int = BigInt(s"$F0N0C0M0${toBinary(E, 5)}011${toBinary(R, 5)}$opcode", 2).toInt
  val loadPSum: Int = BigInt(s"${toBinary(0, 12)}${zeroAddress}100${pSumAdr.b}$opcode", 2).toInt
  object reqSize {
    val inAct: Int = G2*N2*C2*(F2 + S2) * R*C0 * F0*N0*E
    val weight: Int = M0 * R*C0
    val pSum: Int = G2*N2*M2*F2 * M0*E*N0*F0
  }
}

class EyerissDecoderSpecTest extends FlatSpec with ChiselScalatestTester
  with Matchers {
  private val sequencer = DecoderSequencer
  behavior of "test the spec of Decoder"
  it should "work well on decode instructions" in {
    test(new EyerissDecoder) { theDecoder =>
      val theTop = theDecoder.io
      val theClock = theDecoder.clock
      theDecoder.reset.poke(true.B)
      theClock.step()
      theDecoder.reset.poke(false.B)
      theTop.calFin.poke(false.B)
      theTop.instruction.poke(sequencer.loadPart0.U)
      theClock.step(cycles = (new Random).nextInt(15) + 1)
      theTop.instruction.poke(sequencer.loadPart1.U)
      theClock.step(cycles = (new Random).nextInt(15) + 1)
      theTop.instruction.poke(sequencer.loadPart2.U)
      theClock.step(cycles = (new Random).nextInt(15) + 1)
      theTop.instruction.poke(sequencer.loadPart3.U)
      theClock.step()
      theTop.doMacEn.expect(true.B)
      theTop.inActIO.starAdr.expect(sequencer.inActAdr.hex.U)
      theTop.weightIO.starAdr.expect(sequencer.weightAdr.hex.U)
      theTop.inActIO.reqSize.expect(sequencer.reqSize.inAct.U)
      theTop.weightIO.reqSize.expect(sequencer.reqSize.weight.U)
      println(s"inActReqSize = ${theTop.inActIO.reqSize.peek().litValue()}")
      println(s"inActAdr = 0x${theTop.inActIO.starAdr.peek().litValue().toString(16)}")
      println(s"weightReqSize = ${theTop.weightIO.reqSize.peek().litValue()}")
      println(s"weightAdr = 0x${theTop.weightIO.starAdr.peek().litValue().toString(16)}")
      theClock.step(cycles = (new Random).nextInt(15) + 1)
      theTop.valid.expect(false.B)
      theTop.calFin.poke(true.B)
      theClock.step()
      theTop.calFin.poke(false.B)
      theTop.valid.expect(true.B)
      theClock.step(cycles = (new Random).nextInt(15) + 1)
      theTop.instruction.poke(sequencer.loadPSum.U)
      theClock.step()
      theTop.pSumIO.pSumLoadEn.expect(true.B)
      theTop.pSumIO.reqSize.expect(sequencer.reqSize.pSum.U)
      theTop.pSumIO.starAdr.expect(sequencer.pSumAdr.hex.U)
      println(s"pSumReqSize = ${theTop.pSumIO.reqSize.peek().litValue()}")
      println(s"pSumAdr = 0x${theTop.pSumIO.starAdr.peek().litValue().toString(16)}")
    }
  }
}
