package dla.tests.diplomatictest

import chisel3._
import chisel3.tester._
import dla.cluster.{GNMFCS1Config, GNMFCS2Config}
import dla.diplomatic.EyerissDecoder
import dla.pe.MCRENFConfig
import org.scalatest._

import scala.util.Random

class EyerissDecoderSpecTest extends FlatSpec with ChiselScalatestTester
  with Matchers with MCRENFConfig with GNMFCS2Config with GNMFCS1Config {
  private def toInstruction(i: Int): String =
    f"${G2.toBinaryString.toBoolean}%2d$N2%2d"
  /*private def toBinary(i: Int, digits: Int = 8): String =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')*/
  private val inActStrAdr = 0x1a
  private val weightStrAdr = 0x2b
  private val pSumStrAdr = 0x3c
  private val opcode: Binary = "0101011"
  private val instructions1 = 0
  behavior of "test the spec of Decoder"
  it should "work well on decode instructions" in {
    test(new EyerissDecoder) { theDecoder =>
      val theTop = theDecoder.io
      val theClock = theDecoder.clock
      theDecoder.reset.poke(true.B)
      theClock.step()
      theDecoder.reset.poke(false.B)
      theTop.instruction.poke()
    }
  }
}
