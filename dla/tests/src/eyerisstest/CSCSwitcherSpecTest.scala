package dla.tests.eyerisstest

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.tester._
import dla.cluster.GNMFCS2Config
import dla.eyerissTop.CSCSwitcher
import dla.pe.{MCRENFConfig, PESizeConfig}
import dla.tests.GenOneStreamData
import firrtl.options.TargetDirAnnotation
import org.scalatest._

class CSCSwitcherSpecTest  extends FlatSpec with ChiselScalatestTester with Matchers
  with PESizeConfig with MCRENFConfig with GNMFCS2Config {
  private val oneStreamData = new GenOneStreamData
  private val oneInActInData = oneStreamData.inActStream.head.zipWithIndex.map({ case (ints, row) =>
    ints.zipWithIndex.map({ case (data, col) => (data, row, col)})})
  private val oneInActAdrStream = oneStreamData.inActAdrStream.head
  private val oneInActDataStream = oneStreamData.inActDataStream.head
  private val oneWeightInData = oneStreamData.weightStream.head.flatten
  private val oneWeightAdrStream = oneStreamData.weightAdrStream.head
  private val oneWeightDataStream = oneStreamData.weightDataStream.head
  behavior of "test the function of CSCSwitcher"
  chisel3.Driver.emitVerilog(new CSCSwitcher(adrWidth = inActAdrWidth))
  it should "compress common data" in {
    test (new CSCSwitcher(adrWidth = inActAdrWidth)) { theSwitcher =>
      val theTopIO = theSwitcher.io
      val theClock = theSwitcher.clock
      var outAdr: List[Int] = Nil
      var outData: List[Int] = Nil
      theSwitcher.reset.poke(true.B)
      theClock.step()
      theSwitcher.reset.poke(false.B)
      println("----------------- test begin -----------------")
      println("---------- Compress Data into CSC ------------")
      println("--------- test basic data situation ----------")
      theTopIO.matrixHeight.poke(inActMatrixHeight.U)
      theTopIO.matrixWidth.poke(inActMatrixWidth.U)
      theTopIO.vectorNum.poke(inActStreamNum.U)
      theClock.step()
      for (col <- oneInActInData.head.indices) {
        for (row <- oneInActInData.indices) {
          theTopIO.inData.valid.poke(true.B)
          theTopIO.inData.bits.poke(oneInActInData(row)(col)._1.U)
          println(s"[inData@row$row@col$col] inData = (${oneInActInData(row)(col)}, ${theTopIO.inData.ready.peek()})")
          if (oneInActInData(row)(col)._1 != 0)
            println(s"[inData@row$row@col$col] inDataBinary = ${oneInActInData(row)(col)._1.toBinaryString}")
          theTopIO.outData.dataIOs.data.ready.poke(true.B)
          theTopIO.outData.adrIOs.data.ready.poke(true.B)
          if (theTopIO.outData.adrIOs.data.valid.peek().litToBoolean) {
            val adrOut = theTopIO.outData.adrIOs.data.bits.peek().litValue()
            println(s"[outData@row$row@col$col] adr = ($adrOut," +
              s" ${theTopIO.outData.adrIOs.data.valid.peek()})")
            outAdr = outAdr:::List(adrOut.toInt)
          }
          if (theTopIO.outData.dataIOs.data.valid.peek().litToBoolean) {
            val dataOut = theTopIO.outData.dataIOs.data.bits.peek().litValue()
            println(s"[outData@row$row@col$col] data = " +
              s"(${dataOut.toInt.toBinaryString.take(cscDataWidth)}," +
              s" ${theTopIO.outData.dataIOs.data.valid.peek()})")
            outData = outData:::List(dataOut.toInt)
          }
          theClock.step()
        }
      }
      println(s"[info] originalInAct = $oneInActInData")
      println(s"[info] outAdr = $outAdr")
      println(s"[info] goldenAdr = $oneInActAdrStream")
      println(s"[info] outData = $outData")
      println(s"[info] goldenData = $oneInActDataStream")
    }
  }
}
/*
object emitCSCSwitcher extends App with PESizeConfig {
  (new chisel3.stage.ChiselStage).run(Seq(
    ChiselGeneratorAnnotation(() => new CSCSwitcher(adrWidth = inActAdrWidth)),
    TargetDirAnnotation(directory = "test_run_dir")
  ))
}
*/