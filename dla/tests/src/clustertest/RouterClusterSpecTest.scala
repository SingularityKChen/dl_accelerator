package dla.tests.clustertest

import chisel3._
import chisel3.experimental.{DataMirror, Direction}
import chisel3.tester._
import dla.cluster._

import scala.util.Random
import scala.math.pow

class RouterClusterSpecTest extends ClusterSpecTestBasic {
  override val printLogDetails = false
  behavior of "test the spec of Router Cluster"
  it should "work well on PSum Router Cluster" in {
    test (new PSumRouter) { thePSRouter =>
      val theTop = thePSRouter.io
      val theCtrlIO = theTop.ctrlPath
      val theDataIO = theTop.dataPath
      val theClock = thePSRouter.clock
      val randomDelay = (new Random).nextInt(50) + 1
      println("----------------- test begin -----------------")
      println("-------------- PSum Router Spec --------------")
      println("----------- test basic functions -------------")
      thePSRouter.reset.poke(true.B)
      theClock.step()
      thePSRouter.reset.poke(false.B)
      theClock.step()
      // always read from PE and send to GLB
      theCtrlIO.inDataSel.poke(true.B) // read from GLB
      theCtrlIO.outDataSel.poke(true.B) // send it to PE
      // begin to load PSum from GLB and send it into PEArray,
      // then back from PEArray and write back to GLB
      println("------------ begin to poke @ GLB -------------")
      theDataIO.inIOs(1).bits.poke(1.U)
      theDataIO.inIOs(1).valid.poke(true.B)
      println("---------- begin to peek @ PEArray -----------")
      theDataIO.outIOs(0).bits.expect(1.U)
      theDataIO.outIOs(0).valid.expect(true.B)
      theDataIO.outIOs(0).ready.poke(true.B)
      println("------------ expect ready @ GLB --------------")
      theDataIO.inIOs(1).ready.expect(true.B, "port from GLB should ready now")
      theClock.step()
      theDataIO.inIOs(1).valid.poke(false.B)
      theDataIO.outIOs(0).ready.poke(false.B)
      theDataIO.inIOs(1).ready.expect(false.B)
      theClock.step(randomDelay)
      println("---------- begin to poke @ PEArray -----------")
      theDataIO.inIOs(0).bits.poke(2.U)
      theDataIO.inIOs(0).valid.poke(true.B)
      println("------------ begin to peek @ GLB -------------")
      theDataIO.outIOs(1).bits.expect(2.U)
      theDataIO.outIOs(1).valid.expect(true.B)
      theDataIO.outIOs(1).ready.poke(true.B)
      theDataIO.outIOs(0).valid.expect(false.B, "not use now, so you should be false")
      theDataIO.outIOs(2).valid.expect(false.B, "not use now, so you should be false")
      println("---------- expect ready @ PEArray ------------")
      theDataIO.inIOs(0).ready.expect(true.B)
      theDataIO.inIOs.takeRight(2).foreach(x => x.ready.expect(false.B, "not use now, so you should be false"))
      theClock.step()
    }
  }

  it should "work well on InAct Router Cluster" in {
    test (new InActRouter) { theInActRouter =>
      val theTop = theInActRouter.io
      val theClock = theInActRouter.clock
      val theDataIO = theTop.dataPath
      val theCtrlIO = theTop.ctrlPath
      val randomInIO = (new Random).nextInt(inActPortNum)
      val randomInAdr = Seq.fill(inActPortNum) {(new Random).nextInt(pow(2, inActAdrWidth).toInt)}
      println("----------------- test begin -----------------")
      println("------------- InAct Router Spec --------------")
      println("------ test uni-cast with random inPort ------")
      theInActRouter.reset.poke(true.B)
      theClock.step()
      theInActRouter.reset.poke(false.B)
      theClock.step()
      theCtrlIO.inDataSel.poke(randomInIO.U) // from random port
      theCtrlIO.outDataSel.poke(0.U) // uni-cast
      for (inPort <- 0 until inActPortNum) {
        println(s"peek inActIO$inPort with data ${randomInAdr(inPort)}, valid = ${inPort == randomInIO}")
        theDataIO.inIOs(inPort).adrIOs.data.bits.poke(randomInAdr(inPort).U)
        theDataIO.inIOs(inPort).adrIOs.data.valid.poke((inPort == randomInIO).B)
      }
      theDataIO.outIOs(0).adrIOs.data.bits.expect(randomInAdr(randomInIO).U)
      theDataIO.outIOs(0).adrIOs.data.valid.expect(true.B)
      theDataIO.outIOs.takeRight(inActPortNum - 1).foreach(x => x.adrIOs.data.valid.expect(false.B))
      theDataIO.outIOs(0).adrIOs.data.ready.poke(true.B)
      for (inPort <- 0 until inActPortNum) {
        println(s"expect inActIO$inPort ready = ${inPort == randomInIO}")
        theDataIO.inIOs(inPort).adrIOs.data.ready.expect((inPort == randomInIO).B,
          s"randomInIO = $randomInIO should it ready now?")
      }
      theClock.step()
    }
    // TODO: add more test case for different cast mode
  }

  it should "work well on Weight Router Cluster" in {
    test (new WeightRouter) { theWeightRouter =>
      val theTop = theWeightRouter.io
      val theClock = theWeightRouter.clock
      val theCtrlIO = theTop.ctrlPath
      val theDataIO = theTop.dataPath
      val randomInIO = Seq.fill(2){(new Random).nextInt(weightPortNum)}
      val randomInAdr = Seq.fill(weightPortNum) {(new Random).nextInt(pow(2, weightAdrWidth).toInt)}
      println("----------------- test begin -----------------")
      println("------------- Weight Router Spec -------------")
      println("---------- test weight connections -----------")
      println("----------- with random in/OutPort -----------")
      theWeightRouter.reset.poke(true.B)
      theClock.step()
      theWeightRouter.reset.poke(false.B)
      theClock.step()
      theCtrlIO.inDataSel.poke((randomInIO.head == 1).B) // true to port 1, false to port 0
      theCtrlIO.outDataSel.poke((randomInIO.last == 1).B)
      for (inPort <- 0 until weightPortNum) {
        val prefix: String = s"weight@inIOsAdr@$inPort"
        theDataIO.inIOs(inPort).adrIOs.data.bits.poke(randomInAdr(inPort).U)
        theDataIO.inIOs(inPort).adrIOs.data.valid.poke((inPort == randomInIO.head).B)
        println(s"[$prefix] bits = ${randomInAdr(inPort)}, valid = ${inPort == randomInIO.head}")
      }
      for (outPort <- 0 until weightPortNum) {
        val prefix: String = s"weight@outIOsAdr@$outPort"
        println(s"[$prefix] valid = ${outPort === randomInIO.last || outPort == 0}")
        if (outPort === randomInIO.last || outPort == 0) { // always send to PEArray
          println(s"[$prefix] bits = ${randomInAdr(outPort)}")
          theDataIO.outIOs(outPort).adrIOs.data.valid.expect(true.B)
          theDataIO.outIOs(outPort).adrIOs.data.bits.expect(randomInAdr(randomInIO.head).U)
          theDataIO.outIOs(outPort).adrIOs.data.ready.poke(true.B)
        } else {
          theDataIO.outIOs(outPort).adrIOs.data.valid.expect(false.B)
        }
      }
      theDataIO.inIOs(randomInIO.head).adrIOs.data.ready.expect(true.B)
      theClock.step()
    }
  }

  it should "work well on Router Cluster" in {
    test (new RouterCluster(true)) { theRCluster =>
      val theTop = theRCluster.io
      val theClock = theRCluster.clock
      theTop.dataPath.routerData.iRIO.head.inIOs.foreach(x =>
        require(DataMirror.directionOf(x.dataIOs.data.bits) == Direction.Input))
      theTop.dataPath.routerData.iRIO.head.outIOs.foreach(x =>
        require(DataMirror.directionOf(x.dataIOs.data.bits) == Direction.Output))
      println("----------------- test begin -----------------")
      println("---------- Router Cluster Top Spec -----------")
      println("----------- test basic functions -------------")
      theRCluster.reset.poke(true.B)
      theClock.step()
      theRCluster.reset.poke(false.B)
      theClock.step()
      // TODO: add some detailed tests on RouterCluster
    }
  }
}
