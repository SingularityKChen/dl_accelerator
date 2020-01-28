package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe._

class PECluster(val peColSize: Int, val peRowSize: Int, debug: Boolean) extends Module {
  val io = Flipped(new PEAndRouterIO)
  val peRow =  Vec(peRowSize, Module(new ProcessingElement(debug = debug)).io)
  val peArray = Vec(peColSize, peRow)
}
