package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe._

class PECluster(val peColSize: Int, val peRowSize: Int) extends Module {
  val io = IO(new Bundle {})
  val peRow: Vec[Bundle] =  Vec(peRowSize, Module(new ProcessingElement).io)
  val peArray: Vec[Vec[Bundle]] = Vec(peColSize, peRow)
}
