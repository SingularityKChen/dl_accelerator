package dla.dcsc

import chisel3._
import chisel3.util._
import firrtl.annotations.TargetToken.Reset

class DCSC extends Module{
  val io = IO(new Bundle{
    val some_io = Input(Bool())
  })
}
