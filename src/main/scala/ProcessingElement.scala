package pe

import chisel3._
import chisel3.util._

class ProcessingElement extends Module with PESizeConfig {

  // IO of PE
  val io = IO(new Bundle{
    val ifmap = Flipped(Decoupled(UInt(f_width.W))) //input feature map
    val ips = Flipped(Decoupled(UInt(ps_width.W))) //input partial sum
    val filter = Flipped(Decoupled(UInt(f_width.W))) //filter weight
    val ops = Decoupled(UInt(ps_width.W))
  })

  // other components of PE
  val ifmap_fifo = Queue(io.ifmap, fifo_size) //FIFO for input feature map
  val filter_fifo = Queue(io.filter, fifo_size) //FIFO for filter
  val ips_fifo = Queue(io.ips, fifo_size) //FIFO for input partial sum

  // TODO: add the connections of submodules
}

class ProcessingElementControl extends Module with MCRENFConfig {
  val io = IO(new Bundle{
    //val ctrl_mac = new PECtrlToMacIO
    val ctrl_pad = new PECtrlToPadIO
  })
  // some config of RS+
  val loop_width : Int = 3 // the bit-width of mo et al TODO: check the width
  val mcrenf = RegInit(VecInit(Seq.fill(6)(0.U(loop_width.W))))
  // when_carry stores the information of whether m0 === M0.U, et al.
  // logic of PE MAC
  // state machine, the for-loop of m0
  // ps_idle: if not all the MAC operations are done, then load
  // ps_load: load ifmap, filter weight, partial sum, then do the MAC
  // ps_done: finish one mac, then jump to idle or load state
  val ps_idle :: ps_load :: ps_done :: Nil = Enum(3)
  val s_per_mac = RegInit(ps_idle) // the state of the mac process
  val all_mac_is_done = RegInit(false.B) //true when all the mac has been done, then pe will keep in ps_idle
  val when_carry: IndexedSeq[Bool] = mcrenf.zip(MCRENF.map(x=> x - 1)).map{ case (x,y) => x === y.U}
  // TODO: check the MCRENF.map
  switch (s_per_mac) {
    is (ps_idle) {
      when (!all_mac_is_done && io.ctrl_pad.ready) { // when there is any mac leaving, and the pad is ready
        s_per_mac := ps_load
      }
      io.ctrl_pad.valid := false.B
    }
    is (ps_load) {
      io.ctrl_pad.valid := true.B
      // then check whether mcrenf need carry one TODO: check it
      for (i <- 1 until 6) { // from m0 to n0
        when(when_carry.take(i).reduce(_ && _)) {
          mcrenf(i - 1) := 0.U
          mcrenf(i) := mcrenf(i) + 1.U
        }
      }
      when(when_carry.reduce(_ && _)) { // f0
        mcrenf(5) := 0.U
        all_mac_is_done := true.B // then all the macs have been done
      }
      s_per_mac := ps_done
    }
    is (ps_done) {
      io.ctrl_pad.valid := false.B
      mcrenf(0) := mcrenf(0) + 1.U // no matter whether m0 equals to (M0 - 1).U, we add one, then check it
      when (all_mac_is_done) {
        s_per_mac := ps_idle
      }.otherwise{
        s_per_mac := ps_load
      }
    }
  }

  for (i <- 0 until 6) {
    io.ctrl_pad.mcrenf(i) := mcrenf(i)
  }

}

class ProcessingElementPad extends Module with MCRENFConfig with SpadSizeConfig {
  val io = IO(new Bundle{
    val pad_mac = new PEPadToMacIO
    val pad_ctrl = Flipped(new PECtrlToPadIO)
  })
  val ifm_spad = RegInit(VecInit(Seq.fill(ifm_spad_size)(0.U(spad_width.W)))) //reg, input feature map scratch pad
  val filter_spad = SyncReadMem(filter_spad_size, UInt(spad_width.W)) //SRAM, filter scratch pad
  //val filter_spad = RegInit(VecInit(Seq.fill(filter_spad_size)(0.U(spad_width))))
  val ps_spad = RegInit(VecInit(Seq.fill(ps_spad_size)(0.U(spad_width.W)))) //reg, partial sum scratch pad
  val psum_result_reg = RegInit(0.U(f_width.W))
  psum_result_reg := io.pad_mac.psum_result
  private def ifm_idx(m: Vec[UInt]): UInt = m(1) + MCRENF(1).U*(m(2) + MCRENF(2).U*(m(3) + MCRENF(3).U*(m(4) + MCRENF(4).U*m(5))))
  // ifmap_idx = c0 + r0*C0 + e0*R0*C0 + n0*E0*R0*C0 + f0*N0*E0*R0*C0 FIXME: check the input feature map
  private def filter_idx(m: Vec[UInt]): UInt = m(0) + MCRENF.head.U*(m(1) + MCRENF(1).U*m(2))
  // filter_idx = m0 + c0*M0 + r0*C0*M0
  private def psum_idx(m: Vec[UInt]): UInt = m(0) + MCRENF.head.U*(m(3) + MCRENF(3).U*(m(4) + MCRENF(4).U*m(5)))
  // psum_idx = m0 + e0*M0 + n0*E0*M0 + f0*N0*E0*M0
  val psum_result_idx_reg = RegInit(0.U(f_width.W)) // store the index for write back
  // pad_idle: the pad is idle, and if received valid signal from control, then read and send data to mac
  // pad_wait: wait for mac computation
  // pad_write: write the partial sum back
  val pad_idle :: pad_wait :: pad_write :: Nil = Enum(3) // TODO: try use only two states
  val s_pad = RegInit(pad_idle)
  switch (s_pad) {
    is (pad_idle) {
      when(io.pad_ctrl.valid) {
        s_pad := pad_wait
        io.pad_ctrl.ready := false.B
        io.pad_mac.ifmap_cal := ifm_spad(ifm_idx(io.pad_ctrl.mcrenf))
        io.pad_mac.filter_cal := filter_spad.read(filter_idx(io.pad_ctrl.mcrenf)) // TODO: add enable signal with zero buffer
        io.pad_mac.psum_load := ps_spad(psum_idx(io.pad_ctrl.mcrenf))
        psum_result_idx_reg := psum_idx(io.pad_ctrl.mcrenf) // reg the index
      }
      io.pad_ctrl.ready := true.B // tell control module that can read data
    }
    is (pad_wait) {
      s_pad := pad_write
    }
    is (pad_write) {
      s_pad := pad_idle
      ps_spad(psum_result_idx_reg) := psum_result_reg //update the partial sum
    }
  }
}

class ProcessingElementMAC extends Module with PESizeConfig {
  val io = IO(new Bundle{
    //val mac_ctrl = Flipped(new PECtrlToMacIO)
    val mac_pad = Flipped(new PEPadToMacIO)
  })
  io.mac_pad.psum_result := io.mac_pad.psum_load + io.mac_pad.filter_cal * io.mac_pad.ifmap_cal //TODO: add zero buffer
}

class PECtrlToMacIO extends Bundle { // with ready valid
  val rst = Output(Bool())
  //val all_mac_is_done = Output(Bool())
}

class PEPadToMacIO extends Bundle with PESizeConfig {
  val ifmap_cal = Output(UInt(f_width.W))
  val filter_cal = Output(UInt(f_width.W))
  val psum_load = Output(UInt(f_width.W))
  val psum_result = Input(UInt(f_width.W))
}

class PECtrlToPadIO extends DecoupledIO {
  val rst = Output(Bool())
  val mcrenf = Output(Vec(6, UInt(5.W)))
}

trait MCRENFConfig extends PESizeConfig { // contains some scala values
  val M0 : Int = 6 // weights reuse M0 times
  val C0 : Int = 3 // different input feature maps and their weights reuse
  val R : Int = filter_height //
  val E : Int = ofmap_height // same row of weights in a PE
  val N0 : Int = 2 // the number of partial sum
  val F0 : Int = 2 // one row of partial sum
  val MCRENF: List[Int] = List(M0, C0, R, E, N0, F0)
}

trait SpadSizeConfig extends PESizeConfig {
  val ps_spad_size : Int = 24
  val ifm_spad_size : Int = 12
  val filter_spad_size : Int = 224
  val spad_width : Int = f_width // bit-width for three scratch pad
}

trait PESizeConfig { // TODO: move this config to a higher level package
  val filter_height: Int = 2
  val ofmap_height: Int = 2
  val f_width: Int = 16
  val ps_width: Int = 16
  val fifo_size:Int = 4
}
