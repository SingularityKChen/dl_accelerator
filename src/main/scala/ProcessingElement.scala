package pe

import chisel3._
import chisel3.util._
import firrtl.annotations.TargetToken.Reset
// TODO: add reset signal for every module
class ProcessingElement extends Module with PESizeConfig {
  val io = IO(new Bundle{
    val ifmap = Flipped(Decoupled(new DataWithIndex(fm_data_width, fm_addr_width)))  //input feature map
    val ips = Flipped(Decoupled(UInt(ps_data_width.W))) //input partial sum
    val filter = Flipped(Decoupled(new DataWithIndex(filter_data_width, filter_addr_width))) //filter weight
    val ops: DecoupledIO[UInt] = Decoupled(UInt(ps_data_width.W))
  })
  val ifmap_fifo = Module(new Queue(io.ifmap, fifo_size, flow = true)) //FIFO for input feature map
  val filter_fifo = Module(new Queue(io.filter, fifo_size, flow = true)) //FIFO for filter
  val ips_fifo = Module(new Queue(io.ips, fifo_size, flow = true)) //FIFO for input partial sum
  val pe_ctrl = new ProcessingElementControl
  val pe_pad = new ProcessingElementPad
  val pe_mac = new ProcessingElementMAC
  pe_ctrl.io.ctrl_pad <> pe_pad.io.pad_ctrl
  pe_mac.io.mac_pad <> pe_pad.io.pad_mac
  pe_pad.io.ifm_io <> ifmap_fifo.io.deq
  pe_pad.io.filter_io <> filter_fifo.io.deq
  pe_pad.io.ips_io <> ips_fifo.io.deq
  val ops_fifo = Module(new Queue(pe_pad.io.ops_io, fifo_size, flow = true)) //FIFO for output partial sum
  io.ops <> ips_fifo.io.deq
}

class ProcessingElementControl extends Module with MCRENFConfig {
  val io = IO(new Bundle{
    //val ctrl_mac = new PECtrlToMacIO
    val ctrl_pad: DecoupledIO[PECtrlToPadIO] = Decoupled(new PECtrlToPadIO)
  })
  // some config of RS+
  val loop_width : Int = 3 // the bit-width of mo et al TODO: check the width
  val mcrenf_reg = RegInit(VecInit(Seq.fill(6)(0.U(loop_width.W))))
  // when_carry stores the information of whether m0 === M0.U, et al.
  // logic of PE MAC
  // state machine, the for-loop of m0
  // ps_idle: if not all the MAC operations are done, then load
  // ps_load: load ifmap, filter weight, partial sum, then do the MAC
  // ps_done: finish one mac, then jump to idle or load state
  val ps_idle :: ps_load :: ps_done :: Nil = Enum(3)
  val s_per_mac = RegInit(ps_idle) // the state of the mac process
  val all_mac_is_done = RegInit(false.B) //true when all the mac has been done, then pe will keep in ps_idle
  val when_carry: IndexedSeq[Bool] = mcrenf_reg.zip(MCRENF.map(x=> x - 1)).map{ case (x,y) => x === y.U}
  // TODO: check the MCRENF.map
  switch (s_per_mac) {
    is (ps_idle) {
      when (!all_mac_is_done) { // when there is any mac leaving
        s_per_mac := ps_load
      }
      io.ctrl_pad.valid := false.B
    }
    is (ps_load) {
      io.ctrl_pad.valid := true.B
      when (io.ctrl_pad.ready) { //after the pad receives the data
        s_per_mac := ps_done
        io.ctrl_pad.valid := false.B
      }
    }
    is (ps_done) {
      mcrenf_reg(0) := mcrenf_reg(0) + 1.U // no matter whether m0 equals to (M0 - 1).U, we add one, then check it
      // then check whether mcrenf need carry one TODO: check it
      for (i <- 1 until 6) { // from m0 to n0
        when(when_carry.take(i).reduce(_ && _)) {
          if (!isPow2(MCRENF(i-1))) { // if equals to pow2, then don't need reset just let it carries.
            mcrenf_reg(i - 1) := 0.U
          }
          mcrenf_reg(i) := mcrenf_reg(i) + 1.U
        }
      }
      when(when_carry.reduce(_ && _)) { // f0
        if (!isPow2(MCRENF(5))) {
          mcrenf_reg(5) := 0.U
        }
        all_mac_is_done := true.B // then all the macs have been done
        // FIXME: need a signal to let the PE start
      }

      when (all_mac_is_done) {
        s_per_mac := ps_idle
      }.otherwise{
        s_per_mac := ps_load
      }
    }
  }

  for (i <- 0 until 6) {
    io.ctrl_pad.bits.mcrenf(i) := mcrenf_reg(i)
  }

}

class ProcessingElementPad extends Module with MCRENFConfig with SpadSizeConfig {
  val io = IO(new Bundle{
    val pad_mac = new PEPadToMacIO
    val pad_ctrl = Flipped(Decoupled(new PECtrlToPadIO))
    val ifm_io = Flipped(Decoupled(new DataWithIndex(fm_data_width, fm_addr_width)))
    val filter_io = Flipped(Decoupled(new DataWithIndex(filter_data_width, filter_addr_width)))
    val ips_io = Flipped(Decoupled(UInt(ps_data_width.W)))
    val ops_io: DecoupledIO[UInt] = Decoupled(UInt(ps_data_width.W))
  })
  val ifm_data_spad = RegInit(VecInit(Seq.fill(ifm_data_size)(0.U(fm_data_width.W)))) // reg, input feature map data scratch pad
  val ifm_addr_spad = RegInit(VecInit(Seq.fill(ifm_addr_size)(0.U(fm_addr_width.W)))) // reg, input feature map address scratch pad
  val filter_data_spad = SyncReadMem(filter_data_size, UInt(filter_data_width.W)) // SRAM, filter data scratch pad, two cycles to read
  val filter_addr_spad = RegInit(VecInit(Seq.fill(filter_addr_size)(0.U(filter_addr_width)))) // reg, filter address scratch pad
  val ps_data_spad = RegInit(VecInit(Seq.fill(ps_data_size)(0.U(ps_data_width.W)))) // reg, partial sum scratch pad
  val psum_result_reg = RegInit(0.U(ps_data_width.W))
  psum_result_reg := io.pad_mac.psum_result
  private def ifm_idx(m: Vec[UInt]): UInt = m(1) + MCRENF(1).U*(m(2) + MCRENF(2).U*(m(3) + MCRENF(3).U*(m(4) + MCRENF(4).U*m(5))))
  // ifmap_idx = c0 + r0*C0 + e0*R0*C0 + n0*E0*R0*C0 + f0*N0*E0*R0*C0 FIXME: check the input feature map
  private def filter_idx(m: Vec[UInt]): UInt = m(0) + MCRENF.head.U*(m(1) + MCRENF(1).U*m(2))
  // filter_idx = m0 + c0*M0 + r0*C0*M0
  private def psum_idx(m: Vec[UInt]): UInt = m(0) + MCRENF.head.U*(m(3) + MCRENF(3).U*(m(4) + MCRENF(4).U*m(5)))
  // psum_idx = m0 + e0*M0 + n0*E0*M0 + f0*N0*E0*M0
  val psum_result_idx_reg = RegInit(0.U(cal_data_width.W)) // store the index for write back
  // pad_idle: the pad is idle, and if received valid signal from control, then read and send data to mac
  // pad_wait: wait for mac computation
  // pad_write: write the partial sum back
  val pad_idle :: pad_wait :: pad_write :: Nil = Enum(3) // TODO: try use only two states
  val s_pad = RegInit(pad_idle)
  // the_index_m0 = m0 + count_m0
  // addr_m0_index*M0
  switch (s_pad) {
    is (pad_idle) {
      when(io.pad_ctrl.valid) {
        s_pad := pad_wait
        io.pad_ctrl.ready := true.B // tell the controller the pad has received data
        psum_result_idx_reg := psum_idx(io.pad_ctrl.bits.mcrenf) // reg the index
      }
      io.pad_ctrl.ready := false.B
    }
    is (pad_wait) {
      s_pad := pad_write
      io.pad_ctrl.ready := false.B
    }
    is (pad_write) {
      s_pad := pad_idle
      ps_data_spad(psum_result_idx_reg) := psum_result_reg //update the partial sum
    }
  }
  // Pad connection
  // TODO: add enable signal with zero buffer
  io.pad_mac.filter_cal := Mux( s_pad === pad_idle && io.pad_ctrl.valid, filter_data_spad.read(filter_idx(io.pad_ctrl.bits.mcrenf)), 0.U)
  io.pad_mac.ifmap_cal := Mux( s_pad === pad_wait, ifm_data_spad(ifm_idx(io.pad_ctrl.bits.mcrenf)), 0.U)
  io.pad_mac.psum_load := Mux( s_pad === pad_wait, ps_data_spad(psum_idx(io.pad_ctrl.bits.mcrenf)), 0.U)

}

class ProcessingElementMAC extends Module with PESizeConfig {
  val io = IO(new Bundle{
    // val mac_ctrl = Flipped(new PECtrlToMacIO)
    val mac_pad = Flipped(new PEPadToMacIO)
  })
  // TODO: add pipe scala param to improve clock frequency if needed.
  io.mac_pad.psum_result := io.mac_pad.psum_load + io.mac_pad.filter_cal * io.mac_pad.ifmap_cal //TODO: add zero buffer
}

class PECtrlToMacIO extends Bundle { // with ready valid
  val rst = Output(Reset)
  //val all_mac_is_done = Output(Bool())
}

class PEPadToMacIO extends Bundle with PESizeConfig {
  val ifmap_cal = Output(UInt(cal_data_width.W))
  val filter_cal = Output(UInt(cal_data_width.W))
  val psum_load = Output(UInt(cal_data_width.W))
  val psum_result = Input(UInt(cal_data_width.W))
}

class PECtrlToPadIO extends Bundle {
  val rst: Bool = Bool()
  val mcrenf: Vec[UInt] = Vec(6, UInt(5.W))
}

class DataWithIndex(val data_width: Int, val addr_width: Int) extends Bundle {
  val index: UInt = UInt(addr_width.W)
  val data: UInt = UInt(data_width.W)
}

trait MCRENFConfig extends PESizeConfig { // contains some scala values
  val M0: Int = 6 // weights reuse M0 times
  val C0: Int = 3 // different input feature maps and their weights reuse
  val R: Int = filter_height //
  val E: Int = ofmap_height // same row of weights in a PE
  val N0: Int = 2 // the number of partial sum
  val F0: Int = 2 // one row of partial sum
  val MCRENF: List[Int] = List(M0, C0, R, E, N0, F0)
}

trait SpadSizeConfig extends PESizeConfig {
  val ps_data_size: Int = 32
  val ifm_data_size: Int = 16 // 8 for data, 8 for count
  val ifm_addr_size: Int = 9
  val filter_data_size: Int = 96 // 48 for data, 48 for count
  val filter_addr_size: Int = 16
}

trait PESizeConfig { // TODO: move this config to a higher level package
  val filter_height: Int = 2
  val ofmap_height: Int = 2
  val fm_data_width: Int = 12
  val fm_addr_width: Int = 4
  val filter_data_width: Int = 24
  val filter_addr_width: Int = 7
  val cal_data_width: Int = 20
  val ps_data_width: Int = 20
  val fifo_size: Int = 4
}
