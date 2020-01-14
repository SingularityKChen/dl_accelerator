package dla.pe

import chisel3._
import chisel3.util._
import firrtl.annotations.TargetToken.Reset
import dla.dcsc
// TODO: add reset signal for every module
class ProcessingElement extends Module with PESizeConfig {
  val io = IO(new Bundle{
    val data_stream = new DataStreamIO
  })
  val iact_fifo = Module(new Queue(io.data_stream.iact_io, fifo_size, flow = true)) //FIFO for input feature map
  //
  val weight_fifo = Module(new Queue(io.data_stream.weight_io, fifo_size, flow = true)) //FIFO for weight
  // c0 === C0.U, weight_fifo.deq.valid := true.B
  val ips_fifo = Module(new Queue(io.data_stream.ips_io, fifo_size, flow = true)) //FIFO for input partial sum
  // r0 === R.U, ips_fifi.deq.valid := true.B
  val pe_ctrl = new ProcessingElementControl
  val pe_pad = new ProcessingElementPad
  pe_ctrl.io.ctrl_pad <> pe_pad.io.pad_ctrl
  pe_pad.io.data_stream.iact_io <> iact_fifo.io.deq
  pe_pad.io.data_stream.weight_io <> weight_fifo.io.deq
  pe_pad.io.data_stream.ips_io <> ips_fifo.io.deq
  val ops_fifo = Module(new Queue(pe_pad.io.data_stream.ops_io, fifo_size, flow = true)) //FIFO for output partial sum
  //
  io.data_stream.ops_io <> ips_fifo.io.deq
}

class ProcessingElementControl extends Module with MCRENFConfig {
  val io = IO(new Bundle{
    val ctrl_pad: DecoupledIO[PECtrlToPadIO] = Decoupled(new PECtrlToPadIO)
  })
  // some config of RS+

  // logic of PE MAC
  // state machine, the for-loop of m0
  // ps_idle: if not all the MAC operations are done, then load
  // ps_load: load input activations, weights, partial sums, then do the MAC
  // ps_done: finish one mac, then jump to idle or load state
  val ps_idle :: ps_load :: ps_done :: Nil = Enum(3)
  val s_per_mac = RegInit(ps_idle) // the state of the mac process
  val all_mac_is_done = RegInit(false.B) //true when all the mac has been done, then pe will keep in ps_idle
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
}

class ProcessingElementPad extends Module with MCRENFConfig with SpadSizeConfig {
  val io = IO(new Bundle{
    val pad_ctrl = Flipped(Decoupled(new PECtrlToPadIO))
    val data_stream = new DataStreamIO
  })
  val iact_data_spad = RegInit(VecInit(Seq.fill(iact_data_size)(0.U(iac_data_width.W)))) // reg, input feature map data scratch pad
  val iact_addr_spad = RegInit(VecInit(Seq.fill(iact_addr_size)(0.U(iac_addr_width.W)))) // reg, input feature map address scratch pad
  val weight_data_spad = SyncReadMem(weight_data_size, UInt(weight_data_width.W)) // SRAM, weight data scratch pad, two cycles to read
  val weight_addr_spad = RegInit(VecInit(Seq.fill(weight_addr_size)(0.U(weight_addr_width)))) // reg, weight address scratch pad
  val ps_data_spad = RegInit(VecInit(Seq.fill(ps_data_size)(0.U(ps_data_width.W)))) // reg, partial sum scratch pad
  val psum_result_wire: UInt = Wire(UInt(ps_data_width.W))
  val iact_addr_r: UInt = Wire(UInt(iac_addr_width.W)) // read wire for input activation address
  val weight_addr_r: UInt = Wire(UInt(weight_addr_width.W)) // read wire for weight address
  val iact_data_reg = RegInit(0.U(iac_data_width.W))
  val weight_data_reg = RegInit(0.U(weight_data_width.W))
  val psum_load_reg = RegInit(0.U(ps_data_width.W))
  val product_reg = RegInit(0.U(ps_data_width.W)) // reg the product

  val mcrenf_reg = RegInit(VecInit(Seq.fill(6)(0.U(log2Ceil(MCRENF.max).W))))
  val when_carry: IndexedSeq[Bool] = mcrenf_reg.zip(MCRENF.map(x=> x - 1)).map{ case (x,y) => x === y.U}
  // when_carry stores the information of whether m0 === M0.U, et al.
  val iact_io_read: Bool = Wire(Bool())
  val iact_io_read_index = RegNext(mcrenf_reg(1)) // use RegNext to buffer the index dut to write after read
  val weight_io_read: Bool = Wire(Bool())
  val weight_io_read_index = RegNext(mcrenf_reg(2)) // use RegNext to buffer the index dut to write after read
  iact_io_read := (when_carry.takeRight(4).reduce(_ && _) && mcrenf_reg(1) =/= 0.U) || RegNext(when_carry.reduce(_ && _)) // r0 === R0 && E0 && N0 && F0
  weight_io_read := (when_carry(1) && when_carry(2) && mcrenf_reg(0) =/= 0.U) || RegNext(when_carry.take(3).reduce(_ && _)) // r0 === R0 && c0 === C0
  // read when it's the last cycle of its read, but to delay one cycle for index FIXME: rethink the index
  // scratch pad write at the final for loop FIXME: take care of write the last index
  when (io.data_stream.iact_io.valid && iact_io_read) {
    io.data_stream.iact_io.ready := true.B
    iact_data_spad(mcrenf_reg(1) - 1.U) := io.data_stream.iact_io.bits.data
    iact_addr_spad(mcrenf_reg(1) - 1.U) := io.data_stream.iact_io.bits.addr
  }
  when (io.data_stream.weight_io.valid && weight_io_read) {
    io.data_stream.weight_io.ready := true.B
    weight_data_spad(0.U) := io.data_stream.weight_io.bits.data
    weight_addr_spad(0.U) := io.data_stream.weight_io.bits.addr
  }
  private def iact_idx(m: Vec[UInt]): UInt = m(1) + MCRENF(1).U*(m(2) + MCRENF(2).U*(m(3) + MCRENF(3).U*(m(4) + MCRENF(4).U*m(5))))
  // iactap_idx = c0 + r0*C0 + e0*R0*C0 + n0*E0*R0*C0 + f0*N0*E0*R0*C0 FIXME: check the input feature map
  private def weight_idx(m: Vec[UInt]): UInt = m(0) + MCRENF.head.U*(m(1) + MCRENF(1).U*m(2))
  // weight_idx = m0 + c0*M0 + r0*C0*M0
  private def psum_idx(m: Vec[UInt]): UInt = m(0) + MCRENF.head.U*(m(3) + MCRENF(3).U*(m(4) + MCRENF(4).U*m(5)))
  // psum_idx = m0 + e0*M0 + n0*E0*M0 + f0*N0*E0*M0
  val psum_result_idx_reg = RegInit(0.U(cal_data_width.W)) // store the index for write back
  // pad_idle: the pad is idle, and if received valid signal from control, then read and send data to mac
  // pad_iact_addr: read the input activation address
  // pad_iact_data: read the input activation data
  // pad_weight_addr: read the weight address
  // pad_weight_data1: read the weight data
  // pad_weight_data2: wait one cycle as SRAM
  // pad_mpy: wait for mac computation
  // pad_wb: write the partial sum back
  val pad_idle :: pad_iact_addr :: pad_iact_data :: pad_weight_addr :: pad_weight_data1 :: pad_weight_data2 :: pad_mpy :: pad_wb :: Nil = Enum(8)
  val s_pad = RegInit(pad_idle)
  // the_index_m0 = m0 + count_m0
  // addr_m0_index*M0
  switch (s_pad) {
    is (pad_idle) {
      when(io.pad_ctrl.valid) {
        s_pad := pad_iact_addr
        io.pad_ctrl.ready := true.B // tell the controller the pad has received data
        psum_result_idx_reg := psum_idx(mcrenf_reg) // reg the index
      }
      io.pad_ctrl.ready := false.B
    }
    is (pad_iact_addr) {
      s_pad := pad_iact_data
      io.pad_ctrl.ready := false.B
    }
    is (pad_iact_data) {
      s_pad := pad_weight_addr
    }
    is (pad_weight_data1) {
      s_pad := pad_weight_data2
    }
    is (pad_weight_data2) {
      s_pad := pad_mpy
    }
    is (pad_mpy) {
      s_pad := pad_wb
      product_reg := weight_data_reg * iact_data_reg
    }
    is (pad_wb) {
      s_pad := pad_idle
      psum_result_wire := Mux(io.pad_ctrl.bits.ps_enq_or_product, io.data_stream.ips_io.bits, product_reg) + psum_load_reg
      // FIXME: add ready valid signal for ips FIFO
      ps_data_spad(psum_result_idx_reg) := psum_result_wire //update the partial sum
    }
  }
  // Pad connection
  weight_data_reg := Mux( s_pad === pad_idle && io.pad_ctrl.valid, weight_data_spad.read(weight_idx(mcrenf_reg)), 0.U)
  iact_data_reg := Mux( s_pad === pad_iact_data, iact_data_spad(iact_idx(mcrenf_reg)), 0.U)
  psum_load_reg := Mux( s_pad === pad_mpy, ps_data_spad(psum_idx(mcrenf_reg)), 0.U)

}

class DataStreamIO extends Bundle with PESizeConfig {
  val iact_io = Flipped(Decoupled(new DataWithAddr(iac_data_width, iac_addr_width)))
  val weight_io = Flipped(Decoupled(new DataWithAddr(weight_data_width, weight_addr_width)))
  val ips_io = Flipped(Decoupled(UInt(ps_data_width.W)))
  val ops_io: DecoupledIO[UInt] = Decoupled(UInt(ps_data_width.W))
}

class PECtrlToPadIO extends Bundle {
  val rst: Bool = Bool()
  //val mcrenf: Vec[UInt] = Vec(6, UInt(5.W))
  val ps_enq_or_product: Bool = Bool() // true, then read from FIFO; false, then use product
  // TODO: set them to input or outpt with ready valid.
}

class DataWithAddr(val data_width: Int, val addr_width: Int) extends Bundle {
  val addr: UInt = UInt(addr_width.W)
  val data: UInt = UInt(data_width.W)
}

trait MCRENFConfig extends PESizeConfig { // contains some scala values
  val M0: Int = 6 // weights reuse M0 times
  val C0: Int = 3 // different input feature maps and their weights reuse
  val R: Int = weight_height //
  val E: Int = ofmap_height // same row of weights in a PE
  val N0: Int = 2 // the number of partial sum
  val F0: Int = 2 // one row of partial sum
  val MCRENF: List[Int] = List(M0, C0, R, E, N0, F0)
}

trait SpadSizeConfig extends PESizeConfig {
  val ps_data_size: Int = 32
  val iact_data_size: Int = 16
  val iact_addr_size: Int = 9
  val weight_data_size: Int = 192 // 96 if SIMD
  val weight_addr_size: Int = 16
}

trait PESizeConfig { // TODO: move this config to a higher level package
  val weight_height: Int = 2
  val ofmap_height: Int = 2
  val iac_data_width: Int = 12 // 8-bit data and 4-bit count
  val iac_addr_width: Int = 4
  val weight_data_width: Int = 12 // 24 if SIMD
  val weight_addr_width: Int = 7
  val csc_data_width: Int = 8 // compressed sparse column data width
  val csc_count_width: Int = 4 // compressed sparse column count width
  val cal_data_width: Int = 20
  val ps_data_width: Int = 20
  val fifo_size: Int = 4
}
