package pe

import chisel3._
import chisel3.util._

class ProcessingElement(val if_width: Int, val f_width: Int, val ps_width: Int, val fifo_size:Int, val filter_height: Int, val ofmap_height: Int) extends Module {
  val ps_spad_size : Int = 24
  val ifm_spad_size : Int = 12
  val filter_spad_size : Int = 224
  val spad_width : Int = if_width // bit-width for three scratch pad
  // some config of RS+
  val M0 : Int = 6 // weights reuse M0 times
  val C0 : Int = 3 // different input feature maps and their weights reuse
  val R : Int = filter_height //
  val E : Int = ofmap_height // same row of weights in a PE
  val N0 : Int = 2 // the number of partial sum
  val F0 : Int = 2 // one row of partial smu

  // IO of PE
  val io = IO(new Bundle{
    val ifmap = Flipped(Decoupled(UInt(if_width.W))) //input feature map
    val ips = Flipped(Decoupled(UInt(ps_width.W))) //input partial sum
    val filter = Flipped(Decoupled(UInt(f_width.W))) //filter weight
    val ops = Decoupled(UInt(ps_width.W))
  })

  // other components of PE
  val ifmap_fifo = Queue(io.ifmap, fifo_size) //FIFO for input feature map
  val filter_fifo = Queue(io.filter, fifo_size) //FIFO for filter
  val ips_fifo = Queue(io.ips, fifo_size) //FIFO for input partial sum
  val ifm_spad = RegInit(VecInit(Seq.fill(ifm_spad_size)(0.U(spad_width.W)))) //reg, input feature map scratch pad
  val filter_spad = SyncReadMem(filter_spad_size, UInt(spad_width.W)) //SRAM, filter scratch pad
  //val filter_spad = RegInit(VecInit(Seq.fill(filter_spad_size)(0.U(spad_width))))
  val ps_spad = RegInit(VecInit(Seq.fill(ps_spad_size)(0.U(spad_width.W)))) //reg, partial sum scratch pad
  val all_mac_is_done = RegInit(false.B) //true when all the mac has been done, then pe will keep in ps_idle TODO: produce the signal in PE control
  val m0, c0, r0, e0, n0, f0 = RegInit(0.U(5.W)) //maximum value of these are pow(2, 5) TODO: make it configurable
  //val
  // logic of PE
  // state machine, the for-loop of m0
  // ps_idle: if not all the MAC operations are done, then load
  // ps_load: load ifmap, filter weight, partial sum, then do the MAC
  // ps_cal:
  // ps_done: finish one mac, then jump to idle or load state
  val ps_idle :: ps_load :: ps_cal :: ps_done :: Nil = Enum(4)
  val s_per_mac = RegInit(ps_idle) // the state of the mac process
  val ifmap_cal = RegInit(0.U(f_width.W)) // the ifmap value
  val filter_cal = RegInit(0.U(f_width.W)) // the filter weight value
  val psum_load = RegInit(0.U(f_width.W)) // the old partial sum
  val psum_rst = RegInit(0.U(f_width.W)) // the result of the new partial sum after mac
  switch (s_per_mac) {
    is (ps_idle) {
      when (!all_mac_is_done) {
        s_per_mac := ps_load
      }
    }
    is (ps_load) {
      ifmap_cal := ifm_spad(c0 + r0 + e0 + n0 + f0) //FIXME: check the input feature map
      filter_cal := filter_spad.read(m0 + c0 + r0) // TODO: add enable signal with zero buffer
      psum_load := ps_spad(n0 + m0 + e0 + f0)
      s_per_mac := ps_cal
    }
    is (ps_cal) {
      psum_rst := psum_load + filter_cal * ifmap_cal
      s_per_mac := ps_done
    }
    is (ps_done) {
      ps_spad(n0 + m0 + e0 + f0) := psum_rst //update the partial sum
      m0 := m0 + 1.U
      when (m0 === M0.U) {
        m0 := 0.U
        c0 := c0 + 1.U
      }
      when ((c0 === C0.U) && (m0 === M0.U)) {
        c0 := 0.U
        r0 := r0 + 1.U
      }
      when ((r0 === R.U) && (c0 === C0.U) && (m0 === M0.U)) {
        r0 := 0.U
        e0 := e0 + 1.U
      }
      when ((e0 === E.U) && (r0 === R.U) && (c0 === C0.U) && (m0 === M0.U)) {
        e0 := 0.U
        n0 := n0 + 1.U
      }
      when ((n0 === N0.U) && (e0 === E.U) && (r0 === R.U) && (c0 === C0.U) && (m0 === M0.U)) {
        n0 := 0.U
        f0 := f0 + 1.U
      }
      when ((f0 === F0.U) && (n0 === N0.U) && (e0 === E.U) && (r0 === R.U) && (c0 === C0.U) && (m0 === M0.U)) {
        f0 := 0.U
      }

      when (all_mac_is_done) {
        s_per_mac := ps_idle
      }.otherwise{
        s_per_mac := ps_load
      }
    }
  }
}
