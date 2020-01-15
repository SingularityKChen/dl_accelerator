package dla.tests

import utest._
import org.scalatest._
import chisel3._
import chisel3.tester._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import dla.pe.CounterToWrite
class CounterToWriteSpecTest extends FlatSpec with ChiselScalatestTester with Matchers {
  it should "increase the index" in {
    test(new CounterToWrite(4, 12)) { c =>
      c.io.ctw.data_len.poke(5.U)
      c.io.ctw.pad_io_valid.poke(true.B)
      c.io.ctw.write_idx.expect(0.U,"the index should be 0")
      c.clock.step(3)
      c.io.ctw.write_idx.expect(3.U,"the index should be 3")
      c.io.ctw.write_finish.expect(false.B, "not yet")
      c.clock.step(1)
      c.io.ctw.write_idx.expect(4.U,"the index should be 4")
      c.io.ctw.write_finish.expect(true.B, "yes, finshed")
      c.clock.step(1)
      c.io.ctw.pad_io_valid.poke(false.B) // not valid now
      c.io.ctw.write_idx.expect(0.U,"the index should be 4 + 1 = carry, 0")
      c.io.ctw.write_finish.expect(false.B, "not yet")
      c.clock.step(1)
      c.io.ctw.write_idx.expect(0.U,"the index should be 0 + 0 = 0")
      c.io.ctw.write_finish.expect(false.B, "not yet")
      c.clock.step(1)
      c.io.ctw.pad_io_valid.poke(true.B) // begin increase
      c.clock.step(1)
      c.io.ctw.write_idx.expect(1.U,"the index should be 0 + 1 = 1")
      c.io.ctw.write_finish.expect(false.B, "not yet")
      c.io.ctw.pad_io_valid.poke(false.B) // not valid now
      c.clock.step(1)
      c.io.ctw.write_idx.expect(1.U,"the index should be 1 + 0 = 1")
      c.io.ctw.write_finish.expect(false.B, "not yet")
      c.clock.step(1)
      c.io.ctw.pad_io_valid.poke(true.B) // begin increase
      c.io.ctw.write_idx.expect(1.U,"the index should be 1 but will increase the next cycle")
      c.io.ctw.write_finish.expect(false.B, "not yet")
      c.clock.step(1)
      c.io.ctw.write_idx.expect(2.U,"the index should be 1 + 1 = 2")
      c.io.ctw.write_finish.expect(false.B, "not yet")
    }
  }
}