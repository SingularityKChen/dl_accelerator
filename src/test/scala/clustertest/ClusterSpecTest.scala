package dla.test.clustertest

import chisel3._
import chisel3.tester._
import dla.cluster._
import org.scalatest._

class ClusterSpecTest extends FlatSpec with ChiselScalatestTester with Matchers{
  behavior of "test the spec of cluster group"
  it should "work well on GLB Cluster" in {}
  it should "work well on Router Cluster" in {}
  it should "work well on Processing Element Cluster" in {}
  it should "work well on Cluster Group" in {}
}
