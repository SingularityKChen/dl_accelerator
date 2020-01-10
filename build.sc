import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._

object dla extends ScalaModule with PublishModule with ScalafmtModule {
  def scalaVersion = "2.11.12"

  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.2.1",
    //ivy"edu.berkeley.cs::chisel-testers2:0.1.2",
    ivy"edu.berkeley.cs::chiseltest:0.2-SNAPSHOT",
    ivy"com.lihaoyi::os-lib:0.2.7",
    ivy"com.lihaoyi::upickle:0.8.0",
    ivy"com.lihaoyi::ammonite-ops:1.7.1"
  )

  def publishVersion = "0.1.0"

  def pomSettings = PomSettings(
    description = "deep learning accelerator",
    organization = "SingularityKChen",
    url = "https://github.com/SingularityKChen/dl_accelerator",
    licenses = Seq(License.`BSD-3-Clause`),
    versionControl = VersionControl.github("SingularityKChen", "dl_accelerator"),
    developers = Seq(
      Developer("Singularity", "Chen, Chunyun", "https://github.com/SingularityKChen")
    )
  )

  object tests extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.1")

    def testFrameworks = Seq("utest.runner.Framework")
  }
}
