import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._
import $file.chisel3.build
import $file.firrtl.build

object myfirrtl extends firrtl.build.firrtlCrossModule("2.12.10") {
  override def millSourcePath = super.millSourcePath / 'firrtl
}

object mychisel3 extends chisel3.build.chisel3CrossModule("2.12.10") {
  override def millSourcePath = super.millSourcePath / 'chisel3
  def firrtlModule: Option[PublishModule] = Some(myfirrtl)
}

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.12.10"

  override def scalacOptions = Seq("-Xsource:2.11")

  override def moduleDeps: Seq[ScalaModule] = Seq(mychisel3)

  private val macroParadise = ivy"org.scalamacros:::paradise:2.1.0"

  override def compileIvyDeps = Agg(macroParadise)

  override def scalacPluginIvyDeps = Agg(macroParadise)
}

object treadle extends CommonModule with SbtModule {
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.scala-lang.modules:scala-jline:2.12.1",
    ivy"org.json4s::json4s-native:3.6.7"
  )
}

object chiseltest extends CommonModule with SbtModule {
  override def moduleDeps: Seq[ScalaModule] = super.moduleDeps ++ Seq(treadle)
  
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.lihaoyi::utest:0.7.4",
    ivy"com.lihaoyi::os-lib:latest.integration",
    ivy"org.scalatest::scalatest:3.0.8"
  )

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.8",
      ivy"org.scalacheck::scalacheck:1.14.3",
    )

    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

object dla extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.12.10"

  override def moduleDeps: Seq[ScalaModule] = Seq(mychisel3, chiseltest)

  override def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.2.7",
    ivy"com.lihaoyi::upickle:0.8.0",
    ivy"com.lihaoyi::ammonite-ops:1.7.1",
    ivy"org.scalanlp::breeze:1.0"
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
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.8",
      ivy"org.scalacheck::scalacheck:1.14.3",
    )

    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}
