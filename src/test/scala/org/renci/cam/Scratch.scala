package org.renci.cam

import zio.test.Assertion.equalTo
import zio.test._
import zio.{Runtime, ZEnv}

object Scratch extends DefaultRunnableSpec {

  implicit val runtime: Runtime[ZEnv] = Runtime.default

  def spec =
    suite("ScratchSpec")(
      test("1") {
        val asdf = for {
          list <- zio.ZIO.effect(List(1, 2, 3))
        } yield list
        val d = runtime.unsafeRun(asdf)
        assert(d)(equalTo(List(1, 2, 3)))
      }
    )

}
