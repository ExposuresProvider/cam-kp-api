package org.renci.cam

import zio.test.Assertion.equalTo
import zio.test.TestAspect._
import zio.test._

object Scratch extends DefaultRunnableSpec {

  def spec =
    suite("ScratchSpec")(
      testM("1") {
        for {
          list <- zio.ZIO.effect(List(1, 2, 3))
        } yield assert(list)(equalTo(List(1, 2, 3)))
      } @@ ignore
    )

}
