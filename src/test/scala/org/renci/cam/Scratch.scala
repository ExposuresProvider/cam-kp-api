package org.renci.cam

import zio.interop.catz.implicits._
import zio.test.TestAspect._
import zio._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._

object Scratch extends DefaultRunnableSpec {

  def spec =
    suite("ScratchSpec")(
      testM("1") {
        for {
          list <- ZIO.effect(List(1, 2, 3))
        } yield assert(list)(equalTo(List(1, 2, 3)))
      } //@@ ignore
    )

}
