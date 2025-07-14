package com.siriusxm.example.cart

import cats.effect.IO
import com.siriusxm.example.cart.Items.ItemName
import munit.CatsEffectSuite

class PriceServiceIT extends CatsEffectSuite {

  val instance: PriceService[IO] = PriceService.make[IO]

  test("get a price from valid endpoint") {
    val result = instance.getPrice(ItemName("Cheerios"))
    val out    = result.unsafeRunSync()
    assert(out.isRight && out.exists(i => i.title.value == "Cheerios" && i.price.value > 0))
  }

  test("return error from invalid endpoint") {
    val result = instance.getPrice(ItemName("Unknown"))
    val out    = result.unsafeRunSync()
    assert(out.isLeft)
  }
}
