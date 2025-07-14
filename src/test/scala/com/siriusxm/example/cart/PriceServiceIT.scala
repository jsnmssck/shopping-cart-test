package com.siriusxm.example.cart

import cats.effect.IO
import com.siriusxm.example.cart.Items.ItemName
import munit.CatsEffectSuite

class PriceServiceIT extends CatsEffectSuite {

  val instance: PriceService[IO] = PriceService.make[IO]

  test("get a price from valid endpoint") {
    for {
      item <- instance.getPrice(ItemName("Cheerios"))
    } yield assert(item.isRight && item.exists(i => i.title.value == "Cheerios" && i.price.value > 0))
  }

  test("return error from invalid endpoint") {
    for {
      badItem <- instance.getPrice(ItemName("Unknown"))
    } yield assert(badItem.isLeft)
  }
}
