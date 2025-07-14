package com.siriusxm.example.cart

import cats.effect.IO
import com.siriusxm.example.cart.Items.{Item, ItemName}
import io.circe.parser._
import munit.CatsEffectSuite

class ItemsTest extends CatsEffectSuite {
  val instance: Items[IO] = Items.make[IO](testPriceService)

  test("build an item") {
    for {
      res <- instance.buildWithPrice(ItemName("Cheerios"))
    } yield {
      assert(
        res.isDefined &&
          res.exists(_.title == ItemName("Cheerios")) &&
          res.exists(_.price.value == cheeriosPrice)
      )
    }
  }

  test("return None for unknown item") {
    for {
      res <- instance.buildWithPrice(ItemName("Missing"))
    } yield {
      assert(res.isEmpty)
    }
  }

  test("Item is decodable") {
    val input =
      """
        |{
        |  "title": "Cheerios",
        |  "price": 8.43
        |}
        |""".stripMargin

    val result = decode[Item](input)
    assert(result.exists(i => i.title.value == "Cheerios" && i.price.value == BigDecimal(8.43)))

  }
}
