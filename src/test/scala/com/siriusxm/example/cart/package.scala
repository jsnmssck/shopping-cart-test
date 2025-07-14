package com.siriusxm.example

import cats.effect.IO
import com.siriusxm.example.cart.Items.{Item, ItemName, Price}
import io.circe.ParsingFailure

package object cart {

  val cheeriosPrice: BigDecimal          = BigDecimal(3.99)
  val testPriceService: PriceService[IO] = (item: ItemName) => {
    if (item.value == "Cheerios") IO(Right(Item(item, Price(cheeriosPrice))))
    else IO(Left(ParsingFailure("test error", new RuntimeException("parsing error"))))
  }
}
