package com.example

import cats.data.OptionT
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp}
import com.siriusxm.example.cart.Items.ItemName
import com.siriusxm.example.cart.{CartService, Items, PriceService}

object Main extends IOApp.Simple {
  private val priceService: PriceService[IO] = PriceService.make[IO]
  private val itemsService: Items[IO]        = Items.make[IO](priceService)
  private val cartService: CartService[IO]   = CartService.make[IO]

  override def run: IO[Unit] = {
    /*
        Add 2 × cornflakes @ 2.52 each
        Add 1 × weetabix @ 9.98 each
        Subtotal = 15.02
        Tax = 1.88
        Total = 16.90
     */
    val cart                        = CartService.Cart()
    val cartLoad: OptionT[IO, Unit] = for {
      cornflakes <- OptionT(itemsService.buildWithPrice(ItemName("Corn Flakes")))
      weetabix   <- OptionT(itemsService.buildWithPrice(ItemName("Weetabix")))
      c1         <- OptionT.liftF(cartService.addItem(cart, cornflakes, 2))
      c2         <- OptionT.liftF(cartService.addItem(c1, weetabix, 1))
      subtotal   <- OptionT.liftF(cartService.itemSubtotal(c2))
      tax        <- OptionT.liftF(cartService.taxSubtotal(c2))
      total      <- OptionT.liftF(cartService.cartTotal(c2))
      _          <- OptionT.liftF(IO(println(s"subtotal=$subtotal tax=$tax cartTotal=$total")))
      _          <- OptionT.liftF(IO(println(c2.contents)))
    } yield ()

    IO(cartLoad.value.unsafeRunSync())

  }
}
