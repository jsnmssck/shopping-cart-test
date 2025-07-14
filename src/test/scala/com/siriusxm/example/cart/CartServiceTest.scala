package com.siriusxm.example.cart

import cats.effect.IO
import com.siriusxm.example.cart.CartService.Cart
import com.siriusxm.example.cart.Items.{Item, ItemName, Price}
import munit.CatsEffectSuite

class CartServiceTest extends CatsEffectSuite {
  val instance: CartService[IO] = CartService.make[IO]

  test("Add multiple products to cart") {
    val cart = Cart()
    val item = Item(ItemName("item1"), Price(BigDecimal(2.00)))
    for {
      c1 <- instance.addItem(cart, item, 2)
      c2 <- instance.addItem(c1, item, 1)
      c3 <- instance.addItem(c2, item, 2)
      contents = c3.contents
    } yield {
      assert(
        contents.get(ItemName("item1")).exists(_.quantity.value == 5)
      )
    }
  }

  test("Do not add non-positive quantities to the cart") {
    val cart = Cart()
    val item = Item(ItemName("item1"), Price(BigDecimal(2.00)))
    for {
      c1 <- instance.addItem(cart, item, 2)
      c2 <- instance.addItem(c1, item, -1)
    } yield {
      assert(c2.contents.get(ItemName("item1")).exists(_.quantity.value == 2))
    }
  }

  test("Calculate subtotal of items in cart") {
    val cart  = Cart()
    val item1 = Item(ItemName("item1"), Price(BigDecimal(2.00)))
    val item2 = Item(ItemName("item2"), Price(BigDecimal(1.50)))
    for {
      c1       <- instance.addItem(cart, item1, 1)
      c2       <- instance.addItem(c1, item2, 1)
      subtotal <- instance.itemSubtotal(c2)
    } yield {
      assert(subtotal == BigDecimal(3.50))
    }
  }

  test("Calculate taxes of items in cart, rounding up") {
    val cart  = Cart()
    val item1 = Item(ItemName("item1"), Price(BigDecimal(2.00)))
    val item2 = Item(ItemName("item2"), Price(BigDecimal(1.50)))
    for {
      c1       <- instance.addItem(cart, item1, 1)
      c2       <- instance.addItem(c1, item2, 1)
      subtotal <- instance.taxSubtotal(c2)
    } yield {
      assertEquals(subtotal, BigDecimal(0.44)) // raw = 0.4375
    }
  }

  test("Calculate final total including tax of items in cart") {
    val cart  = Cart()
    val item1 = Item(ItemName("item1"), Price(BigDecimal(2.00)))
    val item2 = Item(ItemName("item2"), Price(BigDecimal(1.50)))
    for {
      c1       <- instance.addItem(cart, item1, 1)
      c2       <- instance.addItem(c1, item2, 1)
      subtotal <- instance.cartTotal(c2)
    } yield {
      assertEquals(subtotal, BigDecimal(3.94))
    }
  }

}
