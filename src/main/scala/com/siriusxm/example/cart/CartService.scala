package com.siriusxm.example.cart

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import com.siriusxm.example.cart.CartService.Cart
import com.siriusxm.example.cart.Items.{Item, ItemName}

import io.estatico.newtype.macros.newtype

trait CartService[F[_]] {

  def addItem(cart: Cart, item: Item, qty: Int): F[Cart]

  def itemSubtotal(cart: Cart): F[BigDecimal]

  def taxSubtotal(cart: Cart): F[BigDecimal]

  def cartTotal(cart: Cart): F[BigDecimal]

}

object CartService {

  @newtype case class Quantity(value: Int)

  case class CartItem(item: Item, quantity: Quantity) {
    def add(q: Int): CartItem = this.copy(quantity = Quantity(value = this.quantity.value + q))
  }

  case class Cart(contents: Map[ItemName, CartItem])

  object Cart {
    def apply(): Cart = Cart(Map.empty)
  }

  def make[F[_]: Sync](implicit m: Monad[F]): CartService[F] = new CartService[F] {

    private val taxRate = m.pure(BigDecimal(0.125))

    override def addItem(cart: Cart, item: Item, qty: Int): F[Cart] = {
      val updatedItem = cart.contents.get(item.title).fold(CartItem(item, Quantity(qty)))(_.add(qty))
      m.pure(Cart(cart.contents.updated(item.title, updatedItem)))
    }

    override def itemSubtotal(cart: Cart): F[BigDecimal] = {
      m.pure(
        cart.contents.foldLeft(BigDecimal(0)) { (agg, tup) =>
          val (_, ci) = tup
          agg + ci.item.price.value * ci.quantity.value
        }
      )
    }

    override def taxSubtotal(cart: Cart): F[BigDecimal] =
      itemSubtotal(cart).flatMap(is => taxRate.map(_ * is).map(_.setScale(2, BigDecimal.RoundingMode.UP)))

    override def cartTotal(cart: Cart): F[BigDecimal] =
      itemSubtotal(cart).flatMap(is => taxSubtotal(cart).map(_ + is))
  }
}
