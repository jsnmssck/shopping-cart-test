package com.siriusxm.example.cart

import cats.Monad
import cats.implicits.toFunctorOps
import com.siriusxm.example.cart.Items.{Item, ItemName}

import io.circe._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._
import scala.language.implicitConversions

trait Items[F[_]] {
  def buildWithPrice(name: ItemName): F[Option[Item]]
}

object Items {
  implicit val config: Configuration = Configuration.default

  case class ItemName(value: String)
  object ItemName {
    implicit val itemNameDecoder: Decoder[ItemName] = deriveUnwrappedDecoder[ItemName]
  }

  case class Price(value: BigDecimal)
  object Price {
    implicit val priceDecoder: Decoder[Price] = deriveUnwrappedDecoder[Price]
  }

  case class Item(title: ItemName, price: Price)
  object Item {
    implicit val itemDecoder: Decoder[Item] = deriveConfiguredDecoder[Item]
  }

  def make[F[_]: Monad](priceService: PriceService[F]): Items[F] =
    (name: ItemName) => {
      for {
        itemF <- priceService.getPrice(name)
        item = itemF.toOption
      } yield item
    }
}
