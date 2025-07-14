package com.siriusxm.example.cart

import cats.MonadError
import cats.effect._
import cats.implicits.catsSyntaxApplicativeError
import com.siriusxm.example.cart.Items._
import io.circe._
import io.circe.jawn.decode
import okhttp3.OkHttpClient

trait PriceService[F[_]] {
  def getPrice(item: ItemName): F[Either[Error, Item]]
}

object PriceService {

  def make[F[_]](implicit
      mc: MonadCancel[F, Throwable]
  ): PriceService[F] = { (item: ItemName) =>
    {
      okHttpClientResource.use { client =>
        mc.pure {
          val request = new okhttp3.Request.Builder()
            .url(
              s"https://raw.githubusercontent.com/mattjanks16/shopping-cart-test-data/main/${item.value.toLowerCase.replaceAll(" ", "")}.json"
            )
            .build()
          val response = client.newCall(request).execute()
          val body     = response.body().string()
          decode[Item](body)
        }
      }
    }
  }

  private def okHttpClientResource[F[_]](implicit
      me: MonadError[F, Throwable]
  ): Resource[F, OkHttpClient] =
    Resource.make(
      me.pure {
        new OkHttpClient.Builder()
          .build()
      }
    )(client =>
      me
        .pure {
          client.dispatcher().executorService().shutdown()
          client.connectionPool().evictAll()
        }
        .handleErrorWith(_ => me.unit)
    )

}
