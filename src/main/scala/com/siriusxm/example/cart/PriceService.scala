package com.siriusxm.example.cart

import cats.effect._
import cats.implicits.catsSyntaxApplicativeError
import cats.{Applicative, Monad, MonadError}
import com.siriusxm.example.cart.Items._
import io.circe._
import io.circe.jawn.decode
import okhttp3.OkHttpClient

trait PriceService[F[_]] {
  def getPrice(item: ItemName): F[Either[Error, Item]]
}

object PriceService {

  def make[F[_]: Monad: LiftIO](implicit
      mc: MonadCancel[F, Throwable]
  ): PriceService[F] = { (item: ItemName) =>
    {
      okHttpClientResource.use { client =>
        implicitly[Monad[F]].pure {
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

  private def okHttpClientResource[F[_]: Applicative](implicit
      me: MonadError[F, Throwable],
      app: Applicative[F]
  ): Resource[F, OkHttpClient] =
    Resource.make(
      app.pure {
        new OkHttpClient.Builder()
          .build()
      }
    )(client =>
      app
        .pure {
          client.dispatcher().executorService().shutdown()
          client.connectionPool().evictAll()
        }
        .handleErrorWith(_ => app.unit)
    )

}
