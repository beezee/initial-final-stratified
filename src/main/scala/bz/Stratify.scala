package bz

import scala.language.higherKinds
import scalaz.~>

// G and H are Tagless Initial Encodings
trait Stratify[G[_[_], _], H[_[_], _], F[_]] {

  def apply[A](fa: G[F, A]): H[F, A]
}

object Stratify {

  def transform[G[_[_], _], H[_[_], _], F[_]](implicit S: Stratify[G, H, F]) = {
    type GF[A] = G[F, A]
    type HF[A] = H[F, A]
    new (GF ~> HF) {
      def apply[A](gf: GF[A]) = S(gf)
    }
  }
}
