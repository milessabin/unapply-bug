package unapplybug

import scalaz._
import scalaz.std.option._
import scalaz.std.string._

// From shapeless-contrib: this should go into scalaz proper, once codegen for Unapply is in place
trait UnapplyAux[TC[_[_]], FA, F[_], A] {
  def TC: TC[F]
  def apply(fa: FA): F[A]
}

object UnapplyAux {
  implicit def unapplyAux[TC[_[_]], FA](implicit ev: Unapply[TC, FA]): UnapplyAux[TC, FA, ev.M, ev.A] = new UnapplyAux[TC, FA, ev.M, ev.A] {
    def TC = ev.TC
    def apply(fa: FA) = ev(fa)
  }
}

object UnapplyBug {
  def mkUnapply[FA](f: FA)
    (implicit un: Unapply[Functor, FA]): Unapply[Functor, FA] { type M[x] = un.M[x] ; type A = un.A } = un

  def mkUnapplyAux[FA, F[_], A](f: FA)
    (implicit un: UnapplyAux[Functor, FA, F, A]): UnapplyAux[Functor, FA, F, A] = un

  val oi = Option(23)
  val vis: Validation[String, Int] = Success(13)

  mkUnapply(oi)
  mkUnapply(vis)

  mkUnapplyAux(oi)
  //mkUnapplyAux(vis) // fails with 2.11.x

  // I would actually prefer this variant to work if possible ...
  type UAAux[TC[_[_]], FA, F[_], A0] = Unapply[TC, FA] { type M[x] = F[x] ; type A = A0 }
  
  def mkUAAux[FA, F[_], A](f: FA)
    (implicit un: UAAux[Functor, FA, F, A]): UAAux[Functor, FA, F, A] = un

  //mkUAAux(oi)     // fails with 2.10.x and 2.11.x
  //mkUAAux(vis)    // fails with 2.10.x and 2.11.x
}
