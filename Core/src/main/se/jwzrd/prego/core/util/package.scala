package se.jwzrd.prego.core
/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
package object util {
  type Monadic[M[_], A] = {
    def flatMap[B](f: A => M[B]): M[B]
    def map[B](f: A => B): M[B]
  }

  class ZipM[M[X] <: Monadic[M, X], A] (val a: M[A]) {
    def zip[B](b: M[B]): M[(A, B)] = for {
      a1 <- a
      b1 <- b
    } yield (a1, b1)
  }

  implicit def optionHasZip[A](o: Option[A]): ZipM[Option, A] =
    new ZipM[Option, A](o)
}