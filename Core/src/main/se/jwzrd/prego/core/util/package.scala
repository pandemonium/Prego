package se.jwzrd.prego.core
/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
package object util {
  type Monadic[M[_], A] = {
    def flatMap[B](f: A => M[B]): M[B]
    def map[B](f: A => B): M[B]
  }

  class ZipM[M[X] <: Monadic[M, X], A] (val ma: M[A]) {
    def zip[B](mb: M[B]): M[(A, B)] = for {
      a <- ma
      b <- mb
    } yield (a, b)
  }

  class FoldOption[A] (val oa: Option[A]) {
    def fold[B](f: A => B, b: => B): B =
      oa map f getOrElse b
/*
    def fold[B](f: A => Option[B], b: => B): B =
      oa flatMap f getOrElse b*/
  }

  implicit def optionHasZip[A](o: Option[A]): ZipM[Option, A] =
    new ZipM[Option, A](o)

  implicit def optionHasFold[A](o: Option[A]): FoldOption[A] =
    new FoldOption[A](o)
}