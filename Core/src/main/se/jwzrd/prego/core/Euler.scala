package se.jwzrd.prego.core

import annotation.tailrec
import java.lang.String
import collection.immutable.List
import java.util.ArrayList

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
object Euler {
  lazy val fibonacci: Stream[Long] = 1 #:: 2 #:: (fibonacci.zip (fibonacci tail) map (p => p._1 + p._2))
  lazy val longNumbers: Stream[Long] = 1 #:: longNumbers map (1 +)
  lazy val intNumbers: Stream[Int] = 1 #:: intNumbers map (1 +)

/*
  def sieve: Stream[Long] = {
    def strikeOut(ns: Stream[Long]): Stream[Long] = {
      val h = ns head

      h #:: strikeOut(ns.tail.filter (_ % h != 0L))
    }

    strikeOut(numbers)
  }
*/

/*
  def foo[A](a: Stream[A], b: Stream[A])(implicit i: Integral[A]): A = { import i._; a.head % b.head }

  val l: Long = foo(Stream.cons(2L, Stream.empty), Stream.cons(3L, Stream.empty))
*/

  def sieve[A](source: Stream[A])(implicit i: Integral[A]): Stream[A] = {
    def strikeOut[A](ns: Stream[A])(implicit i: Integral[A]): Stream[A] = {
      import i._;
      val h = ns head

      strikeOut(ns.tail.filter { _ % h != 0L})
    }

    strikeOut(source)
  }

  def primeFactors(n: Long): Stream[Long] = {
    val limit: Double = math.sqrt(n)

    sieve(longNumbers) takeWhile (limit >) filter (n % _ == 0L)
  }

  def largestPrimeFactor(n: Long) = {
    var quote = n

    primeFactors(n) takeWhile { f => quote /= f; quote != 1 } max
  }

  def isPalindromic(n: Long): Boolean =
    isPalindromic (n toString)

  def isPalindromic(s: String): Boolean =
    s == s.reverse

  def isPrime(x: Int, xs: List[Int]): Boolean =
    xs forall (x % _ > 0)

  def isPrime(x: Int, xs: ArrayList[Int]): Boolean = {
    import collection.JavaConversions._
    val cutOff = math.sqrt(x)
    xs takeWhile (cutOff >= _) forall (x % _ > 0)
  }

  @tailrec
  def nextPrime(start: Int, history: List[Int]): (Int, List[Int]) = {
    val candidate = start + 1
    if (isPrime(candidate, history))
      (candidate, candidate :: history)
    else
      nextPrime(candidate, history)
  }

  def nthPrime(n: Int): Int = {
    @tailrec
    def compute(start: Int, left: Int, history: List[Int]): Int = {
      if (left >= 0) {
        val (prime, history1) = nextPrime(start, history)
        compute(prime, left - 1, history1)
      } else start
    }

    compute(1, n - 1, Nil)
  }

  def sumPrimesBelow(n: Int): Int = {
    class Result(var sum: Int, var history: ArrayList[Int])

    val r = intNumbers.takeWhile(n >).foldLeft(new Result(0, new ArrayList[Int](32766))) {
      (r, candidate) =>
        if (isPrime(candidate, r.history)) {
          r.sum += candidate
          r.history.add(candidate)
          r
        } else r
    }
    r.sum
  }

  def chunkBy(source: String, size: Int): List[String] = {
    def nextChunk(s: String, chunks: List[String]): List[String] = s splitAt(5) match {
      case (head, tail) => head :: (if (tail isEmpty) chunks else nextChunk(tail, chunks)) 
      case _ => chunks
    }

    nextChunk(source, Nil) reverse
  }

  def chompingChunkBy(source: String, chunkBy: Int, chompBy: Int): List[String] = {
    def chompAndChunk(s: String, chunks: List[String]): List[String] = {
      chompBySplitBy(s, chompBy, chunkBy) match {
        case (chunk, rest) => chunk :: (if (rest isEmpty) chunks else chompAndChunk(rest, chunks))
        case _ => chunks
      }
    }

    chompAndChunk(source, Nil)
  }

  def chompBySplitBy(s: String, chomp: Int, split: Int): (String, String) =
    (s take(split), if (s.length >= split + chomp) s drop(chomp) else "")

  def digitProduct(s: String) = s map (_ asDigit) product

  def main(args: Array[String]) {
/*
    // #1
    val sum = (0 until 1000) filter { i => i % 3 == 0 || i % 5 == 0 } reduceLeft (_ + _)
    println(sum)

    // #2
    val sum2 = fibonacci filter (_ % 2 == 0) takeWhile (4000000L >=) reduceLeft (_ + _)
    println(sum2)

    // #3
    val primeFactor = largestPrimeFactor(600851475143L)
    println(primeFactor)

    // #4
    val palindrome = (for {p <- 100 to 999
                           q <- 100 to 999
                           if isPalindromic(p * q) } yield p * q) max

    println(palindrome)

    // #5
    val range = (1L to 20L)
    val unit = sieve(longNumbers) takeWhile (20L >) reduceLeft (_ * _)

    // all candidates are atleast divisible by all primes in range
    lazy val candidates: Stream[Long] = unit #:: candidates map (unit +)
    
    val n = candidates find { p => range forall (p % _ == 0L) }
    println(n)

    // #6
    val (sumOfSquares, sum3) = ((0, 0) /: (1 to 100)) { (b, a) => (b._1 + a * a, b._2 + a) }
    val diff = sum3 * sum3 - sumOfSquares
    println(diff)

    // #7
    println("prime 10001: " + nthPrime(10001))

    // #8
    val number = """
      73167176531330624919225119674426574742355349194934
      96983520312774506326239578318016984801869478851843
      85861560789112949495459501737958331952853208805511
      12540698747158523863050715693290963295227443043557
      66896648950445244523161731856403098711121722383113
      62229893423380308135336276614282806444486645238749
      30358907296290491560440772390713810515859307960866
      70172427121883998797908792274921901699720888093776
      65727333001053367881220235421809751254540594752243
      52584907711670556013604839586446706324415722155397
      53697817977846174064955149290862569321978468622482
      83972241375657056057490261407972968652414535100474
      82166370484403199890008895243450658541227588666881
      16427171479924442928230863465674813919123162824586
      17866458359124566529476545682848912883142607690042
      24219022671055626321111109370544217506941658960408
      07198403850962455444362981230987879927244284909188
      84580156166097919133875499200524063689912560717606
      05886116467109405077541002256983155200055935729725
      71636269561882670428252483600823257530420752963450
      """.replaceAll("\\s*", "")

    val greatestProductOfFiveConsecutiveDigits = chompingChunkBy (number, 5, 1) map (digitProduct) max

    println(greatestProductOfFiveConsecutiveDigits)

    // #9
    val pythagoreanTriplets = for {c <- intNumbers;
                                   b <- 1 until c;
                                   a <- 1 until b;
                                   if ((a * a + b * b == c * c)) && (a + b + c == 1000) } yield a * b * c
    println(pythagoreanTriplets head)
*/
/*

    val s = (1 to 10).foldLeft(0)((b, a) => b + a)

    println(s)

    // #10
    val start = System.currentTimeMillis
    val q = sumPrimesBelow(2000000)
    val end = System.currentTimeMillis
    println(q + "; took: " + (end -start))
*/
    // #9
    val pythagoreanTriplets = for {c <- intNumbers
                                   b <- 1 until c
                                   a <- 1 until b
                                   if ((a * a + b * b == c * c)) && (a + b + c == 1000) } yield a * b * c
  }
}