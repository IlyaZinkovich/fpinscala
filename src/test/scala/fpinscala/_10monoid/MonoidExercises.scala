package fpinscala._10monoid

import java.util.concurrent.Executors

import fpinscala._07parallelism.Par
import fpinscala._07parallelism.Par.Par
import fpinscala._08property.{Gen, Prop}
import org.scalatest.{FlatSpec, Matchers}

class MonoidExercises extends FlatSpec with Matchers {

  "Exercise 10.1" should "intAddition intMultiplication booleanOr booleanAnd" in {
    val intAddition: Monoid[Int] = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 + a2

      def zero: Int = 0
    }
    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 * a2

      def zero: Int = 1
    }
    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

      def zero: Boolean = false
    }
    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

      def zero: Boolean = true
    }

    val integers = List(1, 2, 3, 4)
    integers.foldLeft(intAddition.zero)(intAddition.op) should be(10)
    integers.foldLeft(intMultiplication.zero)(intMultiplication.op) should be(24)

    val booleans = List(true, false)
    booleans.foldLeft(booleanOr.zero)(booleanOr.op) should be(true)
    booleans.foldLeft(booleanAnd.zero)(booleanAnd.op) should be(false)
  }

  "Exercise 10.2" should "option" in {
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

      def zero: Option[A] = None
    }

    def dual[A](monoid: Monoid[A]): Monoid[A] = new Monoid[A] {
      def op(a1: A, a2: A): A = monoid.op(a2, a1)

      def zero: A = monoid.zero
    }

    def dualOptionMonoid[B]: Monoid[Option[B]] = dual(optionMonoid)

    val options = List(None, Some(1), Some(2))
    options.foldLeft(optionMonoid[Int].zero)(optionMonoid[Int].op) should be(Some(1))
    options.foldLeft(dualOptionMonoid[Int].zero)(dualOptionMonoid[Int].op) should be(Some(2))
  }

  "Exercise 10.3" should "endofunction" in {
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      def op(a1: A => A, a2: A => A): A => A = a1 compose a2

      def zero: A => A = (a: A) => a
    }
  }

  "Exercise 10.4" should "monoidLaws" in {
    val intAddition: Monoid[Int] = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 + a2

      def zero: Int = 0
    }

    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
      val identity: Prop = Prop.forAll(gen) { value =>
        m.op(m.zero, value) == value && m.op(value, m.zero) == value
      }
      val associativity = Prop.forAll(for {
        a <- gen
        b <- gen
        c <- gen
      } yield (a, b, c)) {
        case (a, b, c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c))
      }
      associativity && identity
    }

    Prop.run(monoidLaws(intAddition, Gen.choose(0, 10)), testCases = 10)
  }

  "Exercise 10.5" should "foldMap" in {
    val intAddition: Monoid[Int] = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 + a2

      def zero: Int = 0
    }

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
      as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
    }

    foldMap(List("1", "2", "3"), intAddition)(string => string.toInt) should be(6)
  }

  "Exercise 10.6" should "foldLeft and foldRight via foldMap" in {
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      def op(a1: A => A, a2: A => A): A => A = a1 compose a2

      def zero: A => A = (a: A) => a
    }

    def dual[A](monoid: Monoid[A]): Monoid[A] = new Monoid[A] {
      def op(a1: A, a2: A): A = monoid.op(a2, a1)

      def zero: A = monoid.zero
    }

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
      as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
    }

    def foldLeft[A, B](as: List[A])(z: B)(op: (B, A) => B): B = {
      foldMap(as, endoMonoid[B])(a => b => op(b, a))(z)
    }

    def foldRight[A, B](as: List[A])(z: B)(op: (A, B) => B): B = {
      foldMap(as, dual(endoMonoid[B]))(a => b => op(a, b))(z)
    }

    val doubles = List(1.0, 2.0, 3.0)
    val z = 1.0
    doubles.foldLeft(z)(_ / _) should be(0.16666666666666666)
    doubles.foldRight(z)(_ / _) should be(1.5)

    foldLeft(doubles)(z)(_ / _) should be(0.16666666666666666)
    foldRight(doubles)(z)(_ / _) should be(1.5)
  }

  "Exercise 10.7" should "foldMapV" in {
    val intAddition: Monoid[Int] = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 + a2

      def zero: Int = 0
    }

    def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      if (as.isEmpty) {
        m.zero
      } else if (as.size == 1) {
        f(as.head)
      } else {
        val (left, right) = as.splitAt(as.size / 2)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
      }
    }

    foldMapV(Vector("1", "2", "3"), intAddition)(_.toInt) should be(6)
  }

  "Exercise 10.8" should "parFoldMap" in {
    val intAddition: Monoid[Int] = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 + a2

      def zero: Int = 0
    }

    def par[A](m: Monoid[A]): Monoid[Par[A]] = {
      new Monoid[Par[A]] {
        override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)

        override def zero: Par[A] = Par.unit(m.zero)
      }
    }

    def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      if (as.isEmpty) {
        m.zero
      } else if (as.size == 1) {
        f(as.head)
      } else {
        val (left, right) = as.splitAt(as.size / 2)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
      }
    }

    def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
      Par.flatMap(Par.parMap(as.toList)(f)) { bs =>
        foldMapV(bs.toIndexedSeq, par(m))((b: B) => Par.lazyUnit(b))
      }
    }

    val es = Executors.newCachedThreadPool()
    parFoldMap(Vector("1", "2", "3"), intAddition)(_.toInt)(es).get() should be(6)
  }

  "Exercise 10.9" should "foldMap is IndexedSeq ordered" in {
    def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      if (as.isEmpty) {
        m.zero
      } else if (as.size == 1) {
        f(as.head)
      } else {
        val (left, right) = as.splitAt(as.size / 2)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
      }
    }

    def isOrdered[A](as: IndexedSeq[A])(order: (A, A) => Boolean): Boolean = {
      val orderDetector = new Monoid[Option[(A, A, Boolean)]] {
        def op(o1: Option[(A, A, Boolean)], o2: Option[(A, A, Boolean)]): Option[(A, A, Boolean)] = (o1, o2) match {
          case (Some((minLeft, maxLeft, orderedLeft)), Some((minRight, maxRight, orderedRight))) =>
            val min = if (order(minLeft, minRight)) minLeft else minRight
            val max = if (order(maxLeft, maxRight)) maxRight else maxLeft
            val leftNotGreaterThanRight = order(maxLeft, minRight) || maxLeft == minRight
            Some((min, max, orderedLeft && orderedRight && leftNotGreaterThanRight))
          case (x, None) => x
          case (None, x) => x
        }

        val zero: Option[(A, A, Boolean)] = None
      }

      foldMapV(as, orderDetector)(element => Some((element, element, true))).forall(_._3)
    }

    isOrdered(Vector(1, 2, 3, 4))((a1, a2) => a1 <= a2) should be(true)
    isOrdered(Vector(1, 2, 5, 3, 4))((a1, a2) => a1 <= a2) should be(false)
  }

  "Exercise 10.10" should "wcMonoid" in {
    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      def op(a1: WC, a2: WC): WC = (a1, a2) match {
        case (Stub(chars1), Stub(chars2)) => Stub(chars1 + chars2)
        case (Stub(chars), Part(lStub, words, rStub)) => Part(chars + lStub, words, rStub)
        case (Part(lStub, words, rStub), Stub(chars)) => Part(lStub, words, rStub + chars)
        case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
          Part(lStub1, words1 + (if ((rStub1 + lStub2).isEmpty) 0 else 1) + words2, rStub2)
      }

      def zero: WC = Stub("")
    }
  }

  "Exercise 10.11" should "word count" in {
    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      def op(a1: WC, a2: WC): WC = (a1, a2) match {
        case (Stub(chars1), Stub(chars2)) => Stub(chars1 + chars2)
        case (Stub(chars), Part(lStub, words, rStub)) => Part(chars + lStub, words, rStub)
        case (Part(lStub, words, rStub), Stub(chars)) => Part(lStub, words, rStub + chars)
        case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
          Part(lStub1, words1 + (if ((rStub1 + lStub2).isEmpty) 0 else 1) + words2, rStub2)
      }

      def zero: WC = Stub("")
    }

    def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      if (as.isEmpty) {
        m.zero
      } else if (as.size == 1) {
        f(as.head)
      } else {
        val (left, right) = as.splitAt(as.size / 2)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
      }
    }

    def wordCount(string: String): Int = {
      def unit(char: Char): WC = {
        if (char.isWhitespace)
          Part("", 0, "")
        else Stub(char.toString)
      }

      def unstub(s: String) = s.length min 1

      foldMapV(string.toIndexedSeq, wcMonoid)(unit) match {
        case Stub(chars) => unstub(chars)
        case Part(lStub, words, rStub) => unstub(lStub) + words + unstub(rStub)
      }
    }

    wordCount("daw damkvds wad, dwa2 ") should be(4)
  }

  "Exercise 10.12" should "Foldable[List], ..." in {
    new Foldable[List] {
      def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
        foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
    }
  }

  "Exercise 10.13" should "Foldable[Tree]" in {
    val foldableTree = new Foldable[Tree] {
      def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
        case Leaf(lValue) => f(lValue, z)
        case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
      }

      def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
        case Leaf(lValue) => f(z, lValue)
        case Branch(left, right) =>
          foldLeft(right)(foldLeft(left)(z)(f))(f)
      }

      def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
        case Leaf(lValue) => f(lValue)
        case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      }
    }
    val concatMonoid = new Monoid[String] {
      def op(a1: String, a2: String): String = a1 + a2

      def zero: String = ""
    }

    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    foldableTree.foldLeft(tree)(2)(Math.pow(_, _).toInt) should be(64)
    foldableTree.foldRight(tree)(2)((a, b) => Math.pow(b, a).toInt) should be(64)
    foldableTree.foldMap(tree)(_.toString)(concatMonoid) should be("123")
  }

  "Exercise 10.14" should "Foldable[Option]" in {
    val foldableOption = new Foldable[Option] {
      def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
        case None => z
        case Some(sValue) => f(sValue, z)
      }

      def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
        case None => z
        case Some(sValue) => f(z, sValue)
      }

      def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
        case None => mb.zero
        case Some(sValue) => f(sValue)
      }
    }

    foldableOption.foldMap(Some("value"))(_.toUpperCase)(new Monoid[String] {
      def op(a1: String, a2: String): String = a1 + a2

      def zero: String = ""
    }) should be("VALUE")
  }

  "Exercise 10.15" should "Foldable to List" in {
    val foldableTree = new Foldable[Tree] {
      def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
        case Leaf(lValue) => f(lValue, z)
        case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
      }

      def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
        case Leaf(lValue) => f(z, lValue)
        case Branch(left, right) =>
          foldLeft(right)(foldLeft(left)(z)(f))(f)
      }

      def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
        case Leaf(lValue) => f(lValue)
        case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      }
    }

    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    foldableTree.toList(tree) should be(List(1, 2, 3))
  }

  "Exercise 10.16" should "productMonoid" in {
    def productMonoid[A, B](monoidA: Monoid[A], monoidB: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      def op(a1: (A, B), a2: (A, B)): (A, B) = (monoidA.op(a1._1, a2._1), monoidB.op(a1._2, a2._2))

      def zero: (A, B) = (monoidA.zero, monoidB.zero)
    }
  }

  "Exercise 10.17" should "functionMonoid" in {
    def functionMonoid[A, B](monoidB: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
      def op(a1: A => B, a2: A => B): A => B = (a: A) => monoidB.op(a1(a), a2(a))

      def zero: A => B = (_: A) => monoidB.zero
    }
  }

  "Exercise 10.18" should "bag" in {
    def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
      as.foldLeft(Map[A, Int]())((map, elem) => map + (elem -> (map.getOrElse(elem, 0) + 1)))
    }

    bag(Vector(1, 2, 3, 1)) should be(Map(1 -> 2, 2 -> 1, 3 -> 1))

    def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map[K, V]()

      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

    val intAddition = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 + a2

      def zero: Int = 0
    }

    def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      if (as.isEmpty) {
        m.zero
      } else if (as.size == 1) {
        f(as.head)
      } else {
        val (left, right) = as.splitAt(as.size / 2)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
      }
    }

    def bagWithMonoid[A](as: IndexedSeq[A]): Map[A, Int] = {
      foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map[A, Int](a -> 1))
    }

    bagWithMonoid(Vector(1, 2, 3, 1)) should be(Map(1 -> 2, 2 -> 1, 3 -> 1))
  }
}
