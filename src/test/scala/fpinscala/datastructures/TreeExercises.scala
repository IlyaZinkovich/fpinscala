package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class TreeExercises extends FlatSpec with Matchers {

  "Exercise 3.25" should "size" in {
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    size(Leaf("leaf")) should be(1)
    size(Branch(Leaf("leaf1"), Leaf("leaf2"))) should be(3)
    size(Branch(Leaf("leaf1"), Branch(Leaf("leaf2"), Leaf("leaf3")))) should be(5)
  }

  "Exercise 3.26" should "max" in {
    def max(tree: Tree[Int]): Int = tree match {
      case Leaf(v) => v
      case Branch(left, right) => max(left) max max(right)
    }

    max(Leaf(1)) should be(1)
    max(Branch(Leaf(2), Leaf(1))) should be(2)
    max(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) should be(3)
  }

  "Exercise 3.27" should "depth" in {
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

    depth(Leaf(1)) should be(0)
    depth(Branch(Leaf(2), Leaf(1))) should be(1)
    depth(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) should be(2)
  }

  "Exercise 3.28" should "map" in {
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    map(Leaf(1))(_ + 1) should be(Leaf(2))
    map(Branch(Leaf(2), Leaf(1)))(_ + 1) should be(Branch(Leaf(3), Leaf(2)))
  }

  "Exercise 3.29" should "fold" in {
    def fold[A, B](tree: Tree[A])(leafMapper: A => B)(f: (B, B) => B): B = tree match {
      case Leaf(v) => leafMapper(v)
      case Branch(left, right) => f(fold(left)(leafMapper)(f), fold(right)(leafMapper)(f))
    }

    def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((left, right) => 1 + left + right)

    def max(tree: Tree[Int]): Int = fold(tree)(v => v)((left, right) => left max right)

    def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((left, right) => 1 + (left max right))

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      fold(tree)(v => Leaf(f(v)): Tree[B])((left, right) => Branch(left, right))

    size(Leaf("leaf")) should be(1)
    size(Branch(Leaf("leaf1"), Leaf("leaf2"))) should be(3)
    size(Branch(Leaf("leaf1"), Branch(Leaf("leaf2"), Leaf("leaf3")))) should be(5)

    max(Leaf(1)) should be(1)
    max(Branch(Leaf(2), Leaf(1))) should be(2)
    max(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) should be(3)

    depth(Leaf(1)) should be(0)
    depth(Branch(Leaf(2), Leaf(1))) should be(1)
    depth(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) should be(2)

    map(Leaf(1))(_ + 1) should be(Leaf(2))
    map(Branch(Leaf(2), Leaf(1)))(_ + 1) should be(Branch(Leaf(3), Leaf(2)))
  }
}
