package fpinscala._07parallelism

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {

    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get(), bf.get()))
  }

  def map[A, B](parA: Par[A])(f: A => B): Par[B] = map2(parA, unit(()))((a, _) => f(a))

  def fork[A](a: => Par[A]): Par[A] = es => {
    es.submit(() => a(es).get())
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(Par.unit[List[A]](Nil))((par, acc) => map2(par, acc)(_ :: _))
  }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fbs: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(fbs))(_.flatten)
  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val index = n(es).get()
    choices(index)(es)
  }

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    choiceN(map(a)(aBoolean => if (aBoolean) 0 else 1))(List(ifTrue, ifFalse))
  }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
    flatMap(key)(choices)(es)
  }

  def flatMap[K, V](key: Par[K])(choices: K => Par[V]): Par[V] = es => {
    val keyResult = key(es).get()
    choices(keyResult)(es)
  }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = es => {
    flatMap(a)(key => key)(es)
  }

  def join[A](a: Par[Par[A]]): Par[A] = es => a(es).get.apply(es)

  def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] = join(map(p)(f))
}

