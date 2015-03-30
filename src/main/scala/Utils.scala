package nnsearch

import scala.collection.immutable.Stream

object Utils {
  def streamGen[Ctx, A]
    (init: Ctx)(gen: Ctx => Option[(A, Ctx)]): Stream[A] =
  {
    val op = gen(init)
    if (op.isEmpty) Stream.Empty
    else op.get._1 #:: streamGen(op.get._2)(gen)
  }
}
