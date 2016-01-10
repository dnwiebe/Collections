package cse.collections

import org.scalatest.path

import scala.collection.immutable.BitSet.BitSetN
import scala.collection.immutable._

/**
  * Created by dnwiebe on 1/9/16.
  */

object PerformanceTest {

  trait Operation {
    def apply[C <: Iterable[_]] (collection: C, iterations: Int): Long = {
      (1 to iterations).foldLeft (0L) {(soFar, _) => soFar + once (collection)} / iterations
    }

    protected def op[C <: Iterable[_]] (collection: C): () => Unit

    private def once[C <: Iterable[_]] (collection: C): Long = {
      val start = System.nanoTime()
      op (collection)
      System.nanoTime () - start
    }
  }

  class Head extends Operation {
    override protected def op[C <: Iterable[_]] (collection: C) = () => collection.head
  }

  class Tail extends Operation {
    override protected def op[C <: Iterable[_]] (collection: C) = () => collection.tail
  }

  class Last extends Operation {
    override protected def op[C <: Iterable[_]] (collection: C) = () => collection.last
  }

  class Size extends Operation {
    override protected def op[C <: Iterable[_]] (collection: C) = () => collection.size
  }

  class Apply extends Operation {
    override protected def op[C <: Iterable[_]] (collection: C) = () => {
      collection.asInstanceOf[Seq[_]] (collection.size / 2)
    }
  }

  class Prepend extends Operation {
    override protected def op[C <: Iterable[_]] (collection: C) = () => {
      collection.asInstanceOf[Seq[_]].+: (collection.head)
    }
  }

  class Append extends Operation {
    override protected def op[C <: Iterable[_]] (collection: C) = () => {
      collection.asInstanceOf[Seq[_]].:+ (collection.head)
    }
  }

  val iterableOps = List (new Head, new Tail, new Last, new Size)
  val seqOps = iterableOps ++ List (new Apply, new Prepend, new Append)
  val SIZE = 100000
  val collections: List[(String, Iterable[_], Int)] = List (
    ("Vector", Vector () ++ (1 to SIZE), 1),
    ("NumericRange", new NumericRange.Inclusive[Int] (1, SIZE, 1), 100),
    ("String", (1 to SIZE).foldLeft ("") {(soFar, _) => soFar + "x"}, 100),
    ("Range", new Range (1, SIZE, 1), 100),
    ("List", (1 to SIZE).toList, 100),
    ("Stream", (1 to SIZE).toStream, 100),
    ("Queue", Queue () ++ (1 to SIZE), 100),
    ("Stack", new Stack () ++ (1 to SIZE), 100),
    ("HashSet", new HashSet () ++ (1 to SIZE), 100),
    ("TreeSet", new TreeSet () (Ordering.Int) ++ (1 to SIZE), 100),
    ("BitSet", new BitSetN ((0L to (SIZE / 32L)).toArray), 100),
    ("ListSet", new ListSet () ++ (1 to SIZE), 100),
    ("HashMap", new HashMap () ++ (1 to SIZE).map {e => (e, e)}.toMap, 100),
    ("TreeMap", new TreeMap () (Ordering.Int) ++ (1 to SIZE).map {e => (e, e)}.toMap, 100),
    ("ListMap", new ListMap () ++ (1 to SIZE).map {e => (e, e)}.toMap, 100)
  )
}

class PerformanceTest extends path.FunSpec {

  import PerformanceTest._

  describe ("Performance testing") {
    pending
    collections.foreach {pair =>
      val (name, collection, iterations) = pair
      val results = if (classOf[Seq[_]].isAssignableFrom (collection.getClass)) {
        getSeqResults (collection.asInstanceOf[Seq[_]], iterations)
      }
      else {
        getIterableResults (collection, iterations)
      }
      it (s"results for ${name}") {
        System.out.println (s"\nResults for ${name}")
        results.foreach {result =>
          System.out.println (s"  ${result._1.getClass.getSimpleName}: ${result._2}ns")
        }
      }
    }
  }

  private def getSeqResults (collection: Seq[_], iterations: Int): List[(Operation, Long)] = {
    seqOps.map {op =>
      (op, op.apply (collection, iterations))
    }
  }

  private def getIterableResults (collection: Iterable[_], iterations: Int): List[(Operation, Long)] = {
    iterableOps.map {op =>
      (op, op.apply (collection, iterations))
    }
  }
}
