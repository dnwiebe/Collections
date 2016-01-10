package cse.collections

import org.scalatest.path

/**
  * Created by dnwiebe on 1/10/16.
  */
class IterableTest extends path.FunSpec {
  describe ("Given two Iterables and an element") {
    val FIRST = List ("one", "two", "three")
    val SECOND = List ("four", "five")
    val ELEMENT = "six"
    val x: Iterable = Nil

    describe ("and concentrating on standard non-functional methods") {

      it ("the slice method extracts an interval of elements" ) {
        assert ((FIRST ::: SECOND).slice (2, 4) === List ("three", "four"))
      }

      it ("the drop method eliminates the first few items") {
        assert ((FIRST ::: SECOND).drop (2) === List ("three", "four", "five"))
      }

      it ("the dropRight method eliminates the last few items") {
        assert ((FIRST ::: SECOND).dropRight (3) === List ("one", "two"))
      }

      it ("the take method preserves the first few items") {
        assert ((FIRST ::: SECOND).take (2) === List ("one", "two"))
      }

      it ("the takeRight method preserves the last few items") {
        assert ((FIRST ::: SECOND).takeRight (3) === List ("three", "four", "five"))
      }

      it ("the grouped method partitions into fixed-size pieces, except for the last piece") {
        assert ((FIRST ::: SECOND).grouped (2).toList === List (
          Seq ("one", "two"),
          Seq ("three", "four"),
          Seq ("five")
        ))
      }

      it ("the head method returns the first item in the list") {
        assert (FIRST.head === "one")
        assert (SECOND.head === "four")
        // This would be a runtime error: Nil.head
      }

      it ("the isEmpty method is a shortcut for checking that size == 0") {
        assert (FIRST.isEmpty === false)
        assert (Nil.isEmpty === true)
      }

      it ("the sameElements function is very much like business equals") {
        assert (FIRST.sameElements (FIRST) === true)
        assert (FIRST.sameElements (SECOND) === false)
        assert (FIRST.sameElements (FIRST.reverse) === false)
      }

      it ("the sliding method takes snapshots of a sliding window") {
        assert ((FIRST ::: SECOND).sliding (3).toList === List (
          Seq ("one", "two", "three"),
          Seq ("two", "three", "four"),
          Seq ("three", "four", "five")
        ))

        assert ((FIRST ::: SECOND).sliding (3, 2).toList === List (
          Seq ("one", "two", "three"),
          Seq ("three", "four", "five")
        ))
      }

      it ("the zip method combines two collections into a collection of pairs") {
        assert (FIRST.zip (SECOND) === List (
          ("one", "four"),
          ("two", "five")
        ))
      }

      it ("the zipWithIndex method zips a collection with its indices") {
        assert (FIRST.zipWithIndex === List (("one", 0), ("two", 1), ("three", 2)))
      }
    }

    describe ("and concentrating on functional methods from Iterable") {

      it ("the exists method is like count, but returns a Boolean") {
        val predicate = {s: String => s.startsWith ("o")}

        assert (FIRST.exists (predicate) === true)
        assert (SECOND.exists (predicate) === false)
      }

      it ("the find method stops at the first match, and returns an Option") {
        val predicate = {s: String => s.startsWith ("t")}

        assert (FIRST.find (predicate) === Some ("two"))
        assert (SECOND.find (predicate) === None)
      }

      it ("the forall method tells whether a predicate holds for every member of the collection") {
        val predicate = {s: String => s.startsWith ("f")}

        assert (FIRST.forall (predicate) === false)
        assert (SECOND.forall (predicate) === true)
      }

      val partialFunction: PartialFunction[String, String] = {
        case e if e.startsWith ("t") => e + " starts with T"
      }
    }
  }
}
