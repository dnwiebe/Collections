package cse.collections

import org.scalatest.path

/**
  * Created by dnwiebe on 1/10/16.
  */
class SeqTest extends path.FunSpec {
  describe ("Given two lists and an element") {
    val FIRST = List ("one", "two", "three")
    val SECOND = List ("four", "five")
    val ELEMENT = "six"

    describe ("and concentrating on binary operators") {

      it ("the ++ operator can be used to concatenate two lists") {
        assert (FIRST ++ SECOND === List ("one", "two", "three", "four", "five"))
      }

      it ("the ++: operator can also be used to concatenate two lists") {
        assert (FIRST ++: SECOND === List ("one", "two", "three", "four", "five"))
      }

      describe ("the +: operator") {
        it ("can be used to prepend an element when called with a dot") {
          assert (FIRST.+: (ELEMENT) === List ("six", "one", "two", "three"))
        }

        it ("since it ends with a colon, reverses its parameters as a binary operator") {
          assert (ELEMENT +: FIRST === List ("six", "one", "two", "three"))
        }

        it ("is also an unapply object, so it can be used to decompose a Seq") {
          assert (+:.unapply (FIRST) === Some ("one", List ("two", "three")))

          FIRST match {
            case (h +: t) => {
              assert (h === "one")
              assert (t === List ("two", "three"))
            }
            case _ => fail ()
          }
        }
      }

      it ("the :+ operator appends an element") {
        assert (FIRST :+ ELEMENT === List ("one", "two", "three", "six"))
      }

      it ("the /: and :\\ operators are not worth demonstrating: use folds instead") {
        assert (true)
      }

      describe ("that are unique to Lists") {

        it ("the :: operator is like +:, but unique to Lists") {
          assert (ELEMENT :: FIRST === List ("six", "one", "two", "three"))
          FIRST match {
            case (h :: t) => {
              assert (h === "one")
              assert (t === List ("two", "three"))
            }
            case _ => fail ()
          }
        }

        it ("the ::: operator prepends a list, and reverses its binary-op parameters") {
          assert (SECOND ::: FIRST === List ("four", "five", "one", "two", "three"))
        }
      }
    }

    describe ("and concentrating on standard non-functional methods") {

      it ("the addString method can be used to pretty-print your sequence") {
        val builder = FIRST.addString (new StringBuilder (), "Sequence FIRST: (\"", "\", \"", "\");")
        assert (builder.toString === """Sequence FIRST: ("one", "two", "three");""")
      }

      it ("the combinations method produces ordered combinations of a given length") {
        assert (FIRST.combinations (2).toList === List (
          Seq ("one", "two"),
          Seq ("one", "three"),
          Seq ("two", "three")
        ))

        assert (FIRST.combinations (3).toList === List (
          Seq ("one", "two", "three")
        ))
      }

      it ("the contains method finds elements") {
        assert (FIRST.contains ("one") === true)
        assert (FIRST.contains ("fifty-seven") === false)
      }

      it ("the containsSlice method finds clusters") {
        assert (FIRST.containsSlice (List ("one", "two")) === true)
        assert (FIRST.containsSlice (List ("two", "three")) === true)
        assert (FIRST.containsSlice (List ("one", "three")) === false)
        assert (FIRST.containsSlice (List ("two", "one")) === false)
      }

      it ("the diff method subtracts another Seq") {
        assert (FIRST.diff (List ("one", "three", "four")) === List ("two"))
        assert (FIRST.diff (SECOND) === FIRST)
        assert (FIRST.diff (FIRST) === Nil)
      }

      it ("the distinct method eliminates duplicates") {
        val withDups = ELEMENT :: FIRST ::: FIRST ::: SECOND ::: FIRST ::: SECOND
        assert (withDups === List ("six", /**/ "one", "two", "three", /**/ "one", "two", "three",
          /**/ "four", "five", /**/ "one", "two", "three", /**/ "four", "five"))
        assert (withDups.distinct === List ("six", /**/ "one", "two", "three", /**/ "four", "five"))
      }

      it ("the drop method eliminates the first few items") {
        assert ((FIRST ::: SECOND).drop (2) === List ("three", "four", "five"))
      }

      it ("the dropRight method eliminates the last few items") {
        assert ((FIRST ::: SECOND).dropRight (3) === List ("one", "two"))
      }

      it ("the endsWith method checks the last few elements") {
        assert ((FIRST ::: SECOND).endsWith (List ("three", "four", "five")) === true)
        assert ((FIRST ::: SECOND).endsWith (List ("three", "four")) === false)
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

      it ("the headOption method is just like head, except it returns an Option") {
        assert (FIRST.headOption === Some ("one"))
        assert (SECOND.headOption === Some ("four"))
        assert (Nil.headOption === None)
      }

      it ("the tail method returns all items but the first") {
        assert (FIRST.tail === List ("two", "three"))
        assert (SECOND.tail === List ("five"))
        assert (SECOND.tail.tail === Nil)
        // This would be an error: Nil.tail
      }

      it ("the last method returns the last element") {
        assert (FIRST.last === "three")
        assert (SECOND.last === "five")
        // This would cause an error: Nil.last
      }

      it ("the lastOption method returns the last element, if any, in an Option") {
        assert (FIRST.lastOption === Some ("three"))
        assert (SECOND.lastOption === Some ("five"))
        assert (Nil.lastOption === None)
      }

      it ("the indexOf method returns the location of the first occurrence of a given value") {
        assert (FIRST.indexOf ("two") === 1)
        assert (FIRST.indexOf ("gorp") === -1)
        assert ((FIRST ::: FIRST).indexOf ("two", 2) === 4)
      }

      it ("the indexOfSlice method is similar, but works with slices") {
        assert ((FIRST ::: SECOND).indexOfSlice (List ("three", "four")) === 2)
        assert ((FIRST ::: SECOND).indexOfSlice (List ("four", "three")) === -1)
        assert ((FIRST ::: SECOND).indexOfSlice (List ("three", "four"), 3) === -1)
        assert ((FIRST ::: SECOND).indexOfSlice (Nil) === 0)
      }

      it ("the lastIndexOf method returns the location of the last occurrence of a given value") {
        assert ((FIRST ::: FIRST).lastIndexOf ("two") === 4)
        assert ((FIRST ::: FIRST).lastIndexOf ("gorp") === -1)
        assert ((FIRST ::: FIRST).lastIndexOf ("two", 2) === 1)
      }

      it ("the lastIndexOfSlice method is similar, but works with slices") {
        assert ((FIRST ::: SECOND).lastIndexOfSlice (List ("three", "four")) === 2)
        assert ((FIRST ::: SECOND).lastIndexOfSlice (List ("four", "three")) === -1)
        assert ((FIRST ::: SECOND).lastIndexOfSlice (List ("three", "four"), 1) === -1)
        assert ((FIRST ::: SECOND).lastIndexOfSlice (Nil) === 5)
      }

      it ("the indices method returns a collection of valid indices") {
        assert (FIRST.indices === List (0, 1, 2))
        assert (SECOND.indices === List (0, 1))
      }

      it ("the intersect method returns a set intersection") {
        assert ((FIRST ::: SECOND).intersect (List ("three", "four", "seven")) === List ("three", "four"))
      }

      it ("the isEmpty method is a shortcut for checking that size == 0") {
        assert (FIRST.isEmpty === false)
        assert (Nil.isEmpty === true)
      }
    }

    describe ("and concentrating on functional methods from Seq") {

      val partialFunction: PartialFunction[String, String] = {
        case e if e.startsWith ("t") => e + " starts with T"
      }

      it ("the collect method is a convenient flatMap for partial functions") {
        assert (FIRST.collect (partialFunction) ===
          List ("two starts with T", "three starts with T"))
      }

      it ("the collectFirst method stops with the first match") {
        assert (FIRST.collectFirst (partialFunction) === Some ("two starts with T"))
        assert (SECOND.collectFirst (partialFunction) === None)
      }

      it ("the compose method combines a provided function with apply") {
        val func = FIRST.compose {s: String => s.length - 3}

        assert (func ("Ted") === "one")
        assert (func ("Bill") === "two")
        assert (func ("Eddie") === "three")
      }

      it ("the corresponds method validates correspondences") {
        val predicate = {(forward: String, backward: String) => forward.reverse == backward}

        assert (FIRST.corresponds (List ("eno", "owt", "eerht")) (predicate) === true)
        assert (FIRST.corresponds (List ("eno", "owt")) (predicate) === false)
        assert (FIRST.corresponds (List ("eno", "owt", "three")) (predicate) === false)
      }

      it ("the count method counts satisfactions of a predicate") {
        val predicate = {s: String => s.startsWith ("t")}

        assert (FIRST.count (predicate) === 2)
      }

      it ("the exists method is like count, but returns a Boolean") {
        val predicate = {s: String => s.startsWith ("o")}

        assert (FIRST.exists (predicate) === true)
        assert (SECOND.exists (predicate) === false)
      }

      it ("the filter method is like count and exists, but returns the matching elements") {
        val predicate = {s: String => s.startsWith ("t")}

        assert (FIRST.filter (predicate) === List ("two", "three"))
        assert (SECOND.filter (predicate) === Nil)
      }

      it ("the filterNot method reverses the predicate") {
        val predicate = {s: String => s.startsWith ("t")}

        assert (FIRST.filterNot (predicate) === List ("one"))
        assert (SECOND.filterNot (predicate) === List ("four", "five"))
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

      it ("the foreach method applies a side-effect lambda to each item. Avoid if possible") {
        var letterCount = 0
        FIRST.foreach {elem => letterCount += elem.length}

        assert (letterCount === 11)
      }

      it ("the flatten method converts a collection of collections to a simple collection") {
        val twoLevels = List (FIRST, SECOND)
        assert (twoLevels === List (List ("one", "two", "three"), List ("four", "five")))

        assert (twoLevels.flatten === List ("one", "two", "three", "four", "five"))
      }

      it ("the flatten method can also be used on Options, which can be looked at as collections") {
        val listOfOptions = List (None, Some ("Billy"), Some ("Angela"), None, None, Some ("Todd"))

        assert (listOfOptions.flatten === List ("Billy", "Angela", "Todd"))
      }

      it ("the flatMap method is just a map followed by a flatten") {
        val mapFunc: String => List[Char] = {s =>
          s.map {c => c}.toList
        }

        assert (FIRST.flatMap (mapFunc) === List ('o', 'n', 'e', 't', 'w', 'o', 't', 'h', 'r', 'e', 'e'))
      }

      it ("the foldLeft method is used to create one value out of many values") {
        assert (FIRST.foldLeft ("---") {(soFar, elem) => soFar + elem} === "---onetwothree")
      }

      it ("the groupBy method creates a map according to a discriminator function") {
        val discriminator: String => Int = {s => s.length}

        assert ((FIRST ::: SECOND).groupBy (discriminator) === Map (
          3 -> List ("one", "two"),
          4 -> List ("four", "five"),
          5 -> List ("three")
        ))
      }

      it ("the indexWhere method finds the first element satisfying a predicate") {
        val predicate = {s: String => (s.length & 0x1) == 0}

        assert ((FIRST ::: SECOND).indexWhere (predicate) === 3)
        assert ((FIRST ::: SECOND).indexWhere (predicate, 4) === 4)
        assert (FIRST.indexWhere (predicate) === -1)
      }

      it ("the lastIndexWhere method finds the last element satisfying a predicate") {
        val predicate = {s: String => (s.length & 0x1) == 0}

        assert ((FIRST ::: SECOND).lastIndexWhere (predicate) === 4)
        assert ((FIRST ::: SECOND).lastIndexWhere (predicate, 3) === 3)
        assert (FIRST.lastIndexWhere (predicate) === -1)
      }
    }

    describe ("and concentrating on functional methods from PartialFunction") {

      it ("the andThen method creates an index-based function") {
        val func = FIRST.andThen {_ + " at work"}

        assert (func (0) === "one at work")
        assert (func (2) === "three at work")
      }

      it ("the applyOrElse method allows use of a default function") {
        val func = {e: Int => s"No element ${e}"}

        assert (FIRST.applyOrElse (0, func) === "one")
        assert (FIRST.applyOrElse (42, func) === "No element 42")
      }

      it ("the isDefinedAt method tells whether a particular index works or not") {
        assert (FIRST.isDefinedAt (-1) === false)
        assert (FIRST.isDefinedAt (0) === true)
        assert (FIRST.isDefinedAt (2) === true)
        assert (FIRST.isDefinedAt (3) === false)
      }
    }
  }
}
