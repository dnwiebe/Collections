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

      it ("the size method returns the length of the collection") {
        assert (FIRST.size === 3)
        assert (SECOND.size === 2)
        assert (Nil.size === 0)
      }

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

      it ("the permutations method produces permutations of the entire collection") {
        assert (FIRST.permutations.toList === List (
          Seq ("one", "two", "three"),
          Seq ("one", "three", "two"),
          Seq ("two", "one", "three"),
          Seq ("two", "three", "one"),
          Seq ("three", "one", "two"),
          Seq ("three", "two", "one")
        ))

        assert (SECOND.permutations.toList === List (
          Seq ("four", "five"),
          Seq ("five", "four")
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

      it ("the endsWith method checks the last few elements") {
        assert ((FIRST ::: SECOND).endsWith (List ("three", "four", "five")) === true)
        assert ((FIRST ::: SECOND).endsWith (List ("three", "four")) === false)
      }

      it ("the headOption method is just like head, except it returns an Option") {
        assert (FIRST.headOption === Some ("one"))
        assert (SECOND.headOption === Some ("four"))
        assert (Nil.headOption === None)
      }

      it ("the tails method returns a sequence of diminishing tails") {
        assert (FIRST.tails.toList === List (
          List ("one", "two", "three"),
          List ("two", "three"),
          List ("three"),
          Nil
        ))
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

      it ("the reverse method returns elements in reverse order: very handy") {
        assert (FIRST.reverse === List ("three", "two", "one"))
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

      it ("the init method returns everything but the last element") {
        assert (FIRST.init === List ("one", "two"))
        assert (SECOND.init === List ("four"))
      }

      it ("the inits method returns a sequence of diminishing sequences") {
        assert (FIRST.inits.toList === List (List ("one", "two", "three"), List ("one", "two"), List ("one"), Nil))
      }

      it ("the intersect method returns a set intersection") {
        assert ((FIRST ::: SECOND).intersect (List ("three", "four", "seven")) === List ("three", "four"))
      }

      it ("the nonEmpty method does the opposite") {
        assert (FIRST.nonEmpty === true)
        assert (Nil.nonEmpty === false)
      }

      it ("the lengthCompare method classifies the length relative to a given value") {
        assert (FIRST.lengthCompare (3) == 0)
        assert (FIRST.lengthCompare (2) > 0)
        assert (FIRST.lengthCompare (4) < 0)
      }

      it ("the max method returns the maximum value by natural ordering") {
        assert (SECOND.max === "four")
      }

      it ("the min method returns the minimum value by natural ordering") {
        assert (SECOND.min === "five")
      }

      it ("the mkString methods convert the collection to a string, much like the addString method") {
        assert (FIRST.mkString === "onetwothree")
        assert (FIRST.mkString (", ") === "one, two, three")
        assert (FIRST.mkString ("<", ">, <", ">") === "<one>, <two>, <three>")
      }

      it ("the patch method replaces a section of the collection with another") {
        assert (FIRST.patch (1, SECOND.reverse, 1) === List ("one", "five", "four", "three"))
      }

      it ("the sum method sums up all the collection elements") {
        assert (FIRST.map {s => s.length}.sum === 11)
        // This would cause a compile-time error: FIRST.sum
      }

      it ("the product method multiplies together all the collection elements") {
        assert (List (2, 3, 4, 5).product === 120)
        // This would cause a compile-time error: FIRST.product
      }

      it ("the sorted method sorts the collection according to its natural ordering") {
        assert (FIRST.sorted === List ("one", "three", "two"))
      }

      it ("the sortBy method sorts the collection according to a provided mapping to a natural ordering") {
        val mapping = {s: String => s.length}

        assert ((FIRST ::: SECOND).sortBy (mapping) === List ("one", "two", "four", "five", "three"))
      }

      it ("the sortWith method sorts the collection according to a provided comparison function") {
        val comparer = {(a: String, b: String) => a.length < b.length}

        assert ((FIRST ::: SECOND).sortWith (comparer) === List ("one", "two", "four", "five", "three"))
      }

      it ("the splitAt method splits the collection at an index") {
        assert ((FIRST ::: SECOND).splitAt (2) === (
          List ("one", "two"),
          List ("three", "four", "five")
        ))
      }

      it ("the startsWith method lets you check a prefix") {
        assert (FIRST.startsWith (List ("one", "two")) === true)
        assert (FIRST.startsWith (List ("two")) === false)
      }

      it ("the transpose method turns a collection of collections on its ear") {
        assert (List (FIRST, FIRST.reverse).transpose === List (
          List ("one", "three"),
          List ("two", "two"),
          List ("three", "one")
        ))
      }

      it ("the union method is much like ::: or :++") {
        assert (FIRST.union (SECOND) === (FIRST ::: SECOND))
      }

      it ("the unzip method does the opposite") {
        assert (FIRST.zip (SECOND).unzip === (
          List ("one", "two"),
          List ("four", "five")
        ))
      }
    }

    //================================================================//
    //================================================================//
    //================================================================//

    describe ("and concentrating on functional methods from Seq") {

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

      it ("the foreach method applies a side-effect lambda to each item. Avoid if possible") {
        var letterCount = 0
        FIRST.foreach {elem => letterCount += elem.length}

        assert (letterCount === 11)
      }

      it ("the map method converts one collection to another around a supplied function") {
        val converter = {s: String => s.length}

        assert (FIRST.map (converter) === List (3, 3, 5))
      }

      it ("the reverseMap method does a map followed by a reverse") {
        val converter = {s: String => s.capitalize}

        assert (FIRST.reverseMap (converter) === List ("Three", "Two", "One"))
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

      it ("the foldLeft method is used to create one value out of many values") {
        assert (FIRST.foldLeft ("---") {(soFar, elem) => soFar + elem} === "---onetwothree")
      }

      it ("the reduceLeft method does the same thing, under some restrictions") {
        assert (FIRST.reduceLeft {(soFar, elem) => soFar + elem} === "onetwothree")
        // This would cause a runtime error: Nil.reduceLeft {(soFar, elem) => soFar + elem}
      }

      it ("the reduceLeftOption method handles empty collections gracefully") {
        assert (FIRST.reduceLeftOption {(soFar, elem) => soFar + elem} === Some ("onetwothree"))
        assert (List[String] ().reduceLeftOption {(soFar, elem) => soFar + elem} === None)
      }

      it ("the groupBy method creates a map according to a discriminator function") {
        val discriminator: String => Int = {s => s.length}

        assert ((FIRST ::: SECOND).groupBy (discriminator) === Map (
          3 -> List ("one", "two"),
          4 -> List ("four", "five"),
          5 -> List ("three")
        ))
      }

      it ("the dropWhile method gets rid of the prefix that satisfies a predicate") {
        assert ((FIRST ::: SECOND).dropWhile {s => s.length < 5} === List ("three", "four", "five"))
      }

      it ("the takeWhile method preserves the prefix that satisfies a predicate") {
        assert ((FIRST ::: SECOND).takeWhile {s => s.length < 5} === List ("one", "two"))
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

      it ("the maxBy method returns the element judged maximum by a supplied function") {
        val evaluator = {s: String => s.foldLeft (0) {(soFar, elem) => soFar + elem.toInt}}

        assert (FIRST.maxBy (evaluator) === "three")
      }

      it ("the minBy method returns the element judged minimum by a supplied function") {
        val evaluator = {s: String => s.foldLeft (0) {(soFar, elem) => soFar + elem.toInt}}

        assert ((FIRST ::: List ("ab")).minBy (evaluator) === "ab")
      }

      it ("the padTo method increases the length if necessary") {
        assert (SECOND.padTo (2, "booga") === List ("four", "five"))
        assert (SECOND.padTo (4, "booga") === List ("four", "five", "booga", "booga"))
        assert (SECOND.padTo (1, "booga") === List ("four", "five"))
      }

      it ("the partition method splits a collection in two based on a predicate") {
        assert ((FIRST ::: SECOND).partition {s => s.startsWith ("t")} === (
          List ("two", "three"),
          List ("one", "four", "five")
        ))
      }

      it ("the prefixLength method tells how many elements at the beginning satisfy a predicate") {
        assert (FIRST.prefixLength { s => s.length < 4 } === 2)
      }

      it ("the segmentLength method does the same, but at a specified position") {
        assert (FIRST.segmentLength (s => s.length < 4, 1) === 1)
      }

      it ("the span method splits the collection into the prefix that satisfies a predicate, and the rest") {
        assert ((FIRST ::: SECOND).span {s => s.length < 4} === (
          List ("one", "two"),
          List ("three", "four", "five")
        ))
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

      it ("the orElse method does the same, except the default function is a partial function") {
        val defaultFunction: PartialFunction[Int, String] = {
          case i if i > 2 => "Too big!"
        }
        val func = FIRST.orElse (defaultFunction)

        assert (func (0) === "one")
        assert (func (2) === "three")
        assert (func (3) === "Too big!")
        // This would cause an error: func (-1)
      }

      it ("the isDefinedAt method tells whether a particular index works or not") {
        assert (FIRST.isDefinedAt (-1) === false)
        assert (FIRST.isDefinedAt (0) === true)
        assert (FIRST.isDefinedAt (2) === true)
        assert (FIRST.isDefinedAt (3) === false)
      }

      it ("the lift method converts plain-function/exception to Some/None") {
        assert (FIRST.lift (0) === Some ("one"))
        assert (FIRST.lift (2) === Some ("three"))
        assert (FIRST.lift (3) === None)
      }
    }
  }
}
