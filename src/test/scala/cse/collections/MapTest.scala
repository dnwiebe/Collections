package cse.collections

import org.scalatest.path

/**
  * Created by dnwiebe on 1/10/16.
  */
class MapTest extends path.FunSpec {

  describe ("Given two Maps") {
    val FIRST = Map ("one" -> 1, "two" -> 2, "three" -> 3)
    val SECOND = Map ("four" -> 4, "five" -> 5)

    describe ("and concentrating on binary operators") {

      it ("the + operator adds one pair") {
        assert (SECOND + ("six" -> 6) === Map ("four" -> 4, "five" -> 5, "six" -> 6))
      }

      it ("or more than one pair, if necessary") {
        assert (SECOND +("six" -> 6, "seven" -> 7) === Map (
          "four" -> 4,
          "five" -> 5,
          "six" -> 6,
          "seven" -> 7
        ))
      }

      it ("the ++ operator can add a list of pairs") {
        val addition = List ("six" -> 6, ("seven", 7))
        assert (SECOND ++ addition === Map (
          "four" -> 4,
          "five" -> 5,
          "six" -> 6,
          "seven" -> 7
        ))
      }

      it ("the ++ operator can also add another Map") {
        val addition = Map ("six" -> 6, ("seven", 7))
        assert (SECOND ++ addition === Map (
          "four" -> 4,
          "five" -> 5,
          "six" -> 6,
          "seven" -> 7
        ))
      }

      it ("the - operator removes elements by key") {
        assert (FIRST - "two" === Map ("one" -> 1, "three" -> 3))
      }

      it ("the -- operator removes multiple elements by key") {
        assert ((FIRST ++ SECOND) -- List ("two", "three", "four") === Map ("one" -> 1, "five" -> 5))
      }
    }

    describe ("and concentrating on standard non-functional methods") {

      it ("the apply method indexes dangerously") {
        assert (FIRST ("two") === 2)
        // This would cause a runtime error: FIRST ("four")
      }

      it ("the get method indexes safely") {
        assert (FIRST.get ("two") === Some (2))
        assert (FIRST.get ("four") === None)
      }

      it ("the keySet method retrieves a set of keys") {
        assert (FIRST.keySet === Set ("one", "two", "three"))
      }

      it ("the values method retrieves a collection of values") {
        assert (FIRST.values.toSet === Set (1, 2, 3))
      }
    }

    //================================================================//
    //================================================================//
    //================================================================//

    describe ("and concentrating on functional methods from Map") {

      it ("the filterKeys method filters values based on keys") {
        assert (FIRST.filterKeys {_.length > 4} === Map ("three" -> 3))
      }

      it ("the mapValues method replaces values according to a function") {
        assert (SECOND.mapValues {_ * 2} === Map ("four" -> 8, "five" -> 10))
      }

      it ("the transform method is like mapValues, but the transform function gets both key and value") {
        assert (SECOND.transform {(k, v) => k.length + v} === Map ("four" -> 8, "five" -> 9))
      }
    }
  }
}
