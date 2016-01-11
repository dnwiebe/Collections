package cse.collections

import org.scalatest.path
// There's also scala.collection.JavaConversions. Try not to use that one: it's all implicit.
import scala.collection.JavaConverters._

/**
  * Created by dnwiebe on 1/10/16.
  */
class InteropTest extends path.FunSpec {
  private def acceptsAndReturnsJavaList (input: java.util.List[Int]): java.util.List[Int] = {
    val output = new java.util.ArrayList[Int] ()
    (0 until input.size ()).foreach {i: Int =>
      output.add (input.get (i) * 2)
    }
    output
  }

  describe ("Scala/Java interop") {

    it ("Scala collections can be converted for use with Java operations") {
      assert (acceptsAndReturnsJavaList (List (3, 4, 5).asJava).asScala === List (6, 8, 10))
    }

    it ("Java collections can be converted for use with Scala operations") {
      val javaInput = new java.util.ArrayList[Int] ()
      javaInput.add (3)
      javaInput.add (4)
      javaInput.add (5)

      val javaOutput = javaInput.asScala.map {_ * 2}.asJava

      val expectedJavaOutput = new java.util.ArrayList[Int] ()
      expectedJavaOutput.add (6);
      expectedJavaOutput.add (8);
      expectedJavaOutput.add (10);
      assert (javaOutput === expectedJavaOutput)
    }
  }
}
