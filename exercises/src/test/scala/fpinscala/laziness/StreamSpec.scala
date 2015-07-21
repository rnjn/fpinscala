import org.scalatest._
import scala.collection.immutable.List
import in.rnjn.fpinscala.laziness._

class StreamSpec extends FunSuite {
  test("empty stream to empty list"){
    assert(List() == Stream.empty.toList)
  }


  test("natural numbers 1-4 to list") {
    assert(List(1, 2, 3, 4) == Stream(1, 2, 3, 4).toList)
  }

}
