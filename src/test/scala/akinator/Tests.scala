package arbres

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Tests extends FunSuite {

  import akinator.Akinator._

  val arbreTemoin = Question("q :Est-ce qu’il a des ailes ?", Question("q :Est-ce qu’il a des plumes ?", Animal("Pélican"), Animal("Chauve-souris")), Animal("Chien"))

  test("fichierToABanimal") {
    assert(fichierToABanimal("arbre") == arbreTemoin)
  }
}