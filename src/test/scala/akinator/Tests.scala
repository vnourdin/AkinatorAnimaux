package arbres

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class Tests extends FunSuite {

  import akinator.Akinator._

  val arbreTemoin = Question("Est-ce qu’il a des ailes ?", Question("Est-ce qu’il a des plumes ?", Animal("Pélican"), Animal("Chauve-souris")), Animal("Chien"))

  test("fichierToABanimal") {
    assert(fichierToABanimal("arbre") == arbreTemoin)
  }

  test("ABanimalToFichier") {
    ABanimalToFichier("arbreFromAnimal", arbreTemoin)
    assert(Source.fromFile("arbre").getLines().toList == Source.fromFile("arbreFromAnimal").getLines().toList)
  }

  test("jeuSimple depuis String") {
    assert(jeuSimple(arbreTemoin, new String("n\no").lines))
    assert(!jeuSimple(arbreTemoin, new String("n\nn").lines))
    assert(jeuSimple(arbreTemoin, new String("o\nn\no").lines))
    assert(!jeuSimple(arbreTemoin, new String("o\no\nn").lines))
  }
  /*
  test("jeuSimple depuis clavier") {
    assert(jeuSimple(arbreTemoin) == true)
  }
  */
  test("jeuLog") {
    System.out.println(jeuLog(arbreTemoin, new String("n\no").lines))
  }

  val arbreTemoinApprentissage = Question("Est-ce qu’il a des ailes ?", Question("Est-ce qu’il a des plumes ?",
    Question("Est-ce qu’il a un goitre ?", Animal("Pélican"), Animal("Pigeon")), Animal("Chauve-souris")), Animal("Chien"))

  test("jeuApprentissage") {
    assert(jeuApprentissage(arbreTemoin, new String("o\no\nn\nPigeon\nEst-ce qu’il a un goitre ?\nn").lines) == arbreTemoinApprentissage)
  }

  test("jeuJeNeSaisPas") {
    assert(jeuSimpleJNSP(arbreTemoin, new String("x\nx\nn\nn\no").lines))
  }
}