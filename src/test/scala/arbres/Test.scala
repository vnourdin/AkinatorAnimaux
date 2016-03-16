package arbres

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestAr extends FunSuite {
import arbres.exo._
    test("nbNoeuds"){
    	assert( nbNoeuds(Noeud[Int](1, Feuille[Int](2), Noeud[Int](3, Feuille[Int](4), Feuille[Int](5)))) == 5)
    }
}