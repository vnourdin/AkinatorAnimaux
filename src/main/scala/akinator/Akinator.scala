package akinator

import scala.io.Source

object Akinator {

  trait ABanimal

  case class Animal(nom: String) extends ABanimal

  case class Question(q: String, oui: ABanimal, non: ABanimal) extends ABanimal


  def fichierToABanimal(nomf: String): ABanimal = {
    val l = Source.fromFile(nomf).getLines().toList

    def aux(l: List[String]): (ABanimal, List[String]) = l match {
      case Nil => throw new Exception("Un arbre binaire plein n'a pas de noeud vide")
      case t :: q if t.startsWith("q :") =>
        val (abanimalG, listG) = aux(q)
        val (abanimalD, listD) = aux(listG)
        (new Question(t.substring(3), abanimalG, abanimalD), listD)
      case t :: q => (new Animal(t), q)
    }
    val (a, liste) = aux(l)

    a
  }

  def jeuSimple(a: ABanimal, it: Iterator[String]): Boolean = a match {
    case Question(q: String, oui: ABanimal, non: ABanimal) if (it.next().equals("o")) => jeuSimple(oui, it)
    case Question(q: String, oui: ABanimal, non: ABanimal) if (it.next().equals("n")) => jeuSimple(non, it)
    case Animal(nom: String) if (it.next().equals("o")) => true
    case Animal(nom: String) if (it.next().equals("n")) => false
  }
}