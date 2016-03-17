package akinator

import java.io._

import scala.io.Source

object Akinator {

  trait ABanimal

  case class Animal(nom: String) extends ABanimal

  case class Question(q: String, oui: ABanimal, non: ABanimal) extends ABanimal


  def fichierToABanimal(cheminVersFichier: String): ABanimal = {
    val l = Source.fromFile(cheminVersFichier).getLines().toList

    def aux(l: List[String]): (ABanimal, List[String]) = l match {
      case Nil => throw new Exception("Un arbre binaire plein n'a pas de noeud vide")
      case t :: q if t.startsWith("q :") =>
        val (abanimalG, listG) = aux(q)
        val (abanimalD, listD) = aux(listG)
        (new Question(t.substring(3), abanimalG, abanimalD), listD)
      case t :: q => (new Animal(t), q)
    }
    val (arbreResultat, liste) = aux(l)

    arbreResultat
  }

  def ABanimalToFichier(cheminVersFichier: String, arbre: ABanimal): Unit = {
    val writer = new FileWriter(new File(cheminVersFichier))

    def aux(ab: ABanimal): Unit = ab match {
      case Animal(nom: String) => writer.write(nom + "\n")
      case Question(q: String, oui: ABanimal, non: ABanimal) => writer.write("q :" + q + "\n")
        aux(oui)
        aux(non)
    }
    aux(arbre)
    writer.close()
  }

  def jeuSimple(arbre: ABanimal, it: Iterator[String]): Boolean = arbre match {
    case Question(q: String, oui: ABanimal, non: ABanimal) if (it.next().equals("o")) => jeuSimple(oui, it)
    case Question(q: String, oui: ABanimal, non: ABanimal) if (it.next().equals("n")) => jeuSimple(non, it)
    case Animal(nom: String) if (it.next().equals("o")) => true
    case Animal(nom: String) if (it.next().equals("n")) => false
  }
}