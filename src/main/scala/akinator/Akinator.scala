package akinator

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
    import java.io._
    val writer = new FileWriter(new File("coucou.txt"))
    writer.write("coucou\n")
    def aux(ab:ABanimal):Unit = ab match{
      case Animal(nom: String) => writer.write(nom)
      case Question(q: String, oui: ABanimal, non: ABanimal) => writer.write(q)
        aux(oui)
        aux(non)
    }
    writer.close()
  }

  def jeuSimple(a: ABanimal, it: Iterator[String]): Boolean = a match {
    case Question(q: String, oui: ABanimal, non: ABanimal) if (it.next().equals("o")) => jeuSimple(oui, it)
    case Question(q: String, oui: ABanimal, non: ABanimal) if (it.next().equals("n")) => jeuSimple(non, it)
    case Animal(nom: String) if (it.next().equals("o")) => true
    case Animal(nom: String) if (it.next().equals("n")) => false
  }
}