package akinator

import scala.io.Source

object Akinator {

  trait ABanimal

  case class Animal(nom: String) extends ABanimal

  case class Question(q: String, oui: ABanimal, non: ABanimal) extends ABanimal

  def fichierToABanimal(nomFichier: String): ABanimal = {
    val listeFichier = Source.fromFile(nomFichier).getLines.toList
    
    def ajouterFils(acc: ABanimal, listeAb: List[String]): ABanimal = listeAb match {
      case Nil => acc
      case t::q if(t.startsWith("q:")) => 
    }
  }
  
  /*
  def ABanimalToFichier(ab: ABanimal) {
    val writer = new FileWriter(new File("coucou.txt"))
    writer.write("coucou\n")
    
    
    writer.close()
  }*/

  def jeuSimple(a: ABanimal, it: Iterator[String]): Boolean = a match {
    case Question(q: String, oui: ABanimal, non: ABanimal) if (it.next().equals("o")) => jeuSimple(oui, it)
    case Question(q: String, oui: ABanimal, non: ABanimal) if (it.next().equals("n")) => jeuSimple(non, it)
    case Animal(nom: String) if (it.next().equals("o")) => true
    case Animal(nom: String) if (it.next().equals("n")) => false
  }
}