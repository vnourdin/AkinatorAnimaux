package akinator

import java.io.{File, FileWriter}

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

  def jeuSimple(arbre: ABanimal, it: Iterator[String]): Boolean = {
    val prochain = it.next()
    arbre match {
      case Question(q: String, oui: ABanimal, non: ABanimal) if prochain.equals("o") =>
        System.out.println(q)
        System.out.println(prochain)
        jeuSimple(oui, it)
      case Question(q: String, oui: ABanimal, non: ABanimal) if prochain.equals("n") =>
        System.out.println(q)
        System.out.println(prochain)
        jeuSimple(non, it)
      case Animal(nom: String) if prochain.equals("o") =>
        System.out.println("Pensez-vous à : " + nom)
        System.out.println(prochain)
        System.out.println("J'ai gagné !\n\n")
        true
      case Animal(nom: String) if prochain.equals("n") =>
        System.out.println("Pensez-vous à : " + nom)
        System.out.println(prochain)
        System.out.println("J'ai perdu =(\n\n")
        false
    }
  }

  def jeuSimple(arbre: ABanimal): Boolean = {
    jeuSimple(arbre, Source.stdin.getLines)
  }

  def jeuLog(arbre: ABanimal, it: Iterator[String]): List[String] = {
    jeuSimple(arbre, it)
    it.toList
  }

  /*
    def jeuApprentissage(a: ABanimal, it: Iterator[String]): ABanimal = {
      val prochain = it.next()
      a match {
        case Question(q: String, oui: ABanimal, non: ABanimal) if prochain.equals("o") => jeuApprentissage(oui, it)
        case Question(q: String, oui: ABanimal, non: ABanimal) if prochain.equals("n") => jeuApprentissage(non, it)
        case Animal(nom: String) if prochain.equals("o") => true
        case Animal(nom: String) if prochain.equals("n") =>
          print("J'ai perdu - quelle est la bonne réponse ?")
          val rep = new Animal(Source.stdin.getLines.next());
          print("Quelle question permet de différencier " + rep.nom + " de " + nom + " ?")
          val question = Source.stdin.getLines.next();
          print("Quelle est la réponse à cette question pour " +
            new Question(
      }
    }

    def jeuSimpleJNSP(a: ABanimal, it: Iterator[String]): Boolean = {
    }*/
}