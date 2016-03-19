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
    System.out.print("\nPensez à un animal - ")

    def aux(arbre: ABanimal, it: Iterator[String]): Boolean = arbre match {
      case Question(q: String, oui: ABanimal, non: ABanimal) =>
        System.out.println(q)
        if (it.next().equals("o"))
          aux(oui, it)
        else
          aux(non, it)
      case Animal(nom: String) =>
        System.out.println("Pensez-vous à : " + nom + "?")
        if (it.next().equals("o")) {
          System.out.println("J'ai gagné =)")
          true
        }
        else {
          System.out.println("J'ai perdu =(")
          false
        }
    }

    aux(arbre, it)
  }


  def jeuSimple(arbre: ABanimal): Boolean = {
    jeuSimple(arbre, Source.stdin.getLines)
  }

  def jeuLog(arbre: ABanimal, it: Iterator[String]): List[String] = {
    System.out.print("\nPensez à un animal - ")

    def aux(arbre: ABanimal, it: Iterator[String]): List[String] = arbre match {
      case Question(q: String, oui: ABanimal, non: ABanimal) =>
        System.out.println(q)
        if (it.next().equals("o"))
          q :: "o" :: aux(oui, it)
        else
          q :: "n" :: aux(non, it)
      case Animal(nom: String) =>
        System.out.println("Pensez-vous à : " + nom + "?")
        if (it.next().equals("o")) {
          System.out.println("J'ai gagné =)")
          List(nom, "o", "J'ai gagné =)")
        }
        else {
          System.out.println("J'ai perdu =(")
          List(nom, "n", "J'ai perdu =(")
        }
    }

    aux(arbre, it)
  }

  def jeuApprentissage(arbre: ABanimal, it: Iterator[String]): ABanimal = {
    System.out.print("\nPensez à un animal - ")

    def aux(arbre: ABanimal, it: Iterator[String]): ABanimal = arbre match {
      case Question(q: String, oui: ABanimal, non: ABanimal) =>
        System.out.println(q)
        if (it.next().equals("o"))
          new Question(q, aux(oui, it), non)
        else
          new Question(q, oui, aux(non, it))
      case Animal(nom: String) =>
        System.out.println("Pensez-vous à : " + nom)
        if (it.next().equals("o")) {
          System.out.println("J'ai gagné =)")
          arbre
        }
        else {
          System.out.println("J'ai perdu - quelle est la bonne réponse ?")
          val bonneRep = it.next()

          System.out.println("Quelle question permet de différencier " + bonneRep + " de " + nom + "?")
          val nouvelleQuestion = it.next()

          System.out.println("Quelle est la réponse à cette question pour " + bonneRep + "?")

          if (it.next().equals("o"))
            new Question(nouvelleQuestion, new Animal(bonneRep), arbre)
          else
            new Question(nouvelleQuestion, arbre, new Animal(bonneRep))
        }
    }

    aux(arbre, it)
  }

  def jeuSimpleJNSP(arbre: ABanimal, it: Iterator[String]): Boolean = {
    System.out.print("\nPensez à un animal - ")

    def aux(arbre: ABanimal, it: Iterator[String]): Boolean = arbre match {
      case Question(q: String, oui: ABanimal, non: ABanimal) =>
        System.out.println(q)
        if (it.next().equals("x")) {
          aux(oui, it)
          aux(non, it)
        } else {
          if (it.next().equals("o"))
            aux(oui, it)
          else
            aux(non, it)
        }
      case Animal(nom: String) =>
        System.out.println("Pensez-vous à : " + nom + "?")
        if (it.next().equals("o")) {
          System.out.println("J'ai gagné =)")
          true
        }
        else {
          false
        }
    }

    aux(arbre, it)
  }
}