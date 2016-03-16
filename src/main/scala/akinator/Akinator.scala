package akinator

import scala.io.Source

object Akinator {

  trait ABanimal

  case class Animal(nom: String) extends ABanimal

  case class Question(q: String, oui: ABanimal, non: ABanimal) extends ABanimal

  def fichierToABanimal(nomf:String):ABanimal = {
    val l = Source.fromFile(nomf).getLines().toList
    
    def aux(l:List[String]) : (ABanimal,List[String]) = l match {
      case Nil => throw new Exception("Un animal ne peut être vide")
      case t::q if(t.indexOf("q :")!=(-1)) => {
        val (ani,list) = aux(q)
        val (ani2,list2) = aux(list)
        (new Question(t,ani,ani2),list2) }
      case t::q if(t.indexOf("q :")==(-1)) => (new Animal(t),q)
     }
    
    val (a,liste) = aux(l)
    a
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