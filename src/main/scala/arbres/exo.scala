package arbres

object exo {

  trait ArbreBin[T]

  case class ArbreVide[T]() extends ArbreBin[T]

  case class Feuille[T](valeur: T) extends ArbreBin[T]

  case class Noeud[T](valeur: T, filsG: ArbreBin[T], filsD: ArbreBin[T]) extends ArbreBin[T]


  def nbNoeuds[T](ar: ArbreBin[T]): Int = ar match {
    case ArbreVide() => throw new NoSuchElementException("Un arbre vide est vide")
    case Feuille(valeur: T) => 1
    case Noeud(valeur: T, filsG: ArbreBin[T], filsD: ArbreBin[T]) => 1 + nbNoeuds(filsG) + nbNoeuds(filsD)
  }
}