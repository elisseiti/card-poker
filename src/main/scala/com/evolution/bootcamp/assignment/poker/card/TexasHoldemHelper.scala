package com.evolution.bootcamp.assignment.poker.card

object TexasHoldemHelper {

  def transformInput(hand: String): List[PokerCard] = {

    def _transformInput(hand: String, acc: List[PokerCard]): List[PokerCard] = {
      val lst: List[String] = List("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
      hand match {
        case "" => acc.sortBy(_.rank).reverse
        case _ =>
          val prefix = getPrefix(hand, lst)
          val remainderString = hand.stripPrefix(prefix)
          val card = PokerCard(
            CardRank.transformToRank(prefix), CardSuit.transformToSuit("" + remainderString.charAt(0)
            )
          )
          _transformInput(remainderString.substring(1), card :: acc)
      }
    }

    _transformInput(hand, List())
  }

  def getPrefix(hand: String, lst: List[String]): String = {
    for (prefix <- lst) {
      if (hand.startsWith(prefix)) {
        return prefix
      }
    }
    ""
  }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] = {

    def _comb[A](n: Int, acc: List[A], ls: List[A]): List[List[A]] = {
      if (ls.isEmpty) acc.reverse :: Nil
      else _comb(n, ls.head +: acc, ls.tail) ::: _comb(n, acc, ls.tail)
    }

    _comb(1, List(), ls).filter(_.length == n)
  }

  def getAllCombinations(boardCards: List[PokerCard], playerCards: List[PokerCard]): List[PokerHand] = {
    combinations(5, boardCards ::: playerCards).map(e => Extractor.extract(e)).sorted(PokerHandOrdering).reverse
  }

  def getHighestPokerHand(boardCards: List[PokerCard], playerCards: List[PokerCard]): PokerHand =
    getAllCombinations(boardCards, playerCards).head

}
