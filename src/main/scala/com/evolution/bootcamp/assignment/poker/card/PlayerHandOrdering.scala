package com.evolution.bootcamp.assignment.poker.card

object PlayerHandOrdering extends math.Ordering[(PokerHand, String)] {
  override def compare(x: (PokerHand, String), y: (PokerHand, String)): Int = {
      val compareOne = PokerHandOrdering.compare(x._1, y._1);
    val compareTwo = Ordering.String.compare(x._2, y._2)

    if (compareOne != 0) compareOne
    else compareTwo
  }

}
