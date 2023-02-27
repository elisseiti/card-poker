package com.evolution.bootcamp.assignment.poker

import com.evolution.bootcamp.assignment.poker.card.{PokerHand, PokerHandOrdering, TexasHoldemHelper, PlayerHandOrdering}

object Solver {
  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands   => processTexasHoldem(board, hands)
      case "omaha-holdem" :: board :: hands   => ErrorPrefix + "The solution doesn't support Omaha Hold'em"
      case "five-card-draw" :: hands          => ErrorPrefix + "The solution doesn't support Five Card Draw"
      case x :: _                             => ErrorPrefix + "Unrecognized game type"
      case _                                  => ErrorPrefix + "Invalid input"
    }
  }

    def processTexasHoldem(board: String, hands: List[String]): String = {
      val boardCards = TexasHoldemHelper.transformInput(board)
      val handsCards = hands.map(e => TexasHoldemHelper.getHighestPokerHand(boardCards, TexasHoldemHelper.transformInput(e)))
        .zip(hands).sorted(PlayerHandOrdering)
      //.map(_._2)

      rankPokerHands(handsCards)
    }

    def rankPokerHands(lst: List[(PokerHand, String)]): String = {
      def _rankPokerHands(lst: List[(PokerHand, String)], acc: String): String = lst match {
        case Nil => acc
        case el :: Nil => acc + el._2
        case el :: tail => if (PokerHandOrdering.compare(el._1, tail.head._1) == 0) _rankPokerHands(tail, acc + el._2 + "=")
        else _rankPokerHands(tail, acc + el._2 + " " )
      }
      _rankPokerHands(lst, "")
    }
}
