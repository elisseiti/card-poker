package com.evolution.bootcamp.assignment.poker.card

import CardRank.CardRank

import scala.annotation.tailrec

object Extractor {
  private def isStraightFlush(hand: List[PokerCard]): Boolean = isStraight(hand) && isFlush(hand)

  private def isStraight(hand: List[PokerCard]) : Boolean = {
    @tailrec
    def _isStraight(hand: List[PokerCard]) : Boolean = {
      hand match {
        case _:: Nil => true
        case el:: rest if el.rank.id - rest.head.rank.id == 1 => _isStraight(rest)
        case _=> false
      }
    }

    if (hand.size != 5) return false
    val lowestVal: List[CardRank] = CardRank.ACE:: CardRank.FIVE :: CardRank.FOUR:: CardRank.THREE:: CardRank.TWO:: Nil
    hand.map(_.rank) == lowestVal || _isStraight(hand)
  }

  private def isFlush(hand: List[PokerCard]): Boolean = hand.groupBy(_.suit).size == 1

  private def isFourOfAKind(hand: List[PokerCard]): Boolean = hand.groupBy(_.rank).count(_._2.size==4) == 1

  private def isFullHouse(hand: List[PokerCard]): Boolean = isThreeOfAKind(hand) && isOnePair(hand)

  private def isThreeOfAKind(hand: List[PokerCard]): Boolean = hand.groupBy(_.rank).count(_._2.size==3) == 1

  private def isTwoPairs(hand: List[PokerCard]) : Boolean = hand.groupBy(_.rank).count(_._2.size == 2) == 2

  private def isOnePair(hand: List[PokerCard]): Boolean = hand.groupBy(_.rank).count(_._2.size ==2) == 1

  def extract(hand: List[PokerCard]): PokerHand = {
    val sortedHand = hand.sortBy(_.rank).reverse
    if (isStraightFlush(sortedHand)) StraightFlush(sortedHand)
    else if (isFourOfAKind(sortedHand)) FourOfAKind(sortedHand)
    else if (isFullHouse(sortedHand)) FullHouse(sortedHand)
    else if (isFlush(sortedHand)) Flush(sortedHand)
    else if (isStraight(sortedHand)) Straight(sortedHand)
    else if (isThreeOfAKind(sortedHand)) ThreeOfAKind(sortedHand)
    else if (isTwoPairs(sortedHand)) TwoPair(sortedHand)
    else if (isOnePair(sortedHand)) OnePair(sortedHand)
    else HighCard(sortedHand)
  }
}
