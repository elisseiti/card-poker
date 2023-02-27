package com.evolution.bootcamp.assignment.poker.card

import CardRank.CardRank

import scala.annotation.tailrec


object PokerHandOrdering extends Ordering[PokerHand] {
  def indexOf(z: PokerHand): Int = {
    z match {
      case _: HighCard => 1
      case _: OnePair => 2
      case _: TwoPair => 3
      case _: ThreeOfAKind => 4
      case _: Straight => 5
      case _: Flush => 6
      case _: FullHouse => 7
      case _: FourOfAKind => 8
      case _: StraightFlush => 9
    }
  }

  implicit override def compare(x: PokerHand, y: PokerHand): Int = {
    (x, y) match {
      case (left: StraightFlush, right: StraightFlush) => compareStraightFlush(left, right)
      case (left: FourOfAKind, right: FourOfAKind) => compareFourOfAKind(left, right)
      case (left: FullHouse, right: FullHouse) => compareFullHouse(left, right)
      case (left: Flush, right: Flush) => compareFlush(left, right)
      case (left: Straight, right: Straight) => compareStraight(left, right)
      case (left: ThreeOfAKind, right: ThreeOfAKind) => compareThreeOfAKind(left, right)
      case (left: TwoPair, right: TwoPair) => compareTwoPair(left, right)
      case (left: OnePair, right: OnePair) => compareOnePair(left, right)
      case (left: HighCard, right: HighCard) => compareHighCard(left, right)
      case (_, _) => indexOf(x) - indexOf(y)
    }
  }

  def compareFourOfAKind(x: FourOfAKind, y: FourOfAKind): Int = {
    val groupX = x.pokerHand.groupBy(e => e.rank).toList.sortBy(f=> (f._2.size, f._1)).map(_._1).reverse
    val groupY = y.pokerHand.groupBy(e => e.rank).toList.sortBy(f=> (f._2.size, f._1)).map(_._1).reverse

    compareListOfCardRank(groupX, groupY)
  }

  def compareFullHouse(x: FullHouse, y: FullHouse): Int = {
    val groupX = x.pokerHand.groupBy(e => e.rank).toList.sortBy(f=> (f._2.size, f._1)).map(_._1).reverse
    val groupY = y.pokerHand.groupBy(e => e.rank).toList.sortBy(f=> (f._2.size, f._1)).map(_._1).reverse

    compareListOfCardRank(groupX, groupY)
  }


  def compareFlush(x: Flush, y: Flush): Int = compareListOfCardRank(
    x.pokerHand.map(_.rank), y.pokerHand.map(_.rank)
  )

  def compareStraight(x: Straight, y: Straight): Int = {
    val left: CardRank = if (x.pokerHand.head.rank == CardRank.ACE && x.pokerHand(4).rank == CardRank.TWO) CardRank.FIVE else x.pokerHand.head.rank
    val right: CardRank = if (y.pokerHand.head.rank == CardRank.ACE && y.pokerHand(4).rank == CardRank.TWO) CardRank.FIVE else y.pokerHand.head.rank

    compareCardRank(left, right)
  }

  def compareStraightFlush(x: StraightFlush, y: StraightFlush): Int = {
    val left: CardRank = if (x.pokerHand.head.rank == CardRank.ACE && x.pokerHand(4).rank == CardRank.TWO) CardRank.FIVE else x.pokerHand.head.rank
    val right: CardRank = if (y.pokerHand.head.rank == CardRank.ACE && y.pokerHand(4).rank == CardRank.TWO) CardRank.FIVE else y.pokerHand.head.rank

    compareCardRank(left, right)
  }

  def compareThreeOfAKind(x: ThreeOfAKind, y: ThreeOfAKind): Int = {
    val groupX = x.pokerHand.groupBy(e => e.rank).toList.sortBy(f=> (f._2.size, f._1)).map(_._1).reverse
    val groupY = y.pokerHand.groupBy(e => e.rank).toList.sortBy(f=> (f._2.size, f._1)).map(_._1).reverse

    compareListOfCardRank(groupX, groupY)
  }


  def compareTwoPair(x: TwoPair, y: TwoPair): Int = {
    val groupX = x.pokerHand.groupBy(e => e.rank).toList.sortBy(f=> (f._2.size, f._1)).map(_._1).reverse
    val groupY = y.pokerHand.groupBy(e => e.rank).toList.sortBy(f=> (f._2.size, f._1)).map(_._1).reverse

    compareListOfCardRank(groupX, groupY)
  }

  def compareOnePair(x: OnePair, y: OnePair): Int = {
    val groupX = x.pokerHand.groupBy(e => e.rank).toList.sortBy(f=> (f._2.size, f._1)).map(_._1).reverse
    val groupY = y.pokerHand.groupBy(e => e.rank).toList.sortBy(f=> (f._2.size, f._1)).map(_._1).reverse

    compareListOfCardRank(groupX, groupY)
  }

  def compareHighCard(x: HighCard, y: HighCard): Int = compareListOfCardRank(x.pokerHand.map(_.rank), y.pokerHand.map(_.rank))

  @tailrec
  private def compareListOfCardRank(x: List[CardRank], y: List[CardRank]): Int =
    (x, y) match {
      case (el :: Nil, el1 :: Nil) => compareCardRank(el, el1)
      case (xHead :: xTail, yHead :: yTail) => if (CardRank.compare(xHead, yHead) == 0) compareListOfCardRank(xTail, yTail)
      else compareCardRank(xHead, yHead)
    }

  private def compareCardRank(left: CardRank, right: CardRank): Int = {
    CardRank.compare(left, right) match {
      case a if a > 0 => 1
      case b if b < 0 => -1
      case _ => 0
    }
  }
}
