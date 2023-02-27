package com.evolution.bootcamp.assignment.poker.card

sealed abstract class PokerHand

case class StraightFlush(pokerHand: List[PokerCard]) extends PokerHand

case class FourOfAKind(pokerHand: List[PokerCard]) extends PokerHand

case class FullHouse(pokerHand: List[PokerCard]) extends PokerHand

case class Flush(pokerHand: List[PokerCard]) extends PokerHand

case class Straight(pokerHand: List[PokerCard]) extends PokerHand

case class ThreeOfAKind(pokerHand: List[PokerCard]) extends PokerHand

case class TwoPair(pokerHand: List[PokerCard]) extends PokerHand

case class OnePair(pokerHand: List[PokerCard]) extends PokerHand

case class HighCard(pokerHand: List[PokerCard]) extends PokerHand
