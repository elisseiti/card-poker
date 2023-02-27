package com.evolution.bootcamp.assignment.poker.card

object CardRank extends Enumeration {
  type CardRank = Value
  val TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, JACK, QUEEN, KING, ACE = Value

  def transformToRank(name: String): Value =
    name match {
      case "2" => TWO
      case "3" => THREE
      case "4" => FOUR
      case "5" => FIVE
      case "6" => SIX
      case "7" => SEVEN
      case "8" => EIGHT
      case "9" => NINE
      case "T" => TEN
      case "J" => JACK
      case "Q" => QUEEN
      case "K" => KING
      case "A" => ACE
      case _ => throw new NoSuchElementException("There is no element with that rank value")
    }

  def compare(left: CardRank, right: CardRank): Int = {
    left.id - right.id
  }
}
