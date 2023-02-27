package com.evolution.bootcamp.assignment.poker.card

object CardSuit extends Enumeration {
  type CardSuit = Value
  val DIAMONDS, CLUBS, HEARTS, SPADES = Value

  def transformToSuit(name: String): Value =
    name match {
      case "d" => DIAMONDS
      case "c" => CLUBS
      case "h" => HEARTS
      case "s" => SPADES
      case _ => throw new NoSuchElementException("There is no suit in the deck with that value")
    }
}
