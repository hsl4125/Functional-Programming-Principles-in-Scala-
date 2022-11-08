package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    (c, r) match {
      case (0, _) => 1
      case (_, 0) => 1
      case (c, r) if c == r => 1
      case (c, r) => pascal(c-1, r-1) + pascal(c, r-1)
    }
  end pascal

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def inner(chars: List[Char], count: Int): Boolean =
      chars match {
        case Nil => count == 0
        case '(' :: xs => inner(xs, count + 1)
        case ')' :: _ if count < 1 => false
        case ')' :: xs => inner(xs, count - 1)
        case _ :: xs => inner(xs, count)
      }
    inner(chars, 0)
  end balance

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || coins.isEmpty)
      0
    else if (money == 0)
      1
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  end countChange



