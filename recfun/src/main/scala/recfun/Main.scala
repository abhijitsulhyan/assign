package recfun

import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    swapSpecialChars("AB?B?B?H=?")
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 && c == 0) 1
    else if (c < 0 || r < 0) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(l: Int, r: Int, localChars: List[Char]): Boolean = {
      if (localChars.isEmpty) {
        l == r && l != 0
      }
      else if (localChars.head == '(') {
        loop(l + 1, r, localChars.tail)
      } else if (localChars.head == ')' && l > r) {
        loop(l, r + 1, localChars.tail)
      } else {
        loop(l, r, localChars.tail)
      }
    }

    loop(0, 0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if(coins.isEmpty) {
      if (money == 0) 1 else 0
    }
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

  def swapSpecialChars(str: String) = {
    val chars = str.toList

    val possibilities = List()

    def loop(chars: List[Char], acc: List[Char]): List[Char] = {
         if(chars.isEmpty) {
           possibilities :+ acc
           println(acc)
           acc
         } else {
           if(chars.head == '?') {
             loop(chars.tail, acc :+ '0')
             loop(chars.tail, acc :+ '1')
           } else {
             loop(chars.tail, acc :+ chars.head)
           }
         }
    }

    loop(str.toList, List())
    println(possibilities)

  }
}
