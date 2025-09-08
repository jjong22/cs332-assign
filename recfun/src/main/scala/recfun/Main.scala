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
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ( c==0 || r == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def innerfunction(parens: List[Char], lparensCount: Int): Boolean = {
      if (parens.isEmpty) lparensCount == 0
      else if (parens.head == '(')
        innerfunction(parens.tail,lparensCount + 1) // cnt + 1, add to stack
      else if (lparensCount == 0) false
      else
        innerfunction(parens.tail,lparensCount - 1) // cnt - 1, pop from stack
    }

    innerfunction(chars.filter(x => x == '(' || x == ')'), 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (coins.isEmpty) if (money == 0) 1 else 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
