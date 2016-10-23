package recfun

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
    if (c > r) 0
    else if (c == 0 || r == 0) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceRec(chars: List[Char], accu: Int): Boolean = {
      if (accu < 0) false
      else chars match {
        case Nil    => accu == 0
        case '('::t => balanceRec(t, accu + 1)
        case ')'::t => balanceRec(t, accu - 1)
        case h::t   => balanceRec(t, accu)
      }
    }

    balanceRec(chars, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else coins match {
      case Nil => 0
      case h::t =>
        countChange(money, t) + countChange(money - h, coins)
    }
  }
}
