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
    if (r < 0 || c < 0 || c > r) -1
    else {
      if (c == 0 || c == r) 1
      else {
        pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def cal(chars: List[Char], left: Int): Boolean = {
      if (chars.isEmpty) {
        left == 0
      } else {
        val h = chars.head
        val n =
          if (h == '(') left + 1
          else if (h == ')') left - 1
          else left
        if (n >= 0) cal(chars.tail, n)
        else false
      }
    }

    cal(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def cal2(left: List[(Int, Int)], count: Int): Int = {
      if (left.isEmpty) {
        count
      } else {
        val b = ListBuffer[(Int, Int)]()
        var update = count

        for ((lastMax, total) <- left) {
          if (total < money) {
            for (c <- coins) {
              if (c >= lastMax) {
                val e = (c, total + c)
                b += e
              }
            }
          } else if (total == money) {
            update += 1
          }
        }

        cal2(b.toList, update)
      }
    }

    val b = coins.map { c => (c, c) }
    cal2(b, 0)
  }
}

