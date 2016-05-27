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
    def pascal(column: Int, row: Int): Int = {
      val wrongPosition: Int = -1
      val initial: Int = 1
      val invalidCellValue: Int = 0

      if(column <= wrongPosition || row <= wrongPosition || column > row) invalidCellValue
      else {
          if(column == 0) initial
          else(pascal(column - 1, row - 1) + pascal(column, row -1))
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      true
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }