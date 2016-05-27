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
      def verify(counter: Int, elements: List[Char]): Boolean = {
          if (elements.isEmpty) {
              if (counter == 0) true
              else false
          } else if(counter < 0) {
              false
          } else {
              var tempCounter = counter
              if (elements.head == '(') {
                  tempCounter = counter + 1
              } else if (elements.head == ')') {
                  tempCounter = counter - 1
              }
              verify(tempCounter, elements.tail)
          }
      }
      verify(0, chars)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
