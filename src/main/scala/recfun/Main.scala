package recfun
import common._

object Main {
  def main(args: Array[String]) {
   /** println("Pascal's Triangle") */
   
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
    if(c == 0 || r == c) 1 
    else (pascal(c-1, r-1) + pascal(c, r-1))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    def countParentheses(chars: List[Char], opened: Int, closed: Int, openParenthesis: Int): Boolean = { 
      if(chars.isEmpty) (opened == closed) && (openParenthesis == 0)
      else if (chars.head.equals('(')) countParentheses(chars.tail, opened+1, closed, openParenthesis+1)
      else if (chars.head.equals(')')) countParentheses(chars.tail, opened, closed+1, if(openParenthesis==0) openParenthesis else openParenthesis-1)
      else countParentheses(chars.tail,opened,closed, openParenthesis) 
    } 
    countParentheses(chars,0,0,0)
  }
       
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if(money > 0 && !coins.isEmpty) (countChange(money-coins.head, coins) + countChange(money, coins.tail))
    else 0      
  }  
  
  
  
}
