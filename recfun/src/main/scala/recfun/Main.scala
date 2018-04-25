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
      if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)      
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      // println(chars)
      var open = 0
      var clos = 0
      val x: List[Char] = chars
      
      def doit(x: List[Char]): Boolean = {
        if (x.head == '(')  open += 1
        if (x.head == ')')  clos += 1
        
        // println("open = " + open + " clos = " + clos)
        if (clos > open) false
        else {
         if (x.length == 1) {
          if (open == clos) true else false 
         } else {
           doit(x.tail)
         }
        } 
      }
      
      if (chars.isEmpty) false else doit(chars)
    }
        
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      var foundall = 0
      var totalAmount = money
      
      import util.control.Breaks._
      import scala.collection.mutable.ListBuffer
      var coinBuf = new ListBuffer[Int]()
      var subsetBuf = new ListBuffer[Int]()
      val mycoins = coins.distinct.filter(_ <= money).sorted
      var coinCountBuf = new ListBuffer[Int]()
      // var subsetSize = 0
      
      def doit(money: Int, coinCountBuf: ListBuffer[Int], pos: Int, subsetBuf: ListBuffer[Int], len: Int, coins: List[Int]): Int = {
          // println("subsetBuf: " + subsetBuf.take(len))
          // println("coinCountBuf: " + coinCountBuf)
          
          if (subsetBuf.take(len).sum == totalAmount) foundall += 1
          
          if (coins.isEmpty) foundall
          else {
            for(i <- pos to coins.length-1) {
              if (coinCountBuf(i) > 0) {
                // println("i = " + i + " len = " + len)
                subsetBuf(len) = coins(i)
                coinCountBuf(i) -= 1          
                doit(money, coinCountBuf, i, subsetBuf, len+1, coins) 
                coinCountBuf(i) += 1
                // println("i:: " + i + " len:: " + len)
              } 
            }
            
            foundall 
          }
      }
      
      for(i <- 0 to mycoins.length-1)  {
        coinCountBuf.append(money/mycoins(i))
        // coinBuf = (coinBuf.toList ::: List.fill(card)(mycoins(i))).to[ListBuffer]      
        
      }
      
      subsetBuf = (coinBuf.toList ::: List.fill(coinCountBuf.sum)(0)).to[ListBuffer]
      
      // println("coinCountBuf : " + coinCountBuf)
      // println("mycoins : " + mycoins)
      if (money == 0 || coins.isEmpty ) 0 else doit(money, coinCountBuf, 0, subsetBuf, 0, mycoins.toList)
      
    }
    
    
  }
