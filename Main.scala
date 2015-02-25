package recfun

object Main {
 
  /**
   * Task 1
   */
  def pascal(c: Int, r: Int): Int = if (c>r)0 else if (c==1|c==r)r else if (c==(r-1))r else (pascal(c-1,r-1)+pascal(c,r-1))
 
		  
  /** 
   *  Task 2 - Part A
   */
  def fib(n: Long): Long = if (n==1|n==0)n else (fib(n-1)+fib(n-2))

  /** 
   *  Task 2 - Part B
   */
  def fastFib(x: Long ): Long = {
	val steps: Long = x-1
			def fibStep(x:(Long, Long)): (Long, Long) = return (x._2,x._1+x._2)
					def step(x:Long,y:(Long,Long)): (Long) = if (x!=steps)step(x+1,fibStep(y))	else return y._2
	step(0,(0,1))
  }

  /**
   * Task 3
   */
  def balance(chars: List[Char]): Boolean = {
    	def checkChar(chars: List[Char], n:Int):Int = {
    		if (chars.isEmpty)return n else {
    			val x: Int = {
    				if(chars.head == '(')n+1
    				else if (chars.head == ')')n-1
    				else n
    			}
    			if (x>=0)checkChar(chars.tail,x) else x //exclude head and repeat with the remaining characters
    		}
    	}
   if (chars.isEmpty) false else { val i= checkChar(chars,0);
   if (i==0)true else false}
  }

  /** 
   *  Task 4
   */
  def countChange(money: Int, coins: List[Int]): Int = {
		def expand(moneyLeft: Int, coins: List[Int]): Int = countChange(moneyLeft-coins.head,coins) + countChange(moneyLeft,coins.tail);
	if (coins.isEmpty)return 0
	else if(money==0)return 1
	else if (money<0)return 0
	else {
		expand(money, coins) 
	}
  }
}
