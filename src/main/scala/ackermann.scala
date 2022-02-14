import scala.util.control.TailCalls._

object Ackermann {
  //The Ackermann function
  //TODO implement optimized tail recursion
  def Ackermann_function(m:Int, n:Int): Int = {
    def helper(m: Int, n:Int): TailRec[Int] = {
      if(m <= 0) {
        done(n + 1)
      } else if (n <= 0) {
        helper(m-1, 1)
      } else {
        helper(m-1, helper(m, n-1).result)
      }
    }
    helper(m, n).result
  }
}
