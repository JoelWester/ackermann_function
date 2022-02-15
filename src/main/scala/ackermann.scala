import Array._

object Ackermann {

  /**Since scala uses up the stack quite quickly when doing recursion we have to be smart about what we compute.
   * By using a dynamic programming approach to define the Ackermann function we can both reduce the stack size and
   * increase computing speed.
   */
  //TODO optimize with dynamic programming
  //The Ackermann function
  def Ackermann_function(m:Int, n:Int): Int = {
    //create a matrix of dimensions m*n
    var cache = ofDim[Int](m+1,n+1)
    for (rows <- cache.indices) {
      for (cols <- cache(0).indices) {
        //All answers for A(0,n) are going to be n+1
        if (rows == 0) {
          cache(rows)(cols) = cols + 1
        } else if (cols == 0){
          //A(m-1,1)
          cache(rows)(cols) = cache(rows-1)(1)
        } else {
          //If both rows and cols are greater than 0, then we have to use A(m-1,A(m,n-1))
          val r = rows - 1
          val c = cache(rows)(cols - 1)
          var ans: Int = 0
          if(r == 0) {
            ans = c + 1
          } else if (c <= n) {
            ans = cache(rows-1)(cache(rows)(cols-1))
          } else {
            ans = (c-n)*r + cache(r)(n)
          }
          cache(m)(n) = ans
        }
      }
    }
    cache(m)(n)
   }
}
