class Ackermann {
  //The Ackermann function
  def Ackermann_function(m:Int, n:Int): Int = {
    var ans: Int = 0
    m match {
      case 0 => ans = n+1
      case _ => n match {
        case 0 => ans = Ackermann_function(m-1, 1)
        case _ => ans = Ackermann_function(m-1,Ackermann_function(m,n-1))
      }
    }
    ans
  }
}
