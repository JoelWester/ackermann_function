import scala.io.StdIn.readLine

object Hyperoperation extends App{
  /** A hyperophyeration sequence is an infinite sequence of arithmetic operations that start from a unary operation(a[n]b v n = 0 => a[0]b = b+1)
   * and continue with addition(n=1), multiplication(n=2) and exponentiation(n=3) and so forth.
   */
  /**
  sealed abstract class hyperoperation
  case class H(a: BigInt, n: BigInt, b:BigInt) extends hyperoperation

  case H(a, 0, b) => Some(S(b))
  case H(a, S(b), 0) => a
  case H(a, S(S(0)), 0) => 0
  case H(a, S(S(S(n))), 0) => S(0)
  case H(a, S(n), S(b)) => H(a,n,H(a,S(n),b))
  */
  def H(a: BigInt, n: BigInt, b:BigInt): BigInt = {
    if(n.compare(BigInt(0)) == 0) {
      b + 1
    } else if ((n == 1)&(b == 0)) {
      a
    } else if ((n == 2)&(b == 0)) {
      0
    } else if((n >= 3)&(b == 0)) {
      1
    } else {
      H(a, n-1, H(a, n, b-1))
    }
  }

  //I/O
  while(true) {
    print("Enter a: ")
    var a = BigInt(readLine())
    print("Enter n: ")
    var n = BigInt(readLine())
    print("Enter b: ")
    var b = BigInt(readLine())
    var result: Option[BigInt] = None
    //Try catch incase it stackoverflows
    try {
      var result = Some(H(a,n,b))
      println(s"H${n}(${a},${b}) => ${result.get}")
    }
    catch {
      case e: StackOverflowError => println("Stack overflow")
      case _: Throwable => println(s"Error: " + _)
    }
  }


}
