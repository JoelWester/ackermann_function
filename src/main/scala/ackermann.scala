import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn.readLine

object Ackermann extends App {

  /**Since scala uses up the stack quite quickly when doing recursion we have to be smart about what we compute.
   * By using a dynamic programming approach to define the Ackermann function we can both reduce the stack size and
   * increase computing speed.
   */
  //Create hashmap
  var cache:mutable.HashMap[String, BigInt] = new mutable.HashMap()
  //The Ackermann function
  def Ackermann_function(m:BigInt, n:BigInt): BigInt = {

    val key: String = m.toString()+"*"+n.toString()

    if (cache.contains(key)) {  //Is it already cached?
      cache(key)
    } else {
      cache.put(key, (Hyperoperation.H(2, m, n+3)-BigInt(3)))
      println(s"A(${m}, ${n}) => ${cache(key)}")
      cache(key)
    }
  }

  for(m <- 0 to 4) {
    try {
      for(n <- 0 to 1000) {
        Ackermann_function(m, n)
      }
    }
    catch {
      case e: StackOverflowError => println("Stack overflow")
      case _: Throwable => println(s"Error: " + _)
    }
  }

  //I/O
  while(true) {
    print("Enter m: ")
    var m = BigInt(readLine())
    print("Enter n: ")
    var n = BigInt(readLine())
    var result: Option[BigInt] = None
    //Try catch incase it stackoverflows
    try {
      var result = Some(Ackermann_function(m,n))
      println(s"A(${m},${n}) => ${result.get}")
    }
    catch {
      case e: StackOverflowError => println("Stack overflow")
      case _: Throwable => println(s"Error: " + _)
    }
  }
}
