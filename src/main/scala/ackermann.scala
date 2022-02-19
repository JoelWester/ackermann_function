import scala.collection.mutable
import scala.io.StdIn.readLine

object Ackermann extends App {

  /**Since scala uses up the stack quite quickly when doing recursion we have to be smart about what we compute.
   * By using a dynamic programming approach to define the Ackermann function we can both reduce the stack size and
   * increase computing speed.
   */
  //TODO optimize with dynamic programming
  //Create hashmap
  var cache:mutable.HashMap[String, BigInt] = new mutable.HashMap()
  var actcall = BigInt(0)
  var call = BigInt(0)
  //The Ackermann function
  def Ackermann_function(m:BigInt, n:BigInt): BigInt = {

    val key: String = m.toString()+"*"+n.toString()

    if (cache.contains(key)) {
      actcall = actcall.+(BigInt(1))
      cache.remove(key).get
    } else if (m.compare(BigInt(0)) == 0) {
      n.+(1)
    } else if (m == 1) {
      n.+(2)
    } else if (m.compare(BigInt(0))>0 & n.compare(BigInt(0)) == 0) {
      call = call.+(1)
      cache.put(key, Ackermann_function(m.-(1), BigInt(1)))
      cache(key)
    } else {
      call = call.+(1)
      call = call.+(1)
      cache.put(key, Ackermann_function(m.-(1), Ackermann_function(m, n.-(1))))
      cache(key)
    }
  }

  while(true) {
    print("Enter m: ")
    var m = BigInt(readLine())
    print("Enter n: ")
    var n = BigInt(readLine())
    var result: Option[BigInt] = None
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
