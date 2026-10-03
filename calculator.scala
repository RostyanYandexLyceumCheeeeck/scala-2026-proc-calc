import scala.util.boundary
import scala.util.boundary.break

/** Software implementation of PROC (PROstoy Calculator) mk. 1 (or mk. 2).
  *
  * You should finish this procedure according to
  * the reference described in `README.md` to complete
  * the assignment.
  */
@main def calculator(commands: String*): Unit = {
  /** Converts given string `s` to integer.
    *
    * Throws [[NumberFormatException]] if `s` can't be converted to integer,
    * but you shouldn't worry about it at this moment.
    */
  def parseInt(s: String): Int = s.toInt

  /** Representation of `acc` register. */
  var acc: Int = 0
  var a: Int = 0
  var b: Int = 0
  var blink: Boolean = false
  // define additional registers here

  boundary {
    for (c <- commands) {
      c match {
          case "+" =>
            acc = a + b
            blink = false
          
          case "-" =>
            acc = a - b
            blink = false
          
          case "*" =>
            acc = a * b
            blink = false
          
          case "/" =>
            if (b == 0) {
              acc = 0
              a = 0
              b = 0
              blink = false
            } else {
              acc = a / b
              blink = false
            }

          case "swap" =>
            val tmp = a
            a = b
            b = tmp
          
          case "blink" =>
            blink = !blink
          
          case "acc" =>
            if (!blink) a = acc else b = acc
            blink = !blink

          case "break" =>
            break()

          case numStr =>
            val num = parseInt(numStr)
            if (!blink) a = num else b = num
            blink = !blink
      }
    }
  }

  println(acc)
}
