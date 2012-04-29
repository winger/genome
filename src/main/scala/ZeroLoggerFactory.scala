import com.dongxiguo.zeroLog._
import com.dongxiguo.zeroLog.formatters.Formatter
import java.io.{Writer, PrintWriter}
import java.util.Calendar
import util.logging.{Logged, ConsoleLogger}

abstract class MyFormatter(loggerNameInitial: => String) extends Formatter with Logged {
  import MyFormatter._
  
  override def log(s: String)

  private var loggerName: () => String =
    createLazy(loggerName_=, loggerNameInitial)

  final def this(singleton: Singleton) = this {
    singleton.asInstanceOf[AnyRef].getClass.getCanonicalName match {
      case MyFormatter.SingletonPattern(className) => className
      case _ =>
        throw new IllegalArgumentException(
          singleton + " should be a singleton object.")
    }
  }

  private def writeTime(buffer: StringBuilder) {
    val now = Calendar.getInstance
    import now.get
    import Calendar._
    buffer ++=
      new ZeroFilledInt(get(YEAR), 4) += '-' ++=
      new ZeroFilledInt(get(MONTH) + 1, 2) += '-' ++=
      new ZeroFilledInt(get(DATE), 2) += ' ' ++=
      new ZeroFilledInt(get(HOUR_OF_DAY), 2) += ':' ++=
      new ZeroFilledInt(get(MINUTE), 2) += ':' ++=
      new ZeroFilledInt(get(SECOND), 2) += ' '
  }

  private def writeHead(buffer: StringBuilder, level: Level) {
    writeTime(buffer)
    buffer ++= loggerName() ++= " " ++= level.name ++= ": "
  }

  implicit override final def pairToAppendee[A](pair: (A, Throwable))(implicit converter: A => Appendee) = {
    buffer: StringBuilder =>
      val (message, thrown) = pair
      converter(message)(buffer)
      buffer += ' '
      thrown.printStackTrace(new PrintWriter(toWriter(buffer)))
  }

  implicit override final def thrownToAppendee(thrown: Throwable) = {
    buffer: StringBuilder =>
      thrown.printStackTrace(new PrintWriter(toWriter(buffer)))
  }

  implicit override final def log(content: Appendee, level: Level) {
    val buffer = new StringBuilder
    writeHead(buffer, level)
    content(buffer)
    log(buffer.toString)
  }

}

private object MyFormatter {

  private val SingletonPattern = """^(.*)\$$""".r

  final class ZeroFilledInt(n: Int, minSize: Int, radix: Int = 10) extends Traversable[Char] {
    override def foreach[U](f: Char => U) {
      assert(minSize >= 0)
      assert(n >= 0)
      var zeros = minSize
      var i = 1
      var t = n
      while(i < t) {
        if (i != 100000000) {
          i *= 10
          zeros -= 1
        } else {
          while (i != 1) {
            f(java.lang.Character.forDigit(t / i, radix))
            t %= i
            i /= 10
          }
          f(java.lang.Character.forDigit(t, radix))
          return
        }
      }
      while (zeros > 0) {
        f('0')
        zeros -= 1
      }
      while (i > 1) {
        i /= 10
        f(java.lang.Character.forDigit(t / i, radix))
        t %= i
      }
    }
  }

  private def toWriter(sb: StringBuilder) = new Writer {
    override final def close() {}
    override final def flush() {}
    override final def write(cbuf: Array[Char], off: Int, len: Int) {
      sb.appendAll(cbuf, off, len)
    }
    override final def write(c: Int) {
      sb += c.asInstanceOf[Char]
    }
  }

  private def createLazy(setter: (() => String) => Unit,
                               initial: => String) = { () =>
    val result = initial
    setter { () => result }
    result
  }

}

object ZeroLoggerFactory {
  def newLogger(singleton: Singleton) =
    (Filter.Info, new MyFormatter(singleton) with ConsoleLogger)
}