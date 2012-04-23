import akka.dispatch._
import akka.util.duration._
import java.util.concurrent.Executors
import ru.ifmo.genome.dna._
import ru.ifmo.genome.dna.Base._
import ru.ifmo.genome.dna.DNASeq._

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object Test extends App {

  implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  def f(s: String) = Future {
    println("start: " + s)
    Thread.sleep(1000)
    println("stop: " + s)
    s
  }
  val sum = for {
    a <- f("A")
    b <- f("B")
    c <- f("C")
  } yield a + b + c

  println(Await.result(sum, 10.seconds))

  ec.shutdown()

  var ar = SmallDNASeq(A, T, G, C, A)
  for (it <- 0 until 5) {
    ar = ar ++ ar
  }
  println(ar.length + " " + ar)
  assert(ar == ar.revComplement.revComplement)
}