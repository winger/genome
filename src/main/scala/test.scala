import ru.ifmo.genome.dna._
import ru.ifmo.genome.dna.Base._
import ru.ifmo.genome.dna.DNASeq._

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

var ar = SmallDNASeq(A, T, G, C, A)
for (it <- 0 until 5) {
  ar = ar ++ ar
}
println(ar.length + " " + ar)
assert(ar == ar.revComplement.revComplement)