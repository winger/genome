import ru.ifmo.genome.dna._
import ru.ifmo.genome.dna.Base._
import ru.ifmo.genome.dna.DNASeq._

/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

var ar = SmallDNASeq(A, T, G, C, A)
println(ar + " " + ar.reverse + " " + ar.complement + " " + ar.revComplement)