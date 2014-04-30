
trait RNG {
	def nextInt:(Int, RNG)
}

object RngUser {
	def nonNegativeInt(rng:RNG):(Int,RNG) = {
		val (i,newRng) = rng.nextInt
		val posI = if(i >= 0) i
		           else ( i + 1 ) * (-1)
		(posI, newRng) 
	}
}

case class Simple(seed:Long) extends RNG {
	def nextInt:(Int, RNG) = {
		val newSeed = (seed * 0x5DEECE66DL + 0xBL) &0xFFFFFFFFFFFFL
		val nextRNG = Simple(newSeed)
		val n = (newSeed >>>16).toInt
		(n, nextRNG)
	}
}