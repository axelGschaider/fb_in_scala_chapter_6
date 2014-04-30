
trait RNG {
	def nextInt:(Int, RNG)
}

object RNG {
	def nonNegativeInt(rng:RNG):(Int,RNG) = {
		val (i,newRng) = rng.nextInt
		val posI = if(i >= 0) i
		           else ( i + 1 ) * (-1)
		(posI, newRng) 
	}

	def biggerThanZero(rng:RNG):(Int,RNG) = {
		val (i,newRng) = nonNegativeInt(rng)
		if(i == 0) biggerThanZero(newRng)
		else       (i, newRng)
	}

	def double(rng:RNG):(Double, RNG) = {
		val (x, r) = biggerThanZero(rng)
		val d = (x-1).toDouble / Int.MaxValue.toDouble
		(d,r)
	}

	def intDouble(rng:RNG):((Int,Double), RNG) = {
		val (i, rng1) = rng.nextInt
		val (d, rng2) = double(rng1)

		( (i,d), rng2 )
	}

	def doubleInt(rng:RNG):((Double,Int),RNG) = intDouble(rng) match {
		case ( (i,d),r ) => ( (d,i),r )
	}

	def double3(rng:RNG):((Double,Double,Double),RNG) = {
		val (d1,r1) = double(rng)
		val (d2,r2) = double(r1)
		val (d3,r3) = double(r2)

		( (d1,d2,d3), r3 )
	}

	def ints(count:Int)(rng:RNG):(List[Int],RNG) = 
		if(count <= 0) (Nil,rng)
		else {
			val (i ,r1) = rng.nextInt
			val (is,r2) = ints( count - 1 )(r1)
			(i::is, r2)
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