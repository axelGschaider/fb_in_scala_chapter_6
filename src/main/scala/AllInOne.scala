
trait RNG {
	def nextInt:(Int, RNG)
}


object RNG {

	type Rand[+A] = RNG => (A, RNG)

	val int: Rand[Int] = _.nextInt

	def unit[A](a:A):Rand[A] =  (a, _)

	def map[A,B](s:Rand[A])(f:A=>B):Rand[B] =
		rng => {
			val (a, rng2) = s(rng)
			(f(a), rng2)
		}

	def map2[A,B,C](ra:Rand[A], rb:Rand[B])(f:(A,B) => C):Rand[C] = 
		rng => {
			val (a,rng2) = ra(rng)
			val (b,rng3) = rb(rng2)
			( f(a,b), rng3 )
		}

	def both[A,B](ra:Rand[A], rb:Rand[B]):Rand[(A,B)] = map2(ra,rb)((_,_))

	def nonNegativeEven:Rand[Int] = 
		map(nonNegativeInt)( i => i - i % 2 )
	
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

	val double:Rand[Double] =
		map(biggerThanZero)(i => (i-1).toDouble / Int.MaxValue.toDouble)

	val randIntDouble:Rand[(Int,Double)] = both(int, double)
	val randDoubleInt:Rand[(Double,Int)] = both(double, int)
	
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