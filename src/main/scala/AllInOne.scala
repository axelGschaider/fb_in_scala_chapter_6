
case class State[S, +A](run:S => (A,S)) {

	import State._

	def flatMap[B](g: A => State[S,B]):State[S,B] = 
		State( s => {
			val (a,s2) = run(s)
			g(a) run s2
		} )

	def map[B](f:A=>B):State[S,B] = flatMap( f andThen unit )

	def map2[B,C]( sb:State[S,B] )(f:(A,B) => C):State[S,C] =
		flatMap(a => sb.map( f(a,_) ) )



}

object State {
	def unit[S,A](a:A):State[S,A] = State( s => (a,s) )

	def prepend[S,A](s:State[S,A], to:State[S, List[A]]):State[S, List[A]] = 
		s.map2(to){_ :: _}

	def get[S]:State[S,S] = State(s => (s,s))
	def set[S](s:S):State[S,Unit] = State( _ => ( (), s ) )

	def modify[S](f:S => S):State[S, Unit] =
		for { s <- get 
		      _ <- set(f(s))
		    } yield ()

	//def sequence[S,A](l:List[State[S,A]]):State[S, List[A]] =
	//	l.foldRight( unit(List.empty[A]) ){ case (s,to) => prepend _ }

}

trait RNG {
	def nextInt:(Int, RNG)
}


object RNG {

	type Rand[+A] = State[RNG, A]

	val int: Rand[Int] = State(_.nextInt)

	def both[A,B](ra:Rand[A], rb:Rand[B]):Rand[(A,B)] = ra.map2(rb)((_,_))

	def nonNegativeEven:Rand[Int] = 
		nonNegativeInt.map( i => i - i % 2 )
	
	def nonNegativeInt:Rand[Int] = 
		State( { rng:RNG =>
			val (i,newRng) = rng.nextInt
			val posI = if(i >= 0) i
		               else ( i + 1 ) * (-1)
			(posI, newRng) 
			} )
		

	def biggerThanZero:Rand[Int] = 
		State( {rng:RNG =>
			val (i,newRng) = nonNegativeInt run rng
			if(i == 0) biggerThanZero run newRng
			else       (i, newRng)
		})

	val double:Rand[Double] =
		biggerThanZero.map(i => (i-1).toDouble / Int.MaxValue.toDouble)

	val intDouble:Rand[(Int,Double)] = both(int, double)
	val doubleInt:Rand[(Double,Int)] = both(double, int)
	
	def double2:Rand[(Double, Double)] = both(double, double)

	def double3:Rand[(Double, Double, Double)] = 
		for { a <- double 
              b <- double
              c <- double
		    } yield (a,b,c)
		//double2.map2( double ){ case ( (a,b), c) => (a,b,c) }
	

	//def sequence[A](fs:List[Rand[A]]):Rand[List[A]] =
	//	fs.foldRight( State.unit( List.empty[A] ) )( State.prepend _ )

	//def ints(count:Int):Rand[List[Int]] = sequence( List.fill(count)(int) )

	def nonNegativeLessThan(n:Int):Rand[Int] = 
		nonNegativeInt flatMap { i => 
			val mod = i % n
			if (i + (n-1) - mod >= 0) State.unit(mod)
			else nonNegativeLessThan(n)
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