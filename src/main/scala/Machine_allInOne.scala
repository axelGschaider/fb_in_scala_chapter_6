import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine( locked : Boolean
	              , candies: Int
	              , coins  : Int
	              ) 

object Machine {

	def setLock(status:Boolean):State[Machine, Unit] = modify(_.copy(locked = status))
	val lock = setLock(true)
	val unlock = setLock(false)
	val isLocked:State[Machine, Boolean] = get.map(_.locked)
	val candyLeft:State[Machine, Boolean] = get.map(_.candies > 0)
	val addCoin:State[Machine, Unit] = modify( m => m.copy( coins = m.coins + 1 ) )
	val dispenseCandy:State[Machine, Unit] = modify( m => m.copy( candies = m.candies - 1 ) )



	def handleCoin:State[Machine, Unit] = for{ _ <- unlock 
                                               _ <- addCoin
	                                         } yield ()
	def handleTurn:State[Machine, Unit] = for { _ <- dispenseCandy 
	                                            _ <- lock
	                                          } yield ()
	def doNothing:State[Machine, Unit] = unit( () )

	def process(input:Input):State[Machine,Unit] = 
		for{ react  <- candyLeft
			 locked <- isLocked
             _      <- (input match { 
             				case _ if (!react)    => doNothing
             				case Coin if(locked)  => handleCoin
             				case Turn if(!locked) => handleTurn
             				case _                => doNothing
                      	})
		   } yield ()

	val evaluate:State[Machine, (Int, Int)] = get.map( s => (s.coins, s.candies) ) 

	def simulateMachine(inputs:List[Input]):State[Machine, (Int,Int)] = inputs match {
		case Nil     => evaluate
		case (i::is) => process(i).flatMap( _ => simulateMachine(is) )
	}

}

