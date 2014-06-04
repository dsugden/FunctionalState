/*

 Effects: If your program doesn't perform any effects, its just going to get hot

 You CAN write scala in a principled way

 In the exclusive sense, functional means no side-effects.

 In the inclusive sense it means a programming style which composes functions in interesting ways

 Immutability is a way to force yourself to track effects

Here we are concerned about computations that carry state:

online banking
movie ticket kiosk
airline ticket purchasing
traffic lights


 State[S,A] allow you to track effects and compose computations

 */


object MutableBad{

  // author Fabian Searwar
  class MDate(dayIn:Int){ var day = dayIn  }

  type Task = Int

  def scheduleTask(task:Task,date:MDate) = ???

  var d = new MDate(22)

  scheduleTask(1, d)    // some other code block has a reference to d, changing d would be bad

  d.day = 33  // WAT??, I should have made a defensive copy of d

}


object FunctionalState {


    class TraditionalMutation{

      var total : List[Int] = Nil   // Mutable List

      /*
         Add i to total, return it's sum
       */
      def sum(i:Int):Int = {
        total = i :: total  // SIDE EFFECT
        if(total.length > 1) 99 else  total.sum // this is an contrived bug based on internal state
      }

      def min(i:Int):Int ={
        val sorted  = total.sorted
        total = total.sorted.tail // SIDE EFFECT
        sorted.head
      }

      def resetTotal:String = {
        val result = total.sum
        total = Nil
        result.toString
      }
    }


  /*
   Brief reminder of what Function composition looks like:

    f:A => B

    g: B => C

    g(f): A => C


    -This only works if I can substitute f with it's value

    - I can only substitute F with it's value if it has no side effect

    - Can't compose if function is => Unit

    
   */

    object TraditionalTest1 {

      val traditional = new TraditionalMutation

      // The type of this function is Int => Int, but it should be TraditionalMutation => Int => (TraditionalMutation,Int)
      val one = traditional.sum(1)

      // I haven't explicitly tracked the fact that traditional.total has changed
      val two = traditional.sum(2)

      // nothing tells me that traditional.total is different, I just better know
      // I have to worry about how  traditional evolves over time
      val three = traditional.sum(3)

    }


    // What if we made Traditional immutable?
    case class Better(total : List[Int])

    object Better{
      def sum(i:Int)(b:Better):(Better,Int) = {
        val result = if(b.total.length > 1) 99 else  b.total.sum
         val newBetter = b.copy(total = i :: b.total)
        (newBetter, result)
      }

    }


    // we notice this pattern  Better  => (Better,Int)

    type BetterUpdate =   Better => (Better,Int)

    val firstF:BetterUpdate = Better.sum(1)
    val secondF:BetterUpdate = Better.sum(2)
    val thirdF:BetterUpdate = Better.sum(3)


//     the external state is threaded through a sequence of computations
      val composeSumResults:BetterUpdate =  better =>  {

        // now I don't have to worry about anything else changing first better

        val (firstState,firstResult) = firstF(better)
        val (secondState, secondResult) = secondF(firstState)
        val (thirdStater, thirdResult) = thirdF(secondState)       // now when this blows up, we have secondState in scope for inspection

        (thirdStater, firstResult + secondResult + thirdResult)
      }

    // we notice  S => (S,A) is useful, almost composable and testable, but awkward... lets do better
    // Lets create a type that wraps the computation, and add some useful combinators: map, flatMap
    case class StateChange[S,A](run: S => (S,A)){

      def map[B](f:A => B):StateChange[S,B] = StateChange{ (s:S) =>
          val(s2,a) = run(s)
          (s2,f(a))
        }

      /*
       A magical moment!!!!!!  Now I can stick 2 StateChange's together: the resulting
       StateChange runs the first one, then uses the resulting state a the param for
       the second one: I have now threaded my state through 2 computations.
       */

      def flatMap[B](f: A => StateChange[S,B]):StateChange[S,B] = StateChange{ (s:S) =>
          val(s2,a) = run(s)
          val stateChangeToB = f(a)
          val (s3,b) = stateChangeToB.run(s2)
          (s3,b)
        }
    }


    def sum(i:Int) = StateChange[Better,Int](s => Better.sum(i)(s))


    def composeAddResultS(one:Int,two:Int,three:Int): StateChange[Better,Int] =
      sum(one).flatMap(
        firstResult =>
          sum(two).flatMap( secondResult =>
            sum(three).map( thirdResult =>  firstResult + secondResult + thirdResult) ))




}
