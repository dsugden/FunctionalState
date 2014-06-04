object UseScalaz {


  def main(args: Array[String]): Unit = {

    import scalaz._


    case class Better(total: List[Int])

    def sum(i: Int): State[Better, Int] = State { b =>
      val result =  if(b.total.length > 1) 99 else  b.total.sum
      val newBetter =b.copy(total = i :: b.total)
      (newBetter, result)
    }


    val initial = State.init[Better] // lift a Better into State

    val test = for {
      r1 <- sum(1)
      state0 <- State.get[Better]
      r2 <- sum(2)
      state1 <- State.get[Better]
      r3 <- sum(3)
      state2 <-  State.get[Better]
    } yield {
      (
        state0 :: state1 :: state2 :: Nil,
        r1 :: r2 :: r3 :: Nil
      )

    }

    val someBetter = Better(Nil)

    test.run(someBetter)._2._1.zipWithIndex.foreach { case (v, i) => println(s"state$i - $v")}

    test.run(someBetter)._2._2.zipWithIndex.foreach { case (v, i) => println(s"result$i - $v")}

  }

}
