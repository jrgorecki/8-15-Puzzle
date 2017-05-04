object Driver{
  def main(args: Array[String]) {

    val goal8 = State(Vector(0, 1, 2, 3, 4, 5, 6, 7, 8))
    
    val test8 = State(Vector(8, 7, 6, 0, 4, 1, 2, 5, 3))

    val t = System.currentTimeMillis


   //  val x = NpuzzleSolver(goal8, test8).solutonManhattanMisplacedAStar
      val y = NpuzzleSolver(goal8, test8).solutionManhattanAStar
       println(s"8 took ${(System.currentTimeMillis - t) / 1000.0}")
       
       val t2 = System.currentTimeMillis
       
       val goal15 = State(Vector(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
       val test15 = State(Vector(12,1,2,15,11,6,5,8,7,10,9,4,0,13,14,3))
       
      // val z = NpuzzleSolver(goal15, test15).solutionManhattanIDAStar
       println(s"15 took ${(System.currentTimeMillis - t2) / 1000.0}")
       
       //val w = NpuzzleSolver(goal15, test15).solutionMisplacedIDAStar
       
        
  }
}
