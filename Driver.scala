object Driver{
  def main(args: Array[String]) {

    val goal8 = State(Vector(0, 1, 2, 3, 4, 5, 6, 7, 8))
    
    //val test8 = State(Vector(8, 7, 6, 0, 4, 1, 2, 5, 3))
    val testshuf = scala.util.Random.shuffle(Vector(0, 1, 2, 3, 4, 5, 6, 7, 8))
    val test8 = State(testshuf)
    val t = System.currentTimeMillis


    val x = NpuzzleSolver(goal8, test8).solutionMisplacedAStar
    println(s"8 took ${(System.currentTimeMillis - t) / 1000.0}")
    val y = NpuzzleSolver(goal8, test8).solutionManhattanAStar
    println(s"8 took ${(System.currentTimeMillis - t) / 1000.0}")
       
       //val t2 = System.currentTimeMillis
       
       //val goal15 = State(Vector(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
       //val testt = scala.util.Random
       //val test15 = State(Vector(1,6,2,3,9,5,7,4,10,0,11,8,13,14,15,12))
       
       //val z = NpuzzleSolver(goal15, test15).solutionManhattanIDAStar
       //println(s"15 took ${(System.currentTimeMillis - t2) / 1000.0}")
       
     // val w = NpuzzleSolver(goal15, test15).solutionMisplacedIDAStar

     var prompt_flag: Boolean = false;
     while(!prompt_flag) {
        userInput = Console.readInt()
        option match {
          case 1 => {
            println("3x3 - Manhattan")
            var t = System.currentTimeMillis
            for(i <- 1 to 20) {
              testManhattanAStar(i)
            }
            println(s"3x3 - Manhattan took ${(System.currentTimeMillis - t) / 1000.0}")
          }
          case 2 => {
            println("3x3 - Misplaced Tiles")
            var t = System.currentTimeMillis
            for(i <- 1 to 20) {
              testMisplacedAStar(i)
            }
            println(s"3x3 - Misplaced Tiles took ${(System.currentTimeMillis - t) / 1000.0}")
          }
          case 3 => {
            println("3x3 - Linear Conflict")
            var t = System.currentTimeMillis
            for(i <- 1 to 20) {
              testLinearConflictAStar(i)
            }
            println(s"3x3 - Linear Conflict took ${(System.currentTimeMillis - t) / 1000.0}")
          }
          case 4 => {
            println("4x4 - Manhattan IDA")
            var t = System.currentTimeMillis
            for(i <- 1 to 20) {
              testManhattanIDAStar(i)
            }
            println(s"4x4 - Manhattan IDA took ${(System.currentTimeMillis - t) / 1000.0}")
          }
          case 5 => {
            println("4x4 - Misplaced IDA")
            var t = System.currentTimeMillis
            for(i <- 1 to 20) {
              testMisplacedIDAStar(i)
            }
            println(s"4x4 - Misplaced IDA took ${(System.currentTimeMillis - t) / 1000.0}")
          }
          case 6 => {
            println("4x4 - Linear Conflict")
            var t = System.currentTimeMillis
            for(i <- 1 to 20) {
              testManhattanAStar(i)
            }
            println(s"4x4 - Linear Conflict took ${(System.currentTimeMillis - t) / 1000.0}")
          }
        }
     }
       
        
  }

  def testManhattanAStar (n: Int) {
    var filename = "boards/3x3_" + n + ".txt"
    println("Level: "+ n)
    var testSteps = 0
    var time = 0.0
    for (line <- Source.fromFile(filename).getLines()) {
      val b = buildBoardFromFile(line)
      var t_0 = System.currentTimeMillis
      var x = NpuzzleSolver(goal8, test8).solutionManhattanAStar
      var t_1 = System.currentTimeMillis - t
      t += (t_1 - t_0)
      testSteps += 1
    }
    var avg = time/testSteps
    println("Average time: "+avg)
  }

  def testMisplacedAStar (n: Int ) {
    var filename = "boards/3x3_" + n + ".txt"
    println("Level: "+ n)
    var testSteps = 0
    var time = 0.0
    for (line <- Source.fromFile(filename).getLines()) {
      val b = buildBoardFromFile(line)
      var t_0 = System.currentTimeMillis
      var x = NpuzzleSolver(goal8, test8).solutionMisplacedAStar
      var t_1 = System.currentTimeMillis - t
      t += (t_1 - t_0)
      testSteps += 1
    }
    var avg = time/testSteps
    println("Average time: "+avg)
  }

  def testLinearConflictAStar(n: Int) {

  }

  def testManhattanIDAStar(n: Int) {
    var filename = "boards/4x4_" + n + ".txt"
    println("Level: "+ n)
    var testSteps = 0
    var time = 0.0
    for (line <- Source.fromFile(filename).getLines()) {
      val b = buildBoardFromFile(line)
      var t_0 = System.currentTimeMillis
      var x = NpuzzleSolver(goal8, test8).solutionManhattanIDAStar
      var t_1 = System.currentTimeMillis - t
      t += (t_1 - t_0)
      testSteps += 1
    }
    var avg = time/testSteps
    println("Average time: "+avg)
  }

  def testMisplacedIDAStar(n: Int) {
    var filename = "boards/4x4_" + n + ".txt"
    println("Level: "+ n)
    var testSteps = 0
    var time = 0.0
    for (line <- Source.fromFile(filename).getLines()) {
      val b = buildBoardFromFile(line)
      var t_0 = System.currentTimeMillis
      var x = NpuzzleSolver(goal8, test8).solutionMisplacedIDAStar
      var t_1 = System.currentTimeMillis - t
      t += (t_1 - t_0)
      testSteps += 1
    }
    var avg = time/testSteps
    println("Average time: "+avg)
  }

  def buildBoardFromFile(s: String): State = {
    var _counter = 0
    var _row = 1
    var _position: (Int, Int) = (0,0)
    var _vect: Vector[Int] = Vector()
    var _splitString = s.split(", ");

    for(row <- 1 to 3) {
      for(col <- 1 to 3) {
        var _test = Integer.parseInt(_splitString(counter))
        if(_test == 0) {
          position = (row, col)
        } else {
          _vect.add(_test)
        }
        counter++
      }
    }
    State(_vect)
  }

  def checkSolvable(vect: Vector[Int]): Boolean = {
    var numInversions = 0;

    for(i <- 0 to vect.length) {
      for(j <- i+1 to vect.length) {
        if(vect(j) > vect(i)) {
          numInversions++
        }
      }
    }
    if(numInversions%2 == 1){
      return false;
    }else{
      return true;
    }
  }

  def randomShuffle() {
    val shuf = scala.util.Random.shuffle(Vector(0, 1, 2, 3, 4, 5, 6, 7, 8))
    println(shuf)
    import java.io._
    val data = shuf
    printToFile(new File("example.txt")) { p =>
      data.foreach(p.println)
    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
