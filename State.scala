import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

/********
 * Represents the current state of the board of an n-puzzle
 */
case class State(board: Vector[Int]) extends Equals {
  
  private val rowSize = Math.sqrt(board.length).toInt //size of the rows/columns in the m x m puzzle

  val emptySpaceIndex = board indexOf (0) //index of the empty space

  /******
   * Heuristic function that calculates the sum of the manhattan distances of all the squares in a given state
   */
  def manhattanBlockDistance(goalState: State) =
    {
      /******
       * returns the manhattan distance from the goal state of an individual box of the n-puzzle    
       */
      def manhattanDistance(index:Int): Int =
      {
        val current = board(index)
        val goal = goalState.board.indexOf(current)
        val currentX = index % rowSize//since columns are (0,3,6).. etc.
        val currentY = index/rowSize//since rows are (0,1,2).. etc. 
        val goalX = goal % rowSize
        val goalY = goal/rowSize
        Math.abs(currentX-goalY) + Math.abs(currentY-goalY)
      }

      /*****
       * iterates through the board, calculating the manhattan distances of each square and totalling them recursively
       */
      @tailrec
      def totalManhattanDistance(index: Int, totalDistance: Int): Int =
        if (index < board.size) totalManhattanDistance(index + 1, totalDistance + manhattanDistance(index))
        else totalDistance

      totalManhattanDistance(0, 0)
    }
  /****************
   * Heuristic function that counts the number of misplaced blocks of a given state
   */
  def misplacedBlocks(goalState: State) =
    {
      /******
       * compares each index with the goal state and recursively counts the total number of misplaced square
       */
      def countMisplacedBlocks(index: Int, count: Int): Int =
        if (index < board.size){ 
          val temp = if (goalState.board(index) == board(index) || board(index) == 0) 0 else 1 //1 = misplaced, 0 = correct spot
          countMisplacedBlocks(index + 1, count + temp)
        }
        else count

      countMisplacedBlocks(0, 0)
    }

  // The top bottom left and right of the emptySpaceIndex box
  val aboveEmptyIndex = if (emptySpaceIndex > (rowSize - 1)) Some(emptySpaceIndex - rowSize) else None
  val belowEmptyIndex = if (emptySpaceIndex < (rowSize - 1) * rowSize) Some(emptySpaceIndex + rowSize) else None
  val leftEmptyIndex = if (emptySpaceIndex % rowSize != 0) Some(emptySpaceIndex - 1) else None
  val rightEmptyIndex = if (emptySpaceIndex % rowSize != rowSize - 1) Some(emptySpaceIndex + 1) else None

}