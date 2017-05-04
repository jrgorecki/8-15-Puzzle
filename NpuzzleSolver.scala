import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

case class NpuzzleSolver(initialState: State, goalState: State) {

    /*******
   * Uses the number of misplaced blocks to create an ordering to perform a* search to solve an n-puzzle
   */
  val misplacedOrdering = new Ordering[(State, List[Move])] {
    def compare(x: (State, List[Move]), y: (State, List[Move])): Int =
      Ordering[Int].compare(
        y._1.misplacedBlocks(goalState) + y._2.size,
        x._1.misplacedBlocks(goalState) + x._2.size)
  }

  /*******
   * Uses manhattan distance to create an ordering to perform a* search to solve an n-puzzle
   */
  val manhattanOrdering = new Ordering[(State, List[Move])] {
    def compare(x: (State, List[Move]), y: (State, List[Move])): Int =
      Ordering[Int].compare(
        y._1.manhattanBlockDistance(goalState) + y._2.size,
        x._1.manhattanBlockDistance(goalState) + x._2.size)
  }



  /**
   * Uses a priority queue ordered according to the selected heuristic to perform an a* search for solving an n-puzzle
   */
  @tailrec
  final def aStarSearch(queue: PriorityQueue[(State, List[Move])], explored: Set[State]): Option[List[Move]] =
    if (queue.length == 0) return None
    else {
      val (state, path) = queue.dequeue
      if (state == goalState) Some(path)//goal reached
      else {
        if (!explored(state)) {
          val children = MoveFinder.availableMoves(state, path, explored)//add children to the queue
          queue ++= children
          aStarSearch(queue, explored + state)//mark explored and continue
        } else
          aStarSearch(queue, explored)
      }
    }
  
  
  /*********************
   * 
   * enter IDAStar Search function here
   * 
   */
  
  
    /**********
     * functions for choosing search method and heuristic
     */
    
    val solutionManhattanAStar: Option[List[Move]] = 
      aStarSearch(PriorityQueue((initialState,List[Move]()))(manhattanOrdering),Set())
      
    val solutionMisplacedAStar: Option[List[Move]] = 
      aStarSearch(PriorityQueue((initialState,List[Move]()))(misplacedOrdering), Set())

  /*  val solutionManhattanIDAStar: Option[List[Move]] = 
      IDAStarSearch(PriorityQueue((initialState,List[Move]()))(manhattanOrdering), Set())
      
    val solutionMisplacedIDAStar: Option[List[Move]] = 
      IDAStarSearch(PriorityQueue((initialState,List[Move]()))(misplacedOrdering), Set())*/


}