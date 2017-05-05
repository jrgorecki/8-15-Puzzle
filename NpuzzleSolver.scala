import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Queue

case class NpuzzleSolver(initialState: State, goalState: State) {
  var min = Int.MaxValue

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
  
  final def IDAStarSearchManhattan(state: State, history: List[Move], fThresh: Int): (Int, Option[List[Move]]) = 
  {
   
      if (state == goalState) {return(-1, Some(history))}
      else {
        //if f-val is less than Thresh, expand, else ignore. 
        val fVal = state.manhattanBlockDistance(goalState) + history.length 
        if(fVal > fThresh) {return (fVal,None) }
        min = Int.MaxValue
        
          for(node <- MoveFinder.availableMovesIDA(state, history))
          {
            
            IDAStarSearchManhattan(node._1, node._2, fThresh) match {
              case (_, Some(z)) => return (-1, Some(z)) 
              case (fval, None) => { 
                if(fval<min) {min = fval; }
              }
              
            }
          }
        return (min, None)
      }
    }
  
  final def IDAManhattan(): Option[List[Move]] = {
    var fThresh = initialState.manhattanBlockDistance(goalState)
    println(fThresh)
    while(true){
      var res = IDAStarSearchManhattan(initialState, List[Move](), fThresh)
      res match 
      {
        case (newThresh, None) => fThresh = newThresh;
        case (_, Some(x)) => return Some(x)
        
        
      }      
    }
    None
  }
  
      
      
      
      
      
/*
  @tailrec
  final def IDAStarSearchMisplaced(queue: Queue[(State, List[Move])], fThresh: Int): Option[List[Move]] = 
  {
    if (queue.length == 0) return IDAStarSearchMisplaced(Queue((initialState, List[Move]())), fThresh + 1)
    else {
      val (state, hist) = queue.dequeue
      if (state == goalState) Some(hist)
      else {
        //if f-val is less than Thresh, expand, else ignore. 
        val fVal = state.misplacedBlocks(goalState) + hist.length 
        if(fVal <= fThresh) {
          queue ++= MoveFinder.availableMovesIDA(state, hist)
          IDAStarSearchMisplaced(queue, fThresh)
        } else { IDAStarSearchMisplaced(queue, fThresh) }
      }
    }
  }
    
  */
    /**********
     * functions for choosing search method and heuristic
     */
    
    val solutionManhattanAStar: Option[List[Move]] = 
      aStarSearch(PriorityQueue((initialState,List[Move]()))(manhattanOrdering),Set())
      
    val solutionMisplacedAStar: Option[List[Move]] = 
      aStarSearch(PriorityQueue((initialState,List[Move]()))(misplacedOrdering), Set())

    val solutionManhattanIDAStar: Option[List[Move]] = { min = Int.MaxValue;
      IDAManhattan()
      
      }
    
    
      
   /* val solutionMisplacedIDAStar: Option[List[Move]] = { min = Int.MaxValue;
      IDAStarSearchMisplaced(initialState,List[Move](), 1) } */


}