//package org.example

import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

/**
 * Represents the state of the puzzle with a vector of integers
 */
case class State(board: Vector[Int]) extends Equals {
  private val boxSize = Math.sqrt(board.length).toInt

  lazy val empty = board indexOf (0)

  // First heuristic function is the Manhattan block distance for states
  def manhatanBlockDistance(goalState: State) =
    {
      def xy(i: Int) = (i % boxSize, i / boxSize)
      def manhatanDistance(i: Int): Int =
        {
          if (board(i) == 0) 0
          else {
            val t = board(i)
            val i2 = goalState.board.indexOf(t)
            val (x1, y1) = xy(i)
            val (x2, y2) = xy(i2)
            Math.abs(x1 - x2) + Math.abs(y1 - y2)
          }
        }

      @tailrec
      def count(i: Int, total: Int): Int =
        if (i < board.size) count(i + 1, total + manhatanDistance(i))
        else total

      count(0, 0)
    }

  // 2nd heuristic function counting the number of misplaced blocks
  def numberOfMisplacedBlocks(goalState: State) =
    {
      @tailrec
      def count(i: Int, misplaced: Int): Int =
        if (i < board.size) count(i + 1, misplaced + (if (goalState.board(i) == board(i) || board(i) == 0) 0 else 1))
        else misplaced

      count(0, 0)
    }

  // The top bottom left and right of the empty box
  lazy val top = if (empty > (boxSize - 1)) Some(empty - boxSize) else None
  lazy val bottom = if (empty < (boxSize - 1) * boxSize) Some(empty + boxSize) else None
  lazy val left = if (empty % boxSize != 0) Some(empty - 1) else None
  lazy val right = if (empty % boxSize != boxSize - 1) Some(empty + 1) else None

}

// The base move trait
sealed trait Move {

  def move(state: State): Option[State] = swapBox(state) map (swap(_, state))

  private def swap(index: Int, state: State) = {
    val board2: Vector[Int] = (state.board updated (state.empty, state.board(index))) updated (index, state.board(state.empty))
    State(board2)
  }
  protected def swapBox(state: State): Option[Int]
}

object Move {
  // Function to give the available moves from a current state excluding already visited nodes
  def availableStates(state: State, moves: List[Move], history: List[Move], explored: Set[State]): Seq[(State, List[Move])] = {
    for {
      m <- moves
      s <- m.move(state)
      if !explored(s)
    } yield (s, m :: history)
  }
}

// The actual moves
case class TopDown() extends Move {
  override def swapBox(state: State) = state.top
}
case class BottomUp() extends Move {
  override def swapBox(state: State) = state.bottom
}
case class LeftRight() extends Move {
  override def swapBox(state: State) = state.left
}
case class RightLeft() extends Move {
  override def swapBox(state: State) = state.right
}

// The puzzle class
case class puzzleSolver(initialState: State, goalState: State) {

  // The ordering with evaluation function = misplaced heuristic function + the distance from the initial node 
  // reversed since we want the smaller numbers first
  val misplacedOrdering = new Ordering[(State, List[Move])] {
    def compare(x: (State, List[Move]), y: (State, List[Move])): Int =
      Ordering[Int].compare(
        y._1.numberOfMisplacedBlocks(goalState) + y._2.size,
        x._1.numberOfMisplacedBlocks(goalState) + x._2.size)
  }

  // The ordering with evaluation function = Manhattan heuristic function + the distance from the initial node
  // reversed since we want the smaller numbers first
  val manhatanOrdering = new Ordering[(State, List[Move])] {
    def compare(x: (State, List[Move]), y: (State, List[Move])): Int =
      Ordering[Int].compare(
        y._1.manhatanBlockDistance(goalState) + y._2.size,
        x._1.manhatanBlockDistance(goalState) + x._2.size)
  }

  // All possible moves, not all of them might be applicable from a given state
  val moves: List[Move] = List(TopDown(), BottomUp(), LeftRight(), RightLeft())

  /**
   * Finds all the possible solutions starting from a given state
   */
  @tailrec
  final def breathFirstSearch(queue: List[(State, List[Move])], explored: Set[State], sol: List[(State, List[Move])]): List[(State, List[Move])] =
    queue match {
      case Nil => sol
      case (state, history) :: xs =>
        if (!explored(state)) {
          breathFirstSearch(xs ++ Move.availableStates(state, moves, history, explored), explored + state, (state, history) :: sol)
        } else {
          breathFirstSearch(xs, explored, sol)
        }
    }

  /**
   * Uses the heuristic function embedded in the priority queue to make a "smarter"
   * selection on the path to follow to the optimal solution
   */
  @tailrec
  final def aStarSearch(queue: PriorityQueue[(State, List[Move])], explored: Set[State]): Option[List[Move]] =
    if (queue.length == 0) return None
    else {
      val (state, history) = queue.dequeue
      if (state == goalState) Some(history)
      else {
        if (!explored(state)) {
          val children = Move.availableStates(state, moves, history, explored)
          queue ++= children
          aStarSearch(queue, explored + state)
        } else
          aStarSearch(queue, explored)
      }
    }
  
  /*final def SMAStarSearch(queue: PriorityQueue[(State, List[Move])], explored: Set[State], level: Int, maxDepth: Int): Option[List[Move]] =
    while true
    {
      if(queue.length == 0) return None
      else{
        val (state, history) = queue.dequeue
        if (state == goalState) Some(history)
        else{
          if(state != goalState && level == maxDepth) return None
          
          else
        }
      }
    }
    * 
    */
   
  @tailrec
  final def IDAStarSearch(queue: PriorityQueue[(State, List[Move])], maxDepth: Int, level: Int, explored: Set[State]): Option[List[Move]] =
    {
         //initially, queue consists of simply the initial state, and an e, maxDepth should be 1,  and level should be 0
         if(queue.length == 0)
         {
           //we have fully examined the "fringe" and there is nothing left.
           //therefore iterate deeper.
            IDAStarSearch(queue.clear + (initialState, List[Move]()), maxDepth + 1, 0, Set[State]())
         }
         //note that the current "level" will is equivalent to the number of Moves in List.
         else(queue.length > 0) 
         {
           //do A* here
             val (state, history) = queue.dequeue
             if(state == goalState) Some(history)
             else{//continue search
               if(!explored(state)){
                 if(history.length < maxDepth){
                   val children = Move.availableStates(state, moves, history, explored)
                   queue ++= children
                   IDAStarSearch(queue, maxDepth, level+1, explored + state)
                 }
                 else{//history==maxDepth, so we have reached the limit
                   //do not expand node,  just carry on to the next node to be dequeued
                   IDAStarSearch(queue, maxDepth, level, explored + state)//add state to explored, because we never want to expand it
                                      
                 }
               }
               else{// we have already visited the head of the queu, so we carry on.
                 IDAStarSearch(queue, maxDepth, level, explored)
               }
             }
             
        }
     }
  
  lazy val allPathsFromInitial = breathFirstSearch(List((initialState, List())), Set(), List())

  // Creating a different solution with exactly the same code but a different 
  // ordering in my queue, so one can easily add solutions with better 
  // heuristic functions
  def bestSolution(ordering: Ordering[(State, List[Move])]) =
    aStarSearch(PriorityQueue((initialState, List[Move]()))(ordering), Set())

  lazy val bestSolutionManhatan: Option[List[Move]] =
    bestSolution(manhatanOrdering)

  lazy val bestSolutionMisplaced: Option[List[Move]] =
    bestSolution(misplacedOrdering)

}

object puzzle{
  import scala.concurrent.{ Future, Promise, future, promise, Await }
  import scala.concurrent.duration.Duration
  import scala.concurrent.ExecutionContext.Implicits.global
  def main(args: Array[String]) {

    val goal8 = State(Vector(0, 1, 2, 3, 4, 5, 6, 7, 8))
    
    val test8 = State(Vector(8, 7, 6, 0, 4, 1, 2, 5, 3))

    val t = System.currentTimeMillis


     val x = puzzleSolver(goal8, test8).bestSolutionManhatan
       println(s"8 took ${(System.currentTimeMillis - t) / 1000.0}")
       
       val t2 = System.currentTimeMillis
       
       val goal15 = State(Vector(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
       val test15 = State(Vector(12,1,2,15,11,6,5,8,7,10,9,4,0,13,14,3))
       
       val y = puzzleSolver(goal15, test15).bestSolutionManhatan
       println(s"15 took ${(System.currentTimeMillis - t) / 1000.0}")
       
        
  }

  val numberOfSolutionsPerDebth: PartialFunction[List[(State, List[Move])], List[(Int, Int)]] = {
    case x => {
      val y = x.groupBy(f => f._2.length)
      val group = for { (i, l) <- y } yield (i, l.length)
      group.toList.sortBy(f => f._1)
    }
  }

  def checkSolution(initial: State, goal: State, moves: List[Move]): Boolean = {
    val goal2 = (initial /: moves.reverse) {
      (s, m) => m.move(s).get
    }
    goal == goal2
  }
}

