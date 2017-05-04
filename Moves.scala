/*********
 * Super class to represent the different moves that can be made in an n-puzzle
 */
sealed trait Move {

  
  /*****
   * make a move by swapping a board location with the empty space
   */
  def move(state: State): Option[State] = swapBox(state) map (swap(_, state))

  /****
   * 
   */
  private def swap(index: Int, state: State) = {
    val tempBoard: Vector[Int] = (state.board updated (state.emptySpaceIndex, state.board(index))) updated (index, state.board(state.emptySpaceIndex))
    State(tempBoard)
  }
  protected def swapBox(state: State): Option[Int]//function to be overridden based on direction of the move
}

/*****
 * case classes to represent the four possible moves and override the swapBox function
 */
case class Down() extends Move {
  override def swapBox(state: State) = state.aboveEmptyIndex
}
case class Up() extends Move {
  override def swapBox(state: State) = state.belowEmptyIndex
}
case class Right() extends Move {
  override def swapBox(state: State) = state.leftEmptyIndex
}
case class Left() extends Move {
  override def swapBox(state: State) = state.rightEmptyIndex
}


/***
 * Object with functions to find available moves
 */
object MoveFinder {
  
  val moves: List[Move] = List(Down(), Up(), Right(), Left())//List of all potential moves
 
 /********
  * Uses indexes of boxes around the space generated in the state class to determine which possible moves are available in the current state
  */
  def availableMoves(state: State, history: List[Move], explored: Set[State]): Seq[(State, List[Move])] = {
    for {
      m <- moves
      s <- m.move(state)
      if !explored(s)
    } yield (s, m :: history)
  }
  
  def availableMovesIDA(state: State, history: List[Move]): Seq[(State, List[Move])] = {//ensures not to go bck and forth between parent and child, w/o "explored"
      
      if(history.length > 0){ val lastMove: Move = history.last
      lastMove match
        {
          case Down() => for{
                            m <-List(Down(), Left(), Right())
                            s <- m.move(state)
                          } yield (s, m::history)
          case Up() => for{
                            m <-List(Up(), Left(), Right())
                            s <- m.move(state)
                          } yield (s, m::history)
          case Right() => for{
                            m <-List(Up(), Down(), Right())
                            s <- m.move(state)
                          } yield (s, m::history)
          case Left() => for{
                            m <-List(Down(), Left(), Up())
                            s <- m.move(state)
                          } yield (s, m::history)
        }
      }
      else 
      {
        for {
      m <- moves
      s <- m.move(state)
      
        } yield (s, m :: history)
      }
   }
 }