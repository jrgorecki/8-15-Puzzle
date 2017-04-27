
type Pos = (Int, Int)
type Tile = Int

 

case class Board( size: Int, tiles: Map[Pos,Tile] ) {

  // swap returns a new board idential to this except that
  // the values at positioin p1 and p2 are swapped
  def swap(p1: Pos, p2: Pos) = {
    val t1 = (tiles get p1, tiles get p2) match {
      case (Some(v1), Some(v2)) => tiles + (p1 -> v2) + (p2 -> v1)
      case (Some(v1), None    ) => (tiles - p1) + (p2 -> v1) 
      case (None,     Some(v2)) => (tiles - p2) + (p1 -> v2) 
      case _                    => tiles
    }
    new Board(size, t1)
  } 
  
val goal = new Board(3,Map(
    (1,1) -> 1, (1,2) -> 2, (1,3) -> 3,
    (2,1) -> 4, (2,2) -> 5, (2,3) -> 6,
    (3,1) -> 7, (3,2) -> 8)  )
//Function to generate legal next moves in a list, preferably already ordered.
    
//Generic tree search
    
