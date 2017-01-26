object SeqTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val s = Vector(1,3,5,6,7,8,3)                   //> s  : scala.collection.immutable.Vector[Int] = Vector(1, 3, 5, 6, 7, 8, 3)
  val l = List(1,4,5,7,8,5,5)                     //> l  : List[Int] = List(1, 4, 5, 7, 8, 5, 5)
  val S = "ABCDEFG"                               //> S  : String = ABCDEFG
 def isPrime(x: Int):Boolean = !(2 to x/2).exists{ d => x%d == 0}
                                                  //> isPrime: (x: Int)Boolean
 isPrime(9)                                       //> res0: Boolean = false

  (1 until 10) flatMap { x =>
  	(1 until x) map { y => (x,y)}} filter (pair =>
  		isPrime(pair._1+pair._2))         //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5), (7,4), (7,6), (8,3), (8,5), (9,2), (9,
                                                  //| 4), (9,8))
   def queens(n: Int): Set[List[Int]] = {
   	def placeQueens(k: Int): Set[List[Int]] = {
   		if(k==0) Set(List())
   		else
   			for {
   				queens <- placeQueens(k - 1)
   				col <- 0 until n
   				if isSafe(col, queens)
   			} yield col::queens
   	}
   	def isSafe(col: Int, queens: List[Int]) :Boolean = {
   		val row = queens.length
   		val queensWithRow = (row - 1 to 0 by -1) zip queens
   		queensWithRow forall {
   			case (r, c) => col != c && math.abs(col - c) != row - r
   		}
   	}
   	placeQueens(n)
   }                                              //> queens: (n: Int)Set[List[Int]]
   queens(4)                                      //> res2: Set[List[Int]] = Set(List(1, 3, 0, 2), List(2, 0, 3, 1))
   queens(6)                                      //> res3: Set[List[Int]] = Set(List(3, 0, 4, 1, 5, 2), List(4, 2, 0, 5, 3, 1), L
                                                  //| ist(2, 5, 1, 4, 0, 3), List(1, 3, 5, 0, 2, 4))
   queens(3)                                      //> res4: Set[List[Int]] = Set()
   (1 to 3).map(e => List(('a',e))).toList        //> res5: List[List[(Char, Int)]] = List(List((a,1)), List((a,2)), List((a,3)))
}