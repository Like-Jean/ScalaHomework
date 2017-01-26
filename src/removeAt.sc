import math.Ordering
object removeAt {
  println("Welcome to the Scala workshee")        //> Welcome to the Scala workshee

  println(1 to 9)                                 //> Range(1, 2, 3, 4, 5, 6, 7, 8, 9)

  val a = List(1, 2, 3, 4, 5, 6)                  //> a  : List[Int] = List(1, 2, 3, 4, 5, 6)
  println(a take 2)                               //> List(1, 2)
  ('a', 'b', 3, 4)                                //> res0: (Char, Char, Int, Int) = (a,b,3,4)

  def mergeSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys) else y :: merge(ys1, xs)
      }

      val (fst, snd) = xs.splitAt(n)
      merge(mergeSort(fst), mergeSort(snd))
    }
  }                                               //> mergeSort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]

  mergeSort(List(2, 1, 5, 3, 4, 6, 8, 1, 2, 3))(Ordering.Int)
                                                  //> res1: List[Int] = List(1, 1, 2, 2, 3, 3, 4, 5, 6, 8)
  mergeSort(List("apple", "pear", "bnana", "peach"))
                                                  //> res2: List[String] = List(apple, bnana, peach, pear)
  List(3, 3, 5, 3, 1, 6, 8, 5, 6, 4).dropWhile{ x => x>2 }
                                                  //> res3: List[Int] = List(1, 6, 8, 5, 6, 4)
  def concat[T](xs: List[T], ys: List[T]): List[T] = (xs foldRight ys)(_::_)
                                                  //> concat: [T](xs: List[T], ys: List[T])List[T]
  
  val c = List(1, 1, 2, 2, 3, 3, 4, 5, 6, 8)      //> c  : List[Int] = List(1, 1, 2, 2, 3, 3, 4, 5, 6, 8)
  val d = List('a', 'b', 'c', 'd', 'e', 'f')      //> d  : List[Char] = List(a, b, c, d, e, f)
  
  concat(c,d)                                     //> res4: List[AnyVal{def getClass(): Class[_ >: Char with Int <: AnyVal]}] = Li
                                                  //| st(1, 1, 2, 2, 3, 3, 4, 5, 6, 8, a, b, c, d, e, f)
  
  def lengthFun[T](xs: List[T]): Int = (xs foldRight 0)((x:T, z:Int) => z+1)
                                                  //> lengthFun: [T](xs: List[T])Int
  def mapFun[T, U](xs: List[T], f: T => U): List[U] = (xs foldRight List[U]())((x:T, ys:List[U]) => f(x)::ys)
                                                  //> mapFun: [T, U](xs: List[T], f: T => U)List[U]
  lengthFun(d)                                    //> res5: Int = 6
  mapFun(d, (x:Char) => x.toInt)                  //> res6: List[Int] = List(97, 98, 99, 100, 101, 102)
}