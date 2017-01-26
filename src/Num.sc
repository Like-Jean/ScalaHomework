object Num {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  def giveNum(xs: List[Int]): Set[List[Int]] = {
  	def giveNumAcc(xs: List[Int], n:Int): Set[List[Int]] = {
  		if(xs.tail.isEmpty) Set(List(n))
  		else {
  			
  		(0 to n).toSet filter { i => n-i >= 0} flatMap{ j =>
  			giveNumAcc(xs.tail,n-j) map { e => e:+j } }
  		}
  	}
  val xsT = xs toVector
	def isLeagal(ys: Set[List[Int]]): Set[List[Int]] = {
		ys filter { zs => xs forall{ x => zs.toVector(x) == zs.count(z => z==x)}}
	}
  	isLeagal(giveNumAcc(xs, xs.length))
  }                                               //> giveNum: (xs: List[Int])Set[List[Int]]
  
  
  giveNum(List(0,1,2,3,4,5,6,7,8,9,10,11,12)) map{ x => x mkString(" ")} mkString("\n")
                                                  //> java.lang.OutOfMemoryError: GC overhead limit exceeded
                                                  //| 	at scala.collection.immutable.List$.newBuilder(List.scala:453)
                                                  //| 	at scala.collection.generic.GenericTraversableTemplate$class.genericBuil
                                                  //| der(GenericTraversableTemplate.scala:70)
                                                  //| 	at scala.collection.AbstractTraversable.genericBuilder(Traversable.scala
                                                  //| :104)
                                                  //| 	at scala.collection.generic.GenTraversableFactory$GenericCanBuildFrom.ap
                                                  //| ply(GenTraversableFactory.scala:57)
                                                  //| 	at scala.collection.generic.GenTraversableFactory$GenericCanBuildFrom.ap
                                                  //| ply(GenTraversableFactory.scala:52)
                                                  //| 	at scala.collection.SeqLike$class.$colon$plus(SeqLike.scala:556)
                                                  //| 	at scala.collection.AbstractSeq.$colon$plus(Seq.scala:41)
                                                  //| 	at Num$$anonfun$main$1$$anonfun$Num$$anonfun$$giveNumAcc$1$2$$anonfun$ap
                                                  //| ply$2.apply(Num.scala:9)
                                                  //| 	at Num$$anonfun$main$1$$anonfun$Num$$anonfun$$giveNumAcc$1$2$$anonfun$ap
                                                  //| ply$2.apply(Num.scala:9)
                                                  //| 	at scala.collection.TraversableLike$$anonfun$map$1.apply(TraversableLike
  
}