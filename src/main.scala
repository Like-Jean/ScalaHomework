object Main extends App{
	val num = 291                            
	
  //转换为2进制
	def toBin(number: Int, bin: List[Int]): List[Int] = number match {
			case 0 => bin
			case _ => toBin(number/2, number%2::bin)
		}                                
	
	//记录二进制中1的位置
	def Ones(bin: List[Int], index: Int, result: List[Int]): List[Int] = bin match {
			case Nil => result
			case x::y => if (x == 0) Ones(y,index-1,result) else Ones(y,index-1,result:+index)
		}                                 
                                                  
  //转换函数	
  def change(number: Int): String = number match {
			case x if(x<=2) => "2(" + x + ")+"
			case _ => "2(" +
									Ones(toBin(number,Nil),toBin(number,Nil).length-1,Nil)
									.map { x=> change(x)}.reduceRight(_+_).dropRight(1) +
									")+"
		}                                 

  println(change(num).dropRight(1))                          
}

  
