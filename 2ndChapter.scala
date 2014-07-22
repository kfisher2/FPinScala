object Chapter2 {

	def fib(n: Int): Int = {
		def loop(fibonaciIndex: Int, n1: Int, acc: Int): Int = {
			if(fibonaciIndex == n) acc
			else loop(fibonaciIndex+1, acc, acc + n1)
		}
		if(n == 0) 0
		else loop(1,0,1)
	}

	def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
		as.sliding(2).forall(adjacent => gt(adjacent(0), adjacent(1)))
	}

	def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
		(a: A) => ((b: B) => f(a, b))
	}

	def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
		(a: A, b: B) => f(a)(b)
	}

	def compose[A,B,C](f: B => C, g: A => B): A => C = {
		a => f(g(a))
	}

}