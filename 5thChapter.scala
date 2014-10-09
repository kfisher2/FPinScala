object Chapter5 {
import Stream._
sealed trait Stream[+A] {

	def toList: List[A] = { //----------------------------------------------------------------Exercise 1
		def go(stream: Stream[A], acc: List[A]): List[A] = this match {
			case Cons(head, tail) => go(tail(), head() :: acc)
			case _ => List()
		}
		go(this,List())
	}

	def take(n: Int): Stream[A] = { //--------------------------------------------------------Exercise 2
		def go(n: Int, stream: Stream[A], acc: Stream[A]): Stream[A] = this match {
			case Cons(head, tail) if(n > 0) => go(n-1, tail(), Cons(()=> head(),()=> acc))
			case _ => acc
		}
		go(n, this, Empty)
	}

	def drop(n: Int): Stream[A] = { 
		def go(n: Int, stream: Stream[A]): Stream[A] = this match {
			case Cons(head, tail) if(n >= 0) => go(n-1, tail())
			case _ => stream
		}
		go(n, this)
	}

	def takeWhile(b: Boolean): Stream[A] = { //--------------------------------------------------Exercise 3
		def go(b: Boolean, stream: Stream[A]): Stream[A] = this match {
			case Cons(head, tail) if(b) => go(b, tail())
			case _ => stream
		}
		go(b, this)
	}

	def exists(p: A => Boolean): Boolean = this match {
		case Cons(h, t) => p(h()) || t().exists(p)
		case _ => false
	}
	def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
		case Cons(h,t) => f(h(), t().foldRight(z)(f))
		case _ => z
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b) //-------------Exercise 4

  def takeWhile(p: A => Boolean): Stream[A] = { //-------------------------------------------Exercise 5
  	foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else empty)
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
	
object Stream {
	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def empty[A]: Stream[A] = Empty

	def apply[A](as: A*): Stream[A] = {
		if (as.isEmpty) empty 
		else cons(as.head, apply(as.tail: _*))
	}

	def constant[A](a: A): Stream[A] = {
  	lazy val infinite: Stream[A] = Stream.cons(a,infinite)
  	infinite
  }

  def from(n: Int): Stream[Int] = {
  	cons(n,from(n+1))
  }

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = 
      cons(a, go(b, a+b))
    go(0, 1)
  }
}

}