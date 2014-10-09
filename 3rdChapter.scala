
object Chapter3 {

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

	object List {

	  def sum(ints: List[Int]): Int = ints match {
		  case Nil => 0
		  case Cons(x,xs) => x + sum(xs)
	  }

	  def product(ds: List[Double]): Double = ds match {
		  case Nil => 1.0
		  case Cons(x,xs) => x * product(xs)
	  }

	  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))
		}

		def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

		def product2(ns: List[Double]) = foldRight(ns, 1.0)((x,y) => if(x == 0.0) 0.0 else x * y) //--------Exercise 7: Yes it can short-circuit

		def apply[A](as: A*): List[A] = {
		  if (as.isEmpty) Nil
			else Cons(as.head, apply(as.tail: _*))
	 	}

	  def tail[A](as: List[A]): List[A] = as match { //------Exercise 2
		  case Nil => Nil
		  case Cons(x,xs) => xs
	  }

	  def setHead[A](as: List[A], x: A): List[A] = as match { //------Exercise 3
		  case Nil => Cons(x, Nil)
		  case Cons(x,xs) => Cons(x, xs)
	  }

	  def drop[A](l: List[A], n: Int): List[A] = l match { //------Exercise 4
	  	case Nil => Nil
	  	case Cons(x, xs) => drop(xs, n-1)
	  }

	  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match { //------Exercise 5
	  	case Nil => Nil
	  	case Cons(x,xs) => 
	  		if(f(x)) dropWhile(xs)(f)
	  		else xs
	  }

	  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
			case Nil => a2
			case Cons(h,t) => Cons(h, append(t, a2))
		}

		def init[A](l: List[A]): List[A] = { //------Exercise 6
			def go(l: List[A], acc: List[A]): List[A] = l match {
				case Nil => acc
				case Cons(x, xs) => go(xs, Cons(x, acc))
			}
			go(l, Nil)
		}

		def length[A](l: List[A]): Int = { //-------Exercise 9	
			foldRight(l, 0)((_,z) => z + 1)
		}

		def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match { //----Exercise 10
			case Nil => z
			case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
		}

		//----------------Exercise 11----------------------
				def sumLeft(ints: List[Int]): Int = {
				  foldLeft(ints, 0)(_+_)
			  }

			  def productLeft(ds: List[Double]): Double = {
				  foldLeft(ds, 1.0)(_*_)
			  }

			  def lengthLeft[A](l: List[A]): Int = {
			  	foldLeft(l,0)((z,_) => z + 1)
			  }
		//--------------------------------------------------

		def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b)) //-----Exercise 12

		def foldLeftAsRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = { //------Exercise 13
			foldRight(reverse(l), z)((b,a) => f(a,b))
		}

		def foldRightAsLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = { //------Exercise 13
			foldLeft(reverse(l), z)((a,b) => f(b,a))
		}

		def appendFold[A](l: List[A], l2: List[A]) = { //-----------Exercise 14
			foldRight(l, l2)(Cons(_,_))
		}


		def flatten[A](list: List[List[A]]): List[A] = { //---------Exercise 15
			foldRight(list, Nil: List[A])(append(_,_))
		}

		def addOne(list: List[Int]): List[Int] = { //---------------Exercise 16
			foldRight(list, Nil: List[Int])((a,b)=>Cons(a+1,b))
		}

		def doublesToString(list: List[Double]): List[String] = { //---------------Exercise 17
			foldRight(list, Nil: List[String])((a,b)=>Cons(a.toString,b))
		}

		def map[A,B](l: List[A])(f: A => B): List[B] = { //----------Exercise 18
			foldRight(l, Nil: List[B])((a,b) => Cons(f(a),b))
		}

		def filter[A](l: List[A])(f: A => Boolean): List[A] = { //-----------Exercise 19
			foldRight(l, Nil: List[A])((a,b) => if(f(a)) Cons(a,b) else b)
		}

		def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = { //----------Exercise 20
			flatten(map(l)(f))
		}

		def filterAsFlatMap[A,B](l: List[A])(f: A => Boolean): List[A] = { //----------Exercise 21
			flatMap(l)(a => if(f(a)) List(a) else Nil)
		}

		def combineAddLists(a1: List[Int], a2: List[Int]): List[Int] = (a1,a2) match { //-------Exercise 22
			case (_,Nil) => Nil
			case (Nil,_) => Nil
			case (Cons(h,t), Cons(h2,t2)) => Cons(h+h2, combineAddLists(t,t2))
		}

		def zipWith[A](a1: List[A], a2: List[A])(f: (A,A) => A): List[A] = (a1,a2) match { //-------Exercise 23
			case (_,Nil) => Nil
			case (Nil,_) => Nil
			case (Cons(h,t),Cons(h2,t2)) => Cons(f(h, h2), zipWith(t,t2)(f))
		}


	}

type ScalaList[T] = scala.collection.immutable.List[T]
	def hasSubsequence[A](l: ScalaList[A], sub: ScalaList[A]): Boolean = { //-------Exercise 24
		val sublistLength = sub.length
		val listChunks = l.sliding(sublistLength)
		listChunks.exists(_ == sub)
	}

 //------------------------------------------Exercise 1
 	val x = List(1,2,3,4,5) match {

	 case Cons(x, Cons(2, Cons(4, _))) => x

	 case Nil => 42

	 case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y

	 case Cons(h, t) => h + List.sum(t)

	 case _ => 101

	} //result = 3

//-----------------------------------------------Trees-----------------------------------------
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	def size[A](tree: Tree[A]): Int = tree match { //----------Exercise 25
		case Leaf(_) => 1
		case Branch(left,right) => size(left) + size(right) //BRANCHES ARE NOT NODES >=O I will not count them
	}

	def maximum(tree: Tree[Int]): Int = tree match { //--------Exercise 26
		case Leaf(num) => num
		case Branch(left,right) => maximum(left) max maximum(right)
	}

	def depth(tree: Tree[Int]): Int = tree match { //----------Exercise 27
		case Leaf(_) => 0
		case Branch(left,right) => 1 + (depth(left) max depth(right))
	}

	def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match { //-------Exercise 28
		case Leaf(value) => Leaf(f(value))
		case Branch(left,right) => Branch(map(left)(f),map(right)(f)) 
	}

}

//---------------------------------------------------------------------------------------------

	def main(args: Array[String]) = {
		val exercise8 = List.foldRight(List(1,2,3),Nil: List[Int])(Cons(_,_)) //-----Exercise 8	
		val list = scala.collection.immutable.List[Int](1,2,3,4)
		val sub = scala.collection.immutable.List[Int](8)
		val check = hasSubsequence(list,sub)
	}

}