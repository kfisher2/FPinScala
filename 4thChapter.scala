import scala.math._

object Chapter4 {
	sealed trait Option[+A] {

	def map[B](f: A => B): Option[B] = this match {
		case None => None
		case Some(a) => Some(f(a))
	}

	def flatMap[B](f: A => Option[B]): Option[B] = this match {
		case None => None
		case Some(value) => f(value)
	}

	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(value) => value
	}

	def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
		case None => ob
		case Some(_) => this
	}

	def filter(f: A => Boolean): Option[A] = this match {
		case Some(value) if(f(value)) => this
		case _ => None
	}

	def avg(seq: Seq[Double]): Option[Double] = {
		if(!seq.isEmpty) Some(seq.sum / seq.length)
		else None
	}

	// def variance(xs: Seq[Double]): Option[Double] = { //-------Exercise 2 NEED HELP
	// 	avg(xs) flatMap (mean => avg(xs.map(x: Double => math.pow(x - mean, 2))))
	// }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

	// def main(args:Array[String]) = {
	// 	val test = Some().variance(Seq(1.0,2.0,3.0,4.0))
	// 		println(test)
	// }
}

