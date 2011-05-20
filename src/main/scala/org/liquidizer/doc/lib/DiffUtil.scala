package org.liquidizer.doc.lib

import scala.xml._
import scala.collection.mutable.PriorityQueue

case class DiffObject[T](val value : T)
case class Deleted[T](oldValue : T) extends DiffObject[T](oldValue)
case class Inserted[T](newValue : T) extends DiffObject[T](newValue)
case class Copied[T](keepValue : T) extends DiffObject[T](keepValue)

class DiffPath[T](val oldList : Seq[T], 
                  val newList : Seq[T],
		  val history : List[DiffObject[T]],
		  val weight : Double) extends Ordered[DiffPath[T]] {

  def isComplete() = oldList.isEmpty && newList.isEmpty

  def successors() : Seq[DiffPath[T]] = {
      if (oldList.isEmpty) {
        Seq(new DiffPath(Nil, newList.tail, Inserted(newList.head) :: history, weight+1.0))
      }
      else if (newList.isEmpty) {
        Seq(new DiffPath(oldList.tail, Nil, Deleted(oldList.head) :: history, weight+0.9))
      }
      else if (oldList.head == newList.head) {
        Seq(new DiffPath(oldList.tail, newList.tail, Copied(oldList.head) :: history, weight))
      } else {
        Seq(
          new DiffPath(oldList.tail, newList, Deleted(oldList.head) :: history, weight+0.9),
          new DiffPath(oldList, newList.tail, Inserted(newList.head) :: history, weight+1))
      }
  }

  def compare(other : DiffPath[T])= -weight.compare(other.weight)
  
  def dominates(other : DiffPath[T]) =
    weight <= other.weight &&
    newList == other.newList &&
    oldList == other.oldList
}

object DiffUtil {
  def diffPath[T](oldList : Seq[T], newList : Seq[T]) : DiffPath[T] = {
    var queue= new PriorityQueue[DiffPath[T]]
    queue += new DiffPath(oldList, newList, Nil, 0.0)
    while (!queue.head.isComplete) {
      val path= queue.dequeue
      queue= queue.filter(!path.dominates(_))
      for (succ <- path.successors)
        queue += succ     
    }
    queue.head
  }

  def diff[T](oldList : Seq[T], newList : Seq[T]) : List[DiffObject[T]] = {
	  diffPath(oldList, newList).history.reverse
  }
  
  def diffWeight[T](oldList : Seq[T], newList : Seq[T]) : Double = {
	  diffPath(oldList, newList).weight;
  }
}

object DiffRenderer {
  def renderDiff(content : List[DiffObject[String]]) : NodeSeq = {
    if (content.isEmpty) NodeSeq.Empty else {
      val head= content.takeWhile(_.getClass==content.head.getClass)
      val tail= content.drop(head.size)
      val body= Text(head.map( _.value ).mkString(" "," "," "))
      content.head match {
	case Copied(_) => Seq(body) ++ renderDiff(tail)
	case Deleted(_) => 
	  <span class="deleted">{body}</span> ++ renderDiff(tail)
	case Inserted(_) => 
          <span class="inserted">{body}</span> ++ renderDiff(tail)
      }
    }
  }

  def renderDiff(str1 : String, str2 : String) : NodeSeq = {
    val s1= str1.split("\\s")
    val s2= str2.split("\\s")
    renderDiff(DiffUtil.diff(s1,s2))
  }
}
