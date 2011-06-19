package org.liquidizer.doc.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._

abstract class Block[T <: Block[T]] {
  val id= "block_"+scala.util.Random.nextLong

  var next : Option[Block[T]] = None
  var prev : Option[Block[T]] = None

  def newBlock() : Block[T]
  def get() = asInstanceOf[T]
  def toList() : List[T]= get() :: next.map { _.toList }.getOrElse(Nil)

  def insertIcon() : NodeSeq = Text("[insert]")
  def deleteIcon() : NodeSeq = Text("[delete]")

  def renderAll(node : NodeSeq) : NodeSeq =
    render(node) ++ next.map { _.renderAll(node) }.getOrElse(NodeSeq.Empty)

  def render(node : NodeSeq) : NodeSeq = {
    div(id + "_deletable",
	Helpers.bind("block", node,
		     "id" -> Text(id.toString),
		     "delete" -> renderDelete(id),
		     "insert" -> renderInsert(id, node))
      ) ++ renderInsertAt(id)
  }

  def div(id : String, node : NodeSeq) = 
    <div id={id}>{ node }</div>

  def renderDelete(id : String) : NodeSeq = 
    if (prev.isEmpty) NodeSeq.Empty else
      SHtml.a(() => deleteBlock(id), deleteIcon())

  def renderInsert(id : String, node : NodeSeq) : NodeSeq = 
    <span id={id+"_insert"}> {
      SHtml.a(() => { insertBlock(id, node) }, insertIcon())
    } </span>
  
  def renderInsertAt(id : String) : NodeSeq =
    div(id+"_insert_at", NodeSeq.Empty)

  def deleteBlock(id : String) : JsCmd = {
    next.foreach { _.prev = prev }
    prev.foreach { _.next = next }
    SetHtml(id+"_insert", NodeSeq.Empty) &
    SetHtml(id+"_deletable", NodeSeq.Empty)
  }

  def insertBlock(id : String, node : NodeSeq) : JsCmd = {
    val block= newBlock()
    block.prev= Some(this)
    block.next= next
    next.foreach { _.prev= Some(block) }
    next= Some(block)
    SetHtml(id+"_insert", renderInsert(block.id+"_pre", node)) &
    SetHtml(id+"_insert_at", renderInsertAt(block.id+"_pre") ++ 
	    block.render(node))
  }

  def append(block : Block[T]) {
    if (next.isEmpty) {
      next= Some(block)
      block.prev= Some(this)
    } else
      next.get.append(block)
  }
}
