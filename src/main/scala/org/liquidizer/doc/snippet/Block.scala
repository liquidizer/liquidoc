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

abstract class Block[T] {
  val id= scala.util.Random.nextLong

  var next : Option[Block[T]] = None
  var prev : Option[Block[T]] = None

  def newBlock() : Block[T]

  def insert() = Text("[insert]")
  def delete() = Text("[delete]")

  def render(node : NodeSeq) : NodeSeq = {
    val sid= "block_"+id
    div(sid + "_deletable",
	Helpers.bind("block", node,
		     "id" -> Text(id.toString),
		     "delete" -> renderDelete(sid),
		     "insert" -> renderInsert(sid, node))
      ) ++ renderInsertAt(sid)
  }

  def div(id : String, node : NodeSeq) = 
    <div id={id}>{ node }</div>

  def renderDelete(id : String) : NodeSeq = 
    if (prev.isEmpty) NodeSeq.Empty else
      SHtml.a(() => deleteBlock(id), delete())

  def renderInsert(id : String, node : NodeSeq) : NodeSeq = 
    <span id={id+"_insert"}> {
      SHtml.a(() => { insertBlock(id, node) }, insert())
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
}

class MyBlock extends Block[MyBlock] {
  def newBlock : MyBlock = {
    new MyBlock()
  }
}

class BlockTest {
  def render(node : NodeSeq) : NodeSeq = {
    val block= new MyBlock
    block.render(node)
  }
}
