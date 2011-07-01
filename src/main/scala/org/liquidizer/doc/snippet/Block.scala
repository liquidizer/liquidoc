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

  var insertAt= id+"_insert_at"
  var rootNode= NodeSeq.Empty
  var showLevel= 10

  def newBlock() : Block[T]
  def get() = asInstanceOf[T]
  def toList() : List[T]= get() :: next.map { _.toList }.getOrElse(Nil)

  def insertIcon() : NodeSeq = Text("[insert]")
  def deleteIcon() : NodeSeq = Text("[delete]")
  def openIcon() : NodeSeq = Text("[-]")
  def closedIcon() : NodeSeq = Text("[+]")
  def level() : Int

  def renderAll(node : NodeSeq) : NodeSeq =
    render(node) ++ next.map { _.renderAll(node) }.getOrElse(NodeSeq.Empty)

  def render(node : NodeSeq) : NodeSeq = {
    rootNode= node
    div(id + "_main", renderContent())
  }

  def isVisible() = showLevel >= level()
  def nextNotHidden() = !next.exists{ !_.isVisible }

  def renderContent() : NodeSeq =
    renderIf(isVisible(), {
    div(id + "_deletable",
	Helpers.bind("block", rootNode,
		     "delete" -> renderDelete(),
		     "insert" -> renderInsert(),
	             "collapse" -> renderCollapse())
      ) ++ renderInsertAt()
    })

  def div(id : String, node : NodeSeq) = 
    <div id={id}>{ node }</div>

  def redraw() : JsCmd =
    SetHtml(id+"_main", renderContent())

  def renderIf(condition : Boolean, node : NodeSeq) = 
    if(condition) node else NodeSeq.Empty

  def renderDelete() : NodeSeq = 
    renderIf (!prev.isEmpty && nextNotHidden,
        SHtml.a(() => deleteBlock(), deleteIcon()))

  def renderInsert() : NodeSeq = 
    renderIf(nextNotHidden,
      SHtml.a(() => { insertBlock() }, insertIcon()))
  
  def renderInsertAt() : NodeSeq =
    div(insertAt, NodeSeq.Empty)

  def renderCollapse() : NodeSeq =
    if (level() >= 10) NodeSeq.Empty else
      SHtml.a(()=> { toggleCollapse() }, {
        if (showLevel <= level()) closedIcon() else openIcon })

  def toggleCollapse() : JsCmd= {
    val child= toList.tail.takeWhile{ _.level() > level()}
    // determine new visibility level
    if (showLevel <= level()) {
      showLevel= child.map { _.level() }.foldLeft(10) { _ min _ }
    }
    else {
      showLevel= level()
    } 
    println("new level "+showLevel)
    child.foreach { _.showLevel= showLevel }
    // create update command
    child.map{ _.redraw() }
    .foldLeft(redraw()) { _ & _ }
  }

  def deleteBlock() : JsCmd = {
    next.foreach { _.prev = prev }
    prev.foreach { _.next = next }
    SetHtml(id+"_insert", NodeSeq.Empty) &
    SetHtml(id+"_deletable", NodeSeq.Empty)
  }

  def insertBlock() : JsCmd = {
    val block= newBlock()
    block.prev= Some(this)
    block.next= next
    next.foreach { _.prev= Some(block) }
    next= Some(block)
    val oldInsertAt= insertAt
    insertAt= block.id+"_insert_pre"
    SetHtml(oldInsertAt, renderInsertAt() ++ block.render(rootNode))
  }

  def append(block : Block[T]) {
    if (next.isEmpty) {
      next= Some(block)
      block.prev= Some(this)
    } else
      next.get.append(block)
  }
}
