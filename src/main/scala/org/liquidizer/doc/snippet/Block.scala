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
  var number: Option[List[Int]]= None

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
    reNumber()
    rootNode= node
    div(id + "_deletable", renderContent) ++ renderInsertAt()
  }

  def isVisible() = showLevel >= level()
  def isCollapsed() = next.exists { !_.isVisible() }

  def renderContent() : NodeSeq =
    renderIf(isVisible(),
	     Helpers.bind("block", rootNode,
			  "delete" -> renderDelete(),
			  "insert" -> renderInsert(),
			  "collapse" -> renderCollapse()))

  def div(id : String, node : NodeSeq) = 
    <div id={id}>{ node }</div>

  /** Rerender all of this block */
  def redraw() : JsCmd =
    SetHtml(id+"_deletable", renderContent())

  /** Only render this node if a condition holds */
  def renderIf(condition : Boolean, node : NodeSeq) = 
    if(condition) node else NodeSeq.Empty

  /** Format the hierarchical block number to String*/
  def formatNumber() =
    number.get.slice(0,level()).mkString(".")

  /** render the replacable container for the block number */
  def renderNumber() =
    <span class="headno" id={id+"_number"}>{ formatNumber() }</span>


  /** recount the numbering */
  def reNumber(always : Boolean = false) : JsCmd = {
    var n= prev.map { _.number.get }.getOrElse{ List(0,0,0,0,0) }
    if (level() < n.length) {
      n = n.slice(0, level()-1) ++
      List(n(level()-1)+1) ++ List(0,0,0,0,0)
    } 
    if (always || !number.exists { _ == n }) {
      number= Some(n)
      (if (level()<10) SetHtml(id+"_number", Text(formatNumber)) else Noop)&
      next.map{ _.reNumber() }.getOrElse{ Noop }
    } else {
      Noop
    }
  }

  /** render a delete button for this block */
  def renderDelete() : NodeSeq = 
    renderIf (!prev.isEmpty && !isCollapsed(),
        SHtml.a(() => deleteBlock(), deleteIcon()))

  /** render a button to insert a new block */
  def renderInsert() : NodeSeq = 
    renderIf(!isCollapsed,
      SHtml.a(() => { insertBlock() }, insertIcon()))
  
  /** render an area into which new blocks are to be inserted */
  def renderInsertAt() : NodeSeq =
    div(insertAt, NodeSeq.Empty)

  /** render a button to collapse and expand sections */
  def renderCollapse() : NodeSeq =
    if (level() >= 10) NodeSeq.Empty else
      SHtml.a(()=> { toggleCollapse() }, {
        if (isCollapsed()) closedIcon() else openIcon })

  /** Collapse and expand subsequent sections */
  def toggleCollapse() : JsCmd= {
    val child= toList.tail.takeWhile{ _.level() > level()}
    // determine new visibility level
    if (isCollapsed()) {
      showLevel= 100
      for (block <- child) {
	showLevel= showLevel min block.level()
	block.showLevel= showLevel
      }
    }
    else {
      showLevel= level()
      child.foreach { _.showLevel= showLevel }
    } 
    // create update command
    child.map{ _.redraw() }
    .foldLeft(redraw()) { _ & _ }
  }

  /** Delete this block */
  def deleteBlock() : JsCmd = {
    next.foreach { _.prev = prev }
    prev.foreach { _.next = next }
    (prev.map { _.redraw() }.getOrElse { Noop }) &
    SetHtml(id+"_insert", NodeSeq.Empty) &
    SetHtml(id+"_deletable", NodeSeq.Empty)
  }

  /** Insert a new block after this one */
  def insertBlock() : JsCmd = {
    val block= newBlock()
    block.prev= Some(this)
    block.next= next
    next.foreach { _.prev= Some(block) }
    next= Some(block)
    val oldInsertAt= insertAt
    insertAt= block.id+"_insert_pre"
    redraw() &
    SetHtml(oldInsertAt, renderInsertAt() ++ block.render(rootNode))
  }

  /** Append a block to the linked block list */
  def append(block : Block[T]) {
    if (next.isEmpty) {
      next= Some(block)
      block.prev= Some(this)
    } else
      next.get.append(block)
  }
}
