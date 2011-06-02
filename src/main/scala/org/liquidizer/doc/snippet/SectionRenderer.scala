package org.liquidizer.doc.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._

class SectionRenderer(val rootTag : Tag, val showTag : Tag, sec : Section) {
  
  val ref= rootTag.content(sec).get
  var show= showTag.content(sec).get

  val styles=List(
    ("h1", "Heading 1"),
    ("h2", "Heading 2"),
    ("p" , "Paragraph"),
    ("ul", "List item"),
    ("ol", "Numberd list item"))

  val id= show.id.is

  def render(node : NodeSeq) : NodeSeq = {
    <div id={"section-" + id}> { bind(node) }</div>
  }

  def bind(node : NodeSeq) : NodeSeq = node.flatMap { bind(_) }

  def bind(node : Node) : NodeSeq = node match {
    case Elem("sec", tag, attrib, scope, children @ _*) =>
      tag match {
        case "edit" => buttonArea()
	case "insert" => insertButton()
        case "content" => contentArea()
	case "branches" => branchArea()
      }

    case Elem(prefix, label, attribs, scope, children @ _*) =>
      Elem(prefix, label, attribs, scope, bind(children) : _*)

    case _ => node
  }

  def buttonArea() =  <div id={"edit"+id}>{ editButton() }</div>
  def editButton() = SHtml.ajaxButton("edit", () => toEditMode()) 
  def saveButton() = SHtml.ajaxSubmit("save", () => save())
  def insertButton() = SHtml.ajaxButton("Insert", () => Noop)

  def toEditMode() = {
    show= Content.create.parent(show).text(show.text.is).style(show.style.is)
    SetHtml("content"+id, editArea()) &
    SetHtml("edit"+id, saveButton())
  }

  def editArea() : NodeSeq = {
    <div> { 
      SHtml.select(styles, Full(show.style.is), text => show.style(text) ) 
    } </div> ++
    <div> { 
      SHtml.textarea(show.text.is, show.text(_)) 
    } </div>
  }

  def save() : JsCmd = {
    val dirty= show.parent.obj.exists {
     p => p.text.is!=show.text.is || p.style.is!=show.style.is
    }
    if (dirty) {
      show.save
    } else {
      show= show.parent.obj.get
    }
    redraw()
  }

  def redraw() : JsCmd = redraw(show)

  def redraw(newShow : Content) : JsCmd = {
    SetHtml("content"+id, {
      if (content==show) { content() }
      else {
	val oldShow= show
	show= newShow
	content(ref, oldShow, newShow)
      }}) &
    SetHtml("edit"+id, editButton()) &
    SetHtml("branches"+id, branches())
  }

  def content() = {
    <div class={"section-"+show.style.is}> {
	DiffRenderer.renderDiff(ref.text.is, show.text.is)
    } </div>
  }

  def content(ref : Content, oldShow : Content, newShow :Content) = {
    <div class={"section-"+newShow.style.is}> {
      DiffRenderer.renderDiff(ref.text.is, oldShow.text.is, newShow.text.is)
    }</div>
  }

  def contentArea() = <div id={"content"+id} >{ content() }</div>

  def branchArea() : NodeSeq = <div id={"branches"+id}>{ branches() }</div>

  def toHtml(trees : List[TagTree]) : NodeSeq = <ul> {
    trees.flatMap { tree => <li> { toHtml(tree) } </li> } 
  } </ul>

  def toHtml(tree : TagTree) : NodeSeq = {
    val src= if (tree.isCurrent) "active" else "inactive"
    val img = <img src={"/images/"+src+".png"}/>
    val icon = SHtml.a(() => { redraw(tree.cur)}, img) 
    
    val subtree= 
      if (tree.isShown) {
	val tagList =( tree.refs -- tree.children.flatMap( _.refs ) )
	tagList.flatMap { tag => tagLink(tag) } ++ {
	  if (!tree.isCurrent && tagList.isEmpty && tree.children.size<=1)
	    <div class="compact"> {
	      toHtml(tree.children)
	    } </div>
	  else
	    toHtml(tree.children)
	}
      } else {
	tree.refs.flatMap {
	  tag => tagLink(tag)
	}
      }
    icon ++ subtree
  }

  def branches() : NodeSeq = {
    val tree= new TagTree(ref, show)
    toHtml(List(tree))
  }

  def tagLink(tag : Tag) : NodeSeq = {
    <a href={
      Helpers.appendParams("/", Seq(
	"root" -> rootTag.id.is.toString,
	"show" -> tag.id.is.toString))}
      class={if (tag==showTag) "active" else "inactive"}>{
      tag.name.is
    }</a>
  }

  def makeTag(tag : Tag) {
    if (show.dirty_?)
      save()
    if (show != ref)
      TagRef.create.tag(tag).content(show).section(sec).save
  }

  def isDirty() = show != showTag.content(sec).get
}
