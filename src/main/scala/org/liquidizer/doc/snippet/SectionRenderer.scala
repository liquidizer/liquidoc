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
    <tr id={"section-" + id}> { bind(node) }</tr>
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

  def redraw() : JsCmd = {
    SetHtml("edit"+id, editButton()) &
    SetHtml("content"+id, content()) &
    SetHtml("branches"+id, branches())
  }

  def contentArea() = <div id={"content"+id} >{ content }</div>

  def content() = {
    <div class={"section-"+show.style.is}> {
      DiffRenderer.renderDiff(ref.text.is, show.text.is)
    }</div>
  }

  def branchArea() : NodeSeq = <div id={"branches"+id}>{ branches() }</div>

  def toHtml(tree : TagTree) : NodeSeq = <li> {
    val src= if (tree.cur==tree.show) "active" else "inactive"
    val img = <img src={"/images/"+src+".png"}/>
    val icon = SHtml.a(() => { show= tree.cur; redraw()}, img) 
    
    val subtree= 
      if (tree.isShown) {
	( tree.refs -- tree.children.flatMap( _.refs ) )
	.flatMap {
	  tag => tagLink(tag)
	} ++
	<ul>{ tree.children.flatMap{ toHtml(_) }}</ul>
      } else {
	tree.refs.flatMap {
	  tag => tagLink(tag)
	}
      }
    icon ++ subtree
  } </li>

  def branches() : NodeSeq = {
    val tree= new TagTree(ref, show)
    <ul>{ toHtml(tree) }</ul>
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
