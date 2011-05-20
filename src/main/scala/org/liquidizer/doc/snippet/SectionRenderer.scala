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

class SectionRenderer(val ref : Content, var show : Content) {

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
    show= Content.create
    .parent(show).section(show.section.is)
    .text(show.text.is).style(show.style.is)
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

  def branches() : NodeSeq = <ul>{ branches(ref) }</ul>

  def branches(cur : Content) : NodeSeq = {
    <li> {
      val src= if (cur==show) "active" else "inactive"
      val refs= TagRef.findAll(By(TagRef.content, cur))
      SHtml.a(() => { show= cur; redraw()}, 
	      <img src={"/images/"+src+".png"}/>) ++ {
	if (refs.isEmpty) NodeSeq.Empty
	else tagLink(refs.first.tag.obj.get)
      } ++ {
        val alts= Content.findAll(By(Content.parent, cur))
        if (alts.isEmpty) NodeSeq.Empty else
          <ul>{ alts.flatMap{ branches(_) } }</ul>
      }
    } </li>
  }

  def tagLink(tag : Tag) = {
    <a href={Helpers.appendParams("/", Seq("root" -> tag.id.is.toString))}>{
      tag.name.is
    }</a>
  }

  def makeTag(tag : Tag) {
    TagRef.create.tag(tag).content(show).save
  }
}
