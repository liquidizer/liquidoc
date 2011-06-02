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

class LiquiDoc {

  val uri= S.uri

  val rootTag= Tag.get(S.param("root").getOrElse("1")).get
  val showId= S.param("show")
  val showTag= showId.map( Tag.get(_).get).getOrElse(rootTag)
  val doc= showTag.doc.obj.get

  var name= ""
  var helpers = List[SectionRenderer]()

  def title(node : NodeSeq) : NodeSeq = 
    Text(doc.name.is + " : " + showTag.name.is)

  def render(node : NodeSeq) : NodeSeq = node.flatMap { render(_) }

  def render(node : Node) : NodeSeq = node match {
    case Elem("test", tag, attribs, scope, children @ _*) =>
      if (tag match {
	case "isHead" => 
	  Tag.find(By(Tag.name, showTag.name), 
		   By(Tag.doc, doc), 
		   OrderBy(Tag.id, Descending)).exists( _==showTag)
	case "hasHistory" => rootTag != showTag
      }) render(node.child) else NodeSeq.Empty

    case Elem("doc", tag, attribs, scope, children @ _*) =>
    tag match {
      case "title" => title(node)
      case "tagName" => Text(showTag.name.is)
      case "history" => history()
      case "content" => render(doc.head.obj.get, children)
      case "makeTagName" => SHtml.text(name, name= _)
      case "makeTag" => SHtml.ajaxSubmit("MakeTag", ()=>{ makeTag(name) })
      case "updateTag" => SHtml.ajaxSubmit("UpdateTag", ()=>{ updateTag })
    }

    case Elem(prefix, label, attribs, scope, children @ _*) =>
      Elem(prefix, label, attribs, scope, render(children) : _*)

    case _ => node
  }

  def render(sec : Section, node : NodeSeq) : NodeSeq = {
    val helper= new SectionRenderer(rootTag, showTag, sec)
    helpers ::= helper

    helper.render(node) ++ renderFollowing(sec,node)
  } 

  def renderFollowing(sec : Section, node : NodeSeq) : NodeSeq = {
    val links= Link.findAll(By(Link.pre, sec))
    links.flatMap { link => render(link.post.obj.get, node) }
  }

  def makeTag(name : String) : JsCmd = {
    if (name.isEmpty) error ("Tag name is empty")
    else {
      val other= Tag.find(By(Tag.doc, doc), By(Tag.name, name))
      if (!other.isEmpty) error("Tag named '"+name+"' already exists")
      else {
	val newTag = Tag.create.name(name).doc(doc).parent(rootTag)
	makeTag(newTag)
      }
    }
  }
  
  def makeTag(newTag : Tag) : JsCmd = {
    newTag.time(TimeUtil.now)
    newTag.save
    helpers.foreach { _.makeTag(newTag) }
    RedirectTo(linkUri(newTag))
  }

  def updateTag() : JsCmd = {
    val dirty= helpers.foldLeft(false) { _ || _.isDirty }
    if (dirty) {
      val newTag = Tag.create.name(showTag.name).doc(doc).parent(showTag)
      makeTag(newTag)
    } else {
      Noop
    }
  }

  def linkUri(target : Tag, head : Boolean = false) : String = {
    val params= Seq(
      "root"->rootTag.id.is.toString, 
      "show"-> (if (head) "#"+target.name.is else target.id.is.toString))
    Helpers.appendParams(uri, params)
  }

  def history() : NodeSeq = {
    val name= showTag.name.is
    def older(tag : Tag) : List[Tag] =
      tag :: (if (tag.parent.obj.isEmpty) Nil else older(tag.parent.obj.get))
    def newer(tag : Tag) : List[Tag] = {
      val next= Tag.find(By(Tag.name, name), By(Tag.parent, tag))
      if (next.isEmpty) Nil else next.get :: newer(next.get)
    }
    var x=0
    (older(showTag).reverse ++ newer(showTag)).flatMap { tag =>
      val href= linkUri(tag)
      if (tag.name.is==name) {
	val style= if (tag==showTag) "active" else "inactive"
	x+=1
	<a href={href} class={style}>{ "["+x+"]" }</a>
      } else {
	<a href={href} class="inactive">{ "["+tag.name.is+"]" }</a>
      }
    } ++ 
      <a href={linkUri(showTag, true)} class={
	if (showId.exists (_.startsWith("#"))) "active" else"inactive"}>{ 
	  "[HEAD]" }</a> ++
    <span> { TimeUtil.formatRelTime(showTag.time.is) } </span>
  }

  def error(msg : String) = SetHtml("error", <div class="error">{msg}</div>)
    
}


