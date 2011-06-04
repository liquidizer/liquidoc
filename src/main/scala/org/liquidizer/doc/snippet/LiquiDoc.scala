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

  var helpers = List[SectionRenderer]()

  def title(node : NodeSeq) : NodeSeq = 
    Text(doc.name.is + " : " + showTag.name.is)

  def render(node : NodeSeq) : NodeSeq = node.flatMap { render(_) }

  def render(node : Node) : NodeSeq = node match {
    case Elem("test", tag, attribs, scope, children @ _*) =>
      if (tag match {
	case "hasHistory" => rootTag != showTag
      }) render(node.child) else NodeSeq.Empty

    case Elem("doc", tag, attribs, scope, children @ _*) =>
    tag match {
      case "title" => title(node)
      case "tagName" => Text(showTag.name.is)
      case "history" => history()
      case "content" => render(doc.head.obj.get, children)
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

  def updateTag() : JsCmd = {
    if (PseudoLogin.loggedIn) {
      val name= PseudoLogin.userName

      Tag.findAll(By(Tag.name,name), By(Tag.isold, false))
      .foreach { _.isold(true) .save }

      val newTag = Tag.create.name(name).doc(doc).parent(showTag)
      newTag.time(TimeUtil.now)
      newTag.save
      helpers.foreach { _.makeTag(newTag) }
      RedirectTo(linkUri(newTag))
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
    val fixHead= showId.exists (_.startsWith("#"))
    val tags= Tag.findAll(By(Tag.name, name),OrderBy(Tag.time, Ascending))
    var x=0
    <a href={linkUri(rootTag, false)} class="inactive">{ "[ROOT]" }</a> ++
    Text(" ") ++
    tags.flatMap { tag =>
      val href= linkUri(tag)
      val style= if (tag==showTag) "active" else "inactive"
      x+=1
      <a href={href} class={style}>{ "["+x+"]" }</a> ++ Text(" ")
    } ++ 
      <a href={linkUri(showTag, !fixHead)} class={
	if (fixHead) "active" else"inactive"}>{ 
	  "[HEAD]" }</a> ++
    <span> { TimeUtil.formatRelTime(showTag.time.is) } </span>
  }

  def error(msg : String) = SetHtml("error", <div class="error">{msg}</div>)
    
}


