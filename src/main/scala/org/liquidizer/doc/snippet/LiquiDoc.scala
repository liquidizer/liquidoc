package org.liquidizer.doc.snippet

import scala.xml.{Elem,Node,NodeSeq,Text}
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._

class LiquiDoc {

  val docUri= S.uri

  val doc : Document= S.param("doc").map { 
    doc => Document.find(By(Document.name, doc)).get
  }.getOrElse { 
    Tag.find(By(Tag.id, S.param("root").get.toLong)).get.doc.obj.get 
  }

  val rootTag : Tag = S.param("root")
  .map { Tag.get(_).get }
  .getOrElse(Tag.find(By(Tag.doc, doc)).get)

  val showId= S.param("show")
  val showTag= showId.map( Tag.get(_).get).getOrElse(rootTag)

  var helpers : Option[SectionRenderer] = None

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
      case "updateTag" => renderUpdateTag(children)
    }

    case Elem(prefix, label, attribs, scope, children @ _*) =>
      Elem(prefix, label, attribs, scope, render(children) : _*)

    case _ => node
  }

  def render(sec : Section, node : NodeSeq) : NodeSeq = {
    val helper= new SectionRenderer(this, sec)
    (if (helper.isEmpty) NodeSeq.Empty else {
      if (helpers.isEmpty) 
	helpers= Some(helper)
      else
	helpers.get.append(helper)
      helper.render(node) 
    }) ++ renderFollowing(sec,node)
  } 

  def renderFollowing(sec : Section, node : NodeSeq) : NodeSeq = {
    val a= Link.findAll(By(Link.pre, sec)).map { _.post.obj.get }
    val b= Link.findAll(ByList(Link.pre, a.map { _.id.is }))
    .map { _.post.obj.get }
    
    (a--b).flatMap { sec => render(sec, node) }
  }

  def renderUpdateTag(node : NodeSeq ) : NodeSeq = {
    var diff= true
    var perma= true
    Helpers
    .bind("doc", node,
	  "diff" -> SHtml.checkbox(diff, diff = _),
	  "perma" -> SHtml.checkbox(perma, perma = _),  
	  "submit" -> SHtml.ajaxSubmit("MakeTag", ()=>updateTag(diff,perma))
	)
  }

  def updateTag(makeDiff : Boolean, perma : Boolean) : JsCmd = {
    if (PseudoLogin.loggedIn) {
      val name= PseudoLogin.userName

      Tag.findAll(By(Tag.name,name), By(Tag.isold,false), By(Tag.doc,doc))
      .foreach { _.isold(true) .save }

      val newTag = Tag.create.name(name).doc(doc).parent(showTag)
      newTag.time(TimeUtil.now)
      newTag.save
      helpers.foreach { _.makeTag(newTag) }
      RedirectTo(linkUri(newTag, makeDiff, !perma))
    } else {
      Noop
    }
  }

  def linkUri(target : Tag, diff : Boolean = true, head : Boolean = false) 
  : String = {
    val tid= if (head) "#"+target.name.is else target.id.is.toString
    val params= if (diff)
      Seq("root" -> rootTag.id.is.toString, "show" -> tid)
    else
      Seq("root" -> tid)
    Helpers.appendParams(docUri, params)
  }

  def history() : NodeSeq = NodeSeq.Empty
    
}


