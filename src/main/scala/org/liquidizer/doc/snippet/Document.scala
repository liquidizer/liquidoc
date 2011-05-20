package org.liquidizer.doc.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._

class Document {

  println("Document created")

  val uri= S.uri

  val rootId= S.param("root").map{ _.toLong }.getOrElse(1L)
  val rootTag= Tag.find(By(Tag.id, rootId)).get

  val showId= S.param("show").map{ _.toLong }.getOrElse(rootId)
  val showTag= Tag.find(By(Tag.id, showId)).get

  val rootContents= Map(rootTag.contents.map{c=> c.section.obj.get -> c}:_*)
  val showContents= Map(showTag.contents.map{c=> c.section.obj.get -> c}:_*)

  var name= "tag"
  var helpers = List[SectionRenderer]()

  def render(node : NodeSeq) : NodeSeq = node.flatMap { render(_) }

  def render(node : Node) : NodeSeq = node match {
    case Elem("doc", tag, attribs, scope, children @ _*) =>
    tag match {
      case "content-tr" => render(showTag.head.obj.get, children)
      case "tagName" => SHtml.text(name, name= _)
      case "makeTag" => SHtml.ajaxSubmit("MakeTag", () => {
        val newTag = Tag.create.name(name).head(rootTag.head.obj.get)
	newTag.save
        helpers.foreach { _.makeTag(newTag) }
	S.redirectTo(Helpers.appendParams(uri, 
	  Seq("root" -> rootId.toString, "show" -> newTag.id.is.toString)))
        })
    }

    case Elem(prefix, label, attribs, scope, children @ _*) =>
      Elem(prefix, label, attribs, scope, render(children) : _*)

    case _ => node
  }

  def render(sec : Section, node : NodeSeq) : NodeSeq = {
    val ref= rootContents.get(sec).get
    val show= showContents.get(sec).get

    val helper= new SectionRenderer(ref, show)
    helpers ::= helper

    helper.render(node) ++ renderFollowing(sec,node)
  } 

  def renderFollowing(sec : Section, node : NodeSeq) : NodeSeq = {
    val links= Link.findAll(By(Link.pre, sec))
    val next= links.filter{
      link => rootContents.values.toList.contains( link.post.obj.get )
    }
    
    links.flatMap { link => render(link.post.obj.get, node) }
  }
}

