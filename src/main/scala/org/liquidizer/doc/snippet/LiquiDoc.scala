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
    Text(doc.name.is)

  def render(node : NodeSeq) : NodeSeq = node.flatMap { render(_) }

  def render(node : Node) : NodeSeq = node match {
    case Elem("test", tag, attribs, scope, children @ _*) =>
      if (tag match {
	case "hasHistory" => rootTag != showTag
	case "loggedIn" => PseudoLogin.loggedIn
      }) render(node.child) else NodeSeq.Empty

    case Elem("doc", tag, attribs, scope, children @ _*) =>
    tag match {
      case "title" => title(node)
      case "score" => score(node)
      case "tagName" => Text(showTag.name.is)
      case "content" => render(doc.head.obj.get, children)
      case "updateTag" => renderUpdateTag(children)
    }

    case Elem(prefix, label, attribs, scope, children @ _*) =>
      Elem(prefix, label, attribs, scope, render(children) : _*)

    case _ => node
  }

  def score(node : NodeSeq) : NodeSeq = {
    val links= DocTagVoter.voterTags(doc).flatMap { tagLink(_) }
    new Uncover(links, 3).next("score", 3)
  }

  def buildSectionRenderers(sec : Section) {
    helpers= Some(new SectionRenderer(this, sec))

    var list= List(sec.id.is)
    while (!list.isEmpty) {
      val a= Link.findAll(ByList(Link.pre, list)).map { _.post.obj.get }
      val b= Link.findAll(ByList(Link.pre, a.map { _.id.is }))
      .map { _.post.obj.get }
      for (nsec <- (a--b)) {
	val helper= new SectionRenderer(this, nsec)
	if (!helper.isEmpty)
	  helpers.get.append(helper)
      }
      list= (a--b).map { _.id.is }
    }
  }

  def render(sec : Section, node : NodeSeq) : NodeSeq = {
    buildSectionRenderers(sec)
    helpers.get.renderAll(node)
  }

  def renderUpdateTag(node : NodeSeq ) : NodeSeq = {
    var diff= true
    var perma= true
    Helpers
    .bind("doc", node,
	  "diff" -> SHtml.checkbox(diff, diff = _),
	  "perma" -> SHtml.checkbox(perma, perma = _),  
	  "submit" -> SHtml.ajaxSubmit("MakeTag", ()=> updateTag(diff,perma))
	)
  }

  def getMyTag() : Tag = {
    val user= PseudoLogin.userName
    Tag.find(By(Tag.name, user),  By(Tag.isold, false), By(Tag.doc, doc))
    .getOrElse{
      Tag.create.name(user).doc(doc).saveMe 
    }
  }

  def updateTag(makeDiff : Boolean, perma : Boolean) : JsCmd = {
    if (PseudoLogin.loggedIn) {
      val name= PseudoLogin.userName
      val tag = getMyTag()
      var pre : Option[Section] = None

      // Copy all references to a new version
      if (perma) {
	tag.isold(true).save
	val newTag= Tag.create.name(tag.name).doc(tag.doc).time(TimeUtil.now)
	newTag.save
	TagRef.findAll(By(TagRef.tag,tag)).foreach {
	  ref =>
	    TagRef.create
	    .tag(newTag).content(ref.content).section(ref.section)
	    .save
	}
      }

      // vote for all shown sections
      if (perma || !makeDiff) {
	for (helper <- helpers.get.toList) {
	  helper.favor(tag)

	  if (!helper.show.isEmpty) {
	    val sec= helper.sec
	    if (!pre.isEmpty) {
	      val link= Link.find(By(Link.pre, pre.get), By(Link.post, sec))
	      if (link.isEmpty) {
		Link.create.pre(pre.get).post(sec).save
	      }
	    }
	    pre= Some(sec)
	  }
	}
      }

      RedirectTo(linkUri(tag, makeDiff, !perma))
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

  /** Format a tag as a permanent link to its content */
  def tagLink(tag : Tag) : NodeSeq = {
    <a href={ linkUri(tag) }
      class={if (tag==showTag) "tag active" else "tag inactive"}>{
      tag.name.is
    }</a>
  }

}


