package org.liquidizer.doc.snippet

import scala.xml.{Node, NodeSeq, Text, Elem}
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._

class LiquiDocHistory {

  val contents= Content.findAll(OrderBy(Content.time, Descending))

  def render(node : NodeSeq) : NodeSeq = 
    contents.flatMap { 
      content => render(node, content) }

  def render(node : NodeSeq, content : Content) : NodeSeq = 
    node.flatMap { render(_, content) }


  def render(node : Node, content : Content) : NodeSeq = {
    node match {

    case Elem("doc", tag, attribs, scope, children @ _*) =>
      tag match {
	case "time" => formatTime(content)
	case "content" => formatContent(content)
	case "supporter" => formatSupporter(content)
	case "title" => formatDocument(content)
      }

      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, render(children, content) : _*)

      case _ => node
    }
  }

  def formatDocument(doc : Document) : NodeSeq = {
    <a href={"/doc/"+doc.name.is}>{ doc.name.is }</a>
  }
  
  def formatTime(content : Content) : NodeSeq = {
    val time= content.time.is
    if (time>0) {
      <span class="time">{
	TimeUtil.formatRelTime(time)
      }</span>
    } else {
      NodeSeq.Empty
    }
  }

  def formatContent(content : Content) : NodeSeq = {
    val oldContent= content.parent.obj
    if (oldContent.isEmpty)
      Text(content.text.is)
    else 
      DiffRenderer.renderDiff(oldContent.get.text.is, content.text.is)
  }

  def formatDocument(content : Content) : NodeSeq = {
    val ref= TagRef.find(By(TagRef.content, content), 
			 NotNullRef(TagRef.tag));
    if (ref.isEmpty) {
      NodeSeq.Empty
    } else {
      val tag= ref.get.tag.obj.get
      val doc= tag.doc.obj.get
      <a href={"/doc/"+Helpers.urlEncode(doc.name.is)}>{
	doc.name.is
      }</a>
    }
  }

  def formatSupporter(content : Content) : NodeSeq = {
    val refs= TagRef.findAll(By(TagRef.content, content), 
			     NotNullRef(TagRef.tag));
    val tags= refs.map { _.tag.obj.get };
    if (tags.isEmpty) 
      NodeSeq.Empty
    else {
      Text("Supported by: ") ++
      tags.map { link(_) }
    }
  }

  def link(target : Tag) : Node = {
    val params= Seq(("show", "#"+target.name.is))
    val doc= target.doc.obj.get
    val uri= Helpers.appendParams("/doc/"+doc.name.is, params)
    <a href={ uri } class="tag inactive">{
      target.name.is
    }</a>
  }

}


