package org.liquidizer.doc.snippet

import scala.xml.{Node, NodeSeq, Text, Elem}
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._

class LiquiDocIndex {

  def render(node : NodeSeq) : NodeSeq = 
    sort(Document.findAll).flatMap { 
      doc => render(node, doc) }

  def render(node : NodeSeq, doc : Document) : NodeSeq = 
    node.flatMap { render(_, doc) }

  def render(node : Node, doc : Document) : NodeSeq = {
    node match {

    case Elem("doc", tag, attribs, scope, children @ _*) =>
      tag match {
	case "title" => format(doc)
	case "tags" => Text(weight(doc).toString)
	case "date" => Text(TimeUtil.formatRelTime(latestEdit(doc)))
      }

      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, render(children, doc) : _*)

      case _ => node
    }
  }

  def format(doc : Document) : NodeSeq = {
    <a href={"/doc/"+doc.name.is}>{ doc.name.is }</a>
  }

  def weight(doc : Document) : Int = {
    Tag.findAll(By(Tag.doc, doc),By(Tag.isold, false)).size
  }

  def latestEdit(doc : Document) : Long = {
    Tag.find(By(Tag.doc, doc), OrderBy(Tag.time, Descending))
    .map { _.time.is }.getOrElse(0)
  }

  def sort(docs : List[Document]) : List[Document] = {
    docs.map { doc => (doc, weight(doc)) }
    .sort { _._2 > _._2 }
    .map { _._1 }
  }
}


