package org.liquidizer.doc.lib

import scala.xml._

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._

class Uncover(var list : List[NodeSeq], val n : Int) {

  def addAction(node : NodeSeq, id : String) : NodeSeq = {
    node.lastOption match {
      case _ if (list.isEmpty) => node
      case Some(<li>{item @ _*}</li>) => 
        node ++ addLink(id, "li") ++ addContent(id)
      case _ => 
        node ++ addLink(id, "span") ++ addContent(id)
    }
  }

  def addLink(id : String, node : String) : NodeSeq= 
    if (list.isEmpty) NodeSeq.Empty
    else {
      <span id={id+"_more"}> {
	Elem(null, node, xml.Null, xml.TopScope,
	     SHtml.a(() =>  {
               SetHtml(id+"_more", NodeSeq.Empty) &
               SetHtml(id+"_next", next(id+"_next", n))
             },
      	     <span class="more">{"[%d more]".format(list.size)}</span>))
      } </span>
    }

  def addContent(id : String) : NodeSeq= <span id={id+"_next"}/>
  
  def next(id : String, n : Int) : NodeSeq = {
    var h= list.take(n).flatMap { x => x }
    list= list.drop(n)
    addAction(h, id)
  }

  def next() : NodeSeq = next("uncover", n)
}
