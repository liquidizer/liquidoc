package org.liquidizer.doc.lib

import scala.xml._

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._

class Uncover(val iter : Iterator[NodeSeq], val n : Int) {

  def addAction(node : NodeSeq, id : String) : NodeSeq = {
    node.last match {
      case <li>{item @ _*}</li> => 
        node ++ addLink(id, "li") ++ addContent(id)
      case _ if (iter.hasNext) => 
        node ++ addLink(id, "span") ++ addContent(id)
      case _ => node
    }
  }

  def addLink(id : String, node : String) : NodeSeq= 
    if (!iter.hasNext) NodeSeq.Empty
    else {
      <span id={id+"_more"}> {
	Elem(null, node, xml.Null, xml.TopScope,
	     SHtml.a(() =>  {
               SetHtml(id+"_more", NodeSeq.Empty) &
               SetHtml(id+"_next", next(id+"_next", n))
             },
      	     <span class="more">[more]</span>))
      } </span>
    }

  def addContent(id : String) : NodeSeq= <span id={id+"_next"}/>
  
  def next(id : String, n : Int) : NodeSeq = {
    var h= NodeSeq.Empty
    while (iter.hasNext && h.size<n) h ++= iter.next
    if (h.isEmpty) NodeSeq.Empty
    else addAction(h, id)
  }

  def next() : NodeSeq = next("uncover", n)
}
