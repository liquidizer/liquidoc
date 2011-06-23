package org.liquidizer.doc.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._

/** The TagTree repressents the version tree for a section */
class TagTree(val cur : Option[Content]) {

  val children : List[TagTree] = 
    if (cur.isEmpty) Nil else
    Content.findAll(By(Content.parent, cur.get)).map {
      content => new TagTree(Some(content)) 
    }.sort { _.refs.size > _.refs.size }

  var refs : List[Tag] = loadRefs()

  def loadRefs() : List[Tag] = { 
    if (cur.isEmpty) Nil else {
      val refs= TagRef.findAll(By(TagRef.content, cur.get)).map {_.tag.is}
      val tags= Tag.findAll(By(Tag.isold,false), ByList(Tag.id, refs))
      tags ++ children.flatMap { _.refs }
    }.removeDuplicates
  }

  def refreshRefs() {
    children.foreach { _.refreshRefs() }
    refs= loadRefs()
  }

  def isCurrent(show : Option[Content]) : Boolean = 
    !cur.isEmpty && !show.isEmpty && cur==show

  def isShown(show : Option[Content]) : Boolean = 
    isCurrent(show) || children.exists { _.isShown(show) }

  def containsCurrent(show : Option[Content]) : Boolean = 
    isCurrent(show) || children.exists( _.containsCurrent(show) )
}

object TagTree {
  def getTrees(sec : Section) : List[TagTree] = {
    TagRef.findAll(By(TagRef.section, sec), NotNullRef(TagRef.content),
		   NullRef(TagRef.tag))
    .map { _.content.obj.get }
    .filter { !_.parent.defined_? }
    .map { content => new TagTree(Some(content)) }
    .sort { _.refs.size > _.refs.size }
  }
}
