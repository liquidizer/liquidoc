package org.liquidizer.doc.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._

/** The TagTree repressents the version tree for a section */
class TagTree(val cur : Option[Content], val show : Option[Content]) {

  val children : List[TagTree] = 
    if (cur.isEmpty) Nil else
    Content.findAll(By(Content.parent, cur.get)).map {
      content => new TagTree(Some(content), show) 
    }.sort { _.refs.size > _.refs.size }


  val refs : List[Tag] = { 
    if (cur.isEmpty) Nil else {
      TagRef.findAll(By(TagRef.content, cur.get))
      .map { _.tag.obj.get }
      .filter { !_.isold.is } ++ 
      children.flatMap { _.refs }
    }.removeDuplicates
  }

  val isCurrent : Boolean = !cur.isEmpty && !show.isEmpty && cur==show

  val isShown : Boolean= isCurrent || children.exists { _.isShown }

  val containsCurrent : Boolean = 
    isCurrent || children.exists( _.containsCurrent )
}
