package org.liquidizer.doc.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._

/** The TagTree repressents the version tree for a section */
class TagTree(val cur : Content, val show : Content) {

  val children : List[TagTree] = 
    Content.findAll(By(Content.parent, cur)).map {
      new TagTree(_, show) 
    }

  val refs : List[Tag] = { 
    TagRef.findAll(By(TagRef.content, cur))
    .map { _.tag.obj.get }
    .filter { !_.isold.is } ++ 
    children.flatMap { _.refs }
  }.removeDuplicates

  val isShown : Boolean = children.foldLeft(cur==show) { _ || _.isShown }

  def conflicts(tag : Tag, sec : Section) = {
    refs.filter { _.content(sec) != show }
  }

  val isCurrent : Boolean = cur==show

  val containsCurrent : Boolean = 
    isCurrent || children.exists(_.containsCurrent)
}
