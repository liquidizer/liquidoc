package org.liquidizer.doc.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

class Document extends LongKeyedMapper[Document] with IdPK {
  def getSingleton = Document
  object name extends MappedString(this, 256)
  object head extends LongMappedMapper(this, Section)

  def getSections() : List[Section] = {
    var list= List(head.is)
    var next= List(head.is)
    while (!next.isEmpty) {
      next= Link.findAll(ByList(Link.pre, next)).map{_.post.is}
      next--= list
      list++= next
    }
    Section.findAll(ByList(Section.id, list))
  }
}

object Document extends Document with LongKeyedMetaMapper[Document] {
}
