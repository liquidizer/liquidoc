package org.liquidizer.doc.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

class Tag extends LongKeyedMapper[Tag] with IdPK {
  def getSingleton = Tag
  object name extends MappedString(this, 50)
  object time extends MappedLong(this)
  object isold extends MappedBoolean(this)
  object doc extends LongMappedMapper(this, Document)

  def content(sec : Section) : Box[Content] = {
    TagRef.find(By(TagRef.tag, this), By(TagRef.section, sec))
    .map { _.content.obj.get }
  }
}

object Tag extends Tag with LongKeyedMetaMapper[Tag] {
  def get(id : String, doc : Document) : Option[Tag] = 
    if (id.startsWith("#")) 
      Tag.find(By(Tag.name, id.substring(1)), 
	       By(Tag.isold,false), By(Tag.doc, doc),
	       OrderBy(Tag.id, Descending))
    else Tag.find(By(Tag.id, id.toLong))
}

class TagRef extends LongKeyedMapper[TagRef] with IdPK {
  def getSingleton = TagRef

  object tag extends LongMappedMapper(this, Tag) with DBIndexed
  object section extends LongMappedMapper(this, Section) with DBIndexed
  object content extends LongMappedMapper(this, Content)
}

object TagRef extends TagRef with LongKeyedMetaMapper[TagRef] {
}
