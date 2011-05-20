package org.liquidizer.doc.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

class Tag extends LongKeyedMapper[Tag] with IdPK {
  def getSingleton = Tag
  object name extends MappedString(this, 50)

  object head extends LongMappedMapper(this, Section)

  def contents() = 
    TagRef.findAll(By(TagRef.tag, this)).map {_.content.obj.get}
}

object Tag extends Tag with LongKeyedMetaMapper[Tag] {
}

class TagRef extends LongKeyedMapper[TagRef] with IdPK {
  def getSingleton = TagRef

  object tag extends LongMappedMapper(this, Tag)
  object content extends LongMappedMapper(this, Content)

}

object TagRef extends TagRef with LongKeyedMetaMapper[TagRef] {
}
