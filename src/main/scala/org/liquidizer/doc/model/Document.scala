package org.liquidizer.doc.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

class Document extends LongKeyedMapper[Document] with IdPK {
  def getSingleton = Document
  object name extends MappedString(this, 256)
  object head extends LongMappedMapper(this, Section)
}

object Document extends Document with LongKeyedMetaMapper[Document] {
}
