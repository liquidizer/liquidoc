package org.liquidizer.doc.model

import net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Content extends LongKeyedMapper[Content] with IdPK {
  def getSingleton = Content
  object parent extends LongMappedMapper(this, Content)
  object style extends MappedString(this, 30)
  object text extends MappedText(this)
}

object Content extends Content with LongKeyedMetaMapper[Content] {

}
