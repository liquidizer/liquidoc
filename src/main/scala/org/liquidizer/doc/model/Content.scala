package org.liquidizer.doc.model

import net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Content extends LongKeyedMapper[Content] with IdPK {
  def getSingleton = Content
  object time extends MappedLong(this)
  object parent extends LongMappedMapper(this, Content)
  object style extends MappedString(this, 30)
  object text extends MappedText(this)
}

object Content extends Content with LongKeyedMetaMapper[Content] {

  val styles=List(
    ("h1", "Heading 1"),
    ("h2", "Heading 2"),
    ("h3", "Heading 3"),
    ("p" , "Paragraph"),
    ("ul", "List item"),
    ("ol", "Numberd list item"))

}
