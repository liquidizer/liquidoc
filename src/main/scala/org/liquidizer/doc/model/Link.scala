package org.liquidizer.doc.model

import net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Link extends LongKeyedMapper[Link] with IdPK {
  def getSingleton = Link
  object pre extends LongMappedMapper(this, Section) with DBIndexed
  object post extends LongMappedMapper(this, Section)
}

object Link extends Link with LongKeyedMetaMapper[Link] {

}
