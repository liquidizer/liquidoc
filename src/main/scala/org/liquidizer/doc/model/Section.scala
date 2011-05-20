package org.liquidizer.doc.model

import net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Section extends LongKeyedMapper[Section] with IdPK {
  def getSingleton = Section
}

object Section extends Section with LongKeyedMetaMapper[Section] {
}
