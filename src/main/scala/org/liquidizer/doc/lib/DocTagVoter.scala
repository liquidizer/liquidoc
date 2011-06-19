package org.liquidizer.doc.lib

import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.doc.model._

object DocTagVoter {
  
  def voterTags(doc : Document) : List[Tag] = {
    val tags= Tag.findAll(By(Tag.doc,doc),By(Tag.isold,false))
    val refs= TagRef.findAll(ByList(TagRef.tag, tags.map{_.id.is}))
    val contents= refs.map { _.content.is }.removeDuplicates
    val votes= Map(contents.map { 
      c => (c, refs.filter { _.content.is == c }.size)
    }:_*)
    val score= Map(tags.map {
      tag =>
	(tag, 
	 refs.filter { _.tag.is == tag.id.is }
	 .map { ref => votes.get( ref.content.is ).getOrElse(0) }
	 .foldLeft(0) { _ + _ }
       )
    }:_*)
    tags.sort { score(_) > score(_) }
  }

}
