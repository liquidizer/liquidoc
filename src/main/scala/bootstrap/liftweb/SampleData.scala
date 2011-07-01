package bootstrap.liftweb

import java.io.File
import scala.io.Source
import scala.xml._

import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._

object SampleData {
  var current : Option[Section]= None
  var tag : Option[Tag]= None

  def update() {
    // loadManifesto(new File("manifesto.xml"))
    val names= Tag.findAll.map { _.name.is }.removeDuplicates.filter(!_.trim.isEmpty)
    println(names)
    val MANIFESTO="Parteiprogramm"
    if (Document.find(By(Document.name, MANIFESTO)).isEmpty) {
    val docs= Document.findAll.slice(0,16)

    val doc= Document.create.name(MANIFESTO)
    doc.head(docs(0).head)
    doc.save

    val secs= docs.flatMap { sec => sec.getSections().filter {_.id.is<250} }
    for (i <- 0 to secs.size-2) {
      val link= Link.find(By(Link.pre, secs(i)), By(Link.post,secs(i+1)))
      println(secs(i).id.is+" -> "+secs(i+1).id.is)
      if (link.isEmpty && secs(i).id.is<secs(i+1).id.is) {
	Link.create.pre(secs(i)).post(secs(i+1)).save
      }
    }

    val todelete= Tag.findAll(ByList(Tag.doc, docs.map{_.id.is}))
    val tagMap= Map(names.map {
      name => (name, {
        val time=Tag.findAll(By(Tag.name, name)).map{_.time.is}.foldLeft(0L){_ max _}
        Tag.create.name(name).time(time).doc(doc).saveMe
	})
	}:_*)
    println(tagMap)
    for (ref <- TagRef.findAll) {
      val refTag= ref.tag.obj
      if (refTag.exists { d => docs.contains(d.doc.obj.get) }) {
        if (refTag.exists {_.isold} || !refTag.exists{!_.name.is.isEmpty}) {
          ref.delete_!
        } else {
          ref.tag(tagMap.get(refTag.get.name.is).get).save
        }
      }
    }
    todelete.foreach {_.delete_!}
    docs.foreach {_.delete_!}
  }
  }

  def makeUpdateTag() {
    for (doc <- Document.findAll) {
      var sec:Option[Section] = doc.head.obj
      val otag= Tag.find(By(Tag.doc, doc))
      val tag= Tag.create.name("PA074").time(TimeUtil.now).doc(doc)
      tag.save
      while (!sec.isEmpty) {
	val ref= TagRef.find(By(TagRef.section, sec.get)).get
	val child= Content.find(By(Content.parent, ref.content.obj.get))
	if (!child.isEmpty) {
	  TagRef.create
	  .section(sec.get)
	  .content(child.get)
	  .tag(tag).save
	}
	val link= Link.find(By(Link.pre, sec.get))
	sec= link.map { _.post.obj.get }
      }
    }
  }
  

  def makeContent(style : String, content : String) = {
    val node= Section.create
    node.save
    if (!current.isEmpty) {
      val link= Link.create.pre(current.get).post(node)
      link.save
    }
    val para= Content.create.style(style).text(nospace(content))
    para.save
    val tref= TagRef.create.content(para).section(node).tag(tag.get)
    tref.save
    if (!tag.get.doc.obj.get.head.defined_?)
      tag.get.doc.obj.get.head(node).save

    current= Some(node)
  }

  def makeDoc(name : String, version : String) = {
    val doc= Document.create.name(name)
    doc.save
    tag= Some(Tag.create.name(version).doc(doc).time(TimeUtil.now))
    tag.get.save
    current= None
  }

  def nospace(str : String) = str.split("\\s+").mkString(" ")

  def loadManifesto(file : File) {
    val manifesto= XML.loadFile(file)
    for (node <- manifesto.child) {
      node match {
        case Elem(_, "h2", attribs, scope,  ch @ _*) =>
	  makeDoc(attribs.get("id").get.text.replaceAll("_"," "), "official")
	  makeContent("h1", ch.text)

        case Elem(_, "h3", attribs, scope,  ch @ _*) =>
	  makeContent("h2", ch.text)

        case Elem(_, "h4", attribs, scope,  ch @ _*) =>
	  makeContent("h3", ch.text)

	case Elem(_, "p", attribs, scope, ch @_*) =>
	  val text= ch.text.trim
	  if (!text.isEmpty)
	    makeContent("p", text)

	case Elem(_, "ol", attribs, scope, ch @_*) =>
	  for (li <- node \\ "li")
	    makeContent("ol", li.child.text)

	case Elem(_, "ul", attribs, scope, ch @_*) =>
	  for (li <- node \\ "li")
	    makeContent("ul", li.child.text)

	case Text(_) =>
	case _ => println("unknown element: "+node)
      }
    }
  }

  def prefix(str : String) = str.substring(0, 10 min str.length)

  val PRE= "^[^: ]+:"

  def loadPA074(file : File) {
    val doc= XML.loadFile(file)
    var ref : Option[Content] = None

    for (p <- doc \\ "p") {
      val text= p.child.text.trim
      if (text.startsWith("bisher:")) {
	val ntext= nospace(text.replaceAll(PRE,"").trim)
        ref= Content.find(By(Content.text, ntext))
	if (!ref.isEmpty) println("MATCHED")
	if (ref.isEmpty) {
	  println("SEARCH : "+prefix(ntext))
	  val m= Content.findAll.filter(
	    x => prefix(x.text.is) == prefix(ntext))
	  println(m.size)
	  m.foreach { x=> println(x.text.is) }
	}
      } else {
        if (!ref.isEmpty && text.startsWith("neu:")) {
	  println("new")
          Content.create
	  .text(nospace(text.replaceAll(PRE,"")))
	  .style(ref.get.style.is)
	  .parent(ref.get)
	  .save
	  ref= None
	}
      }
    }
  }
}
