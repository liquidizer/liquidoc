package org.liquidizer.doc.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._

class SectionRenderer(val rootTag : Tag, val showTag : Tag, sec : Section) {
  
  var random= new scala.util.Random
  val ref= rootTag.content(sec).get
  var show= showTag.content(sec).get

  val styles=List(
    ("h1", "Heading 1"),
    ("h2", "Heading 2"),
    ("p" , "Paragraph"),
    ("ul", "List item"),
    ("ol", "Numberd list item"))

  val id= show.id.is

  def render(node : NodeSeq) : NodeSeq = {
    <div id={"section-" + id}> { bind(node) }</div>
  }

  def bind(node : NodeSeq) : NodeSeq = node.flatMap { bind(_) }

  def bind(node : Node) : NodeSeq = node match {
    case Elem("sec", tag, attrib, scope, children @ _*) =>
      tag match {
        case "content" => contentArea()
	case "branches" => branchArea()
      }

    case Elem(prefix, label, attribs, scope, children @ _*) =>
      Elem(prefix, label, attribs, scope, bind(children) : _*)

    case _ => node
  }

  def editButton()= SHtml.a(()=> toEditMode(), 
			    <img src="/images/edit.png" class="edit"/>)

  def toEditMode() = {
    show= Content.create.parent(show).text(show.text.is).style(show.style.is)
    SetHtml("content"+id, editArea())
  }

  def editArea() : NodeSeq = {
    <table><tr><td colspan="2">{ 
      SHtml.textarea(show.text.is, 
		     text => show.text(text.split("\\s+").mkString(" ")),
		     "rows"->"15", "cols"->"80")
    }</td></tr><tr><td> {
      Text("Style: ") ++
      SHtml.select(styles, Full(show.style.is), text => show.style(text) )
    }</td><td align="right"> {
      Text(" ") ++ saveButton() ++
      Text(" ") ++ cancelButton()
    } </td></tr></table>
  }

  def saveButton()= SHtml.ajaxSubmit("save", () => save())
  def cancelButton()= SHtml.ajaxSubmit("cancel", () => cancel())

  def save() : JsCmd = {
    val dirty= show.parent.obj.exists {
     p => p.text.is!=show.text.is || p.style.is!=show.style.is
    }
    if (dirty) {
      show.save
      redraw(show.parent.obj.get, show)
    } else
      cancel()
  }

  def cancel() : JsCmd = {
    show= show.parent.obj.get
    redraw
  }

  def redraw() : JsCmd = redraw(show, show)

  def redraw(oldShow : Content, newShow : Content) : JsCmd = {
    show = newShow
    SetHtml("content"+id, {
      if (oldShow==newShow) { content() }
      else { content(ref, oldShow, newShow) }}) &
    SetHtml("edit"+id, editButton()) &
    SetHtml("branches"+id, branches())
  }

  def content() = {
    format(show.style.is,
	DiffRenderer.renderDiff(ref.text.is, show.text.is) ++
	editButton)
  }

  def content(ref : Content, oldShow : Content, newShow :Content) = {
    format(newShow.style.is,
	   DiffRenderer.renderDiff(ref.text.is, 
				   oldShow.text.is, newShow.text.is) ++
	   editButton)
  }
  
  def format(style : String, body : NodeSeq) : NodeSeq = style match {
    case "ol" | "ul" =>
      <table><tr><td class="section-li">{ 
	if (style=="ol") "#" else "*" 
      }</td><td> {
	body
      }</td></tr></table>
    case _ => <div class={"section-"+style}> { body } </div>
  }

  def contentArea() = <div id={"content"+id} >{ content() }</div>

  def branchArea() : NodeSeq = <div id={"branches"+id}>{ branches() }</div>
  def branches() : NodeSeq = {
    val tree= new TagTree(ref, show)
    toHtml(List(tree))
  }

  def toHtml(trees : List[TagTree]) : NodeSeq = <ul> {
    val h= trees.flatMap { tree => <li> { toHtml(tree) } </li> }
    val n= if (trees.exists(_.containsCurrent))
      trees.takeWhile(!_.containsCurrent).size + 2 else 0
    new Uncover(h.elements, 5).next("branchvar"+random.nextInt, 5 max n) 
  } </ul>

  def toHtml(tree : TagTree) : NodeSeq = {
    val src= if (tree.isCurrent) "active" else "inactive"
    val img = <img src={"/images/"+src+".png"}/>
    val icon = SHtml.a(() => { redraw(show, tree.cur)}, img) 
    
    val subtree= 
      if (tree.isShown) {
	val tags = tree.refs -- tree.children.flatMap( _.refs )
	tagList(tags) ++ {
	  if (!tree.isCurrent && tags.isEmpty && tree.children.size<=1)
	    <div class="compact"> {
	      toHtml(tree.children)
	    } </div>
	  else
	    toHtml(tree.children)
	}
      } else {
	tagList(tree.refs)
      }
    val votes= if (tree.refs.isEmpty) NodeSeq.Empty else
      <span>{ " (" + tree.refs.size+") " }</span>
    icon ++ votes ++ subtree
  }

  /** Show a list of named tags */
  def tagList(tags : List[Tag]) : NodeSeq = {
    var sorted= tags
    var n= 0
    if (tags.contains(showTag)) {
      sorted=  showTag :: (sorted -- List(showTag))
      n=1
    }
    new Uncover(sorted.elements.map { tagLink(_) }, 3)
    .next("branchvar"+random.nextInt, n)
  }

  /** Format a tag as a permanent link to its content */
  def tagLink(tag : Tag) : NodeSeq = {
    Text(" ")++
    <a href={
      Helpers.appendParams("/", Seq(
	"root" -> rootTag.id.is.toString,
	"show" -> tag.id.is.toString))}
      class={if (tag==showTag) "active" else "inactive"}>{
      tag.name.is
    }</a>
  }

  def makeTag(tag : Tag) {
    if (show.dirty_?)
      save()
    TagRef.create.tag(tag).content(show).section(sec).save
  }
}
