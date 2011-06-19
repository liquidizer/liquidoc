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

class SectionRenderer(val doc: LiquiDoc, val sec: Section) 
extends Block[SectionRenderer] {
  
  val rootTag= doc.rootTag
  val showTag= doc.showTag
  var random= new scala.util.Random
  val ref= rootTag.content(sec)
  var show= showTag.content(sec)
  var showDiff= true

  val styles=List(
    ("h1", "Heading 1"),
    ("h2", "Heading 2"),
    ("h3", "Heading 3"),
    ("p" , "Paragraph"),
    ("ul", "List item"),
    ("ol", "Numberd list item"))

  /** find existing sections cutting two existing ones */
  def intersect(pre : Section, post : Option[Section]) : List[Section]= {
    var out= Link.findAll(By(Link.pre, pre)).map { _.post.is }
    if (!post.isEmpty) {
      var in= Link.findAll(By(Link.post, post.get)).map { _.pre.is }
      out= out intersect in
    }
    Section.findAll(ByList(Section.id, out))
  }

  def newBlock() = {
    // if no section exists, create one
    var out= intersect(sec, next.map { _.get.sec })
    val newSect= out.firstOption
    .getOrElse { 
      val s= Section.create
      s.save
      Link.create.pre(sec).post(s).save
      if (!next.isEmpty) {
	val post= next.get.get.sec
	Link.create.pre(s).post(post).save
      }
      s
    }
    // create a renderer for the new section
    new SectionRenderer(doc, newSect)
  }
  override def insertIcon() =
    if (intersect(sec, next.map {_.get.sec}).exists { 
      isec => !TagRef.find(By(TagRef.section, isec)).isEmpty})
      <img src="/images/insert_active.png" alt="insert"/>
    else {
      if (PseudoLogin.loggedIn)
	<img src="/images/insert.png" alt="insert" title="insert"/>
      else 
	NodeSeq.Empty
    }

  override def deleteIcon() =
    if (PseudoLogin.loggedIn)
      <img src="/images/delete.png" alt="delete" title="delete"/>
    else
      NodeSeq.Empty

  def editIcon() = 
    <img src="/images/edit.png" alt="edit" title="edit"/>

  def favorIcon() = {
    val mytag= doc.getMyTag()
    if (TagRef.find(By(TagRef.tag,mytag),By(TagRef.section,sec),
		    By(TagRef.content, show)).isEmpty)
	<img src="/images/favor.png"/>
    else
      <img src="/images/favor_active.png"/>  
  }

  override def render(node : NodeSeq) : NodeSeq = {
    bind(super.render(node))
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

  def favorButton() : NodeSeq = 
    SHtml.a(() => favor(show), favorIcon())

  def editButton() : NodeSeq = 
    if (!PseudoLogin.loggedIn) NodeSeq.Empty else {
      SHtml.a(()=> toEditMode(), editIcon()) ++ favorButton
    }

  def toEditMode() : JsCmd = {
    val curText= show.map {_.text.is}.getOrElse("")
    val curStyle= show.map {_.style.is}.getOrElse("p")
    show= Some(Content.create.parent(show)
	       .text(curText).style(curStyle))
    SetHtml("content"+id, editArea())
  }

  def editArea() : NodeSeq = {
    <table><tr><td colspan="2">{ 
      SHtml.textarea(show.map{_.text.is}.getOrElse(""), 
		     text => show.get.text(text.split("\\s+").mkString(" ")),
		     "rows"->"15", "cols"->"80")
    }</td></tr><tr><td> {
      Text("Style: ") ++
      SHtml.select(styles, Full(show.get.style.is), 
		   text => show.get.style(text) )
    }</td><td align="right"> {
      Text(" ") ++ saveButton() ++
      Text(" ") ++ cancelButton()
    } </td></tr></table>
  }

  def saveButton()= SHtml.ajaxSubmit("save", () => save())
  def cancelButton()= SHtml.ajaxSubmit("cancel", () => cancel())

  def save() : JsCmd = {
    val showg= show.get
    val dirty= 
      showg.parent.map { _.text.is }.getOrElse("") != showg.text.is ||
      showg.parent.map { _.style.is }.getOrElse("p") != showg.style.is
    if (dirty) {
      showg.save
      if (!showg.parent.defined_?)
	TagRef.create.section(sec).content(showg).save
      redraw(showg.parent.obj, show)
    } else
      cancel()
  }

  def cancel() : JsCmd = {
    show= show.get.parent.obj
    redraw
  }

  def favor(what : Option[Content]) : JsCmd = {
    val mytag= doc.getMyTag()
    val ref= TagRef.find(By(TagRef.tag, mytag), By(TagRef.section, sec))
    .getOrElse { 
      TagRef.create.tag(mytag).section(sec) 
    }
    if (what.isEmpty)
      ref.delete_!
    else
      ref.content(what.get).save
    showDiff= true
    redraw()
  }

  /** Insert action is blocked if an empty edit section exits */
  override def insertBlock(id : String, node : NodeSeq) : JsCmd = {
    if ((isEmpty || next.exists { _.get.isEmpty }) &&
      intersect(sec, next.map {_.get.sec}).isEmpty) Noop
    else super.insertBlock(id, node)
  }

  /** Delete block strokes reference text or makes block disappear */
  override def deleteBlock(id : String) : JsCmd = {
    if (ref.isEmpty && show.isEmpty) super.deleteBlock(id)
    else {
      redraw(show, None)
    }
  }

  def redraw() : JsCmd = redraw(show, show)

  def redraw(oldShow : Option[Content], newShow : Option[Content]) 
  : JsCmd = {
    show = newShow
    SetHtml("content"+id, content(oldShow, newShow)) &
    SetHtml("edit"+id, editButton()) &
    SetHtml("branches"+id, branches())
  }

  def content(oldShow : Option[Content], newShow : Option[Content])={
    val refText= ref.map {_.text.is}.getOrElse ("")
    val oldShowText= oldShow.map {_.text.is}.getOrElse ("")
    val newShowText= newShow.map {_.text.is}.getOrElse ("")
    val style= newShow.map {_.style.is}.getOrElse ("p")

    format(style, {
      if (oldShow==newShow) {
	showDiff= !showDiff
	if (showDiff)
	  Text(newShowText)
	else
	  DiffRenderer.renderDiff(refText, newShowText)
      } else {
	showDiff= true
	DiffRenderer.renderDiff(refText, oldShowText, newShowText)
      }} ++
      Text(" ") ++ editButton)
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

  def contentArea() = 
    <div id={"content"+id} >{ content(show, show) }</div>

  def branchArea() : NodeSeq = 
    <div id={"branches"+id}>{ branches() }</div>

  def branches() : NodeSeq = {
    val trees = if (ref.isEmpty) {
      TagRef.findAll(By(TagRef.section, sec), NullRef(TagRef.tag))
      .map { _.content.obj.get }
      .filter { !_.parent.defined_? }
      .map { content => new TagTree(Some(content), show) }
      .sort { _.refs.size > _.refs.size }
    } else
      List(new TagTree(ref, show))
    toHtml(trees, trees.map { _.refs.size }.foldLeft(0) { _ + _ }
)
  }

  def toHtml(trees : List[TagTree], total : Int) : NodeSeq = 
    if (trees.isEmpty) NodeSeq.Empty else
      <ul> {
	val h= trees.map { tree => <li> { toHtml(tree, total) } </li> }
	val n= if (trees.exists(_.containsCurrent))
	  trees.takeWhile(!_.containsCurrent).size + 2 else 0
	new Uncover(h, 5).next("branchvar"+random.nextInt, 5 max n) 
      } </ul>

  def toHtml(tree : TagTree, n : Int) : NodeSeq = {
    val src= if (tree.isCurrent) "active" else {
      val user= PseudoLogin.userName
      if (tree.refs.exists { _.name.is==user } &&
	  (!tree.isShown || 
	   !tree.children.exists{ _.refs.exists { _.name.is==user}}))
	"favored"
      else
	"inactive"
    }
    val img = <img src={"/images/"+src+".png"}/>
    val icon = SHtml.a(() => { redraw(show, tree.cur)}, img) 
    
    val subtree= 
      if (tree.isShown) {
	val tags = tree.refs -- tree.children.flatMap( _.refs )
	if (!tree.isCurrent && tags.isEmpty && tree.children.size<=1)
	  <div class="compact"> {
	    toHtml(tree.children, n)
	  } </div>
	else
	  tagList(tags, n) ++ toHtml(tree.children, n)
      } else {
        tagList(tree.refs, n)
      }
    icon ++ subtree
  }

  /** Show a list of named tags */
  def tagList(tags : List[Tag], total : Int) : NodeSeq =
    if (tags.size==0) NodeSeq.Empty else {
      Text(" %2.0f%% ".format(100.0* tags.size/total)) ++
      new Uncover(tags.map { doc.tagLink(_) }, 3)
      .next(id+"li"+random.nextInt, 0)
    }

  /** Save tag references to persist current selection */
  def makeTag(tag : Tag, pre : Option[Section] = None) {
    if (show.exists{ _.dirty_?})
      save()
    var pre2= pre
    if (!show.isEmpty) {
      TagRef.create.tag(tag).content(show).section(sec).save
      pre2= Some(sec)
      if (!pre.isEmpty) {
	if (Link.find(By(Link.pre, pre.get), By(Link.post, sec)).isEmpty) {
	  Link.create.pre(pre.get).post(sec).save
	}
      }
    }
    next.foreach { _.get.makeTag(tag, pre2) }
  }

  def isEmpty() = ref.isEmpty && show.isEmpty
}
