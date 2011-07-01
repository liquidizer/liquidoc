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
  var show= showTag.content(sec).or(ref)
  var lastEdit : Box[Content]= None
  var trees= refreshTrees()
  var showDiff= true

  /** find existing sections cutting two existing ones */
  def intersect(pre : Section, post : Option[Section]) : List[Section]= {
    var out= Link.findAll(By(Link.pre, pre)).map { _.post.is }
    if (!post.isEmpty) {
      var in= Link.findAll(By(Link.post, post.get)).map { _.pre.is }
      out= out intersect in
    }
    Section.findAll(ByList(Section.id, out))
  }

  /** create a new block for the insert command */
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
    val renderer= new SectionRenderer(doc, newSect)
    if (!renderer.trees.isEmpty)
      renderer.show= renderer.trees(0).cur
    renderer
  }

  def level(style : String) =
    if (style.startsWith("h")) style.substring(1).toInt else 10

  override def level() : Int = 
    show.map { c => level(c.style.is) }.getOrElse(10)

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
    if (PseudoLogin.loggedIn || ref.isEmpty)
      <img src="/images/delete.png" alt="delete" title="delete"/>
    else
      NodeSeq.Empty

  def editIcon() = 
    <img src="/images/edit.png" alt="edit" title="edit"/>

  def favorIcon() = {
    val mytag= doc.getMyTag()
    if (mytag.dirty_? ||
      TagRef.find(By(TagRef.tag,mytag),By(TagRef.section,sec),
		    By(TagRef.content, show)).isEmpty)
	<img src="/images/favor.png"/>
    else
      <img src="/images/favor_active.png"/>  
  }

  override def renderContent() : NodeSeq = {
    bind(super.renderContent)
  }

  def bind(node : NodeSeq) : NodeSeq = node.flatMap { bind(_) }

  def bind(node : Node) : NodeSeq = node match {
    case Elem("sec", tag, attrib, scope, children @ _*) =>
      tag match {
        case "control" => controlArea()
        case "content" => contentArea()
	case "branches" => branchArea()
      }

    case Elem(prefix, label, attribs, scope, children @ _*) =>
      Elem(prefix, label, attribs, scope, bind(children) : _*)

    case _ => node
  }

  def controlArea() : NodeSeq = 
    renderIf(PseudoLogin.loggedIn && nextNotHidden, {
      <div id={"control_"+id}>{
          SHtml.a(()=> toEditMode(), editIcon()) ++
          <br/> ++ 
	  super.renderDelete()
    }</div>})

  def favorButton() : NodeSeq = SHtml.a(() => favorShown(), favorIcon())

  def toEditMode() : JsCmd = {
    val curText= show.map {_.text.is}.getOrElse("")
    val curStyle= show.map {_.style.is}.getOrElse("p")
    show= Some(Content.create.parent(show.or(ref))
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
      SHtml.select(Content.styles, Full(show.get.style.is), 
		   text => show.get.style(text) )
    }</td><td align="right"> {
      Text(" ") ++ saveButton() ++
      Text(" ") ++ cancelButton()
    } </td></tr></table>
  }

  def saveButton()= SHtml.ajaxSubmit("save", () => save())
  def cancelButton()= SHtml.ajaxSubmit("cancel", () => cancel())

  def save() : JsCmd = {
    var showg= show.get
    val oldShow= showg.parent.obj
    val dirty= 
      showg.parent.map { _.text.is }.getOrElse("") != showg.text.is ||
      showg.parent.map { _.style.is }.getOrElse("p") != showg.style.is
    if (dirty) {
      if (showg.parent.defined_?) {
	// repetitive edits should not show up in history
	if (showg.parent.obj == lastEdit &&
	    Content.find(By(Content.parent, showg)).isEmpty) {
	  showg.parent.obj.get
	  .text(showg.text.is).style(showg.style.is)
	  showg= showg.parent.obj.get
	  show= Some(showg)
	}
      } else {
	// top level content 
	showg.save
	TagRef.create.section(sec).content(showg).save
      }
      showg.save
      lastEdit= Full(showg)
      favor(doc.getMyTag, false)
      trees= refreshTrees()
      redraw(oldShow, show)
    } else {
      cancel()
    }
  }

  def cancel() : JsCmd = {
    show= show.get.parent.obj
    redraw(ref, show)
  }

  def favorShown() : JsCmd = {
    if (show.exists{ _.dirty_?}) save()
    favor(doc.getMyTag(), true)
    showDiff= true
    SetHtml("branches"+id, branches())
  }

  def favor(mytag : Tag, toggle : Boolean= false) {
    mytag.time(TimeUtil.now).save
    val ref= TagRef.find(By(TagRef.tag, mytag), By(TagRef.section, sec))
    .getOrElse { 
      TagRef.create.tag(mytag).section(sec) 
    }
    if (show.isEmpty || (toggle && ref.content.obj==show))
      ref.delete_!
    else
      ref.content(show.get).save
  }

  /** Insert action is blocked if an empty edit section exits */
  override def insertBlock() : JsCmd = {
    if ((isEmpty || next.exists { _.get.isEmpty }) &&
      intersect(sec, next.map {_.get.sec}).isEmpty) Noop
    else super.insertBlock()
  }

  /** Delete block strokes reference text or makes block disappear */
  override def deleteBlock() : JsCmd = {
    if (ref.isEmpty && show.isEmpty) super.deleteBlock()
    else {
      trees= refreshTrees()
      if (show.isEmpty)
	redraw(show, ref)
      else
	redraw(show, None)
    }
  }

  def redraw(oldShow : Option[Content], newShow : Option[Content]) 
  : JsCmd = {
    show = newShow
    SetHtml("content"+id, content(oldShow, newShow)) &
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
      Text(" "))
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

  def refreshTrees() : List[TagTree] = {
    if (ref.isEmpty) {
      TagTree.getTrees(sec)
    } else
      List(new TagTree(ref))
  }

  def branches() : NodeSeq = {
    trees.foreach { _.refreshRefs }
    toHtml(trees, trees.map { _.refs.size }.foldLeft(0) { _ + _ })
  }

  def toHtml(trees : List[TagTree], total : Int) : NodeSeq = 
    renderIf (!trees.isEmpty,
      <ul> {
	val h= trees.map { tree => <li> { toHtml(tree, total) } </li> }
	val n= if (trees.exists(_.containsCurrent(show)))
	  trees.takeWhile(!_.containsCurrent(show)).size + 2 else 0
	new Uncover(h, 5).next("branchvar"+random.nextInt, 5 max n) 
      } </ul>)

  def toHtml(tree : TagTree, n : Int) : NodeSeq = {
    val src= if (tree.isCurrent(show)) "active" else {
      val user= PseudoLogin.userName
      if (tree.refs.exists { _.name.is==user } &&
	  (!tree.isShown(show) || 
	   !tree.children.exists{ _.refs.exists { _.name.is==user}}))
	"favored"
      else
	"inactive"
    }
    val img = <img src={"/images/"+src+".png"}/>
    val icon = SHtml.a(() => { redraw(show, tree.cur)}, img) 
    
    // formatting the sub trees recursively
    val subtree= 
      if (tree.isShown(show)) {
	val tags = tree.refs -- tree.children.flatMap( _.refs )
	if (!tree.isCurrent(show) && tags.isEmpty && tree.children.size<=1)
	  <div class="compact"> {
	    toHtml(tree.children, n)
	  } </div>
	else
	  tagList(tags, n) ++ toHtml(tree.children, n)
      } else {
        tagList(tree.refs, n)
      }

    // button to favor current view
    val favor= renderIf(PseudoLogin.loggedIn, {
      if (tree.isCurrent(show)) 
	favorButton() else <span style="margin: 0 20px 0 20px"/>
    })

    // cancat the answer
    icon ++ favor ++ subtree
  }

  /** Show a list of named tags */
  def tagList(tags : List[Tag], total : Int) : NodeSeq =
    renderIf (tags.size>0, {
      { renderIf(tags.size < total,
	Text(" %2.0f%% ".format(100.0* tags.size/total))) } ++
      new Uncover(tags.map { doc.tagLink(_) }, 3)
      .next(id+"li"+random.nextInt, 0)
    })

  /** Make a default collapse view */
  def defaultCollapse(current : Int, list : List[SectionRenderer]= Nil) {
    showLevel= current min level() 
    if (ref!=show) {
      showLevel= level()
      for (renderer <- list)
        renderer.showLevel= showLevel
    }
    next.foreach { 
      block =>
      block.get.defaultCollapse(showLevel, this :: 
        list.filter { _.level()< block.level() })
    }
  }

  def isEmpty() = ref.isEmpty && show.isEmpty
}
