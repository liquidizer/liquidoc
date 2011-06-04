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

class PseudoLogin {
  
  val uri= S.uri
  val root= S.param("root").getOrElse("1")

  def render(node : NodeSeq) : NodeSeq = 
    <div id="pseudoLogin">{ render() }</div>

  def render() : NodeSeq = {
    if (PseudoLogin.loggedIn) {
      val name = PseudoLogin.userName
      Text("Logged in as ")++ render(name)++ Text(" ") ++
      SHtml.ajaxSubmit("Logout", () => {
	PseudoLogin.user.set(None)
	SetHtml("pseudoLogin", render) })
    } else {
      SHtml.text("", name => {
	PseudoLogin.user.set(if (name.trim.isEmpty) None else Some(name))
      }) ++
      SHtml.ajaxSubmit("Login", () => SetHtml("pseudoLogin", render()))
    }
  }

  def render(name : String) = {
    val tag = Tag.find(By(Tag.isold,false), By(Tag.name, name))
    if (tag.isEmpty)
      Text(name)
    else {
      val tagId= tag.get.id.is.toString
      <a href={Helpers.appendParams(uri, Seq("root"-> root, 
				    "show" -> tagId))}>{name}</a>
    } 
  }
}

object PseudoLogin {
  object user extends SessionVar[Option[String]](None)

  def loggedIn() = !user.is.isEmpty
  def userName() = user.is.getOrElse("")
}
