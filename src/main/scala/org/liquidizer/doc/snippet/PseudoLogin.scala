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

  object user extends SessionVar[Option[String]](None)

  def render(node : NodeSeq) : NodeSeq = 
    <div id="pseudoLogin">{ render() }</div>

  def render() : NodeSeq = {
    if (user.is.isEmpty) {
      SHtml.text("", name => {
	user.set(if (name.trim.isEmpty) None else Some(name))
      }) ++
      SHtml.ajaxSubmit("Login", () => SetHtml("pseudoLogin", render()))
    } else {
      Text("Logged in as "+user.is.get+" ") ++
      SHtml.ajaxSubmit("Logout", () => {
	user.set(None)
	SetHtml("pseudoLogin", render) })
    }
  }

}
