package bootstrap.liftweb

import org.liquidizer.doc.model._
import org.liquidizer.doc.lib._
import net.liftweb.mapper._

object SampleData {
  var current : Option[Section]= None
  var head : Option[Section]= None

  def isHead(tag : Tag) = 
    Tag.find(By(Tag.parent, tag), By(Tag.name, tag.name.is)).isEmpty

  def update() {
    Tag.findAll.foreach { tag => if (!isHead(tag)) tag.isold(true).save }

    if (Tag.findAll.isEmpty) {
      fillDocument()
      makeTag()
    }
  }

  def paragraph(style : String, content : String) = {
    val node= Section.create
    node.save
    if (!current.isEmpty) {
      val link= Link.create.pre(current.get).post(node)
      link.save
    }
    val para= Content.create.style(style).text(content)
    para.save
    val tref= TagRef.create.content(para).section(node)
    tref.save
    if (head.isEmpty) head= Some(node)
    current= Some(node)
  }

  def makeTag() = {
    val doc= Document.create.name("Wahlprogramm").head(head.get)
    doc.save
    val tag= Tag.create.name("v1.0").doc(doc).time(TimeUtil.now)
    tag.save
    TagRef.findAll.foreach { _.tag(tag).save }
  }

  def fillDocument() {
    paragraph("h1",
	      "Freie demokratisch kontrollierte technische Infrastruktur")
    paragraph("p", """In unserer modernen Informations- und Kommunikationsgesellschaft ist es von außerordentlicher Wichtigkeit, dass alle Bürger jederzeit die volle Kontrolle über ihre Informationsverarbeitung und Kommunikation erlangen können, sofern sie dies wünschen. Diese Freiheit aller Bürgerinnen soll verhindern, dass die Macht über Systeme und Daten in den Händen Einzelner konzentriert wird. Sie versucht diese so breit wie möglich auf alle Bürger zu verteilen und so ihre Freiheit und Privatsphäre zu sichern.""")
   paragraph("h2","Offene Standards")
   paragraph("p","""Die freie und andauernde Verwendung von Daten jeder Art durch alle Nutzerinnen mit Systemen ihrer Wahl kann nur erfolgen, wenn diese Daten in einem Format vorliegen, das den Kriterien eines Offenen Standards entspricht. Ähnlich ist es bei der Zusammenarbeit verschiedener technischer Systeme. Diese sind nur dann bei gleicher Funktionalität austauschbar, wenn ihre Schnittstelle ein Offener Standard ist. Wir setzen uns deshalb für den konsequenten Einsatz und die Verbreitung von Offenen Standards ein. Denn so wird die Abhängigkeit von einzelnen Herstellern verringert und ein freier Wettbewerb technischer Lösungen möglich.""")
   paragraph("p","""Dabei verstehen wir einen Offenen Standard als ein Protokoll oder Format, das""")
   paragraph("ol","vollständig, öffentlich, ohne Einschränkungen für alle Beteiligten gleichermaßen zugänglich ist, bewertet und benutzt werden kann,")
   paragraph("ol","ohne Komponenten oder Erweiterungen ist, die von Formaten oder Protokollen abhängen, die selbst nicht dieser Definition entsprechen,")
   paragraph("ol","frei ist von juristischen oder technischen Klauseln, die seine Verwendung von jeglicher Seite oder jeglichem Geschäftsmodell einschränken,")
   paragraph("ol","unabhängig von einem einzelnen Hersteller geleitet und weiterentwickelt wird, in einem Prozess, der einer gleichberechtigten Teilnahme von Wettbewerbern und Dritten offen steht,")
   paragraph("ol","verfügbar ist in verschiedenen vollständigen Implementierungen von verschiedenen Herstellern oder als vollständig freie Implementierung.")

   paragraph("h2","Freie Software")
   paragraph("p","""Wir setzen uns für die Förderung von Software ein, die von allen uneingeschränkt benutzt, untersucht, verbreitet und verändert werden kann. Diese sogenannte Freie Software garantiert ihren Nutzerinnen alle wesentlichen Freiheiten, die notwendig sind, um die Kontrolle über ihre technischen Systeme selbst zu übernehmen und diese gegebenenfalls kollektiv und demokratisch weiter zu entwickeln. Dies leistet einen wesentlichen Beitrag zur Stärkung von Autonomie und Privatsphäre aller Nutzer. Insbesondere Bildungseinrichtungen und die gesamte öffentliche Verwaltung sollen schrittweise darauf hinarbeiten ihre gesamte technische Infrastruktur auf Freie Software umzustellen, um so langfristig Kosten für die öffentlichen Haushalte und die Abhängigkeit von einzelnen Herstellern zu reduzieren. """)
  }
}
