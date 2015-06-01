package controllers

import java.io.FileInputStream

import org.pegdown.PegDownProcessor
import play.api._
import play.api.mvc._
import java.nio.file.Files
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.twirl.api.HtmlFormat

import scala.collection.JavaConversions._
import play.api.Play.current
import scala.io.Source


class Application extends Controller {

  implicit val pegdown = new PegDownProcessor()

  def index = Action {


    val path = Play.application.path.toPath.resolve("dist/repo/index")
    val pattern = "(.*)\\.md".r
    val sections = Files.newDirectoryStream(path, "*.md") map (_.toFile) map {
      section => {
        val key = section.getName match {
          case pattern(n) => n.toString
        }
        val parsed = pegdown.markdownToHtml(Source.fromFile(section).mkString)

        key -> parsed
      }
    }

    val manifest = Json.parse(new FileInputStream(path.resolve("manifest.json").toFile))
    val templateOpt: Option[Template] = (manifest \ "template").as[JsString] match {
      case index if index.equals(JsString("index")) => manifest.validate[IndexTemplate].asOpt
      case _ => None
    }

    templateOpt match {
      case Some(template) => Ok(template.getContent(sections.toMap))
      case None => InternalServerError("Invalid template")
    }

  }

}

abstract class Template {

  def getContent(sections: Map[String, String]): HtmlFormat.Appendable
}

case class Panel(title: String, image: String, link: String)

object Panel {
  implicit val reader: Reads[Panel] = (
    (JsPath \ "title").read[String] and
      (JsPath \ "image").read[String] and
      (JsPath \ "link").read[String]
    )(Panel.apply _)
}

case class IndexTemplate(title: String, content: String, panels: Seq[Panel], panelsPerLine: Option[Map[String, Int]]) extends Template {

  lazy val panelsPerLineWithDefaults = {
    val ensured = panelsPerLine.getOrElse(Map.empty[String, Int])
    ensured.updated("small", ensured.getOrElse("small", Math.max(Math.min(panels.size, 12), 1)))
  }

  lazy val blockClass =
    (panelsPerLineWithDefaults map {
      case (size: String, blocks: Int) => s"$size-block-grid-$blocks"
    }).mkString(" ")


  override def getContent(sections: Map[String, String]) = {
    views.html.index(this, sections.withDefaultValue(""))
  }
}

object IndexTemplate {
  implicit val reader: Reads[IndexTemplate] = (
    (JsPath \ "title").read[String] and
    (JsPath \ "content").read[String] and
      (JsPath \ "panels").read[Seq[Panel]] and
      (JsPath \ "panelsPerLine").readNullable[Map[String, Int]]
    )(IndexTemplate.apply _)
}


