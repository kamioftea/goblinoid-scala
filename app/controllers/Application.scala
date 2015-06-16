package controllers

import java.io.FileInputStream

import org.pegdown.PegDownProcessor
import play.api._
import play.api.mvc._
import java.nio.file.{Path, Files}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.twirl.api.HtmlFormat

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import play.api.Play.current
import scala.io.Source


class Application extends Controller {

  implicit val pegdown = new PegDownProcessor()

  val repoRoot = Play.application.path.toPath.resolve(
    Play.application.configuration.getString("goblinoid.repo.root").getOrElse("repo")
  )

  def index(path: String) = Action {
    val basePath = repoRoot.resolve(path.toString)

    val content = for {
      dir <- tryDirs(basePath, List("", "index"))
      template <- loadTemplate(dir)
    } yield template.getContent(loadSections(dir))

    content match {
      case None => NotFound(views.html.notFound(path))
      case Some(html) => Ok(html)
    }
  }

  @tailrec
  final def tryDirs(basePath: Path, paths: List[String]): Option[Path] = paths match {
    case Nil => None
    case (x :: xs) =>
      val dir = basePath.resolve(x)
      if (Files.isDirectory(dir) && Files.exists(dir.resolve("manifest.json")))
        Some(dir)
      else
        tryDirs(basePath, xs)
  }

  def loadTemplate(path: Path): Option[Template] = {
    val manifest = Json.parse(new FileInputStream(path.resolve("manifest.json").toFile))

    (manifest \ "template").as[JsString] match {
      case index if index.equals(JsString("index")) => manifest.validate[IndexTemplate].asOpt
      case standard if standard.equals(JsString("standard")) => manifest.validate[StandardTemplate].asOpt
      case _ => None
    }
  }

  def loadSections(path: Path): Map[String, String] = {
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

    sections.toMap
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

case class StandardTemplate(title: String, content: String) extends Template {
  override def getContent(sections: Map[String, String]) = {
    views.html.standard(this, sections.withDefaultValue(""))
  }
}

object StandardTemplate {
  implicit val reader: Reads[StandardTemplate] = (
    (JsPath \ "title").read[String] and
      (JsPath \ "content").read[String]
    )(StandardTemplate.apply _)
}

