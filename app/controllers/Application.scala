package controllers

import java.io.{File, FileInputStream}
import java.util.regex.Pattern

import org.pegdown.PegDownProcessor
import play.api._
import play.api.data.validation.ValidationError
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

  lazy val rootNavigationNode = NavigationNode.fromFile(repoRoot)()

  def index(path: String) = Action {
    val basePath = repoRoot.resolve(path)

    val content = for {
      dir <- tryDirs(basePath, List("", "index"))
      template <- loadTemplate(dir)
    } yield template.getHtmlContent(loadSections(dir), "/" + path, rootNavigationNode)

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
  def getHtmlContent(sections: Map[String, String], currentPath: String, menuRootOpt: Option[NavigationNode]): HtmlFormat.Appendable
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


  override def getHtmlContent(sections: Map[String, String], currentPath: String, menuRootOpt: Option[NavigationNode]) = {
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

case class StandardTemplate(title: String, content: String, breadcrumbsOpt: Option[Seq[Breadcrumb]]) extends Template {
  override def getHtmlContent(sections: Map[String, String], currentPath: String, menuRootOpt: Option[NavigationNode]) = {
    views.html.standard(this, sections.withDefaultValue(""), currentPath, menuRootOpt)
  }
}

object StandardTemplate {

  implicit val reader: Reads[StandardTemplate] = (
    (JsPath \ "title").read[String] and
      (JsPath \ "content").read[String] and
      (JsPath \ "breadcrumbs").readNullable[Seq[Breadcrumb]]
    )(StandardTemplate.apply _)
}

case class Breadcrumb(title: String, url: String, current: Option[Boolean])
{
  val currentAttr = if(current.getOrElse(false)) "class=\"current\"" else ""
}

object Breadcrumb {
  implicit lazy val reader: Reads[Breadcrumb] = (
    __(0).read[String] and
    __(1).read[String] and
    __(2).readNullable[Boolean]
    )(Breadcrumb.apply _)
}

case class NavigationNode(path: Option[String], title: Option[String], children: List[NavigationNode])

object NavigationNode {
  def fromFile(root: Path)(file: File = root.toFile): Option[NavigationNode] = {
    if (!file.isDirectory)
      return None

    val children = file.listFiles().toList.filter(f => f.isDirectory && f.getName != "index") flatMap { fromFile(root)(_) }
    val manifestPath = file.toPath.resolve("manifest.json")
    val indexPath = file.toPath.resolve("index")

    if(Files.exists(manifestPath)) {
      val sep = System.getProperty("file.separator")
      val pattern = s"^${Pattern.quote(root.toFile.getAbsolutePath)}(.*?)(${Pattern.quote(sep)}index)?${Pattern.quote(sep)}manifest.json$$".r
      val path = manifestPath.toFile.getAbsolutePath match {
        case pattern(f,_) => Some(if (f == "") "/" else f.replace(sep, "/"))
        case _ => None
      }

      val manifest = Json.parse(new FileInputStream(manifestPath.toFile))
      val title = (manifest \ "title").asOpt[JsString] match {
        case Some(JsString(str)) => Some(str)
        case None => None
      }

      Some(NavigationNode(path, title, children))
    }
    else if (Files.exists(indexPath)) {
      fromFile(root)(indexPath.toFile) match {
        case Some(NavigationNode(index, title, _)) => Some(NavigationNode(index, title, children))
        case None => None
      }
    }
    else children match {
      case Nil => None
      case xs => Some(NavigationNode(None, None, children))
    }
  }
}