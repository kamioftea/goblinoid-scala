package cms

import java.io.FileInputStream
import java.nio.file.{Files, Path}

import play.api.libs.json.{JsString, Json}

import scala.util.{Failure, Success, Try}

/**
 *
 * Created by Jeff on 28/07/2015.
 */
class DirectoryManifest(basePath: Path, templates: Map[String, Template]) extends Manifest
{

  override def getTemplate(path: String): Option[Template] = {

    def tryDirs(path: Path, dirs: List[String]): Option[Path] = dirs match {
      case Nil => None
      case (x :: xs) =>
        val dir = basePath.resolve(x)
        if (Files.isDirectory(dir) && Files.exists(dir.resolve("manifest.json")))
          Some(dir)
        else
          tryDirs(path, xs)
    }

    def loadTemplate(path: Path): Option[Template] = {
      Try {
        val manifest = Json.parse(new FileInputStream(path.resolve("manifest.json").toFile))
        val template = (manifest \ "template").as[JsString].value
        templates.get(template)
      } match {
        case Success(templateOpt) => templateOpt
        case Failure(_) => None
      }
    }

    for {
      dir <- tryDirs(basePath, List("", "index"))
      template <- loadTemplate(dir)
    } yield template.withManifest(this)
  }
}
