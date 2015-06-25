package cms

import java.nio.file.{Files, Path}

import play.api.Play
import play.api.Play.current

import scala.io.Source
import scala.util.Try

/**
 * Provide data from a directory
 *
 * Created by jeff on 25/06/2015.
 */
class DirectoryDataProvider(val root: Path) extends DataProvider {

  override def getFile(path: String): Option[Source] = Try(Source.fromFile(root.resolve(path).toFile)).toOption

}

object DirectoryDataProvider {

  def apply(root: Path): Try[DirectoryDataProvider] = Try {
    if(root.toFile.isDirectory) new DirectoryDataProvider(root)
    else throw new InvalidPathException(root)
  }

  def apply(root: String): Try[DirectoryDataProvider] = DirectoryDataProvider(Play.application.path.toPath.resolve(root))

}

case class InvalidPathException(path: Path) extends RuntimeException
{
  override def toString = "InvalidPathException(" + path.toRealPath() + ")"
}