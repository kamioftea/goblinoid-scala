package cms

import scala.io.Source
import scala.util.Try

/**
 *  Define the interface for getting data for a specific path in the CMS
 *
 *  Created by jeff on 22/06/2015.
 */
trait DataProvider {

  def getFile(path: String): Option[Source]

}
