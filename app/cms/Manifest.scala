package cms

/**
 *
 * Created by jeff on 22/06/2015.
 */
trait Manifest {

  def getTemplate(path: String): Option[Template]

}
