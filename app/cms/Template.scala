package cms

import play.twirl.api.HtmlFormat

/**
 *
 * Created by jeff on 22/06/2015.
 */
trait Template {
  def getHtmlContent(sections: Map[String, String]): HtmlFormat.Appendable

  def withManifest(manifest: Manifest): Template
}
