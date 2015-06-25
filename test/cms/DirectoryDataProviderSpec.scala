package cms

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.test.WithApplication

import scala.io.Source

/**
 *
 * Created by jeff on 25/06/2015.
 */
@RunWith(classOf[JUnitRunner])
class DirectoryDataProviderSpec extends Specification {

  lazy val test_provider = DirectoryDataProvider("repo").get

  "DirectoryDataProviderTest" should {

    "accept a valid string path" in new WithApplication() {
      DirectoryDataProvider("repo") should beSuccessfulTry[DirectoryDataProvider]
    }

    "reject a valid string path" in new WithApplication() {
      DirectoryDataProvider("does-not-exist") should beFailedTry[DirectoryDataProvider].withThrowable[InvalidPathException]
    }

    "getFile should return the correct Source for a valid file" in new WithApplication() {
      test_provider.getFile("basic.file") flatMap (_.getLines().toSeq.headOption) should beSome("Hello World!")
    }

    "getFile should return None for missing file" in new WithApplication() {
      test_provider.getFile("does-not-exist") should beNone
    }

    "getFile should return None for directory" in new WithApplication() {
      test_provider.getFile("index") should beNone
    }

  }
}
