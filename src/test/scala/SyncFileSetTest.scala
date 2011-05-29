package co.torri.filesyncher

import org.scalatest.{FeatureSpec, BeforeAndAfterEach}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._

import java.io.{File => JFile}

class SyncFileSetTest extends FileSyncherSpec {

  behavior of "A Sync file set"

  it should "initially include all the files of a given folder" in {
    SyncFileSet(file).fileSet should be (set.toList.toSet)
  }

  it should "generate a list with all children relative path and md5sum" in {

    SyncFileSet(file).toString should be (
    """
    |123 /path/a.txt
    |456 /path/b.txt
    |789 /path/to/b.txt
    """.stripMargin.trim
    )
  }

  it should "allow the exclusion of files based on a regex" in {
  
  }

  private var file: SyncFile = _
  private var set: Array[SyncFile] = _

  override def beforeEach {
    file = mock[SyncFile]
    set = Array(
      new RemoteSyncFile("123", "/some", None, "/path/a.txt"),
      new RemoteSyncFile("456", "/some", None, "/path/b.txt"),
      new RemoteSyncFile("789", "/some", None, "/path/to/c.txt")
    )
    when(file.children) thenReturn(Some(set))
  }
}
