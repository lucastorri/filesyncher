package co.torri.filesyncher

import scala.collection.GenSeq

import co.torri.filesyncher.SyncFileStatus._

import org.scalatest.{FeatureSpec, BeforeAndAfterEach}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._

import java.io.{File => JFile}

class SyncFileSetTest extends FileSyncherSpec {

  behavior of "A Sync file set"

  it should "initially include all the files of a given folder" in {
    syncset.fileSet.toList should be (set.toList)
  }

  it should "generate a list with all children relative path and md5sum" in {
    syncset.toString should be (
    """
    |123 /path/a.txt
    |456 /path/b.txt
    |789 /path/to/c.txt
    """.stripMargin.trim
    )
  }

  it should "allow the exclusion of files based on a regex" in {
    syncset.exclude("/path/to/.*").fileSet.sameElements(set.toList.filterNot(_ == syncfileC)) should be (true)
  }

  it should "know the differences when compared to another set" in {
    val otherFile = mock[SyncFile]
    val addedFile = new RemoteSyncFile("432", "/some", None, "/path/other/e.txt")
    val otherFileList: Array[SyncFile] = Array(
      new RemoteSyncFile("123", "/some", None, "/path/a.txt"),
      new RemoteSyncFile("765", "/some", None, "/path/b.txt"),
      addedFile
    )
    when(otherFile.children) thenReturn(Some(otherFileList))
    val otherSyncset = SyncFileSet(otherFile)

    val diff = syncset.diff(otherSyncset)
    diff(SAME).fileSet.toList should be (GenSeq(syncfileA))
    diff(ADDED).fileSet.toList should be (GenSeq(addedFile))
    diff(DELETED).fileSet.toList should be (GenSeq(syncfileC))
    diff(MODIFIED).fileSet.toList should be (GenSeq(syncfileB))
  }

  it should "add elements of a different set" in {
    val otherFile = mock[SyncFile]
    val otherFileList: Array[SyncFile] = Array(
      new RemoteSyncFile("321", "/some", None, "/path/a.txt")
    )
    when(otherFile.children) thenReturn(Some(otherFileList))
    val otherSyncset = SyncFileSet(otherFile)

    (syncset ++ otherSyncset).fileSet.toList should be ((set ++ otherFileList).toList)
  }

  private var syncset: SyncFileSet = _
  private var file: SyncFile = _
  private var set: Array[SyncFile] = _
  private var syncfileA: SyncFile = _
  private var syncfileB: SyncFile = _
  private var syncfileC: SyncFile = _

  override def beforeEach {
    file = mock[SyncFile]
    syncfileA = new RemoteSyncFile("123", "/some", None, "/path/a.txt")
    syncfileB = new RemoteSyncFile("456", "/some", None, "/path/b.txt")
    syncfileC = new RemoteSyncFile("789", "/some", None, "/path/to/c.txt")
    set = Array(syncfileA, syncfileB, syncfileC)
    when(file.children) thenReturn(Some(set))
    syncset = SyncFileSet(file)
  }

}
