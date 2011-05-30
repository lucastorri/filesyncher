package co.torri.filesyncher

import org.mockito.Mockito._

import java.io.{File => JFile}
import java.net.URI

class SyncFileTest extends FileSyncherSpec {

  behavior of "A local sync file"

  it should "know its relative path" in {
    syncfile().relativePath should be (FILE)
  }

  it should "know its absolute path" in {
    syncfile().absolutePath should be (FILE_PATH)
  }

  it should "know its md5 hash" in {
    syncfile().md5sum should be ("4fbf4f48765a11d3b0c8e5142d10e138")
  }

  it should "know its children if the file is a directory" in {
    val testFolder = BASE_FOLDER + "/test"
    val dir = mock[JFile]
    val children = Array(new JFile(testFolder + "/a"), new JFile(testFolder + "/b/"))

    when(dir.toURI) thenReturn(new URI("file:" + testFolder))
    when(dir.isDirectory) thenReturn(true)
    when(dir.listFiles) thenReturn(children)

    syncfile(dir).children.get.sameElements(children.filter(_.isFile).map(c => syncfile(c))) should be (true)
  }

  it should "return None for children when its not a directory" in {
    val file = mock[JFile]
    when(file.toURI) thenReturn(new URI("file:" + FILE_PATH))
    when(file.isDirectory) thenReturn(false)

    syncfile(file).children should be (None)
  }

  def syncfile(jfile: JFile = new JFile(FILE_PATH)) = {
    object FileReaderMock {
      def read(f: JFile) = "=content=".getBytes
    }
    SyncFile(file = jfile, syncpath = BASE_FOLDER, reader = FileReaderMock)
  }

  val BASE_FOLDER = "/path/to"
  val FILE = "/some/file"
  val FILE_PATH = BASE_FOLDER + FILE

}

