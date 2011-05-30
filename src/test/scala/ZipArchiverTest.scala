package co.torri.filesyncher

import java.io.{File => JFile, FileOutputStream, ByteArrayOutputStream, ByteArrayInputStream}
import co.torri.filesyncher.ZipArchiver._

import org.mockito.Mockito._

class ZipArchiverTest extends FileSyncherSpec {

  behavior of "a zip compressor"

  it should "compress and decompress zip files" in {

    val testResourcesFolder = new JFile(this.getClass.getClassLoader.getResource("files").toURI).getParentFile
    val fileset = SyncFileSet(SyncFile(testResourcesFolder, testResourcesFolder.toString))

    var tmpFolder = new JFile(System.getProperty("java.io.tmpdir") + JFile.separator + System.currentTimeMillis)
    tmpFolder.mkdirs

    Some(new ByteArrayOutputStream().zip(fileset)).map(out => new ByteArrayInputStream(out.toByteArray).unzip(tmpFolder.toURI))

    filesListFor(testResourcesFolder) should be === (filesListFor(tmpFolder))
  }

  def filesListFor(dir: JFile) = filesIn(dir).map(_.toString.replace(dir.toString, "")).toList.toSet

  def filesIn(f: JFile): Array[JFile] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(filesIn)
  }

}
