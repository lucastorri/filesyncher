package co.torri.filesyncher

import java.io.{File => JFile}
import java.security.MessageDigest

object DefaultFileReader {

  def read(f: JFile) = Array[Byte]()
}

object SyncFile {

  type FileReader = { def read(f: JFile): Array[Byte] }

  def apply(file: JFile, syncpath: String, reader: FileReader = DefaultFileReader) = {
    val sp = if (syncpath.endsWith(JFile.separator)) syncpath.slice(0, syncpath.size - JFile.separator.size)
             else syncpath
    new LocalSyncFile(file, sp, reader)
  }

  def apply(syncpath: String, md5: String, relativePath: String, children: List[(String, String)]) = None

}

sealed trait SyncFile {
  def md5sum: String
  def syncpath: String
  def children: Option[Array[SyncFile]]
  def relativePath: String

  override def equals(o: Any) = o.isInstanceOf[SyncFile] && o.asInstanceOf[SyncFile].relativePath == relativePath
  override def toString = relativePath
}

class RemoteSyncFile(val md5sum: String, val syncpath: String, val children: Option[Array[SyncFile]], val relativePath: String) extends SyncFile

class LocalSyncFile (file: JFile, val syncpath: String, contentReader: SyncFile.FileReader) extends SyncFile {

  val (relativePath, absolutePath) = {
    val FilePathMatcher = ( "^file:(" + syncpath + "(.*))$" ).r
    val FilePathMatcher(absolute, relative) = file.toURI.toString
    (relative, absolute)
  }

  def content = contentReader.read(file)

  def md5sum = BigInt(1,MessageDigest.getInstance("MD5").digest(content)).toString(16)

  def children = if (!file.isDirectory) None else {
    def recursiveListTree(f: JFile): Array[JFile] = {
        val these = f.listFiles
        these ++ these.filter(_.isDirectory).flatMap(recursiveListTree)
    }
    Some(recursiveListTree(file).map(SyncFile(_, syncpath, contentReader)))
  }

}
