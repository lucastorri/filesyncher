package co.torri.filesyncher

import java.io.{File => JFile}
import java.security.MessageDigest

object SyncFile {

  type FileReader = { def read(f: JFile): Array[Byte] }

  def apply(file: JFile, syncpath: String, reader: FileReader = DefaultFileReader) = {
    val sp = if (syncpath.endsWith(JFile.separator)) syncpath.slice(0, syncpath.size - JFile.separator.size)
             else syncpath
    new LocalSyncFile(file, sp, reader)
  }

  def apply(syncpath: String, relativePath: String, md5: String, children: List[(String, String)]) = {
    val childrenFiles = children.map{ case (path, md5) => new RemoteSyncFile(md5, syncpath, None, path) }.toArray
    new RemoteSyncFile(md5, syncpath, Some(childrenFiles.asInstanceOf[Array[SyncFile]]), relativePath)
  }

}

sealed trait SyncFile {
  def md5sum: String
  def syncpath: String
  def children: Option[Array[SyncFile]]
  def relativePath: String

  override def equals(o: Any) = o.isInstanceOf[SyncFile] && o.asInstanceOf[SyncFile].relativePath == relativePath
  override def toString = relativePath
}

private[filesyncher] class RemoteSyncFile(
  val md5sum: String,
  val syncpath: String,
  val children: Option[Array[SyncFile]],
  val relativePath: String
) extends SyncFile

private[filesyncher] class LocalSyncFile(
  file: JFile,
  val syncpath: String,
  contentReader: SyncFile.FileReader
) extends SyncFile {

  private val FilePathMatcher = ( "^file:(" + syncpath + "(.*))$" ).r
  val FilePathMatcher(absolutePath, relativePath) = file.toURI.toString

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

