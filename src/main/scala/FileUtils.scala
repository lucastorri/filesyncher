package co.torri.filesyncher

import scala.util.matching.Regex
import scala.io._
import scala.tools.nsc.io.File
import java.io.{File => JFile, FileFilter}
import java.util.zip.{ZipOutputStream, ZipInputStream, ZipEntry}
import java.io.{InputStream, OutputStream, FileInputStream, FileOutputStream, ByteArrayOutputStream, ByteArrayInputStream}
import java.security.MessageDigest
import co.torri.filesyncher.FileUtils._
import co.torri.filesyncher.{Log => log}
import co.torri.filesyncher.LogLevel._
import resource._
import scala.annotation.tailrec


object FileStatus extends Enumeration {
   val DELETED = Value('D')
   val ADDED = Value('A')
   val MODIFIED = Value('M')
   val SAME = Value('S')
}


object FileUtils {
    
    implicit def string2File(str: String) = new JFile(str)
    
    def recursiveListTree(f: JFile): Array[JFile] = {
        val these = f.listFiles
        these ++ these.filter(_.isDirectory).flatMap(recursiveListTree)
    }
    
    def recursiveListFiles(path: String, filter: FileFilter = AcceptAllFileFilter): List[JFile] = recursiveListTree(path).filter(f => f.isFile && filter.accept(f)).toList
    
    def filehash(f: JFile) = MessageDigest.getInstance("MD5").digest(content(f)).map(_.asInstanceOf[Int]).sum
    
    def content(f: JFile) = try { new File(f).bytes.toArray } catch { case _ => Array[Byte]() }
    
    def delete(files: List[JFile]): Unit = files.foreach{ f =>
        f.delete
        log(FILEOP, "delete: " + f.getAbsolutePath)
    }
    
    def zip(basepath: String, files: List[JFile]): Array[Byte] = {
        if (files.size == 0) return Array[Byte]()
        
        var buf = Array.ofDim[Byte](1024)
        var byteout = new ByteArrayOutputStream
        managed(new ZipOutputStream(byteout)).map { out =>
            files.foreach { f =>
                log(FILEOP, "zip: " + f.getAbsolutePath)
                managed(new FileInputStream(f)).map { in =>
                    out.putNextEntry(new ZipEntry(f.toString.replace(basepath, "")))
                    streamcopy(in, out, buf)
                    out.closeEntry()
                }
            } 
        }
        byteout.toByteArray
    }
    
    def unzip(dest: JFile, zip: Array[Byte]) {
        require(dest.isDirectory)
        var buf = Array.ofDim[Byte](1024)
        var zipinputstream = new ZipInputStream(new ByteArrayInputStream(zip))
        
        var entry = zipinputstream.getNextEntry
        while (entry != null) {
            var f = new JFile(dest.getAbsolutePath + JFile.separator + fixpath(entry.getName))
            f.getParentFile.mkdirs
            if (!f.exists) f.createNewFile
            log(FILEOP, "unzip: " + f.getAbsolutePath)
            var fout = new FileOutputStream(f)
            streamcopy(zipinputstream, fout, buf)
            fout.close
            zipinputstream.closeEntry
            
            entry = zipinputstream.getNextEntry
        }
        zipinputstream.close
    }
    
    def fixpath(path: String) = path.replace("/", JFile.separator).replace("""\""", JFile.separator)
    
    def streamcopy(in: InputStream, out: OutputStream, buf: Array[Byte] = Array.ofDim[Byte](1024)) {
        @tailrec def read(len: Int): Unit = if (len > 0) {
            out.write(buf, 0, len)
            read(in.read(buf))
        }
        read(in.read(buf))
    }
    
    def getFileFilter(exclude: String) = exclude match {
        case null | "" => AcceptAllFileFilter
        case _ => new ExcludeFileFilter(exclude)
    }
}


class FilesWatcher(path: String, filter: FileFilter, poltime: Long = 5000) {
    
    var filestimestamp = getLastFileList
    
    def waitchange {
        var noChanges = true
        while({noChanges = noneAddedOrRemoved(getLastFileList); noChanges}) {
            Thread.sleep(poltime)
        }
    }
    
    private def noneAddedOrRemoved(newFiles: Map[JFile, Long]) = {
        filestimestamp.keys == newFiles.keys &&
        filestimestamp.filter(p => p._2 != newFiles(p._1)).isEmpty
    }
    
    private def getLastFileList = recursiveListFiles(path, filter).map(f => (f, f.lastModified)).toMap
}

class ExcludeFileFilter(exclude: String) extends FileFilter {
    private val windowsFileRegex = "^\\w:".r
    def accept(f: JFile) = !exclude.split(";").map(r => toUnixPath(f.getAbsolutePath).matches(toRegex(r))).reduceLeft(_||_)
    private def toRegex(str: String) = str.replace(".", "\\.").replace("*", ".*")
    private def toUnixPath(path: String) = windowsFileRegex.replaceAllIn(path, "").replace("\\", "/")
    override def toString = exclude
}

object AcceptAllFileFilter extends FileFilter {
    def accept(f: JFile) = true
    override def toString = "(,)"
}