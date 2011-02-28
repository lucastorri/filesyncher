package co.torri.filesyncher

import scala.util.matching.Regex
import java.io.{File, FileFilter}
import java.util.zip.{ZipOutputStream, ZipInputStream, ZipEntry}
import java.io.{InputStream, OutputStream, FileInputStream, FileOutputStream, ByteArrayOutputStream, ByteArrayInputStream}
import java.security.MessageDigest
import co.torri.filesyncher.FileUtils._
import co.torri.filesyncher.log
import co.torri.filesyncher.LogLevel._


object FileStatus extends Enumeration {
   val DELETED = Value('D')
   val ADDED = Value('A')
   val MODIFIED = Value('M')
   val SAME = Value('S')
}


object FileUtils {
    
    implicit def string2File(str: String) = new File(str)
    
    def recursiveListTree(f: File): Array[File] = {
        val these = f.listFiles
        these ++ these.filter(_.isDirectory).flatMap(recursiveListTree)
    }
    
    def recursiveListFiles(path: String, filter: FileFilter = AcceptAllFileFilter): List[File] = recursiveListTree(path).filter(f => f.isFile && filter.accept(f)).toList
    
    def filehash(f: File) = {
        var digest = MessageDigest.getInstance("MD5")
        digest.digest(content(f)).map(_.asInstanceOf[Int]).sum
    }
    
    def content(f: File) = try {
        var fin = new FileInputStream(f)
        var bout = new ByteArrayOutputStream
        streamcopy(fin, bout)
        fin.close
        bout.toByteArray
    } catch { case _ => Array.ofDim[Byte](0) }
    
    def delete(files: List[File]): Unit = files.foreach{ f =>
        f.delete
        //log(FILEOP, "delete: " + f.getAbsolutePath)
    }
    
    def zip(basepath: String, files: List[File]): Array[Byte] = {
        if (files.size == 0) return Array[Byte]()
        var buf = Array.ofDim[Byte](1024)
        var byteout = new ByteArrayOutputStream
        var out = new ZipOutputStream(byteout)
        files.foreach { f =>
            //log(FILEOP, "zip: " + f.getAbsolutePath)
            var in = new FileInputStream(f)
            out.putNextEntry(new ZipEntry(f.toString.replace(basepath, "")))
            
            streamcopy(in, out, buf)
            
            out.closeEntry()
            in.close()
        }
        out.close
        byteout.toByteArray
    }
    
    def unzip(dest: File, zip: Array[Byte]) {
        require(dest.isDirectory)
        var buf = Array.ofDim[Byte](1024)
        var zipinputstream = new ZipInputStream(new ByteArrayInputStream(zip))
        
        var entry = zipinputstream.getNextEntry
        while (entry != null) {
            var f = new File(dest.getAbsolutePath + File.separator + fixpath(entry.getName))
            f.getParentFile.mkdirs
            if (!f.exists) f.createNewFile
            //log(FILEOP, "unzip: " + f.getAbsolutePath)
            var fout = new FileOutputStream(f)
            streamcopy(zipinputstream, fout, buf)
            fout.close
            zipinputstream.closeEntry
            
            entry = zipinputstream.getNextEntry
        }
        zipinputstream.close
    }
    
    def fixpath(path: String) = path.replace("/", File.separator).replace("""\""", File.separator)
    
    def streamcopy(in: InputStream, out: OutputStream, buf: Array[Byte] = Array.ofDim[Byte](1024)) {
        var len = 0
        do {
            len = in.read(buf)
            if (len > 0) out.write(buf, 0, len)
        } while (len > 0)
    }
    /*{
        Source.fromInputStream(in).copyToBuffer()
    }*/
    
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
    
    private def noneAddedOrRemoved(newFiles: Map[File, Long]) = {
        filestimestamp.keys == newFiles.keys &&
        filestimestamp.filter(p => p._2 != newFiles(p._1)).isEmpty
    }
    
    private def getLastFileList = recursiveListFiles(path, filter).map(f => (f, f.lastModified)).toMap
}

class ExcludeFileFilter(exclude: String) extends FileFilter {
    private val windowsFileRegex =  new Regex("^\\w:")
    def accept(f: File) = !exclude.split(";").map(r => toUnixPath(f.getAbsolutePath).matches(toRegex(r))).reduceLeft(_||_)
    private def toRegex(str: String) = str.replace(".", "\\.").replace("*", ".*")
    private def toUnixPath(path: String) = windowsFileRegex.replaceAllIn(path, "").replace("\\", "/")
    override def toString = exclude
}

object AcceptAllFileFilter extends FileFilter {
    def accept(f: File) = true
    override def toString = "(,)"
}