package a

import java.io.File
import FileUtils.filehash
import a.FileStatus._
import a.RelativePathFile._

class RelativePathFile(val pathPrefix: String, val relativePath: String, override val hashCode: Int, val timestamp: Long) {
    
    def this(f: File, prefix: String) = this(generatePathPrefix(prefix), 
                                                f.getAbsolutePath.replace(generatePathPrefix(prefix), ""),
                                                filehash(f),
                                                f.lastModified)
    
    override def equals(o: Any) = o.isInstanceOf[RelativePathFile] &&
                                    relativePath.equals(o.asInstanceOf[RelativePathFile].relativePath)
                                    
    lazy val fullPath = pathPrefix + relativePath
    
    def toPair = relativePath -> this
    
    override def toString = (relativePath, hashCode, timestamp).toString

}
object RelativePathFile {
    
    def generatePathPrefix(path: String) = if (path.endsWith(File.separator)) path.dropRight(1) else path
    
    def apply(files: List[File], pathPrefix: String) = files.map(new RelativePathFile(_, pathPrefix))
    
    private val RelativePathFileRE = """\((.*),\s*(.*),\s*(.*)\)""".r
    
    def apply(str: String, basepath: String = "") = str.split("\n").filterNot(_.trim.isEmpty).map { l =>
        l match {
            case RelativePathFileRE(relativePath, hashCode, timestamp) => new RelativePathFile(basepath, relativePath, hashCode.toInt, timestamp.toLong)
        }
    }.toList
    
    def mapAll(files: List[RelativePathFile]): Map[String, RelativePathFile] = files.foldLeft(Map[String,RelativePathFile]()) { (m,rf) => m + rf.toPair }

    def mapSame(reference: List[RelativePathFile], target: List[RelativePathFile]) = {
        var map = Map[RelativePathFile,RelativePathFile]()
        var removedFiles = target.filterNot(reference.contains)
        removedFiles.foreach(f => map += (null.asInstanceOf[RelativePathFile] -> f))
        var addedFiles = reference.filterNot(target.contains)
        addedFiles.foreach(f => map += (f -> null.asInstanceOf[RelativePathFile]))
        
        reference.filterNot(addedFiles.contains).foreach { ref =>
            map += (ref -> target.find(ref == _).get)
        }
        map
    }
    
    def diff(f: (RelativePathFile,RelativePathFile) => FileStatus.Value, reference: List[RelativePathFile], target: List[RelativePathFile]) = {
        mapSame(reference,target).map { files =>
            files match {
                case (null, _) => (DELETED, files._1, files._2)
                case (_, null) => (ADDED, files._1, files._2)
                case (f1, f2) => (f(f1,f2), f1, f2)
            }
        }.toList
    }

    def timediff(reference: List[RelativePathFile], target: List[RelativePathFile]) = {
        diff((r1,r2) => if (r1.timestamp != r2.timestamp) MODIFIED else SAME, reference, target)
    }
    
    def contentdiff(reference: List[RelativePathFile], target: List[RelativePathFile]) = {
        diff((r1,r2) => if (r1.hashCode != r2.hashCode) MODIFIED else SAME, reference, target)
    }
    
    def filterNewAndModified(diffs: List[(FileStatus.Value, RelativePathFile, RelativePathFile)]) = {
        diffs.filter(f => f._1 == ADDED || f._1 == MODIFIED).map(t => (t._2, t._3))
    }
    
    def filterDeleted(diffs: List[(FileStatus.Value, RelativePathFile, RelativePathFile)]) = {
        diffs.filter(f => f._1 == DELETED).map(t => (t._2, t._3))
    }
    
    def manifest(diffs: List[(FileStatus.Value, RelativePathFile)]) = diffs.mkString("\n")

}