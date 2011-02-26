package a

import java.io.File
import java.util.zip.{ZipOutputStream, ZipInputStream, ZipEntry}
import java.io.{InputStream, OutputStream, FileInputStream, FileOutputStream, ByteArrayOutputStream, ByteArrayInputStream}
import java.security.MessageDigest


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
    
    def recursiveListFiles(path: String): List[File] = recursiveListTree(path).filter(_.isFile).toList
    
    def filehash(f: File) = {
        var digest = MessageDigest.getInstance("MD5")
        digest.digest(content(f)).map(_.asInstanceOf[Int]).sum
    }
    
    def content(f: File) = {
        var fin = new FileInputStream(f)
        var bout = new ByteArrayOutputStream
        streamcopy(fin, bout)
        fin.close
        bout.toByteArray
    }
    
    def delete(files: List[File]) {
        files.foreach(_.delete)
    }
    
    def zip(basepath: String, files: List[File]): Array[Byte] = {
        if (files.size == 0) return Array[Byte]()
        var buf = Array.ofDim[Byte](1024)
        var byteout = new ByteArrayOutputStream
        var out = new ZipOutputStream(byteout)
        files.foreach { f =>
            println("zip: " + f.getAbsolutePath)
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
            println("unzip: " + f.getAbsolutePath)
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
}