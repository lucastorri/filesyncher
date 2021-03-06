package co.torri.filesyncher

import java.io.{File => JFile, InputStream, FileInputStream, FileOutputStream, OutputStream}
import java.util.zip.{ZipOutputStream, ZipInputStream, ZipEntry}
import java.net.URI
import scalax.io._

import co.torri.filesyncher.StreamCopier.streamcopy

object ZipArchiver {

  def zip(zip: OutputStream, files: SyncFileSet) {

    var buf = Array.ofDim[Byte](1024)
    var zipStream = new ZipOutputStream(zip)
    files.fileSet.seq.map { f =>
      zipStream.putNextEntry(new ZipEntry(f.relativePath))
      val fin = new FileInputStream(f.absolutePath)
      streamcopy(fin, zipStream, buf)
      zipStream.closeEntry
      fin.close
    }
    zipStream.close
  }

  def unzip(zip: InputStream, outputFolder: URI) {

    var buf = Array.ofDim[Byte](1024)
    val zipStream = new ZipInputStream(zip)
    var entry = zipStream.getNextEntry
    while(entry != null) {
      val f = new JFile(outputFolder.getPath + JFile.separator + entry.getName)
      f.getParentFile.mkdirs
      if (!f.exists) f.createNewFile
      val fout = new FileOutputStream(f)
      streamcopy(zipStream, fout, buf)
      zipStream.closeEntry
      fout.close
      entry = zipStream.getNextEntry
    }
    zipStream.close
  }

  implicit def outStream2ZipArchiver[T <: OutputStream](out: T) = new {
    def zip(files: SyncFileSet): T = {
      ZipArchiver.zip(out.asInstanceOf[OutputStream], files)
      out
    }
  }

  implicit def inStream2ZipArchiver[T <: InputStream](in: T) = new {
    def unzip(outputFolder: URI): T = {
      ZipArchiver.unzip(in.asInstanceOf[InputStream], outputFolder)
      in
    }
  }

}

