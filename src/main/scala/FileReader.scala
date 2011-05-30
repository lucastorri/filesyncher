package co.torri.filesyncher

import java.io.{File => JFile, FileInputStream, ByteArrayOutputStream}
import co.torri.filesyncher.StreamCopier.streamcopy

object DefaultFileReader {

  def read(f: JFile) = {
    val out = new ByteArrayOutputStream
    streamcopy(new FileInputStream(f), out)
    out.toByteArray
  }
}

