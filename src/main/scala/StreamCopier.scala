package co.torri.filesyncher

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec

object StreamCopier {
  def streamcopy(in: InputStream, out: OutputStream, buf: Array[Byte] = Array.ofDim[Byte](1024)) {
    @tailrec def read(len: Int): Unit = if (len > 0) {
      out.write(buf, 0, len)
      read(in.read(buf))
    }
    read(in.read(buf))
  }
}

