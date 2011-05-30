package co.torri

import java.io.{File => JFile}

package object filesyncher {

  type FileReader = { def read(f: JFile): Array[Byte] }
}
