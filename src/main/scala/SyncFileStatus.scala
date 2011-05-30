package co.torri.filesyncher

object SyncFileStatus extends Enumeration {
  val DELETED = Value('D')
  val ADDED = Value('A')
  val MODIFIED = Value('M')
  val SAME = Value('S')
}

