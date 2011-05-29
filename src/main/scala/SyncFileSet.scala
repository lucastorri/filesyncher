package co.torri.filesyncher

object SyncFileSet {

    def apply(baseDir: SyncFile) = new SyncFileSet(baseDir.children.getOrElse(Array[SyncFile]()).toList.toSet)
}

class SyncFileSet private[SyncFileSet](file: Set[SyncFile]) {

  private var set = file

  def fileSet = set

  override def toString() = set.map(f => f.md5sum + " " + f.relativePath).mkString("\n")
}
