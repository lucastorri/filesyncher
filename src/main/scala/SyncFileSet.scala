package co.torri.filesyncher

import scala.collection.GenSeq

object SyncFileSet {

    def apply(baseDir: SyncFile) = new SyncFileSet(baseDir.children.getOrElse(Array[SyncFile]()).toList.par)
}

class SyncFileSet private[SyncFileSet](val fileSet: GenSeq[SyncFile]) {

  def exclude(regex: String) = n(fileSet.filterNot(_.relativePath.matches(regex)))

  def diff(other: SyncFileSet): Map[SyncFileStatus.Value, SyncFileSet] = {
    val added = other -- this
    val deleted = this -- other
    val modified = this / other
    val same = n(fileSet.filterNot(e => added.fileSet.exists(e ==) || deleted.fileSet.exists(e ==) || modified.fileSet.exists(e ==)))
    import SyncFileStatus._
    Map(
      SAME     -> same,
      ADDED    -> added,
      DELETED  -> deleted,
      MODIFIED -> modified
    )
  }

  def --(other: SyncFileSet) = n(fileSet.filterNot(e => other.fileSet.exists(e ==)))

  def ++(other: SyncFileSet) = n(fileSet ++ other.fileSet)

  def /(other: SyncFileSet) = n(fileSet.filter(e => other.fileSet.find(_ == e).map(_.md5sum != e.md5sum).getOrElse(false)))

  private def n(set: GenSeq[SyncFile]) = new SyncFileSet(set)

  override def toString() = fileSet.map(f => f.md5sum + " " + f.relativePath).mkString("\n")
}
