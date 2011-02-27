package co.torri.filesyncher

object debug {
    var on = false
    def apply(str: String) = if (on) println(str)
}