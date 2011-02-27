package co.torri.filesyncher

object debug {
    var on = true
    def apply(str: String) = if (on) {
        print("[DEBUG] ")
        println(str)
    }
}