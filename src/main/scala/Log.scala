package co.torri.filesyncher

object LogLevel extends Enumeration {
    val SEVERE = Value("[SEVERE]")
    val INFO   = Value("[INFO]")
    val FILEOP = Value("[FILE_OP]")
    val DEBUG  = Value("[DEBUG]")
}

object log {
    var on = true
    var level = LogLevel.INFO
    def apply(l: LogLevel.Value, str: String) = if (on && l.id <= level.id) {
        print(l.toString)
        print(" ")
        println(str)
    }
}