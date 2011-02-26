import a._
import concurrent.ops._

object M {
    
    def main(args: Array[String]) = {
        val server = new SyncServer("/Users/lucastorri/Desktop/synctest-vm/")
        server.start

        spawn {
            val client = new SyncClient("/Users/lucastorri/Desktop/synctest-local/")
            client.start
        }
    }
}

