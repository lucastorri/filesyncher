import a._
import concurrent.ops._

object M {
    
    def main(args: Array[String]) = {
        var actor = args.toList match {
            case ("client" :: serverip :: tail) => new SyncClient("C:\\synctest-local"/*"/Users/lucastorri/Desktop/synctest-local/"*/, serverip, !tail.isEmpty)
            case _ => new SyncServer("C:\\synctest-vm"/*"/Users/lucastorri/Desktop/synctest-vm/"*/)
        }
        actor.start
        println(actor)
        /*
        val server = new SyncServer("/Users/lucastorri/Desktop/synctest-vm/")
        server.start

        val client = new SyncClient("/Users/lucastorri/Desktop/synctest-local/", "127.0.0.1", true)
        client.start
        */
    }
}

