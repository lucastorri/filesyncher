import a._

object M {
    
    def main(args: Array[String]) = {
        var actor = args.toList match {
            case ("client" :: serverip :: tail) => new SyncClient(/*"C:\\synctest-local"*/"/Users/lucastorri/Desktop/synctest-local/", serverip, !tail.isEmpty)
            case _ => new SyncServer("C:\\synctest-vm"/*"/Users/lucastorri/Desktop/synctest-vm/"*/)
        }
        actor.start
        println("=> starting " + actor)
    }
    
}

