import co.torri.filesyncher._
import scala.concurrent.ops._

object M {
    
    def main(args: Array[String]) = {
        var actor = args.toList match {
            case ("client" :: serverip :: tail) => {
                var client = new SyncClient(/*"C:\\synctest-local"*/"/Users/lucastorri/Desktop/synctest-local/", serverip, !tail.isEmpty)
                spawn {
                    Thread.sleep(1000)
                    loop {
                        print("> ")
                        readLine match {
                            case "<=" => client.sendToServer = false
                            case "=>" => client.sendToServer = true
                            case _ => println("Unknown command")
                        }
                    }
                }
                client
            }
            case _ => new SyncServer("C:\\synctest-vm"/*"/Users/lucastorri/Desktop/synctest-vm/"*/).start
        }
        actor.start
        println("=> starting " + actor)

    }
    
    def loop(f: => Unit) = while(true) f
}

