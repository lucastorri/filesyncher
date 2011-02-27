import co.torri.filesyncher._
import scala.concurrent.ops._
import java.io._
import java.util.Properties
import scala.PartialFunction

object Sync {
    
    def main(args: Array[String]) = {
        var configFile = new File(System.getProperty("user.dir") + File.separator + "configs.properties")
        if (!configFile.exists) System.exit(1)
        
        var confs = new Properties
        confs.load(new FileInputStream(configFile))
        var serverip = confs.get("server.ip").toString.trim
        var serverbasepath = confs.get("server.basepath").toString.trim
        var clientbasepath = confs.get("client.basepath").toString.trim
        var defaultflow = confs.get("default.flow").toString.trim
        
        var actor = args.toList match {
            case ("client" :: Nil) => {
                var client = new SyncClient(clientbasepath, serverip, decodeSendToServer(defaultflow))
                spawn {
                    Thread.sleep(1000)
                    loop {
                        print("> ")
                        var line = readLine
                        line match {
                            case "<=" | "=>" => {
                                client.sendToServer = decodeSendToServer(line)
                                if (client.sendToServer) {
                                    println("Sending files to server.")
                                } else {
                                    println("Receiving files from server")
                                }
                            }
                            case "status" => println("flow: " + encodeSendToServer(client.sendToServer))
                            case "" =>
                            case null => System.exit(0)
                            case _ => println("Unknown command: " + line)
                        }
                    }
                }
                client
            }
            case _ => new SyncServer(serverbasepath)
        }
        actor.start
        println("=> starting " + actor)
    }
    
    val decodeSendToServer: PartialFunction[String, Boolean] = {
        case "=>" => true
        case _    => false
    }
    
    val encodeSendToServer: PartialFunction[Boolean, String] = {
        case true  => "=>"
        case false => "<="
    }
    
    def loop(f: => Unit) = while(true) f
}

