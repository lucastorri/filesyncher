import co.torri.filesyncher._
import co.torri.filesyncher.FileUtils._
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
        var tcpport = confs.get("tcp.port").toString.trim.toInt
        var serverbasepath = confs.get("server.basepath").toString.trim
        var clientbasepath = confs.get("client.basepath").toString.trim
        var defaultflow = confs.get("default.flow").toString.trim
        var defaultmonitor = confs.get("default.monitor").toString.trim
        var includeonly = confs.get("include.only").toString.trim
        var exclude = confs.get("exclude").toString.trim
        
        var actor = args.toList match {
            case ("client" :: Nil) => {
                var filter = getFileFilter(includeonly, exclude)
                var client = new SyncClient(clientbasepath, serverip, tcpport, decodeSendToServer(defaultflow), filter, defaultmonitor)
                debug("Server: " + serverip + ":" + tcpport)
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
                            case "status" => {
                                println("flow: " + encodeSendToServer(client.sendToServer))
                                println("debug: " + (if (debug.on) "on" else "off"))
                            }
                            case "debug" => debug.on = true; println("Debug on")
                            case "!debug" => debug.on = false; println("Debug off")
                            case "" =>
                            case null => System.exit(0)
                            case _ => println("Unknown command: " + line)
                        }
                    }
                }
                client
            }
            case _ => new SyncServer(serverbasepath, tcpport, defaultmonitor)
        }
        actor.start
        debug("=> starting " + actor)
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

