import co.torri.filesyncher._
import co.torri.filesyncher.FileUtils._
import scala.concurrent.ops._
import java.io._
import java.util.Properties
import scala.PartialFunction
import co.torri.filesyncher.log
import co.torri.filesyncher.LogLevel._

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
        var exclude = confs.get("exclude").toString.trim
        
        args.toList match {
            case ("client" :: Nil) => {
                var filter = getFileFilter(exclude)
                var client = new SyncClient(clientbasepath, serverip, tcpport, decodeSendToServer(defaultflow), filter, defaultmonitor)
                var notStarted = true
                log(INFO, "Server to connect: " + serverip + ":" + tcpport)
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
                                println("Flow: " + encodeSendToServer(client.sendToServer))
                                println("Log: " + (if (log.on) "on" else "off"))
                                println("Log level: " + log.level)
                            }
                            case "log" => log.on = true; println("Log on")
                            case "!log" => log.on = false; println("Log off")
                            case "info" => log.level = INFO
                            case "fileop" => log.level = FILEOP
                            case "debug" => log.level = DEBUG
                            case "" =>
                            case "start" => {
                                if (notStarted) {
                                    notStarted = true
                                    client.start
                                    Thread.sleep(1500)
                                }
                            }
                            case null => System.exit(0)
                            case _ => println("Unknown command: " + line)
                        }
                    }
                }
                client
            }
            case _ => {
                new SyncServer(serverbasepath, tcpport, defaultmonitor).start
                log.level = FILEOP
                log(INFO, "Server started")
            }
        }
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

