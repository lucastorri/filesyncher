package co.torri.filesyncher

import scala.actors._
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import java.io.{File, FileFilter}
import co.torri.filesyncher.FileUtils._
import co.torri.filesyncher.FileStatus._
import co.torri.filesyncher.BaseActs._
import co.torri.filesyncher.debug
import co.torri.filesyncher.RelativePathFile
import co.torri.filesyncher.RelativePathFile._
import java.io.{InputStream, OutputStream, FileInputStream, FileOutputStream, ByteArrayOutputStream, ByteArrayInputStream}
import java.util.Date


abstract class BaseActs(basepath: String) extends Actor {
    
    protected def sayhello(server: OutputChannel[Any], sendToServer: Boolean, filter: FileFilter) = {
        debug("=> calling server")
        server ! ("hello", sendToServer, filter.toString)
    }
    
    protected def waitclient: (OutputChannel[Any], Boolean, FileFilter) = {
        debug("=> waiting client")
        receive {
            case ("hello", sendToServer: Boolean, exclude: String) => return (sender, sendToServer, getFileFilter(exclude))
            case a: Any => debug("No match with client message: " + a); return (null, false, null)
        }
    }
    
    protected def upload = {
        debug("=> upload")
        receive {
            case "list" => {
                sender ! RelativePathFile(recursiveListFiles(basepath), basepath).mkString("\n")
                receive {
                    case ("giveme", files: String) => {
                        var filelist = RelativePathFile(files, basepath).map(rf => new File(rf.fullPath))
                        sender ! zip(basepath, filelist)
                    }
                }
            }
        }
    }
    
    protected def download(server: OutputChannel[Any]) {
        debug("=> download")
        server ! "list"
        receive {
            case f: String => {
                val serverFiles = RelativePathFile(f)
                val localfiles = RelativePathFile(recursiveListFiles(basepath), basepath)
                val filesdiff = contentdiff(serverFiles, localfiles)
                val newAndModified = filterNewAndModified(filesdiff).map(_._1).mkString("\n")
                val deleted = filterDeleted(filesdiff).map(d => new File(d._2.fullPath))
                delete(deleted)
                server ! ("giveme" -> newAndModified)
                receive {
                    case zipped: Array[Byte] => unzip(basepath, zipped)
                }
            }
        }
    }

}
object BaseActs {
    private val TimeMonitorRE = """(\d+)\s*(\w)""".r
    private val FileChangeMonitorRE = """filechange\s*\((\d+)\)""".r
    def parseMonitor(basepath: String, monitor: String, filter: FileFilter): () => Unit = monitor match {
        case TimeMonitorRE(time, "s") => {() => Thread.sleep(time.toInt * 1000) }
        case TimeMonitorRE(time, "m") => {() => Thread.sleep(time.toInt * 1000 * 60) }
        case TimeMonitorRE(time, "h") => {() => Thread.sleep(time.toInt * 1000 * 60 * 60) }
        case FileChangeMonitorRE(poltime) => var watcher = new FilesWatcher(basepath, filter, poltime.toInt); {() => watcher.waitchange }
    }
}

class SyncServer(basepath: String, port: Int, monitor: String) extends BaseActs(basepath) {

    def act {
        alive(port)
        register('filesync, self)
        
        loop {
            var (sender, sendToServer, filter) = waitclient
            
            if (sendToServer) {
                download(sender)
            } else {
                upload
                debug("Waiting")
                parseMonitor(basepath, monitor, filter)()
                debug("Finished waiting")
            }
        }

    }
}

class SyncClient private(basepath: String, server: AbstractActor, var sendToServer: Boolean, filter: FileFilter, waitfor: () => Unit) extends BaseActs(basepath) {
    
    def this(basepath: String, serverip: String, port: Int, sendToServer: Boolean, filter: FileFilter, monitor: String) = 
                            this(basepath, select(Node(serverip, port), 'filesync), sendToServer, filter, parseMonitor(basepath, monitor, filter))

    def act {
        
        loop {
            sayhello(server, sendToServer, filter)
            
            if (sendToServer) {
                upload
                debug("Waiting")
                waitfor
                debug("Finished waiting")
            } else {
                download(server)
            }
        }
    }
}

//import scala.collection.{ mutable, immutable, generic }
