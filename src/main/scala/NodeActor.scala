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
    
    protected def sayhello(server: OutputChannel[Any], sendToServer: Boolean, filter: FileFilter, monitor: String) = server ! ("hello", sendToServer, filter.toString, monitor)
    
    private val FilterRE = "\\((\\w*),(\\w*)\\)".r
    protected def waitclient(basepath: String): (OutputChannel[Any], Boolean, FileFilter) = receive {
        case ("hello", sendToServer: Boolean, FilterRE(includeOnly,exclude), monitor) => return (sender, sendToServer, getFileFilter(includeOnly, exclude), parseMonitor(basepath, monitor, getFileFilter(includeOnly, exclude)))
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
    private val TimeMonitorRE = """(\d*)\w*(\s)""".r
    def parseMonitor(basepath: String, token: String, watcher: FilesFilter): () => Unit = token match {
        case TimeMonitorRE(time, "s") => {() => Thread.sleep(time.toInt * 1000) }
        case TimeMonitorRE(time, "m") => {() => Thread.sleep(time.toInt * 1000 * 60) }
        case TimeMonitorRE(time, "h") => {() => Thread.sleep(time.toInt * 1000 * 60 * 60) }
        case "filechange" => var watcher = new FilesWatcher(basepath, filter); {() => watcher.waitchange }
    }
}

class SyncServer(basepath: String) extends BaseActs(basepath) {

    def act {
        alive(9011)
        register('filesync, self)
        
        loop {
            var (sender, sendToServer, filter) = waitclient(basepath)
            
            if (sendToServer) {
                download(sender)
            } else {
                upload
            }
        }

    }
}

class SyncClient private (basepath: String, server: AbstractActor, var sendToServer: Boolean, var filter: FileFilter, waitfor: () => Unit, monitor: String) extends BaseActs(basepath) {
    
    def this(basepath: String, serverip: String, sendToServer: Boolean, filter: FileFilter, monitor: String) = 
                            this(basepath, select(Node(serverip, 9011), 'filesync), sendToServer, filter, parseMonitor(monitor, filter), monitor)

    def act {
        
        loop {
            sayhello(server, sendToServer, filter, monitor)
            
            if (sendToServer) {
                upload
                waitfor
            } else {
                download(server)
            }
        }
    }
}

//import scala.collection.{ mutable, immutable, generic }
