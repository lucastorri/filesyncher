package co.torri.filesyncher

import scala.actors._
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import java.io.{File, FileFilter}
import co.torri.filesyncher.FileUtils._
import co.torri.filesyncher.FileStatus._
import co.torri.filesyncher.BaseActs._
import co.torri.filesyncher.RelativePathFile
import co.torri.filesyncher.RelativePathFile._
import java.io.{InputStream, OutputStream, FileInputStream, FileOutputStream, ByteArrayOutputStream, ByteArrayInputStream}
import co.torri.filesyncher.{Log => log}
import co.torri.filesyncher.LogLevel._


abstract class BaseActs(basepath: String) extends Actor {
    
    protected def sayhello(server: OutputChannel[Any], sendToServer: Boolean, filter: FileFilter) = {
        log(INFO, "Greeting server")
        server ! ('hello, sendToServer, filter.toString)
    }
    
    protected def waitclient: (OutputChannel[Any], Boolean, FileFilter) = {
        log(INFO, "Waiting client")
        receive {
            case ('hello, sendToServer: Boolean, exclude: String) => {
                log(INFO, "Client said 'hello'")
                return (sender, sendToServer, getFileFilter(exclude))
            }
        }
    }
    
    protected def upload(filter: FileFilter) = {
        log(INFO, "Uploading")
        receive {
            case 'list => {
                log(DEBUG, "Listing files")
                sender ! RelativePathFile(recursiveListFiles(basepath, filter), basepath).mkString("\n")
                log(DEBUG, "File list sent")
                receive {
                    case ('giveme, files: String) => {
                        var filelist = RelativePathFile(files, basepath).map(rf => new File(rf.fullPath))
                        log(DEBUG, "Files requested:\n" + filelist.mkString("\n"))
                        sender ! zip(basepath, filelist)
                    }
                    case a: Any => log(SEVERE, "Unexpected protocol error. Received " + a)
                }
            }
            case a: Any => log(SEVERE, "Unexpected protocol error. Received " + a)
        }
    }
    
    protected def download(server: OutputChannel[Any], filter: FileFilter) {
        log(INFO, "Downloading")
        server ! 'list
        log(DEBUG, "Requesting list of files")
        receive {
            case f: String => {
                val serverFiles = RelativePathFile(f)
                val localfiles = RelativePathFile(recursiveListFiles(basepath, filter), basepath)
                val filesdiff = contentdiff(serverFiles, localfiles)
                log(DEBUG, "Calculating diff:\n" + filesdiff.mkString("\n"))
                val newAndModified = filterNewAndModified(filesdiff).map(_._1).mkString("\n")
                val deleted = filterDeleted(filesdiff).map(d => new File(d._2.fullPath))
                delete(deleted)
                server ! ('giveme -> newAndModified)
                log(DEBUG, "Requested modified and new files")
                receive {
                    case zipped: Array[Byte] => {
                        log(DEBUG, "Receiving new and modified files")
                        unzip(basepath, zipped)
                    }
                    case a: Any => log(SEVERE, "Unexpected protocol error. Received " + a)
                }
            }
            case a: Any => log(SEVERE, "Unexpected protocol error. Received " + a)
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
                download(sender, filter)
            } else {
                upload(filter)
                parseMonitor(basepath, monitor, filter)()
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
                upload(filter)
                waitfor
            } else {
                download(server, filter)
            }
        }
    }
}

//import scala.collection.{ mutable, immutable, generic }
