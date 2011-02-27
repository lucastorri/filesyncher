package co.torri.filesyncher

import scala.actors._
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import java.io.File
import co.torri.filesyncher.FileUtils._
import co.torri.filesyncher.FileStatus._
import co.torri.filesyncher.RelativePathFile
import co.torri.filesyncher.RelativePathFile._
import java.io.{InputStream, OutputStream, FileInputStream, FileOutputStream, ByteArrayOutputStream, ByteArrayInputStream}
import java.util.Date


abstract class BaseActs(basepath: String) extends Actor {
    
    protected def sayhello(server: OutputChannel[Any], sendToServer: Boolean) = server ! ("hello", sendToServer)
    
    protected def waitclient: (OutputChannel[Any], Boolean) = receive {
        case ("hello", sendToServer: Boolean) => return (sender, sendToServer)
    }
    
    protected def upload = {
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


class SyncServer(basepath: String) extends BaseActs(basepath) {

    def act {
        alive(9011)
        register('filesync, self)
        
        loop {
            var (sender, sendToServer) = waitclient
            
            if (sendToServer) {
                download(sender)
            } else {
                upload
            }
        }

    }
}

class SyncClient private (basepath: String, server: AbstractActor, var sendToServer: Boolean) extends BaseActs(basepath) {
    
    def this(basepath: String, serverip: String, sendToServer: Boolean) = this(basepath, select(Node(serverip, 9011), 'filesync), sendToServer)

    def act {
        
        loop {
            sayhello(server, sendToServer)
            
            if (sendToServer) {
                upload
            } else {
                download(server)
            }

            Thread.sleep(10000)
        }
    }
}

//import scala.collection.{ mutable, immutable, generic }
