package a

import scala.actors._
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import java.io.File
import a.FileUtils._
import a.FileStatus._
import a.RelativePathFile
import a.RelativePathFile._
import java.io.{InputStream, OutputStream, FileInputStream, FileOutputStream, ByteArrayOutputStream, ByteArrayInputStream}
import java.util.Date


abstract class BaseActs(basepath: String) extends Actor {
    
    protected def serveract = {
        var client: OutputChannel[Any] = null
        receive {
            case "hello" => {
                client = sender
                sender ! RelativePathFile(recursiveListFiles(basepath), basepath).mkString("\n")
                receive {
                    case ("giveme", files: String) => {
                        var filelist = RelativePathFile(files, basepath).map(rf => new File(rf.fullPath))
                        sender ! zip(basepath, filelist)
                    }
                }
            }
        }
        client
    }
    
    protected def clientact(server: OutputChannel[Any]) {
        server ! "hello"
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
            val client = serveract
            clientact(client)
        }

    }
}

class SyncClient private (basepath: String, server: AbstractActor) extends BaseActs(basepath) {
    
    def this(basepath: String) = this(basepath, select(Node("127.0.0.1", 9011), 'filesync))

    def act {
        loop {
            clientact(server)
            serveract
            Thread.sleep(10000)
        }
    }
}

//import scala.collection.{ mutable, immutable, generic }
