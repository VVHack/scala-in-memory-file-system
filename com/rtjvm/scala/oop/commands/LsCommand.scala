package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.filesystem.State

class LsCommand extends Command {
  override def apply(stream: InputStream, state: State): (InputStream, State) = {
    val progressedStream = stream.progress()
    try {
      if (progressedStream.getCurToken.size != 0) {
        if (!Command.delimiters.contains(progressedStream.getCurToken)) {
          val dirpath = progressedStream.getCurToken
          val pathList = Command.processPath(dirpath, state.root, state.wd)
          (progressedStream, state.setMessage(state.root.getDirEntryWithPath(pathList).ls))
        }
        else (stream, state.setMessage(state.wd.ls))
      }
      else (stream, state.setMessage(state.wd.ls))
    }
    catch {
      case e: RuntimeException => (progressedStream, state.setMessage(e.getMessage))
      case _: Exception => (progressedStream, state.setMessage("Unknown error occurred"))
    }
  }
}
