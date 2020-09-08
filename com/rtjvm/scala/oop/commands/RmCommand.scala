package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.filesystem.State

class RmCommand extends Command {
  override def apply(stream: InputStream, state: State): (InputStream, State) = {
    val progressedStream = stream.progress()
    val dirpath = progressedStream.getCurToken
    val pathList = Command.processPath(dirpath, state.root, state.wd)
    try {
      if (dirpath.isEmpty) throw new RuntimeException("rm what?")
      val newRoot = state.root.rm(pathList)
      val newWd = newRoot.getDirEntryWithPath(Command.pathToList(state.wd.getFullPath)).toDir
      (progressedStream, State(newRoot, newWd).setMessage(""))
    }
    catch {
      case e: RuntimeException => (progressedStream, state.setMessage(e.getMessage))
      case _: Exception => (progressedStream, state.setMessage("Unknown error occurred"))
    }
  }
}
