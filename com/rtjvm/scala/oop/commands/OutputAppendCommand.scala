package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State

class OutputAppendCommand extends Command {
  override def apply(stream: InputStream, state: State): (InputStream, State) = {
    val progressedStream = stream.progress()
    val dirpath = progressedStream.getCurToken
    val pathList = Command.processPath(dirpath, state.root, state.wd)
    try {
      if (dirpath.isEmpty) throw new RuntimeException("No file specified")
      val newRoot = state.root.touch(pathList)
      val newRootFileWritten = newRoot.appendToFile(pathList, "\n" + state.output)
      val newWd = newRootFileWritten.getDirEntryWithPath(Command.pathToList(state.wd.getFullPath)).toDir
      (progressedStream, State(newRootFileWritten, newWd).setMessage(""))
    }
    catch {
      case e: RuntimeException => (progressedStream, state.setMessage(e.getMessage))
      case _: Exception => (progressedStream, state.setMessage("Unknown error occurred"))
    }
  }
}
