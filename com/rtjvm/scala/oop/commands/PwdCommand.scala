package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.filesystem.State

class PwdCommand extends Command {
  override def apply(stream: InputStream, state: State): (InputStream, State) =
      (stream, state.setMessage("/" + Command.pathToList(state.wd.getFullPath).mkString("/")))
}
