package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.filesystem.State

class UnknownCommand extends Command {
  override def apply(stream: InputStream, state: State): (InputStream, State) = {
    (stream, state.setMessage("Command not found"))
  }
}
