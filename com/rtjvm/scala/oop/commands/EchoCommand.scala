package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State

class EchoCommand extends Command {
  def echo(stream: InputStream, soFar: List[String] = List()): (InputStream, List[String]) = {
    val progressedStream = stream.progress()
    if (progressedStream.getCurToken.isEmpty || Command.delimiters.contains(progressedStream.getCurToken))
      (stream, soFar) // Return the original stream
    else {
      echo(progressedStream, soFar :+ progressedStream.getCurToken)
    }
  }

  override def apply(stream: InputStream, state: State): (InputStream, State) = {
    val progressedStream = stream.progress()
    try {
      if (progressedStream.getCurToken.isEmpty)
        throw new RuntimeException("There is nothing to echo! Echo! Echo! Echo! Echo!")
      else {
        val (processedStream, output) = echo(stream)
        (processedStream, state.setMessage(output.mkString(" ")))
      }
    }
    catch {
      case e: RuntimeException => (stream, state.setMessage(e.getMessage))
      case _: Exception => (stream, state.setMessage("Unknown error occurred"))
    }
  }

}
