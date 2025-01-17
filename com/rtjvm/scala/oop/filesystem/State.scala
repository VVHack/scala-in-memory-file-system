package com.rtjvm.scala.oop.filesystem

import com.rtjvm.scala.oop.files.Directory

class State(val root: Directory, val wd: Directory, val output: String) {
  def show: State = {
    if (output != "")
      println(output)
    print(State.SHELL_TOKEN)
    this
  }

  def setMessage(message: String): State =
    State(root, wd, message)
}

object State {
  val SHELL_TOKEN = "$ "
  def apply(root: Directory, wd: Directory, output: String = ""): State =
    new State(root, wd, output)
}
