package com.rtjvm.scala.oop.files

import com.rtjvm.scala.oop.commands.Command

class File(override val parentPath: String, override val name: String, val contents: String = "")
  extends DirEntry(parentPath, name) {
  def toDir: Directory =
    throw new RuntimeException(s"$name is a file, not a directory")
  def toFile: File = this
  def ls: String = "/" + Command.pathToList(getFullPath).mkString("/")
  def write(newContent: String): File =
    File(parentPath, name, newContent)
  def append(newContent: String): File =
    File(parentPath, name, contents + newContent)
}

object File {
  def empty(parentPath: String, name: String) =
    new File(parentPath, name)
  def apply(parentPath: String, name: String, contents: String) =
    new File(parentPath, name, contents)
}
