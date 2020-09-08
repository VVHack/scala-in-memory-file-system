package com.rtjvm.scala.oop.files

abstract class DirEntry(val parentPath: String, val name: String) {
  def getFullPath: String = {
    parentPath + Directory.SEPARATOR + name
  }
  def toDir: Directory
  def toFile: File
  def ls: String
}

object DirEntry {

}
