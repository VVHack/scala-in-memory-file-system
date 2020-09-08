package com.rtjvm.scala.oop.files

class Directory(override val parentPath: String, override val name: String, val contents: List[DirEntry])
  extends DirEntry(parentPath, name) {

  def getEntryWithName(entryName: String): DirEntry = {
    val entriesWithName = contents.filter(_.name == entryName)
    if (entriesWithName.size == 0) throw new RuntimeException("No such file or directory")
    assert(entriesWithName.size == 1)
    entriesWithName.head
  }

  override def toDir: Directory = this
  def toFile: File =
    throw new RuntimeException(s"$name is a directory, not a file")

  def getDirEntryWithPath(path: List[String]): DirEntry = {
    path match {
      case List() => this
      case entry :: List() => getEntryWithName(entry)
      case entry :: rest => getEntryWithName(entry).toDir.getDirEntryWithPath(path.tail)
    }
  }

  def mkdir(path: List[String]): Directory = {
    path match {
      case List() => throw new RuntimeException("Should never get here")
      case entry :: List() => {
        val entriesWithName = contents.filter(_.name == entry)
        if (entriesWithName.nonEmpty) {
          assert(entriesWithName.size == 1)
          entriesWithName.head.toDir
        } else {
          Directory(parentPath, name, contents :+ Directory.empty(getFullPath, entry))
        }
      }
      case entry :: rest => {
        val subDir = getEntryWithName(entry).toDir.mkdir(path.tail)
        val otherSubDirs = contents.filter(_.name != entry)
        Directory(parentPath, name, otherSubDirs :+ subDir)
      }
    }
  }

  def touch(path: List[String]): Directory = {
    path match {
      case List() => throw new RuntimeException("Should never get here")
      case entry :: List() => {
        val entriesWithName = contents.filter(_.name == entry)
        if (entriesWithName.nonEmpty) {
          assert(entriesWithName.size == 1)
          this
        } else {
          Directory(parentPath, name, contents :+ File.empty(getFullPath, entry))
        }
      }
      case entry :: rest => {
        val subDir = getEntryWithName(entry).toDir.touch(path.tail)
        val otherSubDirs = contents.filter(_.name != entry)
        Directory(parentPath, name, otherSubDirs :+ subDir)
      }
    }
  }

  def writeToFile(path: List[String], content: String): Directory = {
    path match {
      case List() => throw new RuntimeException("Should never get here")
      case entry :: List() => {
        val newContents = contents.filter(_.name != entry)
        Directory(parentPath,
                  name,
                  newContents :+ File.empty(getFullPath, entry).write(content))
      }
      case entry :: rest => {
        val subDir = getEntryWithName(entry).toDir.writeToFile(path.tail, content)
        val otherSubDirs = contents.filter(_.name != entry)
        Directory(parentPath, name, otherSubDirs :+ subDir)
      }
    }
  }

  def appendToFile(path: List[String], content: String): Directory = {
    path match {
      case List() => throw new RuntimeException("Should never get here")
      case entry :: List() => {
        val entriesWithName = contents.filter(_.name == entry)
        if (entriesWithName.nonEmpty) {
          val newContents = contents.filter(_.name != entry)
          assert(entriesWithName.size == 1)
          Directory(parentPath, name, newContents :+ entriesWithName.head.toFile.append(content))
        } else {
          Directory(parentPath,
                    name,
                    contents :+ File.empty(getFullPath, entry).write(content))
        }
      }
      case entry :: rest => {
        val subDir = getEntryWithName(entry).toDir.appendToFile(path.tail, content)
        val otherSubDirs = contents.filter(_.name != entry)
        Directory(parentPath, name, otherSubDirs :+ subDir)
      }
    }
  }

  def ls: String = contents.map(_.name).sorted.mkString("\n")

  def rm(path: List[String]): Directory = {
    path match {
      case List() => throw new RuntimeException("Are you trying to delete the root")
      case entry::List() => {
        Directory(parentPath, name, contents.filter(_.name != entry))
      }
      case entry::rest => {
        val subDir = getEntryWithName(entry).toDir.rm(path.tail)
        val otherSubDirs = contents.filter(_.name != entry)
        Directory(parentPath, name, otherSubDirs :+ subDir)
      }
    }
  }
}

object Directory {
  val SEPARATOR: String = "/"
  val ROOT_PATH: String = "/"

  def ROOT: Directory = Directory.empty("", "")
  def empty(parentPath: String, name: String): Directory =
    new Directory(parentPath, name, List())
  def apply(parentPath: String, name: String, contents: List[DirEntry]): Directory =
    new Directory(parentPath, name, contents)

}
