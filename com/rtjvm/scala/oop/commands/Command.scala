package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State
import sun.security.tools.PathList

trait Command {
  def apply(stream: InputStream, state: State): (InputStream, State)
}

object Command {
  val PIPE = "|"
  val REDIRECT = ">"
  val APPEND = ">>"
  val delimiters: Set[String] = Set(PIPE, REDIRECT, APPEND)
  def from(input: String, state: State): State = {
    val stream = InputStream(input).progress()
    if (stream.getCurToken.size == 0) state
    else {
      val commandName = stream.getCurToken
      val command = commandName match {
        case "pwd" => new PwdCommand()
        case "ls" => new LsCommand()
        case "mkdir" => new MkdirCommand()
        case "cd" => new CdCommand()
        case "rm" => new RmCommand()
        case "touch" => new TouchCommand()
        case "echo" => new EchoCommand()
        case ">" => new OutputRedirectCommand()
        case ">>" => new OutputAppendCommand()
        case "cat" => new CatCommand()
        case _ => new UnknownCommand()
      }
      val (processedStream, newState) = command(stream, state)
      if (processedStream.getCurInput.size != 0) {
        from(processedStream.getCurInput, newState)
      } else newState
    }
  }

  // Path utility functions
  def pathToList(path: List[Char], pathList: List[String] = List()): List[String] = {
    path match {
      case List() => pathList
      case '.' :: '.' :: rest => {
        if (pathList.isEmpty)
          pathToList(rest, pathList :+ "..")
        else if (pathList.last == "" && pathList.dropRight(1).last == "..")
          pathToList(rest, pathList.dropRight(1) :+ "..")
        else
          pathToList(rest, pathList.dropRight(1).dropRight(1))
      }
      case '.' :: rest => {
        if (pathList.isEmpty)
          pathToList(rest, pathList)
        else
          pathToList(rest, pathList.dropRight(1))
      }
      case '/' :: rest => {
        if (pathList.nonEmpty)
          pathToList(rest, pathList :+ "")
        else
          pathToList(rest, pathList)
      }
      case c :: rest => {
        if (pathList.isEmpty)
          pathToList(rest, pathList :+ ("" + c))
        else
          pathToList(rest, pathList.dropRight(1) :+ (pathList.last + c))
      }
    }
  }

  // Not doing path.split because it returns a mutable Array, mutable is bad
  def pathToList(path: String): List[String] = pathToList(path.toList)

  def processPathList(pathList: List[String], rootDir: Directory, curDir: Directory): List[String] = {
    pathList match {
      case ".." :: rest => {
        val parentPathList = pathToList(curDir.parentPath)
        val parentDir = rootDir.getDirEntryWithPath(parentPathList).toDir
        processPathList(rest, rootDir, parentDir)
      }
      case _ => pathToList(curDir.getFullPath) ++ pathList
    }
  }

  def processPath(dirPath: String, rootDir: Directory, curDir: Directory): List[String] = {
    val pathList = pathToList(dirPath)
    if (dirPath.startsWith("/")) {
      processPathList(pathList, rootDir, rootDir)
    } else {
      processPathList(pathList, rootDir, curDir)
    }
  }

  def getFileName(path: String) = {
    val pathList = pathToList(path)
    if (pathList.isEmpty)
      throw new RuntimeException("Are you seriously trying to get a filename for an empty path??")
    pathList.last
  }

  def getDirPath(path: String): String = {
    val pathList = pathToList(path)
    if (pathList.isEmpty)
      throw new RuntimeException("Are you seriously trying to get the directory path for an empty path??")
    val prefix = pathList.dropRight(1).mkString("/")
    if (path.startsWith("/"))
      "/" + prefix
    else
      prefix
  }

}
