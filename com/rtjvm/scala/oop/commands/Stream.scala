package com.rtjvm.scala.oop.commands

class InputStream(input: String, curToken: String = "") {
  def getCurToken: String = curToken
  def getCurInput: String = input
  def getNextToken(stripped: String): String = {
    val spaceIdx = stripped.indexOf(' ')
    if (spaceIdx == -1) stripped
    else stripped.slice(0, stripped.indexOf(' '))
  }
  def progress(): InputStream = {
    val stripped = input.trim
    val nextToken = getNextToken(stripped)
    InputStream(stripped.drop(nextToken.size), nextToken)
  }
}

object InputStream {
  def apply(input: String, curToken: String = "") =
    new InputStream(input, curToken)
}
