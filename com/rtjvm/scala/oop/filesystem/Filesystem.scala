package com.rtjvm.scala.oop.filesystem

import java.util.Scanner

import com.rtjvm.scala.oop.commands.Command
import com.rtjvm.scala.oop.files.Directory

// We invented an immutable while loop!!! val ftw (no vars allowed)
case class WhileLoop[T](input: T, cond: T => Boolean) {
  def apply(run: T => T): Unit = {
    if (!cond(input)) ()
    else WhileLoop(run(input), cond)(run)
  }
}

object Filesystem extends App {
  print("Loop test: ")
  WhileLoop[Int](5, _ < 25) { num =>
    print(num + " ")
    num + 1
  }
  println

  val scanner = new Scanner(System.in)
  val root = Directory.ROOT
  val initialState = State(root, root)
  initialState.show
  WhileLoop[State](initialState, _ => true) { state =>
    Command.from(scanner.nextLine().trim(), state).show
  }
}
