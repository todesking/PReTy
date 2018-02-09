
package com.todesking.prety.util

sealed abstract class PP {
  override def toString = toString(0)
  def toString(level: Int): String
  def isEmpty: Boolean
  def indent: PP = PP.Indent(Seq(this))
}
object PP {
  import scala.language.implicitConversions

  implicit def stringToPP(v: String) = Line(v)
  implicit def seqToPP(v: Seq[PP]) = Items(v)

  def indent(ps: PP*) = Indent(ps.filterNot(_.isEmpty))
  def apply(ps: PP*) = Items(ps.filterNot(_.isEmpty))

  case class Line(value: String) extends PP {
    override def isEmpty = false
    override def toString(level: Int) =
      "  " * level + value
  }
  case class Items(items: Seq[PP]) extends PP {
    override def isEmpty = items.isEmpty
    override def toString(level: Int) =
      items.map(_.toString(level)).mkString("\n")
  }
  case class Indent(items: Seq[PP]) extends PP {
    override def isEmpty = items.isEmpty
    override def toString(level: Int) =
      items.map(_.toString(level + 1)).mkString("\n")
  }
}
