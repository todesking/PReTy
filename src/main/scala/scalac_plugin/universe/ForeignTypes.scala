package com.todesking.prety.scalac_plugin.universe

trait ForeignTypes {
  type Pos
  type Tree
  type ValSym
  type FunSym
  type TypeSym

  def refinementSrcFromFun(f: FunSym): Seq[String]
  def funName(f: FunSym): String
  def funParamNames(f: FunSym): Seq[Seq[String]]
  def valName(v: ValSym): String
  def funParamSymss(f: FunSym): Seq[Seq[ValSym]]
}
