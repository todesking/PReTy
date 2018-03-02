package com.todesking.prety.universe

trait Envs { self: ForeignTypes with ForeignTypeOps with Queries with Values with Props with Preds with Exprs with Worlds =>
  private[this] def nf(kind: String, key: String) = throw new RuntimeException(s"$kind $key not found")

  class Env(
    val global: GlobalEnv,
    val values: Map[String, Value],
    val theValue: Value) {

    def findProp(name: String, targetType: TypeSym): PropKey =
      global.findProp(name, targetType)

    def findValue(name: String): Value =
      values.get(name) getOrElse nf("Value", name)

    def findWorld(tpe: TypeSym): World =
      global.findWorld(tpe)
    def findOp(tpe: TypeSym, name: String): (Expr, Expr) => Expr =
      global.findOp(tpe, name)
  }

  class GlobalEnv(
    val props: Map[String, PropKey],
    val worlds: Map[TypeSym, World]) {

    def findProp(name: String, targetType: TypeSym): PropKey = name match {
      case "_" =>
        PropKey("_", targetType, targetType)
      case name =>
        props.get(name) getOrElse nf("Property", name)
    }
    def findWorld(tpe: TypeSym): World =
      worlds.get(tpe) getOrElse nf("World", tpe.toString)
    def findOp(tpe: TypeSym, name: String): (Expr, Expr) => Expr = {
      import query.{ types => T }
      if (tpe <:< query.types.int)
        name match {
          case ">" => {
            case (l: CoreExpr, r: CoreExpr) if l.tpe <:< T.int && r.tpe <:< T.int =>
              CoreExpr.INT_GT(l, r)
          }
          case "<" => {
            case (l: CoreExpr, r: CoreExpr) if l.tpe <:< T.int && r.tpe <:< T.int =>
              CoreExpr.INT_GT(r, l)
          }
          case "==" => {
            case (l: CoreExpr, r: CoreExpr) if l.tpe <:< T.int && r.tpe <:< T.int =>
              CoreExpr.INT_EQ(l, r)
          }
        }
      else nf("Operator", s"$tpe.$name")
    }
  }

  lazy val globalEnv: GlobalEnv =
    new GlobalEnv(
      props = Map(),
      worlds = Map(
        query.types.int -> new IntWorld))

  def buildEnv(values: Map[String, Value], theValue: Value): Env = new Env(
    globalEnv,
    values = values,
    theValue = theValue)
}
