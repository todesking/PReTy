package com.todesking.prety.universe

trait Envs { self: ForeignTypes with ForeignTypeOps with Queries with Values with Props with Preds with Exprs with Worlds =>

  case class Env(
    val global: GlobalEnv,
    val binding: Map[String, Value],
    ) {

      def values: Set[Value] = binding.values.toSet

    private[this] def vnf(key: String) =
      throw new RuntimeException(s"Value $key not found(env: ${binding.keys.mkString(", ")})")

    def findProp(name: String, targetType: TypeSym): PropKey =
      global.findProp(name, targetType)

    def findValue(name: String): Value =
      binding.get(name) getOrElse vnf(name)

    def findWorld(tpe: TypeSym): World =
      global.findWorld(tpe)
    def findOp(tpe: TypeSym, name: String): (Expr, Expr) => Expr =
      global.findOp(tpe, name)
    def bind(mapping: Map[String, Value]): Env =
      bind(mapping.toSeq: _*)
    def bind(mapping: (String, Value)*): Env =
      Env(global, binding ++ mapping)
  }

  class GlobalEnv(
    val props: Map[String, PropKey],
    val worlds: Map[TypeSym, World]) {

    private[this] def nf(kind: String, key: String) =
      throw new RuntimeException(s"$kind $key not found")

    def findProp(name: String, targetType: TypeSym): PropKey = name match {
      case "_" =>
        findWorld(targetType).selfPropKey
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
              CoreExpr.INT_LT(l, r)
          }
          case "==" => {
            case (l: CoreExpr, r: CoreExpr) if l.tpe <:< T.int && r.tpe <:< T.int =>
              CoreExpr.INT_EQ(l, r)
          }
          case unk =>
            nf("Operator", s"$tpe.$name")
        }
      else nf("Operator", s"$tpe.$name")
    }
  }

  lazy val globalEnv: GlobalEnv =
    new GlobalEnv(
      props = Map(),
      worlds = Map(
        query.types.int -> new IntWorld))

  def buildEnv(binding: Map[String, Value]): Env = new Env(
    globalEnv,
    binding = binding,
    )
}
