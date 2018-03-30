package com.todesking.prety.universe

trait Envs { self: ForeignTypes with Values with Templates with Preds with Exprs with Props =>
  case class Env(
    binding: Map[String, Value],
    conds: Set[Value],
    unconds: Set[Value]) {

    def values: Set[Value] = binding.values.toSet ++ conds ++ unconds

    private[this] def vnf(key: String) =
      throw new RuntimeException(s"Value $key not found(env: ${binding.keys.mkString(", ")})")

    def findValue(name: String): Value =
      binding.get(name) getOrElse vnf(name)

    def bind(mapping: Map[String, Value]): Env =
      bind(mapping.toSeq: _*)
    def bind(mapping: (String, Value)*): Env =
      copy(binding = binding ++ mapping)

    def cond(v: Value): Env =
      copy(conds = conds + v)
    def uncond(v: Value): Env =
      copy(unconds = unconds + v)
  }
  object Env {
    val empty: Env =
      Env(Map(), Set(), Set())
    def apply(binding: Map[String, Value]): Env =
      empty.copy(binding = binding)
  }
}
