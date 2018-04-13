package com.todesking.prety

package object util {
  def uniqueMap[A, B](items: Seq[(A, B)]): Map[A, B] =
    items.groupBy(_._1).toMap.mapValues {
      case Seq((_, v)) => v
      case vs => throw new RuntimeException(s"Name duplicate: ${vs.head._1}")
    }.toSeq.toMap
}
