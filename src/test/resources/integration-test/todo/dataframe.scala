// pending

import com.todesking.prety.refine

@refine.proxy("scala.collection.Seq")
@refine.defineMemberUF("@values: ValuesProperty[A]")
trait Seq[A] {
}

// ColSchema property:
//   name: String
//   ast
//   isValid(schema: Map[String, DFType]): Boolean
//   select(schema: Map[String, DFType]): (String, DFType)

@refine.proxy("org.apache.spark.sql.Column")
@refine.defineMemberUF("@schema: ColSchemaProperty")
trait Column {
}

@refine.proxy("org.apache.spark.sql.DataSet")
@refine.defineMemberUF("@schema: SchemaProperty")
trait Dataset[A] {
  @refine("cs: {#1: this.@Selectable}, _: this.@Select(cs.@items: _*)")
  def select(cs: Seq[Column]): Dataset[A]
}

object DF {
  @refine("df: @DF{id: Int}")
  def foo(df: DataFrame): Unit = {
    df.select('id, ('id + 1).as("id_plus_1"))
  }

  @refine("col: df.@Selectable(col), _: df.@Select(col, @df.col{count: Long})")
  def groupCount(df: DataFrame, col: Column): DataFrame =
    df.groupBy(col).count
}
