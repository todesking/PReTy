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
  @refine("cs: {#1: {@schema: @df.canSelect(this.@schema, _)}, _: {@schema: @df.Select(this.@schema, cs.@values)")
  def select(cs: Seq[Column]): Dataset[A]
}
