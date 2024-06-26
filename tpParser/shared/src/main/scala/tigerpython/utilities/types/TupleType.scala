package tigerpython.utilities
package types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 30.06.2016.
  * Updated by Tobias Kohn on 30.06.2016.
  */
class TupleType(val itemTypes: Array[DataType]) extends
  PrimitiveType("tuple[%s]".format(itemTypes.map(_.name).mkString(", ")),
    BuiltinTypes.TUPLE_TYPE, BuiltinTypes.TUPLE_TYPE.fields) {

  def length: Int = itemTypes.length
}
object TupleType {
  private val tuples1 = collection.mutable.Map[DataType, TupleType]()
  private val tuples2 = collection.mutable.Map[(DataType, DataType), TupleType]()

  def apply(itemTypes: Array[DataType]): DataType =
    itemTypes.length match {
      case 0 =>
        BuiltinTypes.TUPLE
      case 1 =>
        tuples1.getOrElseUpdate(itemTypes(0), new TupleType(itemTypes))
      case 2 =>
        tuples2.getOrElseUpdate((itemTypes(0), itemTypes(1)), new TupleType(itemTypes))
      case _ =>
        new TupleType(itemTypes)
    }

  def apply(itemTypes: DataType*): DataType =
    apply(itemTypes.toArray)
}
