package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 02.08.2016.
  */
class NameMap() {
  val map = collection.mutable.Map[String, DataType]()

  def apply(key: String) = map(key)

  def contains(key: String) = map.contains(key)

  def get(key: String) = map.get(key)

  def getOrElse(key: String, default: =>DataType): DataType = map.getOrElse(key, default)

  def getOrElseUpdate(key: String, op: => DataType): DataType = map.getOrElseUpdate(key, op)

  def size = map.size

  def toMap: Map[String, DataType] = map.toMap

  def update(key: String, value: DataType): Unit =
    map.get(key) match {
      case Some(oldValue) if oldValue != value =>
        map(key) = DataType.getCompatibleType(value, oldValue)
      case _ =>
        map(key) = value
    }

  // Plain overwrite, bypassing update's merge-on-reassignment: needed when replacing a
  // pre-registered stub with the real object it stands in for, where the two are
  // never `==` (distinct object identity) but aren't a genuine type-widening case.
  def forceSet(key: String, value: DataType): Unit =
    map(key) = value

  def ++=(xs : Map[String, DataType]) = map ++= xs
}
