package tigerpython.utilities
package types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 15.10.2017.
  */
class PythonClass(val name: String, val bases: Array[ClassType]) extends ClassType {
  var initFunction: FunctionType = _
  var source: String = _
  var sourcePos: Int = -1

  protected val classFields = new NameMap()
  protected val instanceFields = new NameMap()
  protected val protectedFields = new NameMap()

  for (base <- bases) {
    classFields ++= base.getFields
    instanceFields ++= base.getInstanceFields
    protectedFields ++= base.getProtectedFields
    if (initFunction != null)
      base match {
        case pyClass: PythonClass =>
          initFunction = pyClass.initFunction
        case _ =>
      }
  }

  def getFields: Map[String, DataType] = classFields.toMap

  override def getFullName: String =
    if (source != null)
      source
    else
      super.getFullName

  def getInstanceFields: Map[String, DataType] = instanceFields.toMap

  override def getParamsString: String =
    if (initFunction != null)
      initFunction.getParamsString
    else
      super.getParamsString

  override def getSignature: Signature =
    if (initFunction != null)
      initFunction.getSignature
    else
      super.getSignature

  override def getProtectedFields: Map[String, DataType] = protectedFields.toMap

  def isSubclassOf(base: DataType): Boolean =
    if (base != this) {
      for (b <- bases)
        if (b.isSubclassOf(base))
          return true
      false
    } else
      true

  def setField(name: String, dataType: DataType): Unit = setFieldVia(name, dataType, merge = true)

  // Used only to replace a pre-registered stub (AstWalker.buildMethodStub) with the
  // real PythonFunction once its def is actually walked. NameMap.update merges via
  // DataType.getCompatibleType whenever a name is assigned a second time with a
  // different value - correct for genuine type-widening (e.g. a var reassigned across
  // branches), but two distinct PythonFunction objects never compare equal, so a plain
  // setField here would collapse straight to ANY_TYPE instead of replacing the stub.
  def overwriteField(name: String, dataType: DataType): Unit = setFieldVia(name, dataType, merge = false)

  private def setFieldVia(name: String, dataType: DataType, merge: Boolean): Unit = {
    def assign(fields: NameMap, n: String, v: DataType): Unit =
      if (merge) fields(n) = v else fields.forceSet(n, v)
    dataType match {
      case function: PythonFunction if function.isMethod =>
        assign(instanceFields, name, function)
        // A classmethod (params(0) is SelfClass, not SelfInstance) must also be
        // reachable via the class name itself (e.g. Factory.make(...)), which is
        // its normal calling convention - unlike a regular instance method, which
        // is ordinarily only called through an instance.
        if (function.params.nonEmpty && function.params(0).dataType.isInstanceOf[SelfClass])
          assign(classFields, name, function)
        if (name == "__init__")
          initFunction = function
      case _: FunctionType =>
        assign(classFields, name, dataType)
      case _ =>
        assign(classFields, name, dataType)
        assign(instanceFields, name, dataType)
    }
  }

  def setInstanceField(name: String, dataType: DataType): Unit = setInstanceFieldVia(name, dataType, merge = true)

  // See overwriteField - same reasoning, for the instance-field side of a stub replacement.
  def overwriteInstanceField(name: String, dataType: DataType): Unit = setInstanceFieldVia(name, dataType, merge = false)

  private def setInstanceFieldVia(name: String, dataType: DataType, merge: Boolean): Unit = {
    dataType match {
      case function: PythonFunction if name == "__init__" =>
        initFunction = function
      case _ =>
    }
    if (merge)
      instanceFields(name) = dataType
    else
      instanceFields.forceSet(name, dataType)
  }
}
