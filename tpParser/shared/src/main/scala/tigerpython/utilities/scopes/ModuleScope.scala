package tigerpython.utilities
package scopes

import tigerpython.parser.ast.AstNode
import types.{DataType, Package, PythonFunction}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 02.08.2016.
  */
class ModuleScope(sourceLength: Int, val module: Package, val moduleLoader: ModuleLoader) extends Scope {
  val startPos: Int = 0
  val endPos: Int = sourceLength
  private val globals = collection.mutable.Set[String]()

  val extNameInfo = new ExtNameInfo()

  // Function defs eligible for call-site parameter-type inference: module-level free
  // functions, and methods of classes that are themselves defined directly at module
  // level (not nested in another function/class). Registered by AstWalker.walkFunction
  // as they're encountered, and consumed once by AstWalker.reinferParamsFromCallSites
  // after the whole module has been walked, to refine parameter types from call-site
  // evidence. parentScope is whichever scope the FunctionScope actually lives in
  // (this ModuleScope for a free function, a ClassScope for a method) - patching needs
  // it to replace the stale FunctionScope in the right place, see Scope.replaceScope.
  val inferableFunctionDefs: collection.mutable.ArrayBuffer[ModuleScope.InferableFunctionRecord] =
    collection.mutable.ArrayBuffer()

  // Function stubs pre-registered by AstWalker.preRegisterModuleFunctions before the
  // module is walked for real, so a top-level function can call a sibling defined
  // later in the module from inside its own body (deferred execution - by the time
  // the call actually runs, the whole module has finished defining everything, same
  // as ClassScope.preRegisteredMethodStubs but one level up). Looked up by
  // AstWalker.walkFunction to migrate any call-site evidence recorded against the stub
  // onto the real PythonFunction once that function's def is actually reached.
  val preRegisteredFunctionStubs: java.util.IdentityHashMap[AstNode.FunctionDef, PythonFunction] =
    new java.util.IdentityHashMap()

  override def getModule: ModuleScope = this

  def define(name: String, dataType: DataType): Unit =
    if (module != null)
      module.setField(name, dataType)

  override def getCurrentPath: String = getPackageName

  def getLocals: Map[String, DataType] =
    if (module != null)
      module.getFields
    else
      Map()

  def getPackageName: String = module.getFullName

  override def importModule(moduleName: String, importName: String): Option[DataType] =
    if (importName == null || importName == "")
      Some(moduleLoader.importName(moduleName))
    else if (moduleName == "." || moduleName == module.name)
      findName(importName)
    else if (moduleName.startsWith(".") || moduleName.takeWhile(_ != '.') == module.name) {
      val modName = moduleName.dropWhile(_ != '.').dropWhile(_ == '.')
      if (modName != "")
        findName(modName) match {
          case Some(dt) =>
            dt.findField(importName)
          case _ =>
            None
        }
      else
        importModule(".", importName)
    } else
      Some(moduleLoader.importNameFrom(moduleName, importName))

  def addGlobal(name: String): Unit =
    if (name != null && name != "")
      globals += name

  override def isLocal(name: String): Boolean = !globals.contains(name)

  override def incNameUseCounter(name: AstNode.Name): Unit =
    extNameInfo += name
}
object ModuleScope {
  case class InferableFunctionRecord(defNode: AstNode.FunctionDef,
                                      pythonFunction: PythonFunction,
                                      functionScope: FunctionScope,
                                      parentScope: Scope)
}
