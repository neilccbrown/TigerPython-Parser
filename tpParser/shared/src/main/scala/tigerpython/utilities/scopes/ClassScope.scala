package tigerpython.utilities
package scopes

import tigerpython.parser.ast.AstNode
import types.{DataType, PythonClass, PythonFunction}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 01.07.2016.
  */
class ClassScope(val startPos: Int, val endPos: Int, val pyClass: PythonClass) extends Scope {

  // Method stubs pre-registered by AstWalker.walkClass before any of the class's own
  // method bodies are walked, so a method can call an as-yet-unwalked sibling defined
  // later in the same class body (self.use(1) where use appears below). Looked up by
  // AstWalker.walkFunction to migrate any call-site evidence recorded against the stub
  // onto the real PythonFunction once that method's def is actually reached.
  val preRegisteredMethodStubs: java.util.IdentityHashMap[AstNode.FunctionDef, PythonFunction] =
    new java.util.IdentityHashMap()

  def define(name: String, dataType: DataType): Unit =
    pyClass.setField(name, dataType)

  override def getCurrentClass: Option[ClassScope] = Some(this)

  override def getCurrentPath: String =
    "%s.%s".format(super.getCurrentPath, pyClass.name)

  def isLocal(name: String): Boolean = getLocals.contains(name)

  def getLocals: Map[String, DataType] = pyClass.getFields ++ pyClass.getInstanceFields
}
