package tigerpython.utilities.scopes

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 28.06.2016.
  * Updated by Tobias Kohn on 30.06.2016.
  */
trait SourceReader {
  def listFiles(): Array[String]
  def listFiles(path: String): Array[String]
  def listFiles(path: String, extension: String): Array[String]
  def loadTextFile(path: String): String
  def isFileInvalidated(path: String): Boolean
  def getPythonModuleList: Array[String]
  def getPythonModulePath(name: String): Option[String]
}
