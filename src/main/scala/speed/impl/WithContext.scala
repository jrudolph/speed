package speed.impl

import scala.reflect.macros.Context

trait WithContext {
  val c: Context

  def trace(msg: String): Unit = {}
}
