package speed.impl

object Debug {
  def show[T](t: T): T = macro SpeedMacros.showTree[T]
  def rangeForeach(range: Any): (List[Int], List[Int]) = macro SpeedMacros.rangeForeachImpl
}
