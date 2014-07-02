package speed.impl

import scala.reflect.macros.Context

object MacroEntry {
  def entryP0[R](c: Context): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
  def entryP1[R](c: Context)(f: c.Expr[Any]): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
  def entryImplicitP0[I, R](c: Context)(i: c.Expr[I]): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
  def entryImplicitP1[I, R](c: Context)(i: c.Expr[I]): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
  def entryFoldLeft[T, U](c: Context)(init: c.Expr[U])(f: c.Expr[(U, T) ⇒ U]): c.Expr[U] =
    new TransformingSpeedContext[c.type](c).run[U]
}

