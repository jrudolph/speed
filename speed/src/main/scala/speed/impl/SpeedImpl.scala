package speed.impl

import scala.reflect.macros.Context

class TransformingSpeedContext[C <: Context](val c: C) extends SpeedImpl {
  private[impl] def run[T]: c.Expr[T] = c.Expr[T](transform(c.macroApplication))
}

class SpeedContext[C <: Context](val c: C) extends SpeedImpl

trait SpeedImpl extends WithContext with Model with Analyzer with Generation with Optimizer with ConstantFolding with ContextHelpers {
  import c.universe._

  def analyze(t: Tree): OperationChain
  def optimize(chain: OperationChain): OperationChain
  def generate(chain: OperationChain): Tree

  def transform(f: Tree): Tree = finish(generate(optimize(analyze(f))))
}