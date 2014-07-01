package speed.impl

import scala.reflect.macros.Context

class TransformingSpeedContext[C <: Context](val c: C) extends SpeedImpl {
  private[impl] def run[T]: c.Expr[T] = c.Expr[T](transform(c.macroApplication))
}

class SpeedContext[C <: Context](val c: C) extends SpeedImpl

trait SpeedImpl extends WithContext with Analyzer with Generation with Optimizer with ConstantFolding with ContextHelpers {
  import c.universe._

  case class Closure(valName: TermName, application: Tree, init: Tree)
  case class Closure2(valName1: TermName, valName2: TermName, application: Tree, init: Tree)

  sealed trait Generator {
    def withInits(inits: Tree*): InitAddingGenerator = InitAddingGenerator(this, inits)
  }
  sealed trait InnerGenerator extends Generator {
    def outer: Generator
    def transformOuter(f: Generator ⇒ Generator): InnerGenerator
  }
  case class MappingGenerator(outer: Generator, f: Closure) extends InnerGenerator {
    def transformOuter(f: Generator ⇒ Generator): MappingGenerator = copy(outer = f(outer))
  }
  case class FlatMappingGenerator(outer: Generator, valName: TermName, innerGenerator: Generator) extends InnerGenerator {
    def transformOuter(f: Generator ⇒ Generator): FlatMappingGenerator = copy(outer = f(outer))
  }
  case class FilteringGenerator(outer: Generator, f: Closure) extends InnerGenerator {
    def transformOuter(f: Generator ⇒ Generator): FilteringGenerator = copy(outer = f(outer))
  }
  case class RangeGenerator(start: Tree, end: Tree, by: Tree, inclusive: Tree) extends Generator
  case class ArrayGenerator(array: Tree) extends Generator
  case class InitAddingGenerator(outer: Generator, inits: Seq[Tree]) extends InnerGenerator {
    def transformOuter(f: Generator ⇒ Generator): InitAddingGenerator = copy(outer = f(outer))
  }

  sealed trait TerminalOperation
  case class Foreach(f: Closure) extends TerminalOperation
  case class FoldLeft(init: Tree, f: Closure2) extends TerminalOperation
  case class Reduce(tpe: Type, f: Closure2) extends TerminalOperation

  case class Sum(numeric: Tree) extends TerminalOperation
  case class Min(ordering: Tree) extends TerminalOperation
  case class Max(ordering: Tree) extends TerminalOperation
  case object Size extends TerminalOperation

  case class OperationChain(generator: Generator, terminal: TerminalOperation)

  override def analyze(t: Tree): OperationChain = {
    val chain = super.analyze(t)
    //println(s"Chain for '${c.universe.show(t)}': $chain")

    chain
  }
  def optimize(chain: OperationChain): OperationChain
  def generate(chain: OperationChain): Tree

  def transform(f: Tree): Tree = finish(generate(optimize(analyze(f))))
}