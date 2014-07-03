package speed.impl

trait Model { self: SpeedImpl ⇒
  import c.universe._

  case class Closure(valName: TermName, application: Tree, init: Tree, isPure: Boolean = true)
  case class Closure2(valName1: TermName, valName2: TermName, application: Tree, init: Tree, isPure: Boolean = true)

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
  case class ReverseGenerator(outer: Generator) extends InnerGenerator {
    def transformOuter(f: Generator ⇒ Generator): ReverseGenerator = copy(outer = f(outer))
  }
  case class RangeGenerator(start: Tree, end: Tree, by: Tree, inclusive: Tree) extends Generator
  case class IndexedGenerator(array: Tree) extends Generator
  case class ListGenerator(list: Tree, listTpe: Type) extends Generator
  case class InitAddingGenerator(outer: Generator, inits: Seq[Tree]) extends InnerGenerator {
    def transformOuter(f: Generator ⇒ Generator): InitAddingGenerator = copy(outer = f(outer))
  }
  case class TakeGenerator(outer: Generator, number: Tree) extends InnerGenerator {
    def transformOuter(f: Generator ⇒ Generator): TakeGenerator = copy(outer = f(outer))
  }

  sealed trait TerminalOperation
  case class Foreach(f: Closure) extends TerminalOperation
  case class FoldLeft(init: Tree, f: Closure2) extends TerminalOperation
  case class Reduce(tpe: Type, f: Closure2) extends TerminalOperation

  case class Sum(numeric: Tree) extends TerminalOperation
  case class Min(ordering: Tree) extends TerminalOperation
  case class Max(ordering: Tree) extends TerminalOperation
  case object Size extends TerminalOperation
  case class Count(closure: Closure) extends TerminalOperation
  case object MkString extends TerminalOperation
  case class To(canBuildFrom: Tree) extends TerminalOperation
  case class Forall(closure: Closure) extends TerminalOperation

  /** A special kind of terminal operation that allows to append generator stages */
  case class GenerationChangingTerminal(add: Generator ⇒ InnerGenerator, term: TerminalOperation) extends TerminalOperation

  case class OperationChain(generator: Generator, terminal: TerminalOperation)
}
