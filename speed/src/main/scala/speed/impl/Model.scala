/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013-2014 Johannes Rudolph
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
  case class Exists(closure: Closure) extends TerminalOperation

  /** A special kind of terminal operation that allows to append generator stages */
  case class GenerationChangingTerminal(add: Generator ⇒ InnerGenerator, term: TerminalOperation) extends TerminalOperation

  case class OperationChain(generator: Generator, terminal: TerminalOperation)
}
