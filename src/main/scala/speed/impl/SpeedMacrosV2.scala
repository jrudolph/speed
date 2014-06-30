package speed.impl

import scala.reflect.macros.Context

object SpeedMacrosV2 {
  def entryP0[R](c: Context): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
  def entryP1[R](c: Context)(f: c.Expr[Any]): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
  def entryImplicitP1[I, R](c: Context)(i: c.Expr[I]): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
  def entryFoldLeft[T, U](c: Context)(init: c.Expr[U])(f: c.Expr[(U, T) ⇒ U]): c.Expr[U] =
    new TransformingSpeedContext[c.type](c).run[U]
}

class TransformingSpeedContext[C <: Context](val c: C) extends SpeedImpl {
  private[impl] def run[T]: c.Expr[T] = c.Expr[T](transform(c.macroApplication))
}

class SpeedContext[C <: Context](val c: C) extends SpeedImpl

trait SpeedImpl extends WithContext with Analyzer with Generation with Optimizer with ConstantFolding with ContextHelpers {
  import c.universe._

  case class Closure(valName: TermName, application: Tree, init: Tree)
  case class Closure2(valName1: TermName, valName2: TermName, application: Tree, init: Tree)

  sealed trait Generator
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
  case class RangeGenerator(start: Tree, end: Tree, by: Tree, inclusive: Boolean) extends Generator
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

trait Analyzer { self: SpeedImpl ⇒
  import c.universe._

  def analyze(t: Tree): OperationChain = t match {
    case q"$inner.foreach[..${ _ }]($f)"         ⇒ OperationChain(analyzeGen(inner), Foreach(closure1(f)))
    case q"$inner.foldLeft[..${ _ }]($init)($f)" ⇒ OperationChain(analyzeGen(inner), FoldLeft(init, closure2(f)))
    case q"$inner.reduce[$t]($f)"                ⇒ OperationChain(analyzeGen(inner), Reduce(t.tpe, closure2(f)))
    case q"$inner.sum[..${ _ }]($num)"           ⇒ OperationChain(analyzeGen(inner), Sum(num))
    case q"$inner.min[..${ _ }]($ord)"           ⇒ OperationChain(analyzeGen(inner), Min(ord))
    case q"$inner.max[..${ _ }]($ord)"           ⇒ OperationChain(analyzeGen(inner), Max(ord))
    case q"$inner.size"                          ⇒ OperationChain(analyzeGen(inner), Size)
  }
  def analyzeGen(t: Tree): Generator = t match {
    case q"$inner.map[..${ _ }]($f)"    ⇒ MappingGenerator(analyzeGen(inner), closure1(f))
    case q"$inner.filter[..${ _ }]($f)" ⇒ FilteringGenerator(analyzeGen(inner), closure1(f))
    case q"$inner.flatMap[..${ _ }]($f)" ⇒
      val Closure(valName, q"$innerGeneratorTree: @speed.dontfold", _) = closure1(f)

      FlatMappingGenerator(analyzeGen(inner), valName, analyzeGen(innerGeneratorTree))

    case q"${ _ }.RangesAreSpeedy($r).speedy" ⇒ range(r)
    case q"${ _ }.ArraysAreSpeedy[..${ _ }]($a).speedy" ⇒ ArrayGenerator(a)

    case _ ⇒ error(s"Unknown Prefix: $t")
  }
  def range(t: Tree): RangeGenerator = t match {
    case q"${ _ }.intWrapper($from).to($to)"               ⇒ RangeGenerator(from, to, c.literal(1).tree, true)
    case q"${ _ }.intWrapper($from).to($to).by($by)"       ⇒ RangeGenerator(from, to, by, true)
    case q"${ _ }.intWrapper($from).until($until)"         ⇒ RangeGenerator(from, until, c.literal(1).tree, false)
    case q"${ _ }.intWrapper($from).until($until).by($by)" ⇒ RangeGenerator(from, until, by, false)
  }
  def closure1(fTree: Tree): Closure = fTree match {
    // try to find literal anonymous functions
    case q"( $i => $body )"             ⇒ Closure(i.name, q"{ $body }: @speed.dontfold()", q"")
    //case q"( ($i: ${ _ }) => $body )"   ⇒ AnonFunc(i.asInstanceOf[ValDef].name, q"{ $body }", q"")
    // this matches partial evaluation (like `println _`)
    case Block(Nil, q"( $i => $body )") ⇒ Closure(i.name, q"{ $body }: @speed.dontfold()", q"")
    case _ ⇒
      c.warning(fTree.pos, s"Couldn't extract anonymous function implementation here. '$fTree'")
      val fun = c.fresh(newTermName("funInit"))
      val iVar = c.fresh(newTermName("i"))
      Closure(iVar, q"$fun($iVar)", q"val $fun = $fTree")
  }
  def closure2(fTree: Tree): Closure2 =
    fTree match {
      // try to find literal anonymous functions
      case q"( ($i1, $i2) => $body )"             ⇒ Closure2(i1.name, i2.name, q"{ $body }: @speed.dontfold()", q"")
      // this matches partial evaluation (like `println _`)
      case Block(Nil, q"( ($i1, $i2) => $body )") ⇒ Closure2(i1.name, i2.name, q"{ $body }: @speed.dontfold()", q"")
      case _ ⇒
        val fun = c.fresh(newTermName("funInit"))
        Closure2(newTermName("i1"), newTermName("i2"), q"$fun(i1, i2)", q"val $fun = $fTree")
    }
}

trait Generation extends RangeGeneration { self: SpeedImpl ⇒
  import c.universe._
  case class GeneratorSetup(inits: Seq[Tree], body: Tree) {
    def prependInits(inits: Seq[Tree]): GeneratorSetup = copy(inits = inits ++ this.inits)
  }
  object GeneratorSetup {
    implicit def treeToSetup(t: Tree): GeneratorSetup = GeneratorSetup(Nil, t)
  }
  case class TerminalOperationSetup(inits: Seq[Tree], inner: Tree, result: Tree)

  def generate(chain: OperationChain): Tree = {
    val varName = newTermName(c.fresh("yyyy$"))

    val term = generateTerminal(chain.terminal, varName)
    //println(s"Term: $term")
    val GeneratorSetup(genInits, gen) = generateGen(chain.generator, varName, term.inner)
    //println(s"Gen: $gen")

    q"""
      ..${genInits ++ term.inits}

      $gen

      ${term.result}
    """
  }

  def generateGen(gen: Generator, expectedValName: TermName, application: Tree): GeneratorSetup = gen match {
    case RangeGenerator(start, end, by, incl) ⇒ generateRange(start, end, by, incl, expectedValName, application)
    case MappingGenerator(outer, f) ⇒
      val tempName = c.fresh(newTermName("m$"))
      val body =
        q"""
            val $expectedValName = {
              val ${f.valName} = $tempName
              ${f.application}
            }
            $application
          """

      generateGen(outer, tempName, body)

    case FlatMappingGenerator(outer, innerValName, innerGenerator) ⇒
      val GeneratorSetup(inits, innerLoop) = generateGen(innerGenerator, expectedValName, application)
      generateGen(outer, innerValName, innerLoop).prependInits(inits)

    case FilteringGenerator(outer, f) ⇒
      val tempName = c.fresh(newTermName("m$"))
      val body =
        q"""
            if ({
              val ${f.valName} = $tempName
              ${f.application}
            }) {
              val $expectedValName = $tempName
              $application
            }
          """

      generateGen(outer, tempName, body)

    case InitAddingGenerator(outer, inits) ⇒ generateGen(outer, expectedValName, application).prependInits(inits)

    //case _ => q"()"
  }
  def generateTerminal(terminal: TerminalOperation, valName: TermName): TerminalOperationSetup = terminal match {
    case Foreach(f) ⇒
      val body: Tree = q"""
      {
        val ${f.valName} = $valName
        ${f.application}
      }
      """

      TerminalOperationSetup(Seq(f.init), body, q"()")

    case FoldLeft(init, f) ⇒
      val accVar = c.fresh(newTermName("acc$"))
      val inits = Seq(q"var $accVar = ${init}", f.init)

      val body =
        q"""
          $accVar = {
            val ${f.valName1} = $accVar
            val ${f.valName2} = $valName
            ${f.application}
          }
        """

      TerminalOperationSetup(inits, body, q"$accVar")

    case Reduce(tpe, f) ⇒
      val accVar = c.fresh(newTermName("acc$"))
      val emptyVar = c.fresh(newTermName("empty$"))
      val Closure2(v1, v2, application, funcInit) = f
      def b(b: Boolean) = Literal(Constant(b))
      val a1Type = tpe
      val neutralA1 = neutralElement(tpe)
      val inits = Seq(
        q"var $accVar: $a1Type = $neutralA1",
        q"var $emptyVar = ${b(true)}",
        funcInit)
      val body =
        q"""
          if ($emptyVar) {
            $emptyVar = ${b(false)}
            $accVar = $valName
          } else
            $accVar = {
              val $v1 = $accVar
              val $v2 = $valName
              $application
            }
        """

      val result =
        q"""
              if ($emptyVar) throw new UnsupportedOperationException("Can't reduce empty range")
              else $accVar
            """

      TerminalOperationSetup(inits, body, result)
  }
}

trait RangeGeneration { self: SpeedImpl ⇒
  import c.universe._

  def generateRange(start: Tree, end: Tree, step: Tree, isInclusive: Boolean, varName: TermName, application: Tree): Tree = {
    val upOp = newTermName(if (isInclusive) "$less$eq" else "$less")
    val downOp = newTermName(if (isInclusive) "$greater$eq" else "$greater")
    val startVar = c.fresh(newTermName("start"))
    val endVar = c.fresh(newTermName("end"))
    val stepVar = c.fresh(newTermName("step"))
    val terminalElementVar = c.fresh(newTermName("terminalElement"))

    // a variable whose value decides which implementation to use
    //  1: count up and compare with `<` / `<=`
    // -1: count down and compare with `>` / `>=`
    //  0: safe version, if abs(step) != 1, to make sure not to overflow the bounds
    // we invoke that here eagerly and use our handy partial evaluation implementation
    // to figure out if we can decide now or not
    // if we can't figure it out now, we choose the variant which always works
    val decider =
      foldConstants(
        q"""
          $step match {
            case 0 => throw new IllegalArgumentException("step cannot be 0.")
            case 1 => 1
            case -1 => -1
            case _ =>
              if ($step > 0)
                if ($end.toLong + $step > Int.MaxValue) 0 // overflow looming
                else 1
              else
                if ($end.toLong + $step < Int.MinValue) 0 // overflow looming
                else -1
          }""") match {
          case l @ Literal(Constant(x))           ⇒ x
          case Block(_, l @ Literal(Constant(x))) ⇒ x
          case x @ _                              ⇒ 0
        }

    val body = decider match {
      case 1 ⇒ // count up
        q"""
          var $varName = $start
          val $endVar = $end
          val $stepVar = $step

          while ($varName $upOp $endVar) {
            $application
            $varName += $stepVar
          }
        """
      case -1 ⇒ // count down
        q"""
          var $varName = $start
          val $endVar = $end
          val $stepVar = $step

          while ($varName $downOp $endVar) {
            $application
            $varName += $stepVar
          }
        """
      case 0 ⇒
        q"""
          val $startVar = $start
          val $endVar = $end
          val $stepVar = $step

          val $terminalElementVar = {
            val gap = $endVar.toLong - $startVar.toLong
            val isExact = gap % $stepVar == 0
            val hasStub = $isInclusive || !isExact
            val longLength = gap / $stepVar + (if (hasStub) 1 else 0)
            val isEmpty =
            ($startVar > $endVar && $stepVar > 0) ||
              ($startVar < $endVar && $stepVar < 0) ||
              ($startVar == $endVar && !$isInclusive)
            val numRangeElements =
              if (isEmpty) 0
              else longLength
            ($startVar.toLong + numRangeElements * $stepVar).toInt
          }

          var $varName = $startVar
          while ($varName != $terminalElementVar) {
            $application
            $varName += $stepVar
          }
        """
    }

    body
  }
}

trait Optimizer { self: SpeedImpl ⇒
  import c.universe._

  def optimize(chain: OperationChain): OperationChain =
    OperationChain(optimizeGen(chain.generator), optimizeTerminal(chain.terminal))

  def optimizeGen(gen: Generator): Generator = gen match {
    case ArrayGenerator(array) ⇒
      val arrayVar = c.fresh(newTermName("array$"))
      val init = q"val $arrayVar = $array: @speed.dontfold"

      InitAddingGenerator(
        MappingGenerator(RangeGenerator(q"0", q"$arrayVar.length", q"1", false), Closure("idx", q"$arrayVar(idx)", q"")),
        Seq(init))
    case i: InnerGenerator ⇒ i.transformOuter(optimizeGen)
    case _                 ⇒ gen
  }

  def optimizeTerminal(terminal: TerminalOperation): TerminalOperation = terminal match {
    case Sum(num) ⇒ FoldLeft(q"$num.zero", closure2(q"$num.plus(_, _)"))
    case Min(ord) ⇒ minOrMax("min", ord)
    case Max(ord) ⇒ minOrMax("max", ord)
    case Size     ⇒ FoldLeft(q"0", closure2(q"((num, _) => num + 1)"))
    case _        ⇒ terminal
  }

  def minOrMax(op: TermName, ord: Tree): TerminalOperation = {
    val Ord = typeOf[Ordering[_]].typeSymbol.asClass
    val OrdT = Ord.typeParams(0).asType.toType
    val cand = OrdT.asSeenFrom(ord.tpe, Ord)

    Reduce(cand, closure2(q"$ord.$op(_, _)"))
  }
}