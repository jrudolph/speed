package speed.impl

import scala.reflect.macros.Context

object SpeedMacrosV2 {
  def entryF1[T, U, R](c: Context)(f: c.Expr[T ⇒ U]): c.Expr[R] =
    new TransformingSpeedContext[c.type](c).run[R]
}

class TransformingSpeedContext[C <: Context](val c: C) extends SpeedImpl {
  private[impl] def run[T]: c.Expr[T] = c.Expr[T](transform(c.macroApplication))
}

class SpeedContext[C <: Context](val c: C) extends SpeedImpl

trait SpeedImpl extends WithContext with Analyzer with Generation with Optimizer with ConstantFolding {
  import c.universe._

  case class Closure(valName: TermName, application: Tree, init: Tree)

  trait Closure2

  sealed trait Generator
  case class MappingGenerator(inner: Generator, f: Closure) extends Generator
  case class RangeGenerator(start: Tree, end: Tree, by: Tree, inclusive: Boolean) extends Generator
  case class ArrayGenerator(array: Tree) extends Generator

  sealed trait TerminalOperation
  case class Foreach(f: Closure) extends TerminalOperation
  case class FoldLeft(init: Tree, f: Closure2) extends TerminalOperation

  case class Sum(numeric: Tree) extends TerminalOperation

  case class OperationChain(generator: Generator, terminal: TerminalOperation)

  override def analyze(t: Tree): OperationChain = {
    val chain = super.analyze(t)
    println(s"Chain for '${c.universe.show(t)}': $chain")

    chain
  }
  def optimize(chain: OperationChain): OperationChain
  def generate(chain: OperationChain): Tree

  def transform(f: Tree): Tree = finish(generate(optimize(analyze(f))))
}

trait Analyzer { self: SpeedImpl ⇒
  import c.universe._

  def analyze(t: Tree): OperationChain = t match {
    case q"$inner.foreach[..${ _ }]($f)" ⇒ OperationChain(analyzeGen(inner), Foreach(closure1(f)))
  }
  def analyzeGen(t: Tree): Generator = t match {
    //case q"$inner.map[..${_}]($f)" => MappingGenerator(analyzeGen(inner), closure1(f))
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
}

trait Generation extends RangeGeneration { self: SpeedImpl ⇒
  import c.universe._
  case class TerminalOperationSetup(inits: Seq[Tree], inner: Tree, result: Tree)

  def generate(chain: OperationChain): Tree = {
    val varName = newTermName(c.fresh("yyyy$"))

    val term = generateTerminal(chain.terminal, varName)
    println(s"Term: $term")
    val gen = generateGen(chain.generator, varName, term.inner)
    println(s"Gen: $gen")

    q"""
      ..${term.inits}

      $gen

      ${term.result}
    """
  }

  def generateGen(gen: Generator, valName: TermName, application: Tree): Tree = gen match {
    case RangeGenerator(start, end, by, incl) ⇒ generateRange(start, end, by, incl, valName, application)
    case MappingGenerator(inner, f) ⇒
      val body =
        q"""
            val $valName= ${f.application}
            $application
          """

      generateGen(inner, f.valName, body)
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
    OperationChain(optimizeGen(chain.generator), chain.terminal)

  def optimizeGen(gen: Generator): Generator = gen match {
    case ArrayGenerator(array) ⇒
      MappingGenerator(RangeGenerator(q"0", q"$array.length", q"1", false), Closure("idx", q"$array(idx)", q""))
    case _ ⇒ gen
  }
}