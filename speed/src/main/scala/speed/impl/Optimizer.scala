package speed.impl

import scala.annotation.tailrec

trait Optimizer { self: SpeedImpl ⇒
  import c.universe._

  def optimize(chain: OperationChain): OperationChain = {
    @tailrec def optimizeTerminalOnce(chain: OperationChain): OperationChain =
      optimizeTerminal(chain.terminal) match {
        case GenerationChangingTerminal(cons, term) ⇒ optimizeTerminalOnce(OperationChain(cons(chain.generator), term))
        case term @ _                               ⇒ OperationChain(optimizeGen(chain.generator), term)
      }

    optimizeTerminalOnce(chain)
  }

  def optimizeGen(gen: Generator): Generator = gen match {
    case ArrayGenerator(array) ⇒
      val arrayVar = c.fresh(newTermName("array$"))
      val init = q"val $arrayVar = $array"

      InitAddingGenerator(
        MappingGenerator(RangeGenerator(q"0", q"$arrayVar.length", q"1", q"false"), Closure("idx", q"$arrayVar(idx)", q"")),
        Seq(init))
    case i: InnerGenerator ⇒ i.transformOuter(optimizeGen)
    case _                 ⇒ gen
  }

  def optimizeTerminal(terminal: TerminalOperation): TerminalOperation = terminal match {
    case Sum(num) ⇒ FoldLeft(q"$num.zero", closure2(q"$num.plus(_, _)"))
    case Min(ord) ⇒ minOrMax("min", ord)
    case Max(ord) ⇒ minOrMax("max", ord)
    case Size     ⇒ FoldLeft(q"0", closure2(q"((num, _) => num + 1)"))
    case Count(f) ⇒ GenerationChangingTerminal(FilteringGenerator(_, f), Size)
    case _        ⇒ terminal
  }

  def minOrMax(op: TermName, ord: Tree): TerminalOperation = {
    val Ord = typeOf[Ordering[_]].typeSymbol.asClass
    val OrdT = Ord.typeParams(0).asType.toType
    val cand = OrdT.asSeenFrom(ord.tpe, Ord)

    Reduce(cand, closure2(q"$ord.$op(_, _)"))
  }
}