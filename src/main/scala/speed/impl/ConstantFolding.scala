package speed
package impl

import scala.util.control.NonFatal

trait ConstantFolding { self: SpeedHelper ⇒
  import c.universe._

  def finish(tree: Tree): Tree = c.resetAllAttrs(foldConstants(tree))

  def foldConstants(tree: Tree): Tree = {
    trace(s"Input to partially: $tree")
    new ConstantFolder().transform(tree)
  }

  /**
   * This constant folder has only a crude notion of lexical scopes, so be careful
   */
  class ConstantFolder(initEnv: Map[Name, Constant] = Map.empty) extends Transformer {
    var environmentStack = collection.immutable.Stack[Map[Name, Constant]](Map.empty)

    def createBinding(sym: Name, value: Constant): Unit = {
      val top = environmentStack.head
      assert(!top.contains(sym))
      environmentStack = environmentStack.pop.push(top + (sym -> value))
    }

    def envContains(sym: Name): Boolean = environmentStack.exists(_.contains(sym))
    def lookup(sym: Name): Constant = environmentStack.find(_.contains(sym)).get(sym)
    def pushContext() = environmentStack = environmentStack.push(Map.empty)
    def popContext() = environmentStack = environmentStack.pop

    override def transform(tree: Tree): Tree = {
      def binaryOp(e1: Tree, e2: Tree, op: TermName, ifOneIsConstant: Boolean = false)(calc: PartialFunction[(Any, Any), Any]) =
        (transform(e1), transform(e2)) match {
          case (Literal(Constant(a)), Literal(Constant(b))) ⇒ Literal(Constant(calc((a, b))))
          case (Literal(Constant(a)), b) if ifOneIsConstant && calc.isDefinedAt((a, b)) ⇒ Literal(Constant(calc((a, b))))
          case (a, Literal(Constant(b))) if ifOneIsConstant && calc.isDefinedAt((a, b)) ⇒ Literal(Constant(calc((a, b))))
          case (x1, x2) ⇒ q"$x1 $op $x2"
        }
      def unaryOp(e1: Tree, op: TermName)(calc: Any ⇒ Any) =
        transform(e1) match {
          case lit @ Literal(Constant(a)) ⇒ Literal(Constant(calc(a)))
          case x                          ⇒ q"$x.$op"
        }

      tree match {
        case q"$x: ($t @speed.dontfold)" ⇒
          trace(s"Matched type ascription annotation: $x")
          val x_1 = RemoveDontFold.transform(x)
          val t_1 = RemoveDontFold.transform(t)
          q"$x_1: $t_1"
        case q"$x: @speed.dontfold" ⇒
          trace(s"Matched plain ascription: $x")
          RemoveDontFold.transform(x)
        case q"$x: ${ tpe: TypeTree }" ⇒
          trace(s"Matched TypeTree ascription: $x $tpe ")
          tpe.original match {
            case tq"$t @speed.dontfold()" ⇒
              val x_1 = RemoveDontFold.transform(x)
              val t_1 = RemoveDontFold.transform(t)
              q"$x_1: $t_1"
            case t ⇒
              val inner = transform(x)
              q"$inner: $tpe"
          }

        case _: Block ⇒
          pushContext()
          val res = super.transform(tree)
          popContext()
          res match {
            case Block(Nil, l: Literal) ⇒ l
            case _                      ⇒ res
          }
        case v @ Ident(name) if envContains(name) ⇒
          trace(s"Replaced constant binding for $name")
          Literal(lookup(name))

        case v @ q"val $x = $expr" ⇒
          trace(s"Modifiers for $v: ${v.asInstanceOf[ValDef].mods} ${expr.productPrefix}")
          //trace(s"Trying to figure out value of $x ($expr), env has values for ${env.keys.mkString(", ")}")
          transform(expr) match {
            case lit @ Literal(constant) ⇒
              trace(s"Found literal binding for $x (${v.symbol}): $constant")
              createBinding(x, constant)
              q""
            case expr ⇒
              q"val $x = $expr"
          }
        case q"- $expr" ⇒
          unaryOp(expr, "unary_$minus") {
            case i: Int ⇒ (-i): Int
          }
        case q"$expr.toLong" ⇒
          unaryOp(expr, "toLong") {
            case i: Int ⇒ i.toLong
          }
        case q"$expr.toInt" ⇒
          unaryOp(expr, "toInt") {
            case i: Long ⇒ i.toInt
            case i: Int  ⇒ i
          }
        // There's a lots of code duplication coming. The reason is that
        // this is the easiest way of making sure that we exactly match
        // the primitive implicit conversions Scala is also doing.
        //
        // It would be possible to a much more general algorithm here:
        // for every binary operation that is pure
        //   * generate the tree representing the operation
        //   * evaluate the operation
        //   * insert the result
        case q"$e1 + $e2" ⇒
          binaryOp(e1, e2, newTermName("$plus")) {
            case (i1: Int, i2: Int)   ⇒ i1 + i2
            case (i1: Int, i2: Long)  ⇒ i1 + i2
            case (i1: Long, i2: Int)  ⇒ i1 + i2
            case (i1: Long, i2: Long) ⇒ i1 + i2
          }

        case q"$e1 - $e2" ⇒
          binaryOp(e1, e2, newTermName("$minus")) {
            case (i1: Int, i2: Int)   ⇒ i1 - i2
            case (i1: Int, i2: Long)  ⇒ i1 - i2
            case (i1: Long, i2: Int)  ⇒ i1 - i2
            case (i1: Long, i2: Long) ⇒ i1 - i2
          }
        case q"$e1 * $e2" ⇒
          binaryOp(e1, e2, newTermName("$times")) {
            case (i1: Int, i2: Int)   ⇒ i1 * i2
            case (i1: Int, i2: Long)  ⇒ i1 * i2
            case (i1: Long, i2: Int)  ⇒ i1 * i2
            case (i1: Long, i2: Long) ⇒ i1 * i2
          }
        case q"$e1 / $e2" ⇒
          binaryOp(e1, e2, newTermName("$div")) {
            case (i1: Int, i2: Int)   ⇒ i1 / i2
            case (i1: Int, i2: Long)  ⇒ i1 / i2
            case (i1: Long, i2: Int)  ⇒ i1 / i2
            case (i1: Long, i2: Long) ⇒ i1 / i2
          }
        case q"$e1 % $e2" ⇒
          binaryOp(e1, e2, newTermName("$percent"), ifOneIsConstant = true) {
            // this doesn't check for pureness of the first expression
            case (_, 1)               ⇒ 0
            case (_, 1L)              ⇒ 0L
            case (i1: Int, i2: Int)   ⇒ i1 % i2
            case (i1: Int, i2: Long)  ⇒ i1 % i2
            case (i1: Long, i2: Int)  ⇒ i1 % i2
            case (i1: Long, i2: Long) ⇒ i1 % i2
          }
        case q"$e1 == $e2" ⇒
          binaryOp(e1, e2, newTermName("$eq$eq")) {
            case (i1: Int, i2: Int)   ⇒ i1 == i2
            case (i1: Int, i2: Long)  ⇒ i1 == i2
            case (i1: Long, i2: Int)  ⇒ i1 == i2
            case (i1: Long, i2: Long) ⇒ i1 == i2
          }
        case q"$e1 > $e2" ⇒
          binaryOp(e1, e2, newTermName("$greater")) {
            case (i1: Int, i2: Int)   ⇒ i1 > i2
            case (i1: Int, i2: Long)  ⇒ i1 > i2
            case (i1: Long, i2: Int)  ⇒ i1 > i2
            case (i1: Long, i2: Long) ⇒ i1 > i2
          }
        case q"$e1 < $e2" ⇒
          binaryOp(e1, e2, newTermName("$less")) {
            case (i1: Int, i2: Int)   ⇒ i1 < i2
            case (i1: Int, i2: Long)  ⇒ i1 < i2
            case (i1: Long, i2: Int)  ⇒ i1 < i2
            case (i1: Long, i2: Long) ⇒ i1 < i2
          }
        case q"$e1 || $e2" ⇒
          // do a bit of peephole optimization
          (transform(e1), transform(e2)) match {
            case (Literal(Constant(true)), _)      ⇒ Literal(Constant(true))
            case (Literal(Constant(false)), other) ⇒ other

            // this one is wrong if the first operand does side-effects
            case (_, Literal(Constant(true)))      ⇒ Literal(Constant(true))
            case (other, Literal(Constant(false))) ⇒ other
            case (x1, x2)                          ⇒ q"$x1 || $x2"
          }
        case q"$e1 && $e2" ⇒
          // do a bit of peephole optimization
          (transform(e1), transform(e2)) match {
            case (Literal(Constant(true)), other) ⇒ other
            case (Literal(Constant(false)), _)    ⇒ Literal(Constant(false))

            // this one is wrong if the first operand does side-effects
            case (_, Literal(Constant(false)))    ⇒ Literal(Constant(false))
            case (other, Literal(Constant(true))) ⇒ other
            case (x1, x2)                         ⇒ q"$x1 && $x2"
          }
        case q"! $expr" ⇒
          unaryOp(expr, "unary_$bang") {
            case b: Boolean ⇒ !b
          }
        case q"if ($cond) $rawThenB else $rawElseB" ⇒
          def thenB = transform(rawThenB)
          def elseB = transform(rawElseB)
          transform(cond) match {
            case Literal(Constant(true))  ⇒ thenB
            case Literal(Constant(false)) ⇒ elseB
            case x                        ⇒ q"if ($x) $thenB else $elseB"
          }

        case m @ Match(selector, cases) ⇒
          transform(selector) match {
            case lit @ Literal(selectorValue) ⇒
              trace(s"Found literal binding for match selector: $selectorValue ($selector), trying to run match")
              val allConstant = cases.forall(_.pat match {
                case Literal(c)          ⇒ true
                case Ident(nme.WILDCARD) ⇒ true
                case _                   ⇒ false
              })
              val lastPattern = c.universe.showRaw(cases.last.pat)
              trace(s"Only constant pattern branches: $allConstant: $lastPattern")

              if (allConstant) {
                val matchingCase =
                  cases.find(_.pat match {
                    case Literal(`selectorValue`) ⇒ true
                    case Ident(nme.WILDCARD)      ⇒ true
                    case _                        ⇒ false
                  }).get

                transform(matchingCase.body)
              } else super.transform(m)

            case sel ⇒
              trace(s"Got selector $sel")
              super.transform(m)
          }

        case x ⇒ super.transform(x)
      }
    }
  }

  /** Removes the annotation from nested trees */
  object RemoveDontFold extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case tq"$t @speed.dontfold()"    ⇒ transform(t)
      case q"$x: ($t @speed.dontfold)" ⇒ q"$x: $t"
      case q"$x: ${ tpe: TypeTree }"   ⇒ q"$x: ${transform(tpe.original)}"
      case _                           ⇒ super.transform(tree)
    }
  }
}
