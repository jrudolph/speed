package speed

import scala.reflect.macros.Context
import scala.annotation.tailrec

object SpeedMacros {
  def foreachImpl[T](c: Context)(f: c.Expr[Int ⇒ T]): c.Expr[Unit] = {

    val t =
      new Helper[c.type](c) with MethodHelper {
        import c.universe._

        override def run(fTree: Tree): Tree = {

          val AnonFunc(valName, application, init) = extractAnonFunc(fTree)

          partiallyEvaluate(generateGeneral(start, end, by, inclusive, Seq(init), valName, application))
        }
      }.run(f.tree)
    //trace(s"Result for ${c.prefix} was: $t")

    c.Expr[Unit](t)
  }

  def foldLeftImpl[A, B](c: Context)(init: c.Expr[B])(f: c.Expr[(B, A) ⇒ B]): c.Expr[B] = {
    val t =
      new Helper[c.type](c) with SpeedHelper {
        import c.universe._

        override def trace(msg: String) {
          println(msg)
        }

        override def run(fTree: Tree): Tree = {
          val accVar = c.fresh(newTermName("acc"))
          val AnonFunc2(v1, v2, application, funcInit) = extractAnonFunc2(fTree)
          val inits = Seq(q"var $accVar = ${init.tree}", funcInit)
          val body =
            q"""
                $accVar = {
                  val $v1 = $accVar
                  $application
                }
              """

          partiallyEvaluate(generateForCallChain(c.prefix.tree, inits, v2, body, q"$accVar"))
        }

        /*   c.prefix.tree match {
          case q"$expr.map[${ _ }]($mapFunc)" ⇒
            val (start, end, by, inclusive) = matchConstructor(expr)

            val accVar = c.fresh(newTermName("acc"))

            val AnonFunc(m1, mapApplication, mapIBlock) = extractAnonFunc(mapFunc)
            val AnonFunc2(v1, v2, application, iBlock) = extractAnonFunc2(fTree)
            val initBlock = Seq(q"var $accVar = ${init.tree}", iBlock, mapIBlock)
            val body =
              q"""
                $accVar = {
                  val $v1 = $accVar
                  val $v2 = $mapApplication
                  $application
                }
              """

            generateForPrefix(start, end, by, inclusive, initBlock, m1, body, q"$accVar")
        }*/
      }.run(f.tree)

    println(s"Result for ${c.prefix} was: $t")
    c.Expr[B](t)
  }

  def sumImpl[A, B >: A](c: Context { type PrefixType = MappedRange[A] })(num: c.Expr[Numeric[B]]): c.Expr[B] = {
    import c.universe._
    c.Expr[B](q"${c.prefix.tree}.foldLeft(${num.tree}.zero)(${num.tree}.plus)")
    //reify(c.prefix.splice.foldLeft(num.splice.zero)(num.splice.plus))
  }

  /*def sizeImpl(c: Context): c.Expr[Int] = c.Expr[Int] {
    new Helper[c.type](c) with MethodHelper {
      import c.universe._
      override def run = q"($from - $to) / $by"
    }.run
  }*/
  def normalRangeConv(c: Context)(f: c.Expr[FastSteppedRange]): c.Expr[Range] =
    c.Expr[Range] {
      new Helper[c.type](c) with SpeedHelper {

        import c.universe._

        override def run = {
          val (start, end, by, inclusive) = matchConstructor(f.tree)
          if (inclusive) q"Range.inclusive($start, $end, $by)"
          else q"Range($start, $end, $by)"
        }

      }.run
    }
  def mappedRangeImpl[U](c: Context)(f: c.Expr[MappedRange[U]]): c.Expr[Seq[U]] =
    c.Expr[Seq[U]] {
      new Helper[c.type](c) with SpeedHelper {

        import c.universe._

        override def run = f.tree match {
          case q"$expr.map[${ _ }]($mapFunc)" ⇒
            val (start, end, by, inclusive) = matchConstructor(expr)

            if (inclusive) q"Range.inclusive($start, $end, $by).map($mapFunc)"
            else q"Range($start, $end, $by).map($mapFunc)"
        }

      }.run
    }

  def rangeForeachImpl(c: Context)(range: c.Expr[Any]): c.Expr[(List[Int], List[Int])] = {
    val r = range.asInstanceOf[c.Expr[Range]]
    c.universe.reify {
      import speed._
      val buffer = new scala.collection.mutable.ListBuffer[Int]
      var count = 0
      val x = 12
      r.splice.foreach { element ⇒
        val gap = x
        require(count < 1000, "Too many iterations needed")
        count += 1
        buffer += element
      }
      //println(c.literal(range.toString).splice, buffer.toList, r.splice.iterator.toList, buffer.toList + "/" + r.splice.iterator.toList)
      assert(buffer.toList == r.splice.iterator.toList, buffer.toList + "/" + r.splice.iterator.toList)
      (buffer.toList, r.splice.iterator.toList)
    }
  }

  def arrayForeachImpl[T, U](c: Context)(f: c.Expr[T ⇒ U]): c.Expr[Unit] =
    c.Expr[Unit] {
      val t =
        new Helper[c.type](c) with SpeedHelper {

          import c.universe._

          override def run = {
            val AnonFunc(valName, application, init) = extractAnonFunc(f.tree)

            val array = c.prefix.tree match {
              case q"$x.refArrayOps[$y]($array)" ⇒ array
              case q"$x.wrapIntArray($array)"    ⇒ array
            }
            val arrayVar = c.fresh(newTermName("array"))
            val idxVar = c.fresh(newTermName("idx"))
            q"""
            import speed._
            $init
            val $arrayVar = $array
            (0 until $arrayVar.length).foreach { $idxVar ⇒
               val $valName = $arrayVar($idxVar)
               $application
            }
          """
          }

        }.run

      println(s"Generated :$t")
      t
    }
}

trait SpeedHelper { self: QuasiquoteCompat ⇒
  import c.universe._

  def matchConstructor(tree: Tree): (Tree, Tree, Tree, Boolean) = tree match {
    case q"${ _ }.intWrapper($from).to($to)"               ⇒ (from, to, c.literal(1).tree, true)
    case q"${ _ }.intWrapper($from).to($to).by($by)"       ⇒ (from, to, by, true)
    case q"${ _ }.intWrapper($from).until($until)"         ⇒ (from, until, c.literal(1).tree, false)
    case q"${ _ }.intWrapper($from).until($until).by($by)" ⇒ (from, until, by, false)
  }

  final def generateForCallChain(tree: Tree, init: Seq[Tree], varName: TermName, application: Tree, blockExpr: Tree): Tree = {
    tree match {
      case q"$expr.map[${ _ }]($mapFunc)" ⇒
        val AnonFunc(m1, mapApplication, mapInit) = extractAnonFunc(mapFunc)
        val tempVar = c.fresh(newTermName("temp"))
        val body =
          q"""
            val $tempVar = $mapApplication

            {
              val $varName = $tempVar
              $application
            }
          """

        generateForCallChain(expr, init :+ mapInit, m1, body, blockExpr)
      case q"$expr.flatMap[${ _ }]($name => $rangeFunc)" ⇒
        //val AnonFunc(m1, mapApplication, mapInit) = extractAnonFunc(flatmapFunc)

        val innerLoop =
          generateForCallChain(rangeFunc, Nil, varName, application, q"()")
        println(s"Inner loop was: $innerLoop")

        /*
        val body =
          q"""
            val $varName = $mapApplication
            $application
          """
          */

        generateForCallChain(expr, init, name.name, innerLoop, blockExpr)
      case expr ⇒
        val (start, end, by, inclusive) = matchConstructor(expr)
        generateGeneral(start, end, by, inclusive, init, varName, application, blockExpr)
    }
  }

  def generateGeneral(start: Tree, end: Tree, step: Tree, isInclusive: Boolean, init: Seq[Tree], varName: TermName, application: Tree, blockExpr: Tree = q"()"): Tree = {
    val upOp = newTermName(if (isInclusive) "$less$eq" else "$less")
    val downOp = newTermName(if (isInclusive) "$greater$eq" else "$greater")
    val startVar = c.fresh(newTermName("start"))
    val endVar = c.fresh(newTermName("end"))
    val stepVar = c.fresh(newTermName("step"))

    // a variable whose value decides which implementation to use
    //  1: count up and compare with `<` / `<=`
    // -1: count down and compare with `>` / `>=`
    //  0: safe version, if abs(step) != 1, to make sure not to overflow the bounds
    // we invoke that here eagerly and use our handy partial evaluation implementation
    // to figure out if we can decide now or not
    // if we can't figure it out now, we choose the variant which always works
    val deciderVar =
      partiallyEvaluate(
        q"""
          val $startVar = $start
          val $endVar = $end
          val $stepVar = $step
          $stepVar match {
            case 0 => throw new IllegalArgumentException("step cannot be 0.")
            case 1 => 1
            case -1 => -1
            case _ =>
              if ($stepVar > 0)
                if ($endVar.toLong + $stepVar > Int.MaxValue) 0 // overflow looming
                else 1
              else
                if ($endVar.toLong + $stepVar < Int.MinValue) 0 // overflow looming
                else -1
          }""") match {
          case l @ Literal(Constant(x)) ⇒ l
          case x @ _                    ⇒ Literal(Constant(0))
        }

    q"""
      ..$init
      val $startVar = $start
      val $endVar = $end
      val $stepVar = $step

      $deciderVar match {
        case 1 => // count up
          var $varName = $startVar

          while ($varName $upOp $endVar) {
            $application
            $varName += $stepVar
          }

        case -1 => // count down
          var $varName = $startVar
          while ($varName $downOp $endVar) {
            $application
            $varName += $stepVar
          }

        case 0 => // don't count but play it safe because of potential overflows at the integer bounds
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
          val terminalElement = ($startVar.toLong + numRangeElements * $stepVar).toInt

          var $varName = $startVar
          while ($varName != terminalElement) {
            $application
            $varName += $stepVar
          }
      }
      $blockExpr
    """
  }

  def partiallyEvaluate(tree: Tree): Tree = {
    trace(s"Input to partially: $tree")
    c.resetAllAttrs(new ConstantFolder().transform(c.typeCheck(c.resetAllAttrs(tree))))
  }

  def trace(msg: String): Unit = {}

  /**
   * This partial evaluator has no sensible notion of lexical scopes, so be careful
   */
  class ConstantFolder(initEnv: Map[Symbol, Constant] = Map.empty) extends Transformer {
    var environmentStack = collection.immutable.Stack[Map[Symbol, Constant]](Map.empty)

    def createBinding(sym: Symbol, value: Constant): Unit = {
      val top = environmentStack.head
      assert(!top.contains(sym))
      environmentStack = environmentStack.pop.push(top + (sym -> value))
    }

    def envContains(sym: Symbol): Boolean = environmentStack.exists(_.contains(sym))
    def lookup(sym: Symbol): Constant = environmentStack.find(_.contains(sym)).get(sym)
    def pushContext() = environmentStack = environmentStack.push(Map.empty)
    def popContext() = environmentStack = environmentStack.pop

    override def transform(tree: Tree): Tree = {
      def binaryOp(e1: Tree, e2: Tree, op: TermName)(calc: ((Any, Any)) ⇒ Any) =
        (transform(e1), transform(e2)) match {
          case (Literal(Constant(a)), Literal(Constant(b))) ⇒ Literal(Constant(calc((a, b))))
          case (x1, x2)                                     ⇒ q"$x1 $op $x2"
        }
      def unaryOp(e1: Tree, op: TermName)(calc: Any ⇒ Any) =
        transform(e1) match {
          case lit @ Literal(Constant(a)) ⇒ Literal(Constant(calc(a)))
          case x                          ⇒ q"$x.$op"
        }

      tree match {
        /*case Block(stats, expr) ⇒
                  pushContext()
                  val res = super.transform(tree)
                  popContext()
                  res*/
        case v @ Ident(name) if envContains(v.symbol) ⇒
          trace(s"Replaced constant binding for $name")
          Literal(lookup(v.symbol))

        case v @ q"val $x = $expr" ⇒
          //trace(s"Trying to figure out value of $x ($expr), env has values for ${env.keys.mkString(", ")}")
          transform(expr) match {
            case lit @ Literal(constant) ⇒
              trace(s"Found literal binding for ${v.symbol}: $constant")
              createBinding(v.symbol, constant)
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
          binaryOp(e1, e2, newTermName("$percent")) {
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

  case class AnonFunc(valName: TermName, application: Tree, init: Tree)

  def extractAnonFunc(fTree: Tree): AnonFunc =
    c.resetAllAttrs(fTree) match {
      // try to find literal anonymous functions
      case q"( $i => $body )"             ⇒ AnonFunc(i.name, q"{ $body }", q"")
      // this matches partial evaluation (like `println _`)
      case Block(Nil, q"( $i => $body )") ⇒ AnonFunc(i.name, q"{ $body }", q"")
      case _ ⇒
        val fun = c.fresh(newTermName("funInit"))
        val iVar = c.fresh(newTermName("i"))
        AnonFunc(iVar, q"$fun($iVar)", q"val $fun = $fTree")
    }

  case class AnonFunc2(valName1: TermName, valName2: TermName, application: Tree, init: Tree)

  def extractAnonFunc2(fTree: Tree): AnonFunc2 =
    c.resetAllAttrs(fTree) match {
      // try to find literal anonymous functions
      case q"( ($i1, $i2) => $body )"             ⇒ AnonFunc2(i1.name, i2.name, q"{ $body }", q"")
      // this matches partial evaluation (like `println _`)
      case Block(Nil, q"( ($i1, $i2) => $body )") ⇒ AnonFunc2(i1.name, i2.name, q"{ $body }", q"")
      case _ ⇒
        val fun = c.fresh(newTermName("funInit"))
        AnonFunc2(newTermName("i1"), newTermName("i2"), q"$fun(i1, i2)", q"val $fun = $fTree")
    }

}

trait MethodHelper extends SpeedHelper { self: QuasiquoteCompat ⇒
  val (start, end, by, inclusive) = matchConstructor(c.prefix.tree)
}

abstract class Helper[C <: Context](val c: C) extends QuasiquoteCompat {
  import c.universe._
  type Tr = Tree

  def run(fTree: Tree): Tree = ???
  def run: Tree = ???
  def old(fTree: Tr): Tr = ???
}
