package speed
package impl

import scala.reflect.macros.Context
import scala.annotation.tailrec
import scala.reflect.ClassTag

object SpeedMacros {
  def trace(msg: String) {}

  def foreachImpl[T](c: Context)(f: c.Expr[Int ⇒ T]): c.Expr[Unit] = {
    val t =
      new Helper[c.type](c) with SpeedHelper {
        import c.universe._

        def run: Tree = {
          val AnonFunc(valName, application, init) = extractAnonFunc(f.tree)

          finish(generateForCallChain(c.prefix.tree, Seq(init), valName, application, q"()"))
        }
      }.run

    c.Expr[Unit](t)
  }

  def foldLeftImpl[A, B](c: Context)(init: c.Expr[B])(f: c.Expr[(B, A) ⇒ B]): c.Expr[B] = {
    val t =
      new Helper[c.type](c) with SpeedHelper {
        import c.universe._

        def run: Tree = {
          val accVar = c.fresh(newTermName("acc"))
          val AnonFunc2(v1, v2, application, funcInit) = extractAnonFunc2(f.tree)
          val inits = Seq(q"var $accVar = ${init.tree}", funcInit)
          val body =
            q"""
              $accVar = {
                val $v1 = $accVar
                $application
              }
            """

          finish(generateForCallChain(c.prefix.tree, inits, v2, body, q"$accVar"))
        }
      }.run

    c.Expr[B](t)
  }

  def sumImpl[A, B >: A](c: Context { type PrefixType = OptimizedColl[A] })(num: c.Expr[Numeric[B]]): c.Expr[B] = {
    import c.universe._
    c.Expr[B](q"${c.prefix.tree}.foldLeft(${num.tree}.zero)(${num.tree}.plus)")
  }
  def sizeImpl[A](c: Context { type PrefixType = OptimizedColl[A] }): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"${c.prefix.tree}.foldLeft(0)((num, _) => num + 1)")
  }
  def reduceImpl[A1: c.WeakTypeTag](c: Context)(op: c.Expr[(A1, A1) ⇒ A1]): c.Expr[A1] = {
    import c.universe._
    c.Expr[A1] {
      new Helper[c.type](c) with SpeedHelper {
        def run: Tree = {
          val accVar = c.fresh(newTermName("acc"))
          val emptyVar = c.fresh(newTermName("empty"))
          val AnonFunc2(v1, v2, application, funcInit) = extractAnonFunc2(op.tree)
          def b(b: Boolean) = Literal(Constant(b))
          val a1Type = c.weakTypeOf[A1]
          val neutralA1 = neutralElement[A1]
          val inits = Seq(
            q"var $accVar: $a1Type = $neutralA1",
            q"var $emptyVar = ${b(true)}",
            funcInit)
          val body =
            q"""
                if ($emptyVar) {
                  $emptyVar = ${b(false)}
                  $accVar = $v2
                } else
                  $accVar = {
                    val $v1 = $accVar
                    $application
                  }
              """

          val result =
            q"""
              if ($emptyVar) throw new UnsupportedOperationException("Can't reduce empty range")
              else $accVar
            """

          finish(generateForCallChain(c.prefix.tree, inits, v2, body, result))
        }
      }.run
    }
  }

  def normalRangeConv(c: Context)(f: c.Expr[OptimizedColl[Int]]): c.Expr[Range] =
    c.Expr[Range] {
      new Helper[c.type](c) with SpeedHelper {

        import c.universe._

        def run = {
          val (start, end, by, inclusive) = matchConstructor(f.tree)
          if (inclusive) q"Range.inclusive($start, $end, $by)"
          else q"Range($start, $end, $by)"
        }

      }.run
    }
  def mappedRangeImpl[U](c: Context)(f: c.Expr[OptimizedColl[U]]): c.Expr[Seq[U]] =
    c.Expr[Seq[U]] {
      new Helper[c.type](c) with SpeedHelper {

        import c.universe._

        def run = f.tree match {
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

  def arrayOpImpl[T, U, R](c: Context)(f: c.Expr[T ⇒ U]): c.Expr[R] =
    c.Expr[R](genericArrayOpImpl(c)(f.tree))

  def genericArrayOpImpl[T, U, R](c: Context)(f: c.Tree): c.Tree =
    new Helper[c.type](c) with SpeedHelper {

      import c.universe._

      override def run = {
        val (array, targs, op) = c.macroApplication match {
          case q"$x.arrayOps[$y]($array).$op[..$targs](..${ _ })"  ⇒ (array, targs, op)
          case q"$x.wrapIntArray($array).$op[..$targs](..${ _ })"  ⇒ (array, targs, op)
          case q"$x.wrapLongArray($array).$op[..$targs](..${ _ })" ⇒ (array, targs, op)
        }
        val arrayVar = c.fresh(newTermName("array_temp"))
        q"""
              val $arrayVar = $array
              (_root_.speed.intWrapper(0) until $arrayVar.length).map($arrayVar(_)).$op[..$targs]($f)
            """
      }
    }.run

  def arraySumImpl[T](c: Context)(num: c.Expr[Numeric[T]]): c.Expr[T] =
    c.Expr[T](genericArrayOpImpl(c)(num.tree))

  def showTree[T](c: Context)(t: c.Expr[T]): c.Expr[T] = { println(s"Show '${c.universe.show(t)}'"); t }
}

trait SpeedHelper extends ConstantFolding { self: QuasiquoteCompat ⇒
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
      case q"$expr.filter($filterFunc)" ⇒
        val AnonFunc(f1, filterApplication, filterInit) = extractAnonFunc(filterFunc)
        val body =
          q"""
             if ($filterApplication) {
               val $varName= $f1
               $application
             }
          """

        generateForCallChain(expr, init :+ filterInit, f1, body, blockExpr)
      case q"$expr.flatMap[${ _ }]($name => $rangeFunc)" ⇒
        val innerLoop =
          generateForCallChain(rangeFunc, Nil, varName, application, q"()")

        generateForCallChain(expr, init, name.name, innerLoop, blockExpr)

      case Block(stats, expr) ⇒
        generateForCallChain(expr, init ++ stats, varName, application, blockExpr)

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

    q"""
      ..$init

      $body

      $blockExpr
    """
  }

  def trace(msg: String): Unit = {}

  case class AnonFunc(valName: TermName, application: Tree, init: Tree)

  def extractAnonFunc(fTree: Tree): AnonFunc =
    c.resetAllAttrs(fTree) match {
      // try to find literal anonymous functions
      case q"( $i => $body )"             ⇒ AnonFunc(i.name, q"{ $body }: @speed.dontfold()", q"")
      //case q"( ($i: ${ _ }) => $body )"   ⇒ AnonFunc(i.asInstanceOf[ValDef].name, q"{ $body }", q"")
      // this matches partial evaluation (like `println _`)
      case Block(Nil, q"( $i => $body )") ⇒ AnonFunc(i.name, q"{ $body }: @speed.dontfold()", q"")
      case _ ⇒
        c.warning(fTree.pos, s"Couldn't extract anonymous function implementation here. '$fTree'")
        val fun = c.fresh(newTermName("funInit"))
        val iVar = c.fresh(newTermName("i"))
        AnonFunc(iVar, q"$fun($iVar)", q"val $fun = $fTree")
    }

  case class AnonFunc2(valName1: TermName, valName2: TermName, application: Tree, init: Tree)

  def extractAnonFunc2(fTree: Tree): AnonFunc2 =
    c.resetAllAttrs(fTree) match {
      // try to find literal anonymous functions
      case q"( ($i1, $i2) => $body )"             ⇒ AnonFunc2(i1.name, i2.name, q"{ $body }: @speed.dontfold()", q"")
      // this matches partial evaluation (like `println _`)
      case Block(Nil, q"( ($i1, $i2) => $body )") ⇒ AnonFunc2(i1.name, i2.name, q"{ $body }: @speed.dontfold()", q"")
      case _ ⇒
        val fun = c.fresh(newTermName("funInit"))
        AnonFunc2(newTermName("i1"), newTermName("i2"), q"$fun(i1, i2)", q"val $fun = $fTree")
    }

  lazy val IntTag = c.weakTypeOf[Int]
  lazy val LongTag = c.weakTypeOf[Long]
  lazy val FloatTag = c.weakTypeOf[Float]
  lazy val DoubleTag = c.weakTypeOf[Double]
  lazy val ShortTag = c.weakTypeOf[Short]
  lazy val ByteTag = c.weakTypeOf[Byte]
  lazy val BooleanTag = c.weakTypeOf[Boolean]
  lazy val CharTag = c.weakTypeOf[Char]
  def neutralElement[A1: c.WeakTypeTag]: Tree =
    lit(c.universe.weakTypeOf[A1] match {
      case IntTag     ⇒ 0
      case LongTag    ⇒ 0L
      case FloatTag   ⇒ 0f
      case DoubleTag  ⇒ 0d
      case ShortTag   ⇒ 0.toShort
      case ByteTag    ⇒ 0.toByte
      case BooleanTag ⇒ false
      case CharTag    ⇒ 0.toChar
      case _          ⇒ null
    })
  def lit(v: Any) = Literal(Constant(v))
}

trait MethodHelper extends SpeedHelper { self: QuasiquoteCompat ⇒
  val (start, end, by, inclusive) = matchConstructor(c.prefix.tree)
}

abstract class Helper[C <: Context](val c: C) extends QuasiquoteCompat {
  import c.universe._

  def run: Tree

  def runAndShow = {
    val res = run
    println(res)
    res
  }
}
