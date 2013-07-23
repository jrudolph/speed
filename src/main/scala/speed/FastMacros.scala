package speed

import scala.reflect.macros.Context

object FastMacros {
  def foreachImpl[T](c: Context)(f: c.Expr[Int ⇒ T]): c.Expr[Unit] = {

    val t =
      new Helper[c.type](c) {
        import c.universe._

        def run(fTree: Tree): Tree = {
          val q"${ _ }.intWrapper($from).to($to)" = c.prefix.tree
          println(s"From: $from to: $to")

          println(c.universe.showRaw(fTree))
          println(c.universe.showRaw(q"{{{ x => { println(x) } }}}"))
          val (valName: TermName, application: Tree) =
            c.resetAllAttrs(fTree) match {
              case q"( $i => $body )"             ⇒ (i.name, q"{ $body }")
              case Block(Nil, q"( $i => $body )") ⇒ (i.name, q"{ $body }")
              case _                              ⇒ (newTermName("i"), q"$fTree(i)")
            }

          q"var $valName = $from; while($valName <= $to) { $application; $valName+=1 }"
        }
      }.run(f.tree)
    println(s"Result was: $t")
    c.Expr[Unit](t)
  }
}

abstract class Helper[C <: Context](val c: C) extends QuasiquoteCompat {
  import c.universe._
  def run(fTree: Tree): Tree
}
