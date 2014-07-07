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

package net.virtualvoid.macros.tools

import net.virtualvoid.macros.tools

import scala.reflect.macros.Context
import scala.reflect.internal.annotations.compileTimeOnly

trait Reifier extends WithContext {
  import c.universe._
  trait Expr[+T] {
    @compileTimeOnly("splice can only be used inside of reify")
    def splice: T = ???
    def tree: Tree
  }

  implicit def autoConv[T](exp: c.Expr[T]): Expr[T] = new Expr[T] { def tree = exp.tree }
  implicit def autoConvReverse[T](e: Expr[T]): c.Expr[T] = c.Expr[T](e.tree)
  implicit def convToUnit[T](exp: Expr[T]): Expr[Unit] = new Expr[Unit] { def tree = exp.tree }

  @compileTimeOnly("reified can only be used inside of reify")
  implicit def Reified[T](any: T): { def reified: Expr[T] } = ???

  def Expr[T](t: Tree): Expr[T] = new Expr[T] { def tree = t }
  def reify[T](t: T): Expr[T] = macro ReifierImpl.reifyImpl[T]
  def reifyShow[T](t: T): Expr[T] = macro ReifierImpl.reifyShowImpl[T]

  @compileTimeOnly("reifyInner can only be used inside of reify")
  def reifyInner[T](t: T): Expr[T] = ???
}

object ReifierImpl {
  def reifyShowImpl[T: c.WeakTypeTag](c: Context { type PrefixType = Reifier })(t: c.Expr[T]): c.Expr[c.prefix.value.Expr[T]] = {
    val res = reifyImpl(c)(t)
    c.info(t.tree.pos, s"For '${t.tree}': ${c.universe.show(res)}", false)
    res
  }
  def reifyImpl[T: c.WeakTypeTag](c: Context { type PrefixType = Reifier })(t: c.Expr[T]): c.Expr[c.prefix.value.Expr[T]] = {
    import c.universe._

    case class PlaceholderDef(orig: Tree, args: Seq[Tree], tpes: Seq[Type])
    var placeholders = Map.empty[TermName, PlaceholderDef]
    def addPlaceholder(name: TermName, ph: PlaceholderDef): Unit =
      placeholders = placeholders.updated(name, ph)

    object InnerReify {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case q"${ _ }.reifyInner[..${ _ }]($exp)"      ⇒ Some(exp)
        case q"${ _ }.Reified[..${ _ }]($exp).reified" ⇒ Some(exp)
        case _                                         ⇒ None
      }
    }

    object RemoveInnerReify extends Traverser {
      var args: Seq[Tree] = _
      var tpes: Seq[Type] = _

      override def traverse(tree: Tree): Unit = tree match {
        case InnerReify(exp) ⇒
          args = args :+ CreatePlaceholders.transform(exp)
          tpes = tpes :+ exp.tpe
        case _ ⇒ super.traverse(tree)
      }

      def run(t: Tree): PlaceholderDef = {
        args = Seq.empty
        tpes = Seq.empty
        traverse(t)
        PlaceholderDef(t, args, tpes)
      }
    }

    object CreatePlaceholders extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case q"$expr.splice" ⇒
          val name = c.fresh(newTermName("placeholder$"))
          val placeholder = RemoveInnerReify.run(expr)
          addPlaceholder(name, placeholder)

          //println(s"Found splice for $expr: $placeholder")

          q"$name(..${placeholder.args})"

        case _ ⇒ super.transform(tree)
      }
    }

    class FindDefinitions extends Traverser {
      var definitions = Set.empty[Symbol]

      override def traverse(tree: Tree): Unit = tree match {
        case v: ValDef ⇒
          definitions += v.symbol
          traverse(v.rhs)
        case _ ⇒ super.traverse(tree)
      }

      def run(t: Tree): Set[Symbol] = {
        traverse(t)
        definitions
      }
    }

    class HygienifyDefs(defs: Map[Symbol, TermName]) extends Transformer {
      override def transform(t: Tree): Tree = t match {
        case v @ ValDef(mods, name, tpt, rhs) if defs.contains(v.symbol) ⇒
          ValDef(mods, defs(v.symbol), tpt, transform(rhs))
        case x: Ident if defs.contains(x.symbol) ⇒
          //println(s"Replaced Ident($x)")
          Ident(defs(x.symbol))
        case s: Select if defs.contains(s.symbol) ⇒
          //println(s"Replaced Select($s)")
          Select(s.qualifier, defs(s.symbol))
        case _ ⇒ super.transform(t)
      }
    }

    val withPlaceholders = CreatePlaceholders.transform(t.tree)

    val allDefs = (new FindDefinitions).run(withPlaceholders)
    //println(s"Found defs: $allDefs in $t")

    val newNames = allDefs.map { s ⇒
      s -> c.fresh(newTermName(s.asTerm.name.asInstanceOf[TermName].decoded + "$"))
    }.toMap

    val freshenized = (new HygienifyDefs(newNames)).transform(withPlaceholders)
    val justTheNames = newNames.values.toSet

    val univ = c.typeCheck(q"${c.prefix}.c.universe")

    //println(s"Before reification $freshenized")
    val reified = c.reifyTree(univ, EmptyTree, freshenized)
    //println(s"Reified: $reified")

    val pref = c.prefix
    def buildExpr[T: c.WeakTypeTag](t: Tree): Tree = q"new $pref.Expr[${c.weakTypeTag[T]}] { val tree = $t.asInstanceOf[$pref.c.universe.Tree] }"

    val justTheBuilder = reified match {
      case Block(_, Apply(Apply(_, List(_, Block(List(ClassDef(_, _, _, Template(_, _, List(_, DefDef(_, _, _, _, _, Block(_, justTheBuilder)))))), _))), _)) ⇒ justTheBuilder
    }
    def extractUniverse(t: Tree): TermName = t match {
      case Apply(fun, _)   ⇒ extractUniverse(fun)
      case Select(qual, _) ⇒ extractUniverse(qual)
      case Ident(x)        ⇒ x.toTermName
    }
    val v = extractUniverse(justTheBuilder)

    //println(s"Just the builder: ${justTheBuilder.productPrefix} $v $justTheBuilder")

    class InsertInnerReifies extends Transformer {
      var args = Seq.empty[Tree]
      var tpes = Seq.empty[Type]
      override def transform(tree: Tree): Tree = tree match {
        case InnerReify(_) ⇒
          val res = ReplacePlaceholder.transform(args(0))
          val tpe = tpes(0)
          args = args.tail
          tpes = tpes.tail

          buildExpr(res)(c.WeakTypeTag(tpe.widen))
        case _ ⇒ super.transform(tree)
      }

      def run(ph: PlaceholderDef, results: Seq[Tree]): Tree = {
        args = results
        tpes = ph.tpes
        transform(ph.orig)
      }
    }

    object NewTermName {
      def unapply(tree: Tree): Option[String] = tree match {
        // Scala 2.10
        case q"${ _ }.newTermName(${ Literal(Constant(name: String)) })" ⇒ Some(name)
        // Scala 2.11
        case q"${ _ }.TermName(${ Literal(Constant(name: String)) })" ⇒ Some(name)
        case _ ⇒ None
      }
    }

    object ReplacePlaceholder extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case q"${ _ }.Apply(${ _ }.Ident(${ NewTermName(name) }), ${ _ }.List.apply(..$args))" if name.startsWith("placeholder$") ⇒
          val before = placeholders(newTermName(name))
          val placed = (new InsertInnerReifies).run(before, args)

          //println(s"Found placeholder!!! $name\nBefore: $before\nAfter: $placed")
          q"${placed}.tree.asInstanceOf[$$u.Tree]"
        case _ ⇒ super.transform(tree)
      }
    }

    val replaced = ReplacePlaceholder.transform(reified)

    def createFreshName(name: TermName): Tree = q"val $name = ${c.prefix}.c.fresh(${c.prefix}.c.universe.newTermName(${name.decoded + "$"}))"
    object ReplaceFreshNames extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case NewTermName(name) if justTheNames(name) ⇒
          //println(s"Found instance of $name: $tree")
          q"${Ident(name)}.asInstanceOf[$$u.TermName]"
        case _ ⇒ super.transform(tree)
      }
    }

    val withFreshNames =
      q"""
      ..${justTheNames.toSeq.map(createFreshName(_))}

      ${ReplaceFreshNames.transform(replaced)}
    """

    c.Expr[c.prefix.value.Expr[T]](atPos(t.tree.pos)(c.resetLocalAttrs(withFreshNames)))
  }
}
