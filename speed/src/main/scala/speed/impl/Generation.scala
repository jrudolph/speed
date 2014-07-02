package speed.impl

trait Generation extends RangeGeneration with ListGeneration { self: SpeedImpl ⇒
  import c.universe._
  case class GeneratorSetup(inits: Seq[Tree], body: Tree) {
    def prependInits(inits: Seq[Tree]): GeneratorSetup = copy(inits = inits ++ this.inits)
  }
  object GeneratorSetup {
    implicit def treeToSetup(t: Tree): GeneratorSetup = GeneratorSetup(Nil, t)
  }
  case class TerminalOperationSetup(inits: Seq[Tree], inner: Tree, result: Tree)

  def generate(chain: OperationChain): Tree = {
    val cancelVar = c.fresh(newTermName("cancel$"))
    val varName = newTermName(c.fresh("value$"))

    val term = generateTerminal(chain.terminal, varName, cancelVar)
    //println(s"Term: $term")
    val GeneratorSetup(genInits, gen) = generateGen(chain.generator, varName, term.inner, cancelVar)
    //println(s"Gen: $gen")

    q"""
      var $cancelVar = false
      ..${genInits ++ term.inits}

      $gen

      ${term.result}
    """
  }

  def generateGen(gen: Generator, expectedValName: TermName, application: Tree, cancelVar: TermName): GeneratorSetup = gen match {
    case RangeGenerator(start, end, by, incl) ⇒ generateRange(start, end, by, incl, expectedValName, application, cancelVar)
    case ListGenerator(l, tpe)                ⇒ generateList(l, tpe, expectedValName, application, cancelVar)
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

      generateGen(outer, tempName, body, cancelVar)

    case FlatMappingGenerator(outer, innerValName, innerGenerator) ⇒
      val GeneratorSetup(inits, innerLoop) = generateGen(innerGenerator, expectedValName, application, cancelVar /* FIXME: is this correct? */ )
      generateGen(outer, innerValName, innerLoop, cancelVar).prependInits(inits)

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

      generateGen(outer, tempName, body, cancelVar)

    case InitAddingGenerator(outer, inits) ⇒ generateGen(outer, expectedValName, application, cancelVar).prependInits(inits)

    //case _ => q"()"
  }
  def generateTerminal(terminal: TerminalOperation, valName: TermName, cancelVar: TermName): TerminalOperationSetup = terminal match {
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
      def b(b: Boolean) = Literal(Constant(b))
      val a1Type = tpe
      val neutralA1 = neutralElement(tpe)
      val inits = Seq(
        q"var $accVar: $a1Type = $neutralA1",
        q"var $emptyVar = ${b(true)}",
        f.init)
      val body =
        q"""
          if ($emptyVar) {
            $emptyVar = ${b(false)}
            $accVar = $valName
          } else
            $accVar = {
              val ${f.valName1} = $accVar
              val ${f.valName2} = $valName
              ${f.application}
            }
        """

      val result =
        q"""
              if ($emptyVar) throw new UnsupportedOperationException("Can't reduce empty range")
              else $accVar
            """

      TerminalOperationSetup(inits, body, result)

    case MkString ⇒
      val sbVar = c.fresh(newTermName("sb$"))
      val init = q"val $sbVar = new java.lang.StringBuilder"
      val body = q"$sbVar.append($valName.toString)"
      val result = q"$sbVar.toString"

      TerminalOperationSetup(Seq(init), body, result)

    case To(cbf) ⇒
      val builderVar = c.fresh(newTermName("builder$"))
      val init = q"val $builderVar = $cbf()"
      val body = q"$builderVar += $valName"
      val result = q"$builderVar.result()"

      TerminalOperationSetup(Seq(init), body, result)

    case Forall(f) ⇒
      val resultVar = c.fresh(newTermName("result$"))
      val init = q"var $resultVar = true"
      val body = q"""
        $resultVar = {
          val ${f.valName} = $valName
          ${f.application}
        }
        $cancelVar = !$resultVar
      """
      val result = q"$resultVar"

      TerminalOperationSetup(Seq(init), body, result)
  }
}