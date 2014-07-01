package speed.impl

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