package speed.impl

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
    case q"$inner.count($f)"                     ⇒ OperationChain(analyzeGen(inner), Count(closure1(f)))
    case q"$inner.mkString"                      ⇒ OperationChain(analyzeGen(inner), MkString)
    case q"$inner.to[..${ _ }]($cbf)"            ⇒ OperationChain(analyzeGen(inner), To(cbf))
    case q"$inner.forall($f)"                    ⇒ OperationChain(analyzeGen(inner), Forall(closure1(f)))
  }
  def analyzeGen(t: Tree): Generator = t match {
    case q"$inner.map[..${ _ }]($f)"        ⇒ MappingGenerator(analyzeGen(inner), closure1(f))
    case q"$inner.filter[..${ _ }]($f)"     ⇒ FilteringGenerator(analyzeGen(inner), closure1(f))
    case q"$inner.withFilter[..${ _ }]($f)" ⇒ FilteringGenerator(analyzeGen(inner), closure1(f))
    case q"$inner.reverse"                  ⇒ ReverseGenerator(analyzeGen(inner))
    case q"$inner.flatMap[..${ _ }]($f)" ⇒
      val cl = closure1(f)
      val innerGenTree = cl.application match {
        case q"$innerGeneratorTree: @speed.dontfold" ⇒ innerGeneratorTree
      }

      FlatMappingGenerator(analyzeGen(inner), cl.valName, analyzeGen(innerGenTree))

    case q"${ _ }.RangesAreSpeedy($r).speedy" ⇒ range(r)
    case q"${ _ }.IndexedSeqsAreSpeedy[..${ _ }]($s).speedy" ⇒ IndexedGenerator(q"$s: @speed.dontfold")
    case q"${ _ }.ArraysAreSpeedy[..${ _ }]($a).speedy" ⇒ IndexedGenerator(q"$a: @speed.dontfold")
    case q"${ _ }.ListsAreSpeedy[..${ _ }]($l).speedy" ⇒ ListGenerator(q"$l: @speed.dontfold", l.tpe.widen)

    case _ ⇒ error(s"Unknown Prefix: $t")
  }
  def range(t: Tree): Generator = t match {
    case q"${ _ }.intWrapper($from).to($to)"               ⇒ RangeGenerator(from, to, c.literal(1).tree, q"true")
    case q"${ _ }.intWrapper($from).to($to).by($by)"       ⇒ RangeGenerator(from, to, by, q"true")
    case q"${ _ }.intWrapper($from).until($until)"         ⇒ RangeGenerator(from, until, c.literal(1).tree, q"false")
    case q"${ _ }.intWrapper($from).until($until).by($by)" ⇒ RangeGenerator(from, until, by, q"false")
    case Block(inits, expr)                                ⇒ range(expr).withInits(inits: _*)
    case _ ⇒
      val rangeVar = c.fresh(newTermName("range$"))
      val init = q"val $rangeVar = $t: @speed.dontfold"
      RangeGenerator(q"$rangeVar.start", q"$rangeVar.end", q"$rangeVar.step", q"$rangeVar.isInclusive")
        .withInits(init)
  }
  def closure1(fTree: Tree): Closure = fTree match {
    // try to find literal anonymous functions
    case q"( $i => $body )"             ⇒ Closure(i.name, q"{ ${cleanBody(i, body)} }: @speed.dontfold()", q"")
    //case q"( ($i: ${ _ }) => $body )"   ⇒ AnonFunc(i.asInstanceOf[ValDef].name, q"{ $body }", q"")
    // this matches partial evaluation (like `println _`)
    case Block(Nil, q"( $i => $body )") ⇒ Closure(i.name, q"{ ${cleanBody(i, body)} }: @speed.dontfold()", q"")
    case _ ⇒
      c.warning(fTree.pos, s"Couldn't extract anonymous function implementation here. '$fTree'")
      val fun = c.fresh(newTermName("funInit"))
      val iVar = c.fresh(newTermName("i"))
      Closure(iVar, q"$fun($iVar)", q"val $fun = $fTree")
  }

  /**
   *  Makes sure that references to the parameters of a lambda gets detached from the parameters.
   *  Otherwise, the references won't be correctly re-resolved (to whatever the variable name was now bound to)
   *  when the body is typechecked. Leaving this out will lead to ugly compiler crashes in the backend.
   */
  def cleanBody(valDef: ValDef, body: Tree): Tree = new ClosureCleaner(Set(valDef.symbol)).transform(body)
  def cleanBody(valDef1: ValDef, valDef2: ValDef, body: Tree): Tree = new ClosureCleaner(Set(valDef1.symbol, valDef2.symbol)).transform(body)
  class ClosureCleaner(candidates: Set[Symbol]) extends Transformer {
    override def transform(t: Tree): Tree = t match {
      // make sure to make a clean copy of the ident if the ident refers to a former parameter of the lambda
      case i: Ident if candidates(i.symbol) ⇒ Ident(i.name)
      case _                                ⇒ super.transform(t)
    }
  }

  def closure2(fTree: Tree): Closure2 =
    fTree match {
      // try to find literal anonymous functions
      case q"( ($i1, $i2) => $body )"             ⇒ Closure2(i1.name, i2.name, q"{ ${cleanBody(i1, i2, body)} }: @speed.dontfold()", q"")
      // this matches partial evaluation (like `println _`)
      case Block(Nil, q"( ($i1, $i2) => $body )") ⇒ Closure2(i1.name, i2.name, q"{ ${cleanBody(i1, i2, body)} }: @speed.dontfold()", q"")
      case _ ⇒
        val fun = c.fresh(newTermName("funInit"))
        Closure2("i1", "i2", q"$fun(i1, i2)", q"val $fun = $fTree")
    }
}
