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
  }
  def analyzeGen(t: Tree): Generator = t match {
    case q"$inner.map[..${ _ }]($f)"        ⇒ MappingGenerator(analyzeGen(inner), closure1(f))
    case q"$inner.filter[..${ _ }]($f)"     ⇒ FilteringGenerator(analyzeGen(inner), closure1(f))
    case q"$inner.withFilter[..${ _ }]($f)" ⇒ FilteringGenerator(analyzeGen(inner), closure1(f))
    case q"$inner.reverse"                  ⇒ ReverseGenerator(analyzeGen(inner))
    case q"$inner.flatMap[..${ _ }]($f)" ⇒
      val Closure(valName, q"$innerGeneratorTree: @speed.dontfold", _) = closure1(f)

      FlatMappingGenerator(analyzeGen(inner), valName, analyzeGen(innerGeneratorTree))

    case q"${ _ }.RangesAreSpeedy($r).speedy" ⇒ range(r)
    case q"${ _ }.ArraysAreSpeedy[..${ _ }]($a).speedy" ⇒ ArrayGenerator(q"$a: @speed.dontfold")
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
  def closure2(fTree: Tree): Closure2 =
    fTree match {
      // try to find literal anonymous functions
      case q"( ($i1, $i2) => $body )"             ⇒ Closure2(i1.name, i2.name, q"{ $body }: @speed.dontfold()", q"")
      // this matches partial evaluation (like `println _`)
      case Block(Nil, q"( ($i1, $i2) => $body )") ⇒ Closure2(i1.name, i2.name, q"{ $body }: @speed.dontfold()", q"")
      case _ ⇒
        val fun = c.fresh(newTermName("funInit"))
        Closure2(newTermName("i1"), newTermName("i2"), q"$fun(i1, i2)", q"val $fun = $fTree")
    }
}
