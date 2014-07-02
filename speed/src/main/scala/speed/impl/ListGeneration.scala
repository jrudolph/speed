package speed.impl

trait ListGeneration { self: SpeedImpl ⇒
  import c.universe._

  def generateList(l: Tree, listTpe: Type, expectedValName: TermName, application: Tree, cancelVar: TermName): Tree = {
    val curName = c.fresh(newTermName("cur$"))
    val curConsName = c.fresh(newTermName("curCons$"))
    val tpe = TypeTree(listTpe)
    val List = typeOf[List[_]].typeSymbol.asClass
    val ListA = List.typeParams(0).asType.toType
    val innerTpe = TypeTree(ListA.asSeenFrom(listTpe, List))

    val inner = listTpe.asInstanceOf[Type]

    q"""
      var $curName = $l
      while ($curName.isInstanceOf[_root_.scala.collection.immutable.::[_]] && !$cancelVar) {
        val $curConsName = $curName.asInstanceOf[_root_.scala.collection.immutable.::[$innerTpe]]
        var $expectedValName = $curConsName.head
        $application
        $curName = $curConsName.tail
      }
    """
  }
}
