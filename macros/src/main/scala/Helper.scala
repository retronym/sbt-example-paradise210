import scala.reflect.macros.Context

// uses the technique described in:
// http://docs.scala-lang.org/overviews/macros/overview.html#writing_bigger_macros
class Helper[C <: Context](val c: C) extends QuasiquoteCompat {
  import c.universe._

  // to learn more about quasiquotes, visit:
  // http://docs.scala-lang.org/overviews/macros/quasiquotes.html
  def hello = q"""
    println("hello world!")
  """

  def tryCatchNPE(body: Tree): Tree = {
    val npe = c.mirror.staticClass("java.lang.NullPointerException")
    q"try { $body } catch { case _: $npe => null}"
  }

  object elvisTransformer extends Transformer {
    /** val temp$N = qual; if (qual eq null) null else qual.name */
    def guardedSelect(select: Select, qual: Tree, name: TermName): Tree = {
      /** Construct a symbol for a temporary val used to store the result of `qual`. */
      def tempSym0(qual: Tree): Symbol = {
        val tempSym  = currentOwner.newTermSymbol(c.fresh("temp$"))
        val symtab   = c.universe.asInstanceOf[reflect.internal.SymbolTable]
        tempSym.asInstanceOf[symtab.Symbol]
          .setInfo(qual.tpe.asInstanceOf[symtab.Type])
          .setPos(qual.pos.asInstanceOf[symtab.Position])
          .asInstanceOf[Symbol]
      }
      val tempSym = tempSym0(qual)
      def tempRef = Ident(tempSym).setType(qual.tpe).setPos(qual.pos)
      val b = q"""
        ${ValDef(tempSym, qual)}
        if ($tempRef eq null)
          null
        else
          ${treeCopy.Select(select, tempRef, name)}
      """
      c.typeCheck(b)
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case sel @ q"$qual.${name : TermName}"
             if typeOf[Null] <:< qual.tpe => guardedSelect(sel, transform(qual), name)
        case x                            => super.transform(tree)
      }
    }
  }

   // A hack to get the current owner without needing to downcast the
   // macro context to `reflect.macros.runtime.Context`.
   // We initalize our transformer below with this owner. This is
   // only needed if the transformer implementation manually creates
   // the symbols for the temporary vals it extracts.
   def currentOwner: Symbol = {
     val dummyTree = c.typeCheck(reify({def dummy$0 = 0} ).tree)
     dummyTree.collect { case dd: DefDef => dd.symbol.owner }.head
   }
}
