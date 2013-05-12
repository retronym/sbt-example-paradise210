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
      val tempName: TermName = newTermName(c.fresh("temp$"))
      val b = q"""
        val $tempName = $qual
        if ($tempName eq null)
          null
        else
          $tempName.$name
      """
      c.typeCheck(b)
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case sel @ Select(qual, name: TermName)
             if typeOf[Null] <:< qual.tpe => guardedSelect(sel, transform(qual), name)
        case x                            => super.transform(tree)
      }
    }
  }
}