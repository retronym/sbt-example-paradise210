import language.experimental.macros
import scala.reflect.macros.Context

object Macros {
  def hello = macro impl
  def impl(c: Context) = {
    // uses the technique described in:
    // http://docs.scala-lang.org/overviews/macros/overview.html#writing_bigger_macros
    val helper = new Helper[c.type](c)
    c.Expr[Unit](helper.hello)
  }

  def elvis[A >: Null](a: A): A = macro elvisImpl[A]

  def elvisImpl[A >: Null: c.WeakTypeTag](c: Context)(a: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val npe = c.mirror.staticClass("java.lang.NullPointerException")
    val tree =
      Try(a.tree,
          List(
            CaseDef(
              Typed(Ident(nme.WILDCARD), Ident(npe)),
              EmptyTree,
              Literal(Constant(null))
            )
          ),
          EmptyTree
      )
    c.Expr[A](tree)
  }
}