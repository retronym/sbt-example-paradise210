object Test extends App {
  Macros.hello

  object O {
    class C {
      class D {
        def e = "e"
        def g = throw new java.lang.NullPointerException
      }
      def dNull: D = null
      def d = new D
    }
    def cNull: C = null
    def c = new C
  }
  val e: String = Macros.elvis(O.c.d.e)
  val e1: String = Macros.elvis(O.cNull.d.e)
  println(e1)
  val e2: String = Macros.elvis(O.c.dNull.e)
  println(e2)

  try {
    println(Macros.elvis(O.c.d.g))
    println("NPE should not have been caught.")
  } catch {
    case _: java.lang.NullPointerException => // okay
  }

  Macros.elvis({ def id[A](a: A) = a; identity(O.c.dNull.e) })

  // Demonstrating the lack of hygiene in the elvis macro;
  // `Ident(newTypeName("NullPointerException"))` will bind to
  // this class, rather than `j.l.NPE`!
  //
  // Making this class `final` means that `catch { case _: Test.NPE => }`
  // results in a compile error, rather than just a warning.
  final class NullPointerException
}
