object Test extends App {
  Macros.hello

  object O {
    class C {
      class D {
        def e = "e"
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
}