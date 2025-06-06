package pretty_printer

import org.junit.Test
import org.junit.Assert._

import pretty_printer.Prettyprinter._

class TestsProgram {

  val is0: IndentSpec = Map.empty[Spec, Int];
  val is2: IndentSpec = Map(PROGR -> 2, WHILE -> 5);

  @Test
  def Test_prettyPrintIn_1(): Unit = {
    assertEquals("X", prettyPrintIn(List(Var("X"))))
  }

  @Test
  def Test_prettyPrintIn_2(): Unit = {
    assertEquals("X, Y", prettyPrintIn(List(Var("X"), Var("Y"))))
  }

  @Test
  def Test_prettyPrintOut_1(): Unit = {
    assertEquals("X", prettyPrintOut(List(Var("X"))))
  }

  @Test
  def Test_prettyPrintOut_2(): Unit = {
    assertEquals("X, Y", prettyPrintOut(List(Var("X"), Var("Y"))))
  }

  @Test
  def Test_prettyPrintProgram_1(): Unit = {
    val program: Program =
      Progr(
        List(Var("X")),
        List(
          Set(Var("Y"), Nl),
          While(
            VarExp("X"),
            List(
              Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
              Set(Var("X"), Tl(VarExp("X")))
            )
          )
        ),
        List(Var("Y"))
      );
    assertEquals(
      List(
        "read X",
        "%",
        "  Y := nil ;",
        "  while X do",
        "       Y := (cons (hd X) Y) ;",
        "       X := (tl X)",
        "  od",
        "%",
        "write Y"
      ),
      prettyPrintProgram(program, is2)
    )
  }

  @Test
  def Test_prettyPrintProgram_2(): Unit = {
    val program: Program =
      Progr(
        List(Var("X")),
        List(
          Set(Var("Y"), Nl),
          While(
            VarExp("X"),
            List(
              Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
              While(
                VarExp("X"),
                List(
                  Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
                  Set(Var("X"), Tl(VarExp("X")))
                )
              ),
              Set(Var("X"), Tl(VarExp("X")))
            )
          )
        ),
        List(Var("Y"))
      );
    assertEquals(
      List(
        "read X",
        "%",
        "  Y := nil ;",
        "  while X do",
        "       Y := (cons (hd X) Y) ;",
        "       while X do",
        "            Y := (cons (hd X) Y) ;",
        "            X := (tl X)",
        "       od ;",
        "       X := (tl X)",
        "  od",
        "%",
        "write Y"
      ),
      prettyPrintProgram(program, is2)
    )
  }

  @Test
  def Test_prettyPrint_1(): Unit = {
    val program: Program =
      Progr(
        List(Var("X")),
        List(
          Set(Var("Y"), Nl),
          While(
            VarExp("X"),
            List(
              Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
              Set(Var("X"), Tl(VarExp("X")))
            )
          )
        ),
        List(Var("Y"))
      );
    assertEquals(
      "read X" + "\n" +
        "%" + "\n" +
        "  Y := nil ;" + "\n" +
        "  while X do" + "\n" +
        "       Y := (cons (hd X) Y) ;" + "\n" +
        "       X := (tl X)" + "\n" +
        "  od" + "\n" +
        "%" + "\n" +
        "write Y",
      prettyPrint(program, is2)
    )
  }

  @Test
  def Test_prettyPrint_2(): Unit = {
    val program: Program =
      Progr(
        List(Var("X")),
        List(
          Set(Var("Y"), Nl),
          While(
            VarExp("X"),
            List(
              Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
              Set(Var("X"), Tl(VarExp("X")))
            )
          )
        ),
        List(Var("Y"))
      );
    assertEquals(
      "read X" + "\n" +
        "%" + "\n" +
        " Y := nil ;" + "\n" +
        " while X do" + "\n" +
        "  Y := (cons (hd X) Y) ;" + "\n" +
        "  X := (tl X)" + "\n" +
        " od" + "\n" +
        "%" + "\n" +
        "write Y",
      prettyPrint(program, is0)
    )
  }
}
