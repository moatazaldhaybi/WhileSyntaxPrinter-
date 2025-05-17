package pretty_printer

import org.junit.Test
import org.junit.Assert._

import pretty_printer.Prettyprinter._

class TestsCommands {
  
  val is0: IndentSpec = Map.empty[Spec, Int];
  val is2: IndentSpec = Map(IF -> 2, WHILE -> 4);
  val is3: IndentSpec = Map(IF -> 2, FOR -> 4, WHILE -> 4);

  @Test
  def Test_prettyPrintCommand_Nop(): Unit = {
    assertEquals(List("nop"), prettyPrintCommand(Nop, is0))
  }

  @Test
  def Test_prettyPrintCommand_Set(): Unit = {
    assertEquals(
      List("X := (cons X Y)"),
      prettyPrintCommand(Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))), is0)
    )
  }

  @Test
  def Test_prettyPrintCommands_0(): Unit = {
    assertEquals(List("nop ;", "nop"), prettyPrintCommands(List(Nop, Nop), is0))
  }

  @Test
  def Test_prettyPrintCommands_1(): Unit = {
    assertEquals(List("nop"), prettyPrintCommands(List(Nop), is0))
  }

  @Test
  def Test_prettyPrintCommands_2(): Unit = {
    assertEquals(
      List("X := (cons X Y)"),
      prettyPrintCommands(
        List(Set(Var("X"), Cons(VarExp("X"), VarExp("Y")))),
        is0
      )
    )
  }

  @Test
  def Test_prettyPrintCommands_4(): Unit = {
    assertEquals(
      List("X := (cons X Y) ;", "Y := (cons Z Y)"),
      prettyPrintCommands(
        List(
          Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))),
          Set(Var("Y"), Cons(VarExp("Z"), VarExp("Y")))
        ),
        is0
      )
    )
  }

  @Test
  def Test_prettyPrintCommand_While_1(): Unit = {
    assertEquals(
      List("while X =? Y do", " X := (cons X Y)", "od"),
      prettyPrintCommand(
        While(
          Eq(VarExp("X"), VarExp("Y")),
          List(Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))))
        ),
        is0
      )
    )
  }

  @Test
  def Test_prettyPrintCommand_While_2(): Unit = {
    assertEquals(
      List("while X =? Y do", " X := (cons X Y) ;", " Y := (cons Z Y)", "od"),
      prettyPrintCommand(
        While(
          Eq(VarExp("X"), VarExp("Y")),
          List(
            Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))),
            Set(Var("Y"), Cons(VarExp("Z"), VarExp("Y")))
          )
        ),
        is0
      )
    )
  }

  @Test
  def Test_prettyPrintCommand_While_3(): Unit = {
    assertEquals(
      List("while X =? Y do", "    X := (cons X Y)", "od"),
      prettyPrintCommand(
        While(
          Eq(VarExp("X"), VarExp("Y")),
          List(Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))))
        ),
        is2
      )
    )
  }

  @Test
  def Test_prettyPrintCommand_While_4(): Unit = {
    assertEquals(
      List(
        "while X =? Y do",
        "    X := (cons X Y) ;",
        "    Y := (cons Z Y)",
        "od"
      ),
      prettyPrintCommand(
        While(
          Eq(VarExp("X"), VarExp("Y")),
          List(
            Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))),
            Set(Var("Y"), Cons(VarExp("Z"), VarExp("Y")))
          )
        ),
        is2
      )
    )
  }

  @Test
  def Test_prettyPrintCommand_For_1(): Unit = {
    assertEquals(
      List("for X do", " X := (cons X Y)", "od"),
      prettyPrintCommand(
        For(VarExp("X"), List(Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))))),
        is0
      )
    )
  }

  @Test
  def Test_prettyPrintCommand_For_2(): Unit = {
    assertEquals(
      List("for X do", " X := (cons X Y) ;", " Y := (cons Z Y)", "od"),
      prettyPrintCommand(
        For(
          VarExp("X"),
          List(
            Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))),
            Set(Var("Y"), Cons(VarExp("Z"), VarExp("Y")))
          )
        ),
        is0
      )
    )
  }

  @Test
  def Test_prettyPrintCommand_For_3(): Unit = {
    assertEquals(
      List("for X do", "    X := (cons X Y)", "od"),
      prettyPrintCommand(
        For(VarExp("X"), List(Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))))),
        is3
      )
    )
  }

  @Test
  def Test_prettyPrintCommand_For_4(): Unit = {
    assertEquals(
      List("for X do", "    X := (cons X Y) ;", "    Y := (cons Z Y)", "od"),
      prettyPrintCommand(
        For(
          VarExp("X"),
          List(
            Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))),
            Set(Var("Y"), Cons(VarExp("Z"), VarExp("Y")))
          )
        ),
        is3
      )
    )
  }

  @Test
  def Test_prettyPrintCommand_If_1(): Unit = {
    assertEquals(
      List(
        "if X =? Y then",
        " X := (cons X Y) ;",
        " Y := (cons Z Y)",
        "else",
        " nop",
        "fi"
      ),
      prettyPrintCommand(
        If(
          Eq(VarExp("X"), VarExp("Y")),
          List(
            Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))),
            Set(Var("Y"), Cons(VarExp("Z"), VarExp("Y")))
          ),
          List(Nop)
        ),
        is0
      )
    )
  }

  @Test
  def Test_prettyPrintCommand_If_2(): Unit = {
    assertEquals(
      List(
        "if X =? Y then",
        "  X := (cons X Y) ;",
        "  Y := (cons Z Y)",
        "else",
        "  nop",
        "fi"
      ),
      prettyPrintCommand(
        If(
          Eq(VarExp("X"), VarExp("Y")),
          List(
            Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))),
            Set(Var("Y"), Cons(VarExp("Z"), VarExp("Y")))
          ),
          List(Nop)
        ),
        is3
      )
    )
  }
}
