package pretty_printer

/** Définitions des types pour la construction d'un arbre de syntaxe abstraite
  * (AST)
  *   - le type Variable décrit les identificateurs pouvant être des paramètres
  *     ou des variables locales
  *   - le type Expression décrit les différentes formes d'expression du langage
  *     WHILE
  *   - le type Command décrit les différentes commandes du langage WHILE
  *   - le type Program décrit la structure d'un programme WHILE
  */

sealed trait Variable
case class Var(name: String) extends Variable

sealed trait Expression
case object Nl extends Expression
case class Cst(name: String) extends Expression
case class VarExp(name: String) extends Expression
case class Cons(arg1: Expression, arg2: Expression) extends Expression
case class Hd(arg: Expression) extends Expression
case class Tl(arg: Expression) extends Expression
case class Eq(arg1: Expression, arg2: Expression) extends Expression

sealed trait Command
case object Nop extends Command
case class Set(variable: Variable, expression: Expression) extends Command
case class While(condition: Expression, body: List[Command]) extends Command
case class For(count: Expression, body: List[Command]) extends Command
case class If(
    condition: Expression,
    then_commands: List[Command],
    else_commands: List[Command]
) extends Command

sealed trait Program
case class Progr(in: List[Variable], body: List[Command], out: List[Variable])
    extends Program
