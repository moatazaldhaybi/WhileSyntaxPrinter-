//bINOME : ALDHAYBI Moataz ET BOUAMAMA ELYAS

package pretty_printer

import scala.util.Try

/** définition d'une exception pour le cas des listes vides de commandes
  */
case object ExceptionListeVide extends Exception

/** UN PRETTY-PRINTER POUR LE LANGAGE WHILE
  */
object Prettyprinter {

  /** définition d'un type pour les spécifications
    */
  sealed trait Spec
  case object WHILE extends Spec
  case object FOR extends Spec
  case object IF extends Spec
  case object PROGR extends Spec

  /** définition d'un type pour la table d'association des spécifications
    * d'indentation
    */
  type IndentSpec = Map[Spec, Int]

  /** définition d'une valeur d'indentation par défaut
    */
  val indentDefault: Int = 1

  /** TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
    */

  /** @param expression
    *   : un AST décrivant une expression du langage WHILE
    * @return
    *   une chaîne représentant la syntaxe concrète de l'expression
    */
  // TODO Projet 1
  def prettyPrintExpr(expression: Expression): String =
    expression match
      case Nl           => "nil"
      case Cst(name)    => name
      case VarExp(name) => name
      case Cons(arg1, arg2) =>
        "(cons " + prettyPrintExpr(arg1) + " " + prettyPrintExpr(arg2) + ")"
      case Hd(arg) => "(hd " + prettyPrintExpr(arg) + ")"
      case Tl(arg) => "(tl " + prettyPrintExpr(arg) + ")"
      case Eq(arg1, arg2) =>
        prettyPrintExpr(arg1) + " =? " + prettyPrintExpr(arg2)

  /** FONCTIONS AUXILIAIRES DE TRAITEMENT DE CHAINES POUR L'INDENTATION DES
    * COMMANDES OU LA PRESENTATION DU PROGRAMME
    */

  /** recherche d'une valeur d'indentation dans une une table d'association de
    * spécifications d'indentation
    *
    * @param context
    *   une chaîne de caractères décrivant un contexte d'indentation
    * @param is
    *   une table d'association de spécifications d'indentation, chaque
    *   spécification étant un couple (un contexte,une indentation) les
    *   contextes possibles seront les valeurs du type Spec
    * @return
    *   l'indentation correspondant à context
    */

  // TODO Projet 1
  def indentSearch(context: Spec, is: IndentSpec): Int =
    is.getOrElse(context, indentDefault)

  /** création d'une indentation
    *
    * @param n
    *   un nombre naturel
    * @return
    *   une chaîne de n espaces
    */
  // TODO Projet 1
  def makeIndent(n: Int): String =
    n match
      case 0 => ""
      case _ => " " + makeIndent(n - 1)

  /** ajout d'une chaîne devant chaque élément d'une liste non vide de chaînes
    *
    * @param pref
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de pref devant chaque
    *   élément de strings
    */
  // TODO Projet 1
  def appendStringBeforeAll(pref: String, strings: List[String]): List[String] =
    strings.map(s => pref + s)
    /*strings match
      case head :: Nil => pref + head :: Nil
      case head :: next => pref + head :: appendStringBeforeAll(pref,next)*/

  /** ajout d'une chaîne après chaque élément d'une liste non vide de chaînes
    *
    * @param suff
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de suff après chaque
    *   élément de strings
    */
  // TODO Projet 1
  def appendStringAfterAll(suff: String, strings: List[String]): List[String] =
    strings.map(s => s + suff)

  /** ajout d'une chaîne après le dernier élément d'une liste non vide de
    * chaînes
    *
    * @param suff
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de suff après le
    *   dernier élément de strings
    */
  // TODO Projet 1
  def appendStringAfterLast(suff: String, strings: List[String]): List[String] =
    strings match
      case head :: Nil  => head + suff :: Nil
      case head :: next => head :: appendStringAfterLast(suff, next)

  /** ajout d'une chaîne après chaque élément d'une liste non vide de chaînes
    * sauf le dernier
    *
    * @param suff
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de suff après chaque
    *   élément de strings sauf le dernier
    */
  // TODO Projet 1
  def appendStringAfterAllButLast(
      suff: String,
      strings: List[String]
  ): List[String] =
    strings match
      case head :: Nil => head :: Nil
      case head :: next =>
        head + suff :: appendStringAfterAllButLast(suff, next)

  /** TRAITEMENT DES COMMANDES DU LANGAGE WHILE
    */

  /** @param command
    *   : un AST décrivant une commande du langage WHILE
    * @param is
    *   : une table d'association de spécifications d'indentation
    * @return
    *   une liste de chaînes représentant la syntaxe concrète de la commande
    */
  // TODO Projet 1
  def prettyPrintCommand(command: Command, is: IndentSpec): List[String] = {
    command match {
      case Nop                  => List("nop")

      case Set(Var(name), expr) => List(s"$name := ${prettyPrintExpr(expr)}")

      case While(cond, body) =>
        val indent = makeIndent(indentSearch(WHILE, is))
        val bodyLines =
          appendStringBeforeAll(indent, prettyPrintCommands(body, is))
        ("while " + prettyPrintExpr(cond) + " do") :: bodyLines ::: List("od")

      case For(count, body) =>
        val indent = makeIndent(indentSearch(FOR, is))
        val bodyLines =
          appendStringBeforeAll(indent, prettyPrintCommands(body, is))
        ("for " + prettyPrintExpr(count) + " do") :: bodyLines ::: List("od")
        
      case If(cond, thenCmds, elseCmds) =>
        val indent = makeIndent(indentSearch(IF, is))
        val thenLines =
          appendStringBeforeAll(indent, prettyPrintCommands(thenCmds, is))
        val elseLines =
          appendStringBeforeAll(indent, prettyPrintCommands(elseCmds, is))
        ("if " + prettyPrintExpr(cond) + " then") :: thenLines ::: List(
          "else"
        ) ::: elseLines ::: List("fi")
    }
  }

  /** @param commands
    *   : une liste non vide d'AST décrivant une liste non vide de commandes du
    *   langage WHILE
    * @param is
    *   : une table d'association de spécifications d'indentation
    * @return
    *   une liste de chaînes représentant la syntaxe concrète de la liste de
    *   commandes
    */
  // TODO Projet 1
  def prettyPrintCommands(
      commands: List[Command],
      is: IndentSpec
  ): List[String] =
    commands match {
      case Nil => throw ExceptionListeVide
      case cmd :: Nil     => prettyPrintCommand(cmd, is)
      case cmd :: rest =>
        appendStringAfterLast(" ;", prettyPrintCommand(cmd, is)) ::: prettyPrintCommands(rest, is)

    }

  /** TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
    */

  /** @param vars
    *   : une liste non vide décrivant les paramètres d'entrée d'un programme du
    *   langage WHILE
    * @return
    *   une chaîne représentant la syntaxe concrète des paramètres d'entrée du
    *   programme
    */
  // TODO Projet 1
  def prettyPrintIn(vars: List[Variable]): String =
    vars match
      case Var(name) :: Nil  => name
      case Var(name) :: next => name + ", " + prettyPrintIn(next)

  /** @param vars
    *   : une liste non vide décrivant les paramètres de sortie d'un programme
    *   du langage WHILE
    * @return
    *   une chaîne représentant la syntaxe concrète des paramètres de sortie du
    *   programme
    */
  // TODO Projet 1
  def prettyPrintOut(vars: List[Variable]): String =
    prettyPrintIn(vars)

  /** @param program
    *   : un AST décrivant un programme du langage WHILE
    * @param is
    *   : une table d'association de spécifications d'indentation
    * @return
    *   une liste de chaînes représentant la syntaxe concrète du programme
    */
  // TODO Projet 1
  def prettyPrintProgram(program: Program, is: IndentSpec): List[String] = {
    val indent = makeIndent(indentSearch(PROGR, is))
    val input = "read " + prettyPrintIn(program.asInstanceOf[Progr].in)
    val body = appendStringBeforeAll(indent, prettyPrintCommands(program.asInstanceOf[Progr].body, is))
    val output = "write " + prettyPrintOut(program.asInstanceOf[Progr].out)

    // On assemble tout cela dans une liste de chaînes.
    List(input, "%") ++ body ++ List("%", output)
  }

  /** @param program
    *   : un AST décrivant un programme du langage WHILE
    * @param is
    *   : une table d'association de spécifications d'indentation
    * @return
    *   une chaîne représentant la syntaxe concrète du programme
    */
  // TODO TP2
  def prettyPrint(program: Program, is: IndentSpec): String =
    prettyPrintProgram(program, is).mkString("\n")

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

  val is: IndentSpec = Map(PROGR -> 2, WHILE -> 5);
  def main(args: Array[String]): Unit = {
    println("Bienvenue au projet pretty_printer.")
    // Décommentez la ligne ci-dessous lorsque prettyPrint sera définie.
    println(prettyPrint(program, is));
  }

}
