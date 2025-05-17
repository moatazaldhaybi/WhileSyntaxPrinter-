package pretty_printer

import org.junit.Test
import org.junit.Assert._

import pretty_printer.Prettyprinter._
class TestsStrings {


  val is2: IndentSpec = Map(IF -> 2, WHILE -> 4);
  @Test
  def Test_indentSearch_1(): Unit = {
    assertEquals(2, indentSearch(IF, is2))
  }

  @Test
  def Test_indentSearch_2(): Unit = {
    assertEquals(4, indentSearch(WHILE, is2))
  }

  @Test
  def Test_indentSearch_3(): Unit = {
    assertEquals(indentDefault, indentSearch(FOR, is2))
  }

  @Test
  def Test_makeIndent_1(): Unit = {
    assertEquals("", makeIndent(0))
  }

  @Test
  def Test_makeIndent_2(): Unit = {
    assertEquals("        ", makeIndent(8))
  }

  @Test
  def Test_appendStringBeforeAll_1(): Unit = {
    assertEquals(
      List("prefsuff1", "prefsuff2", "prefsuff3"),
      appendStringBeforeAll("pref", List("suff1", "suff2", "suff3"))
    )
  }

  @Test
  def Test_appendStringBeforeAll_2(): Unit = {
    assertEquals(
      List("prefsuff1"),
      appendStringBeforeAll("pref", List("suff1"))
    )
  }

  @Test
  def Test_appendStringAfterAll_1(): Unit = {
    assertEquals(
      List("pref1suff", "pref2suff", "pref3suff"),
      appendStringAfterAll("suff", List("pref1", "pref2", "pref3"))
    )
  }

  @Test
  def Test_appendStringAfterAll_2(): Unit = {
    assertEquals(List("pref1suff"), appendStringAfterAll("suff", List("pref1")))
  }

  @Test
  def Test_appendStringAfterAllButLast_1(): Unit = {
    assertEquals(
      List("pref1suff", "pref2suff", "pref3"),
      appendStringAfterAllButLast("suff", List("pref1", "pref2", "pref3"))
    )
  }

  @Test
  def Test_appendStringAfterAllButLast_2(): Unit = {
    assertEquals(
      List("pref1"),
      appendStringAfterAllButLast("suff", List("pref1"))
    )
  }

  @Test
  def Test_appendStringAfterLast_1(): Unit = {
    assertEquals(
      List("pref1", "pref2", "pref3suff"),
      appendStringAfterLast("suff", List("pref1", "pref2", "pref3"))
    )
  }

  @Test
  def Test_appendStringAfterLast_2(): Unit = {
    assertEquals(
      List("pref1suff"),
      appendStringAfterLast("suff", List("pref1"))
    )
  }

}
