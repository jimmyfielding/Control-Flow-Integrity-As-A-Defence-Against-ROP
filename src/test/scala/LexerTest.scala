import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.FlatSpec

/**
class SetSuite extends FunSuite {

  test("An empty Set should have size 0") {
    assert(Set.empty.size == 0)
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    assertThrows[NoSuchElementException] {
      Set.empty.head
    }
  }
}
*/

class LexerTest extends FlatSpec with Matchers {

	they should "tokenise Integer Literals correctly" in {
		Lexer("1")			should equal (List(T_Integer("1")))
		Lexer("123")		should equal (List(T_Integer("123")))
		Lexer ("0")			should equal (List(T_Integer("0")))
		Lexer("1e")			should equal (List(T_Integer("1"), T_Identifier("e")))
		Lexer("99")			should equal (List(T_Integer("99")))
		Lexer("e27")		should equal (List(T_Identifier("e27")))
		Lexer("1 2 3")	should equal (List(T_Integer("1"), T_Integer("2"), T_Integer("3")))
	}

	they should "tokenise Identifiers correctly" in {
		Lexer("a")			should equal (List(T_Identifier("a")))
		Lexer("myID")		should equal (List(T_Identifier("myID")))
		Lexer("a b c")	should equal (List(T_Identifier("a"),T_Identifier("b"), T_Identifier("c")))
		Lexer("10id")		should equal (List(T_Integer("10"), T_Identifier("id")))
	}

	they should "tokenise keywords correctly" in {
		Lexer("if")			should equal (List(T_If()))
		Lexer("then")		should equal (List(T_Then()))
		Lexer ("else")	should equal (List(T_Else()))
		Lexer("def")		should equal (List(T_Def()))
		Lexer("while")	should equal (List(T_While()))
		Lexer("do")			should equal (List(T_Do()))
		Lexer("repeat")	should equal (List(T_Repeat()))
		Lexer("until")	should equal (List(T_Until()))
	}

	they should "tokenise brackets correctly" in {
		Lexer("{")			should equal (List(T_LeftCurlyBracket()))
		Lexer("}")			should equal (List(T_RightCurlyBracket()))
		Lexer("(")			should equal (List(T_LeftBracket()))
		Lexer(")")			should equal (List(T_RightBracket()))
		Lexer("{}")			should equal (List(T_LeftCurlyBracket(), T_RightCurlyBracket()))
		Lexer("()")			should equal (List(T_LeftBracket(), T_RightBracket()))
	}

	they should "tokenise operators correctly" in {
		Lexer(",")			should equal (List(T_Comma()))
		Lexer(";")			should equal (List(T_SemiColon()))
		Lexer("=")			should equal (List(T_Equal()))
		Lexer(":=")			should equal (List(T_Assign()))
		Lexer("x y :=") should equal (List(T_Identifier("x"), T_Identifier("y"), T_Assign()))
	}

	they should "tokenise arithmetic operators correctly" in {
		Lexer("+")			should equal (List(T_Plus()))
		Lexer("-")			should equal (List(T_Minus()))
		Lexer("*")			should equal (List(T_Times()))
		Lexer("/")			should equal (List(T_Div()))
	}

	they should "tokenise boolean operators correctly" in {
		Lexer("<")			should equal (List(T_LessThan()))
		Lexer("<=")			should equal (List(T_LessThanEq()))
		Lexer("==")			should equal (List(T_EqualComp()))
		Lexer(">=")			should equal (List(T_GreaterThanEq()))
		Lexer(">")			should equal (List(T_GreaterThan()))
	}

	they should "produce errors where appropiate" in {
		Lexer("''")			should equal (List(T_LexerError("string matching regex `[0-9]+' expected but `'' found")))
		Lexer(":")			should equal (List(T_LexerError("string matching regex `[0-9]+' expected but `:' found")))
	}

}
