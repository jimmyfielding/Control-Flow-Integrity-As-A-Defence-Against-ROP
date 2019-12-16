
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

/**
	* Lexer for source language
	* Created by Jim on 13/10/2017.
	*/


object Lexer extends RegexParsers with CompilationError {

	override def skipWhitespace: Boolean = true

	override val whiteSpace = "[ \n\t\r\f]+".r

	/**
		*
		* @param s
		* @return
		*/
	def apply(s: String): List[LexerToken] = {
		parse(tokens, s) match {
			case NoSuccess(msg, next) => List(new T_LexerError(msg))
			case Success(result, next) => result
		}
	}

	/**
		*
		* @return
		*/
	def tokens: Parser[List[LexerToken]] = {
		phrase(rep1(semiColon ||| leftBracket ||| rightBracket ||| lessThan ||| lessThanEq ||| equalComp ||| greaterThanEq ||| greaterThan ||| comma ||| leftCurlyBracket ||| rightCurlyBracket |||
			equal ||| plus ||| times ||| minus ||| div ||| assign ||| `def` ||| `if` ||| `then` ||| `else` ||| `while` ||| `do` ||| repeat ||| until | identifier | intLiteral)) ^^ { rawTokens => processTokens(rawTokens) }
	}

	/**
		*
		*
		* @param allTokens
		* @return
		*/
	def processTokens(allTokens: List[LexerToken]): List[LexerToken] = {
		return allTokens
	}

	/**
		*
		* @return
		*/
	def `def` = "def" ^^ (_ => T_Def())

	/**
		*
		* @return
		*/
	def `if` = "if" ^^ (_ => T_If())

	/**
		*
		* @return
		*/
	def `then` = "then" ^^ (_ => T_Then())

	/**
		*
		* @return
		*/
	def `else` = "else" ^^ (_ => T_Else())

	/**
		*
		* @return
		*/
	def `while` = "while" ^^ (_ => T_While())

	/**
		*
		* @return
		*/
	def `do` = "do" ^^ (_ => T_Do())

	/**
		*
		* @return
		*/
	def repeat = "repeat" ^^ (_ => T_Repeat())

	/**
		*
		* @return
		*/
	def until = "until" ^^ (_ => T_Until())

	/**
		*
		* @return
		*/
	def semiColon = ";" ^^ (_ => T_SemiColon())

	/**
		*
		* @return
		*/
	def leftBracket = "(" ^^ (_ => T_LeftBracket())

	/**
		*
		* @return
		*/
	def rightBracket = ")" ^^ (_ => T_RightBracket())

	/**
		*
		* @return
		*/
	def equal = "=" ^^ (_ => T_Equal())

	/**
		*
		* @return
		*/
	def lessThan = "<" ^^ (_ => T_LessThan())

	/**
		*
		* @return
		*/
	def lessThanEq = "<=" ^^ (_ => T_LessThanEq())

	/**
		*
		* @return
		*/
	def equalComp = "==" ^^ (_ => T_EqualComp())

	/**
		*
		* @return
		*/
	def greaterThanEq = ">=" ^^ (_ => T_GreaterThanEq())

	/**
		*
		* @return
		*/
	def greaterThan = ">" ^^ (_ => T_GreaterThan())

	/**
		*
		* @return
		*/
	def comma = "," ^^ (_ => T_Comma())

	/**
		*
		* @return
		*/
	def leftCurlyBracket = "{" ^^ (_ => T_LeftCurlyBracket())

	/**
		*
		* @return
		*/
	def rightCurlyBracket = "}" ^^ (_ => T_RightCurlyBracket())

	/**
		*
		* @return
		*/
	def assign = ":=" ^^ (_ => T_Assign())

	/**
		*
		* @return
		*/
	def plus = "+" ^^ (_ => T_Plus())

	/**
		*
		* @return
		*/
	def times = "*" ^^ (_ => T_Times())

	/**
		*
		* @return
		*/
	def minus = "-" ^^ (_ => T_Minus())

	/**
		*
		* @return
		*/
	def div = "/" ^^ (_ => T_Div())

	/**
		*
		* @return
		*/
	def identifier: Parser[T_Identifier] = {
		"[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { s => T_Identifier(s) }
	}

	/**
		*
		* @return
		*/
	def intLiteral: Parser[T_Integer] = {
		"[0-9]+".r ^^ { s => T_Integer(s) }
	}

}
