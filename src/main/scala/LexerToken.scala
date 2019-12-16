
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

/**
  * Created by Jim on 13/10/2017.
  */

sealed trait LexerToken extends Positional
case class T_Def() extends LexerToken
case class T_If() extends LexerToken
case class T_Then() extends LexerToken
case class T_Else() extends LexerToken
case class T_While() extends LexerToken
case class T_Do() extends LexerToken
case class T_Repeat() extends LexerToken
case class T_Until() extends LexerToken
case class T_Identifier(var s: String) extends LexerToken
case class T_Integer(var s: String) extends LexerToken
case class T_SemiColon() extends LexerToken
case class T_LeftBracket() extends LexerToken
case class T_RightBracket() extends LexerToken
case class T_Equal() extends LexerToken
case class T_LessThan() extends LexerToken
case class T_LessThanEq() extends LexerToken
case class T_EqualComp() extends LexerToken
case class T_GreaterThanEq() extends LexerToken
case class T_GreaterThan() extends LexerToken
case class T_Comma() extends LexerToken
case class T_LeftCurlyBracket() extends LexerToken
case class T_RightCurlyBracket() extends LexerToken
case class T_Assign() extends LexerToken
case class T_Plus() extends LexerToken
case class T_Times() extends LexerToken
case class T_Minus() extends LexerToken
case class T_Div() extends LexerToken
case class T_LexerError(var msg: String) extends LexerToken
